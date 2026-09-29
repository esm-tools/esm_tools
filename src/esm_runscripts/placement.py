"""
Rank placement for the ``taskset`` launcher.

With ``computer.taskset: true`` every MPI rank is placed through two pieces:
``SLURM_HOSTFILE`` (one node name per global rank) and ``prog_<model>.sh``
(pins the rank's threads with ``taskset``). Both read the table computed here,
so the node a rank lands on and the cores it is pinned to always agree.

The table is one ``(node_index, first_core)`` pair per global rank, in rank
order. Ranks keep the component order of ``valid_model_names`` (that is what
``--multi-prog`` and the component communicators rely on); only the physical
placement is decided here.

Placement rules
---------------
- Components are laid out one after another with a running core counter,
  as the original taskset launcher did. A component starts on the next free
  core, so it shares a node with the one before it where they meet.
- Every rank starts on a multiple of its own thread count and never crosses a
  node boundary. With thread counts of 1, 2, 4 or 8 this keeps each rank inside
  one L3 group on AMD EPYC nodes.
- A component can be interleaved into other components' nodes::

      xios:
          interleave_into: [oifs]
          ranks_per_node: 2

  When a host component (here ``oifs``) opens a node, the end of that node is
  reserved for the guest's next ``ranks_per_node`` ranks, and the host fills the
  cores before it. Nodes are reserved only while the guest still has ranks
  left, so once all guest ranks are placed the host's remaining nodes are its
  own again. With several hosts, a guest takes its slots from the host that
  comes first in rank order and moves on to the next when that one ends.
  A host node that carries guests gets fewer host ranks, which node-aware
  partitions (e.g. FESOM's ``n_part``) have to match.
"""

from collections import defaultdict

from esm_tools import user_error


def _components(config):
    """Components that own MPI ranks, in rank order, as (name, tasks, omp)."""
    comps = []
    for model in config["general"]["valid_model_names"]:
        if "tasks" not in config[model]:
            continue
        if not (
            config[model].get("execution_command") or config[model].get("executable")
        ):
            continue
        comps.append(
            (
                model,
                int(config[model]["tasks"]),
                int(config[model].get("omp_num_threads", 1)),
            )
        )
    return comps


def _guests(config, comps):
    """``{guest: (hosts, ranks_per_node)}`` for interleaved components, checked."""
    names = [c[0] for c in comps]
    guests = {}
    for name, tasks, omp in comps:
        hosts = config[name].get("interleave_into")
        if not hosts:
            continue
        if isinstance(hosts, str):
            hosts = [hosts]
        for host in hosts:
            if host not in names:
                user_error(
                    "taskset placement",
                    f"``{name}.interleave_into`` names ``{host}``, which is not a "
                    f"component with MPI ranks in this run ({', '.join(names)}).",
                )
            if config[host].get("interleave_into"):
                user_error(
                    "taskset placement",
                    f"``{host}`` is itself interleaved and cannot host ``{name}``.",
                )
        guests[name] = (hosts, int(config[name].get("ranks_per_node", 1)))
    return guests


def compute_placement(config, cores_per_node):
    """
    Returns ``(table, nodes)``: ``table[rank] = (node_index, first_core)`` for
    every global rank, and the number of nodes the job needs.
    """
    comps = _components(config)
    guests = _guests(config, comps)
    tasks_of = {name: tasks for name, tasks, omp in comps}
    omp_of = {name: omp for name, tasks, omp in comps}
    for name, omp in omp_of.items():
        if omp > cores_per_node:
            user_error(
                "taskset placement",
                f"``{name}`` ranks have {omp} threads, more than the {cores_per_node} "
                "cores of a node.",
            )

    first_rank = {}
    rank = 0
    for name, tasks, omp in comps:
        first_rank[name] = rank
        rank += tasks
    table = [None] * rank

    busy = defaultdict(set)  # node -> cores already taken
    placed = {name: 0 for name in guests}  # guest ranks placed so far

    def free(node, core, omp):
        return all(c not in busy[node] for c in range(core, core + omp))

    def take(node, core, omp):
        busy[node].update(range(core, core + omp))

    def open_host_node(host, node):
        """Reserve the end of ``node`` for the guests of ``host`` that still need slots."""
        pending = [
            g
            for g, (hosts, rpn) in guests.items()
            if host in hosts and placed[g] < tasks_of[g]
        ]
        # widest ranks first, so that every block stays aligned to its thread count
        pending.sort(key=lambda g: -omp_of[g])
        top = cores_per_node
        for g in pending:
            rpn = guests[g][1]
            omp = omp_of[g]
            for _ in range(min(rpn, tasks_of[g] - placed[g])):
                core = (top - omp) // omp * omp
                while core >= 0 and not free(node, core, omp):
                    core -= omp
                if core < 0:
                    user_error(
                        "taskset placement",
                        f"No room for a ``{g}`` rank ({omp} threads) on a ``{host}`` "
                        f"node. Lower ``{g}.ranks_per_node``.",
                    )
                take(node, core, omp)
                table[first_rank[g] + placed[g]] = (node, core)
                placed[g] += 1
                top = core

    cursor = 0  # global core index: node * cores_per_node + core
    for name, tasks, omp in comps:
        if name in guests:
            continue
        is_host = any(name in hosts for hosts, rpn in guests.values())
        opened = set()
        for r in range(tasks):
            while True:
                core = -(-cursor // omp) * omp
                node, offset = divmod(core, cores_per_node)
                if offset + omp > cores_per_node:
                    cursor = (node + 1) * cores_per_node
                    continue
                if is_host and node not in opened:
                    opened.add(node)
                    open_host_node(name, node)
                if not free(node, offset, omp):
                    cursor = core + omp
                    continue
                break
            take(node, offset, omp)
            table[first_rank[name] + r] = (node, offset)
            cursor = core + omp

    for g, (hosts, rpn) in guests.items():
        if placed[g] < tasks_of[g]:
            user_error(
                "taskset placement",
                f"``{g}`` has {tasks_of[g]} ranks, but its hosts ({', '.join(hosts)}) "
                f"only had room for {placed[g]} at {rpn} per node. Raise "
                f"``{g}.ranks_per_node`` or add a host.",
            )

    nodes = max(node for node, _ in table) + 1 if table else 0
    return table, nodes


def write_placement(table, path):
    """One ``node_index first_core`` line per global rank."""
    with open(path, "w") as f:
        for node, first_core in table:
            f.write(f"{node} {first_core}\n")
