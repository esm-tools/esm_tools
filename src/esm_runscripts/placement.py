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
  as the original taskset launcher did.
- Every rank starts on a multiple of its own thread count and never crosses a
  node boundary. With thread counts of 1, 2, 4 or 8 this keeps each rank inside
  one L3 group on AMD EPYC nodes.
- A component can be interleaved into other components' nodes::

      xios:
          interleave_into: [fesom, oifs]
          ranks_per_node: 1

  Each node of the listed host components then reserves ``ranks_per_node *
  omp_num_threads`` cores at its end for the guest, the hosts start on a fresh
  node and fill the remaining cores, and the guest's ranks go into the reserved
  cores, host node by host node. The host therefore gets fewer ranks per node,
  which node-aware partitions (e.g. FESOM's ``n_part``) have to match.
"""

from loguru import logger

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


def _aligned(core, omp, cores_per_node):
    """First core >= ``core`` that is a multiple of ``omp`` and keeps the rank on one node."""
    if omp > cores_per_node:
        user_error(
            "taskset placement",
            f"A rank with {omp} threads does not fit on a node with {cores_per_node} cores.",
        )
    core = -(-core // omp) * omp
    if core % cores_per_node + omp > cores_per_node:
        core = -(-core // cores_per_node) * cores_per_node
    return core


def compute_placement(config, cores_per_node):
    """
    Returns ``(table, nodes)``: ``table[rank] = (node_index, first_core)`` for
    every global rank, and the number of nodes the job needs.
    """
    comps = _components(config)
    names = [c[0] for c in comps]
    first_rank = {}
    rank = 0
    for name, tasks, omp in comps:
        first_rank[name] = rank
        rank += tasks
    table = [None] * rank

    # Guests and the cores each host node reserves for them
    guests = {}
    reserve = {}
    for name, tasks, omp in comps:
        hosts = config[name].get("interleave_into")
        if not hosts:
            continue
        if isinstance(hosts, str):
            hosts = [hosts]
        rpn = int(config[name].get("ranks_per_node", 1))
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
            reserve[host] = reserve.get(host, 0) + rpn * omp
        guests[name] = (hosts, rpn, omp)

    # Hosts and plain components, in rank order, with a running core counter
    host_nodes = {}
    core = 0
    for name, tasks, omp in comps:
        if name in guests:
            continue
        if name in reserve:
            # a host starts on a fresh node and leaves ``reserve`` cores free on each
            core = -(-core // cores_per_node) * cores_per_node
            usable = cores_per_node - reserve[name]
            if usable < omp:
                user_error(
                    "taskset placement",
                    f"Interleaving leaves {usable} cores per ``{name}`` node, "
                    f"less than one ``{name}`` rank ({omp} threads).",
                )
            host_nodes[name] = []
            for r in range(tasks):
                core = _aligned(core, omp, cores_per_node)
                if core % cores_per_node + omp > usable:
                    core = -(-core // cores_per_node) * cores_per_node
                node = core // cores_per_node
                if not host_nodes[name] or host_nodes[name][-1] != node:
                    host_nodes[name].append(node)
                table[first_rank[name] + r] = (node, core % cores_per_node)
                core += omp
            # the reserved cores of the last host node stay reserved
            core = -(-core // cores_per_node) * cores_per_node
        else:
            for r in range(tasks):
                core = _aligned(core, omp, cores_per_node)
                table[first_rank[name] + r] = (core // cores_per_node, core % cores_per_node)
                core += omp

    # Guests into the reserved cores of their hosts' nodes
    used = {}  # node -> cores already taken in its reserved block
    for name, (hosts, rpn, omp) in guests.items():
        slots = []
        for host in hosts:
            start = cores_per_node - reserve[host]
            for node in host_nodes[host]:
                for k in range(rpn):
                    offset = start + used.get(node, 0)
                    if offset % omp:
                        user_error(
                            "taskset placement",
                            f"``{name}`` ranks would start on core {offset} of a "
                            f"``{host}`` node, which is not a multiple of its "
                            f"{omp} threads. Choose ``ranks_per_node`` and thread "
                            "counts whose reserved blocks stay aligned.",
                        )
                    slots.append((node, offset))
                    used[node] = used.get(node, 0) + omp
        tasks = [t for n, t, o in comps if n == name][0]
        if tasks > len(slots):
            user_error(
                "taskset placement",
                f"``{name}`` has {tasks} ranks but its hosts ({', '.join(hosts)}) "
                f"offer {len(slots)} slots ({rpn} per node on "
                f"{sum(len(host_nodes[h]) for h in hosts)} nodes). Raise "
                f"``{name}.ranks_per_node`` or add a host.",
            )
        if tasks < len(slots):
            logger.warning(
                f"taskset placement: {len(slots) - tasks} reserved ``{name}`` slots "
                "stay empty."
            )
        for r in range(tasks):
            table[first_rank[name] + r] = slots[r]

    nodes = max(node for node, _ in table) + 1 if table else 0
    return table, nodes


def write_placement(table, path):
    """One ``node_index first_core`` line per global rank."""
    with open(path, "w") as f:
        for node, first_core in table:
            f.write(f"{node} {first_core}\n")
