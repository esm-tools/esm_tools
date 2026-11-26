"""
Tests for ``esm_runscripts.placement``, the rank placement of the ``taskset`` launcher.

Each table entry is ``(node_index, first_core)`` for one global rank, in rank order.
"""

import pytest

from esm_runscripts import placement


def make_config(*components):
    """
    Builds a minimal config from ``(name, tasks, omp_num_threads, extra)`` tuples, in
    rank order. ``extra`` is a dict of additional component keys (or ``None``).
    """
    config = {"general": {"valid_model_names": []}}
    for name, tasks, omp, extra in components:
        config["general"]["valid_model_names"].append(name)
        config[name] = {
            "tasks": tasks,
            "omp_num_threads": omp,
            "execution_command": f"{name}.x",
        }
        config[name].update(extra or {})
    return config


# Packed placement (no interleaving, no ranks_per_node)
# -----------------------------------------------------


def test_packed_awicm3_like_layout():
    config = make_config(
        ("fesom", 130, 1, None),
        ("oifs", 12, 8, None),
        ("rnfmap", 1, 1, None),
        ("xios", 1, 1, None),
    )
    table, nodes = placement.compute_placement(config, 128)

    assert len(table) == 144
    # fesom fills node 0 and the first 2 cores of node 1
    assert table[:128] == [(0, core) for core in range(128)]
    assert table[128:130] == [(1, 0), (1, 1)]
    # oifs starts at the next core aligned to its 8 threads, sharing node 1
    assert table[130:142] == [(1, 8 + 8 * r) for r in range(12)]
    # rnfmap and xios continue right after oifs
    assert table[142] == (1, 104)
    assert table[143] == (1, 105)
    assert nodes == 2


def test_packed_component_starts_aligned_in_shared_node():
    config = make_config(("a", 3, 1, None), ("b", 2, 8, None))
    table, nodes = placement.compute_placement(config, 16)
    assert table == [(0, 0), (0, 1), (0, 2), (0, 8), (1, 0)]
    assert nodes == 2


def test_packed_rank_never_crosses_a_node():
    # 8-thread ranks on 12-core nodes: only one fits per node
    config = make_config(("a", 1, 1, None), ("b", 3, 8, None))
    table, nodes = placement.compute_placement(config, 12)
    b_ranks = table[1:]
    assert [node for node, core in b_ranks] == [1, 2, 3]
    assert all(core + 8 <= 12 for node, core in b_ranks)
    assert nodes == 4


def test_components_without_tasks_or_executable_are_skipped():
    config = make_config(("fesom", 2, 1, None), ("oifs", 2, 1, None))
    config["general"]["valid_model_names"].insert(1, "oasis3mct")
    config["oasis3mct"] = {}
    config["general"]["valid_model_names"].append("lpj")
    config["lpj"] = {"tasks": 4}
    table, nodes = placement.compute_placement(config, 8)
    assert table == [(0, 0), (0, 1), (0, 2), (0, 3)]
    assert nodes == 1


def test_empty_config():
    config = make_config()
    assert placement.compute_placement(config, 8) == ([], 0)


# Interleaving (interleave_into + ranks_per_node)
# -----------------------------------------------


def test_interleave_guest_at_end_of_host_nodes():
    config = make_config(
        ("oifs", 10, 4, None),
        ("xios", 3, 1, {"interleave_into": ["oifs"], "ranks_per_node": 2}),
    )
    table, nodes = placement.compute_placement(config, 16)

    oifs, xios = table[:10], table[10:]
    # 2 xios ranks at the end of node 0, the last one at the end of node 1
    assert xios == [(0, 15), (0, 14), (1, 15)]
    # oifs gets 3 ranks on nodes 0 and 1 (no room for a 4th), and 4 on node 2,
    # where no xios slots are reserved anymore
    assert oifs == [
        (0, 0), (0, 4), (0, 8),
        (1, 0), (1, 4), (1, 8),
        (2, 0), (2, 4), (2, 8), (2, 12),
    ]
    assert nodes == 3


def test_interleave_into_as_string():
    config = make_config(
        ("oifs", 2, 4, None),
        ("xios", 1, 1, {"interleave_into": "oifs", "ranks_per_node": 1}),
    )
    table, _ = placement.compute_placement(config, 8)
    assert table == [(0, 0), (1, 0), (0, 7)]


def test_interleave_guest_moves_to_next_host():
    config = make_config(
        ("a", 2, 4, None),
        ("b", 2, 4, None),
        ("g", 3, 1, {"interleave_into": ["a", "b"], "ranks_per_node": 1}),
    )
    table, nodes = placement.compute_placement(config, 8)
    a, b, g = table[:2], table[2:4], table[4:]
    assert a == [(0, 0), (1, 0)]
    # ``a`` ends on node 1, so ``b`` reserves the last guest slot there
    assert g == [(0, 7), (1, 7), (1, 6)]
    assert b == [(2, 0), (2, 4)]
    assert nodes == 3


def test_interleave_widest_guest_reserved_first():
    config = make_config(
        ("host", 1, 4, None),
        ("narrow", 1, 1, {"interleave_into": ["host"], "ranks_per_node": 1}),
        ("wide", 1, 2, {"interleave_into": ["host"], "ranks_per_node": 1}),
    )
    table, _ = placement.compute_placement(config, 8)
    host, narrow, wide = table
    assert wide == (0, 6)
    assert narrow == (0, 5)
    assert host == (0, 0)


# ranks_per_node for components that are not interleaved
# -------------------------------------------------------


def test_ranks_per_node_spreads_ranks_evenly():
    config = make_config(
        ("rnfmap", 1, 1, None),
        ("fesom", 200, 1, {"ranks_per_node": 96}),
        ("oifs", 2, 8, None),
    )
    table, nodes = placement.compute_placement(config, 128)

    rnfmap, fesom, oifs = table[:1], table[1:201], table[201:]
    assert rnfmap == [(0, 0)]
    # fesom starts on a fresh node and uses 3 of every 4 cores
    assert fesom[:6] == [(1, 0), (1, 1), (1, 2), (1, 4), (1, 5), (1, 6)]
    assert fesom[95] == (1, 126)
    assert fesom[96] == (2, 0)
    assert fesom[199] == (3, 9)
    # the next component starts on the next node
    assert oifs == [(4, 0), (4, 8)]
    assert nodes == 5


def test_ranks_per_node_aligned_to_threads():
    config = make_config(("c", 4, 2, {"ranks_per_node": 3}))
    table, _ = placement.compute_placement(config, 16)
    assert table == [(0, 0), (0, 4), (0, 10), (1, 0)]


# Realistic layout: AWIESM3 v3.4.2 TCO319L137-DARS2 on Levante
# -------------------------------------------------------------
# From ``runscripts/awiesm3/v3.4.2/awiesm3-v3.4.2-levante-TCO319L137-DARS2_2y.yaml``
# (branch ``feat/awiesm3-v3.4-co2``), with the component order of
# ``general.valid_model_names`` and ``rnfmap`` from ``configs/setups/awiesm3``.
# ``oasis3mct`` owns no MPI ranks. Levante nodes have 128 cores.

LEVANTE_CORES = 128


def awiesm3_tco319_config(fesom_extra=None, xios_extra=None):
    config = make_config(
        ("fesom", 5120, 2, fesom_extra),
        ("oifs", 640, 8, None),
        ("rnfmap", 1, LEVANTE_CORES, None),
        ("lpj_guess", 64, 4, None),
        ("xios", 4, 32, xios_extra),
    )
    config["general"]["valid_model_names"].insert(4, "oasis3mct")
    config["oasis3mct"] = {}
    return config


def nodes_by_component(config, table):
    """``{component: {node: ranks}}`` from a placement table."""
    layout = {}
    rank = 0
    for name in config["general"]["valid_model_names"]:
        if "tasks" not in config[name]:
            continue
        nodes = layout.setdefault(name, {})
        for node, first_core in table[rank : rank + config[name]["tasks"]]:
            nodes[node] = nodes.get(node, 0) + 1
        rank += config[name]["tasks"]
    return layout


def assert_no_core_used_twice(config, table):
    used = set()
    rank = 0
    for name in config["general"]["valid_model_names"]:
        if "tasks" not in config[name]:
            continue
        omp = config[name]["omp_num_threads"]
        for node, first_core in table[rank : rank + config[name]["tasks"]]:
            assert first_core + omp <= LEVANTE_CORES
            cores = {(node, core) for core in range(first_core, first_core + omp)}
            assert not used & cores
            used |= cores
        rank += config[name]["tasks"]


def test_awiesm3_tco319_runscript_layout():
    config = awiesm3_tco319_config()
    table, nodes = placement.compute_placement(config, LEVANTE_CORES)
    layout = nodes_by_component(config, table)

    assert len(table) == 5120 + 640 + 1 + 64 + 4
    # Every component fills whole nodes, one after another
    assert layout["fesom"] == {node: 64 for node in range(0, 80)}
    assert layout["oifs"] == {node: 16 for node in range(80, 120)}
    assert layout["rnfmap"] == {120: 1}
    assert layout["lpj_guess"] == {121: 32, 122: 32}
    assert layout["xios"] == {123: 4}
    assert nodes == 124
    # Ranks are packed and aligned to their threads
    assert table[:3] == [(0, 0), (0, 2), (0, 4)]
    assert table[5120:5122] == [(80, 0), (80, 8)]
    assert table[-4:] == [(123, 0), (123, 32), (123, 64), (123, 96)]
    assert_no_core_used_twice(config, table)


def test_awiesm3_tco319_fesom_ranks_per_node():
    config = awiesm3_tco319_config(fesom_extra={"ranks_per_node": 48})
    table, nodes = placement.compute_placement(config, LEVANTE_CORES)
    layout = nodes_by_component(config, table)

    # 48 FESOM ranks per node (106 full nodes and 32 ranks on the last one)
    assert layout["fesom"] == {**{node: 48 for node in range(106)}, 106: 32}
    # Three of every four 2-core pairs used
    assert [core for node, core in table[:6]] == [0, 2, 4, 8, 10, 12]
    # The next components start on the next node
    assert layout["oifs"] == {node: 16 for node in range(107, 147)}
    assert layout["rnfmap"] == {147: 1}
    assert layout["lpj_guess"] == {148: 32, 149: 32}
    assert layout["xios"] == {150: 4}
    assert nodes == 151
    assert_no_core_used_twice(config, table)


def test_awiesm3_tco319_xios_interleaved_into_oifs():
    config = awiesm3_tco319_config(
        xios_extra={"interleave_into": ["oifs"], "ranks_per_node": 1}
    )
    table, nodes = placement.compute_placement(config, LEVANTE_CORES)
    layout = nodes_by_component(config, table)

    assert layout["fesom"] == {node: 64 for node in range(0, 80)}
    # One XIOS server at the end (cores 96-127) of the first 4 OpenIFS nodes
    assert table[-4:] == [(80, 96), (81, 96), (82, 96), (83, 96)]
    # OpenIFS gets 12 ranks on those nodes and 16 on the other 37
    assert layout["oifs"] == {
        **{node: 12 for node in range(80, 84)},
        **{node: 16 for node in range(84, 121)},
    }
    # No node of its own for XIOS
    assert layout["rnfmap"] == {121: 1}
    assert layout["lpj_guess"] == {122: 32, 123: 32}
    assert nodes == 124
    assert_no_core_used_twice(config, table)


# Errors
# ------


@pytest.mark.parametrize(
    "components, cores_per_node",
    [
        # unknown host
        ([("a", 2, 1, None), ("g", 1, 1, {"interleave_into": ["nope"]})], 8),
        # a guest cannot host another guest
        (
            [
                ("a", 2, 1, None),
                ("b", 1, 1, {"interleave_into": ["a"]}),
                ("g", 1, 1, {"interleave_into": ["b"]}),
            ],
            8,
        ),
        # more threads than cores
        ([("a", 1, 16, None)], 8),
        # a host cannot also set ranks_per_node
        (
            [
                ("a", 2, 1, {"ranks_per_node": 2}),
                ("g", 1, 1, {"interleave_into": ["a"]}),
            ],
            8,
        ),
        # ranks_per_node too wide for a node
        ([("a", 10, 2, {"ranks_per_node": 10})], 16),
        # no room for the guest slots on a host node
        (
            [
                ("a", 1, 1, None),
                ("g", 3, 2, {"interleave_into": ["a"], "ranks_per_node": 3}),
            ],
            4,
        ),
        # not enough host nodes for all guest ranks
        (
            [
                ("a", 1, 1, None),
                ("g", 3, 1, {"interleave_into": ["a"], "ranks_per_node": 1}),
            ],
            8,
        ),
    ],
    ids=[
        "unknown_host",
        "guest_as_host",
        "too_many_threads",
        "host_with_ranks_per_node",
        "ranks_per_node_too_wide",
        "no_room_for_guest",
        "not_enough_guest_slots",
    ],
)
def test_invalid_layouts_stop(components, cores_per_node):
    config = make_config(*components)
    with pytest.raises(SystemExit):
        placement.compute_placement(config, cores_per_node)


# File
# ----


def test_write_placement(tmp_path):
    path = tmp_path / placement.PLACEMENT_FILE
    placement.write_placement([(0, 0), (0, 8), (1, 0)], str(path))
    assert path.read_text() == "0 0\n0 8\n1 0\n"
