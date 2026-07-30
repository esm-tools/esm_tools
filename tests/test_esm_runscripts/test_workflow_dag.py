"""DAG-invariance tests for the workflow ordering vocabulary.

The workflow-ordering rename (run_after/run_before -> preceded_by) must NOT
change the shape of the assembled execution graph. These tests pin the DAG
(`next_submit` edges + entry/exit points) produced by
``complete_plans`` -> ``order_plans`` and assert it is identical whether the
ordering is expressed in the legacy keys or the new ``preceded_by`` key.

Characterization tests (legacy keys) lock the current shape and must stay green
through the refactor. The ``preceded_by`` tests drive the new behaviour.
"""

import copy
import importlib.util
import pathlib
import sys
import warnings

import deprecation
import pytest

# Load workflow.py directly from *this* branch. Importing the esm_runscripts
# package would pull in dask/distributed (arch-broken in this env) and may
# resolve to a different checkout; loading the module by path with this
# branch's src/ on sys.path keeps the test pinned to the code under review.
_REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO / "src"))
_spec = importlib.util.spec_from_file_location(
    "wf_under_test", _REPO / "src/esm_runscripts/workflow.py"
)
workflow = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(workflow)


def build_dag(sub_plans, **gw_fields):
    """Run the DAG builders on a minimal workflow dict; return its shape.

    ``gw_fields`` carries the workflow-level entry/exit keys in whichever
    vocabulary the test uses (``first_task_in_queue``/``last_task_in_queue`` or
    ``entry_point``/``exit_point``). The returned shape (``next_submit`` edges,
    ``called_from`` back-pointers, and the entry/exit plan *names*) is
    vocabulary-independent, so the same golden DAG can be asserted for both.
    """
    gw = {"plans": {}, "sub_plans": copy.deepcopy(sub_plans)}
    gw.update(gw_fields)
    config = {"general": {"workflow": gw}}
    workflow.translate_legacy_workflow_keys(config)
    workflow.complete_plans(config)
    workflow.order_plans(config)
    wf = config["general"]["workflow"]
    plans = wf["plans"]
    return {
        "next_submit": {p: plans[p].get("next_submit") for p in sorted(plans)},
        "called_from": {p: plans[p].get("called_from") for p in sorted(plans)},
        "first": wf.get("entry_point", wf.get("first_task_in_queue")),
        "last": wf.get("exit_point", wf.get("last_task_in_queue")),
    }


# The canonical default chain: prepcompute -> compute -> tidy, each its own
# plan (1:1 default clustering).
DEFAULT_CHAIN_LEGACY = {
    "prepcompute": {"parent_plan": "prepcompute", "run_before": "compute"},
    "compute": {"parent_plan": "compute", "run_before": "tidy"},
    "tidy": {"parent_plan": "tidy", "run_after": "compute"},
}

# The golden DAG the default chain must always assemble to.
GOLDEN_DEFAULT_DAG = {
    "next_submit": {
        "prepcompute": ["compute"],
        "compute": ["tidy"],
        "tidy": ["prepcompute"],  # loop-back edge closes the run cycle
    },
    "called_from": {
        "prepcompute": "tidy",
        "compute": "prepcompute",
        "tidy": "compute",
    },
    "first": "prepcompute",
    "last": "tidy",
}


# Same chain, expressed purely in the new single ordering property.
DEFAULT_CHAIN_NEW = {
    "prepcompute": {"parent_plan": "prepcompute"},  # entry: no preceded_by
    "compute": {"parent_plan": "compute", "preceded_by": "prepcompute"},
    "tidy": {"parent_plan": "tidy", "preceded_by": "compute"},
}


def test_default_chain_legacy_keys_assembles_golden_dag():
    """Legacy run_after/run_before build the canonical prepcompute->compute->tidy DAG.

    Characterization: locks the current DAG shape. Must stay green through the
    rename so the graph provably cannot change.
    """
    dag = build_dag(
        DEFAULT_CHAIN_LEGACY,
        first_task_in_queue="prepcompute",
        last_task_in_queue="tidy",
    )
    assert dag == GOLDEN_DEFAULT_DAG


def test_default_chain_preceded_by_assembles_same_dag():
    """The new preceded_by vocabulary builds the identical DAG (drives the rename)."""
    dag = build_dag(
        DEFAULT_CHAIN_NEW,
        entry_point="prepcompute",
        exit_point="tidy",
    )
    assert dag == GOLDEN_DEFAULT_DAG


# Coupled chain: couple_in runs before prepcompute (bumps the entry point),
# couple_out runs after tidy (bumps the exit point). Exercises the run_before
# flip and both entry/exit adjustments.
COUPLED_LEGACY = {
    "couple_in": {"parent_plan": "couple_in", "run_before": "prepcompute"},
    "prepcompute": {"parent_plan": "prepcompute", "run_before": "compute"},
    "compute": {"parent_plan": "compute", "run_before": "tidy"},
    "tidy": {"parent_plan": "tidy", "run_after": "compute"},
    "couple_out": {"parent_plan": "couple_out", "run_after": "tidy"},
}

COUPLED_NEW = {
    "couple_in": {"parent_plan": "couple_in"},
    "prepcompute": {"parent_plan": "prepcompute", "preceded_by": "couple_in"},
    "compute": {"parent_plan": "compute", "preceded_by": "prepcompute"},
    "tidy": {"parent_plan": "tidy", "preceded_by": "compute"},
    "couple_out": {"parent_plan": "couple_out", "preceded_by": "tidy"},
}


def test_coupled_chain_vocabularies_agree():
    """Legacy and preceded_by expressions of the coupled chain build the same DAG."""
    legacy = build_dag(
        COUPLED_LEGACY, first_task_in_queue="prepcompute", last_task_in_queue="tidy"
    )
    new = build_dag(COUPLED_NEW, entry_point="couple_in", exit_point="couple_out")
    assert legacy == new


def test_coupled_entry_and_exit_are_bumped():
    """couple_in becomes the entry, couple_out the exit, via the ordering chain."""
    dag = build_dag(COUPLED_NEW, entry_point="couple_in", exit_point="couple_out")
    assert dag["first"] == "couple_in"
    assert dag["last"] == "couple_out"


def test_legacy_keys_emit_versioned_deprecation_warning():
    """Legacy keys warn with the deprecated-in / removed-in version schedule."""
    with pytest.warns(deprecation.DeprecatedWarning) as record:
        build_dag(
            DEFAULT_CHAIN_LEGACY,
            first_task_in_queue="prepcompute",
            last_task_in_queue="tidy",
        )
    messages = " ".join(str(w.message) for w in record)
    assert "deprecated as of 6.67.0" in messages
    assert "removed in 7.0.0" in messages


def test_canonical_keys_emit_no_deprecation_warning():
    """A config already on the new vocabulary triggers no deprecation warning."""
    with warnings.catch_warnings():
        warnings.simplefilter("error", deprecation.DeprecatedWarning)
        build_dag(DEFAULT_CHAIN_NEW, entry_point="prepcompute", exit_point="tidy")
