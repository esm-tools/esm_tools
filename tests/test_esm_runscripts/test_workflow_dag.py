"""DAG-invariance tests for the workflow ordering vocabulary.

The workflow-ordering rename (run_after/run_before -> preceded_by) must NOT
change the shape of the assembled execution graph. Two guarantees:

* the assembled DAG (``next_submit`` edges + entry/exit points) is byte-identical
  whether a workflow is expressed in the legacy keys or the canonical
  ``preceded_by`` key -- asserted as a self-comparison, no stored golden needed;
* the absolute shape is pinned to a golden file via ``pytest-regressions``
  (``data_regression``), so an unintended change to the graph fails loudly.
  Regenerate an intentional change with ``pytest --force-regen``.

Input workflows live as YAML under ``data/`` (loaded via ``pytest-datadir``'s
``shared_datadir``) so the fixtures are real ``general.workflow`` blocks rather
than inline dicts.
"""

import copy
import warnings

import deprecation
import pytest
import yaml
from esm_runscripts import workflow


def build_dag(workflow_block):
    """Assemble ``workflow_block`` and return the shape of its execution graph.

    ``workflow_block`` is a ``general.workflow`` dict (sub_plans + entry/exit
    keys) in either vocabulary. The returned shape (``next_submit`` edges,
    ``called_from`` back-pointers, and the entry/exit plan *names*) is
    vocabulary-independent, so both vocabularies must produce the same value.
    """
    gw = {"plans": {}, **copy.deepcopy(workflow_block)}
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


# --- input workflows (data/*.yaml via pytest-datadir) ----------------------
# The default chain (prepcompute -> compute -> tidy, each its own plan) and the
# coupled chain (couple_in runs before prepcompute -> bumps the entry;
# couple_out runs after tidy -> bumps the exit), each expressed once in the
# legacy keys and once in the canonical ``preceded_by`` key.


def _workflow_fixture(filename):
    """Make a fixture that loads ``data/<filename>.yaml`` as a workflow block."""

    @pytest.fixture
    def _fixture(shared_datadir):
        return yaml.safe_load((shared_datadir / f"{filename}.yaml").read_text())

    return _fixture


default_chain_legacy = _workflow_fixture("workflow_default_legacy")
default_chain_new = _workflow_fixture("workflow_default_new")
coupled_legacy = _workflow_fixture("workflow_coupled_legacy")
coupled_new = _workflow_fixture("workflow_coupled_new")


# --- absolute-shape golden (pytest-regressions) ----------------------------


def test_default_chain_shape(default_chain_new, data_regression):
    """Pin the assembled default-chain DAG to its golden file."""
    data_regression.check(build_dag(default_chain_new))


def test_coupled_chain_shape(coupled_new, data_regression):
    """Pin the assembled coupled-chain DAG to its golden file."""
    data_regression.check(build_dag(coupled_new))


# --- vocabulary invariance (self-comparison, no golden) --------------------


def test_default_chain_vocabularies_agree(default_chain_legacy, default_chain_new):
    """Legacy and preceded_by expressions of the default chain build one DAG."""
    assert build_dag(default_chain_legacy) == build_dag(default_chain_new)


def test_coupled_chain_vocabularies_agree(coupled_legacy, coupled_new):
    """Legacy and preceded_by expressions of the coupled chain build one DAG."""
    assert build_dag(coupled_legacy) == build_dag(coupled_new)


def test_coupled_entry_and_exit_are_bumped(coupled_new):
    """couple_in becomes the entry, couple_out the exit, via the ordering chain."""
    dag = build_dag(coupled_new)
    assert dag["first"] == "couple_in"
    assert dag["last"] == "couple_out"


# --- deprecation behaviour -------------------------------------------------


def test_legacy_keys_emit_versioned_deprecation_warning(default_chain_legacy):
    """Legacy keys warn with the deprecated-in / removed-in version schedule."""
    with pytest.warns(deprecation.DeprecatedWarning) as record:
        build_dag(default_chain_legacy)
    messages = " ".join(str(w.message) for w in record)
    assert "deprecated as of 6.67.0" in messages
    assert "removed in 7.0.0" in messages


def test_canonical_keys_emit_no_deprecation_warning(default_chain_new):
    """A config already on the new vocabulary triggers no deprecation warning."""
    with warnings.catch_warnings():
        warnings.simplefilter("error", deprecation.DeprecatedWarning)
        build_dag(default_chain_new)
