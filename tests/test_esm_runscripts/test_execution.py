"""Tests for native execution dispatch over a resolved Workflow."""

import pytest

from esm_runscripts.pplan.execution import advance, dispatch_in_process
from esm_runscripts.pplan.plan import Executor


def _default_config():
    return {
        "general": {
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
        },
        "fesom": {"nproc": 128},
        "computer": {"partitions": {"compute": {"name": "compute"}}},
    }


def test_dispatch_in_process_runs_steps_for_an_in_process_sub_plan(monkeypatch):
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        plan_module,
        "run_steps",
        lambda sub_plan, config: (calls.append(sub_plan.name), config)[1],
    )

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.plans["prepcompute"].executor == Executor.IN_PROCESS

    result = dispatch_in_process(workflow, "prepcompute", {"general": {}})

    assert calls == ["prepcompute"]
    assert result == {"general": {}}


def test_dispatch_in_process_rejects_a_batch_sub_plan():
    from esm_runscripts.pplan import plan as plan_module

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.plans["compute"].executor == Executor.BATCH

    with pytest.raises(ValueError, match="compute"):
        dispatch_in_process(workflow, "compute", {"general": {}})


def test_advance_dispatches_the_triggered_in_process_sub_plan(monkeypatch):
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        plan_module,
        "run_steps",
        lambda sub_plan, config: (calls.append(sub_plan.name), config)[1],
    )

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.resubmit_triggers == {"tidy": ["prepcompute"]}

    result = advance(workflow, "tidy", {"general": {}})

    assert calls == ["prepcompute"]
    assert result == {"prepcompute": {"general": {}}}


def test_advance_returns_empty_dict_for_a_sub_plan_that_triggers_nothing():
    from esm_runscripts.pplan import plan as plan_module

    workflow = plan_module.Workflow.default(_default_config())

    assert advance(workflow, "prepcompute", {"general": {}}) == {}


def test_advance_raises_for_a_non_in_process_trigger_target():
    from esm_runscripts.pplan import plan as plan_module

    config = _default_config()
    config["fesom"]["workflow"] = {
        "next_run_triggered_by": "tidy",
        "subjobs": {
            "postprocessing": {
                "run_after": "tidy",
                "run_on_queue": "pp",
                "submit_to_batch_system": True,
                "nproc": 1,
            }
        },
    }
    config["computer"]["partitions"]["pp"] = {"name": "pp"}

    workflow = plan_module.Workflow.resolve(config)
    assert workflow.plans["postprocessing"].executor == Executor.BATCH

    with pytest.raises(NotImplementedError, match="postprocessing"):
        advance(workflow, "tidy", {"general": {}})
