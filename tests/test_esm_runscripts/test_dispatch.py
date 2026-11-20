"""Tests for native execution dispatch over a resolved Workflow."""

import pytest

from esm_runscripts.execution.dispatch import advance, run_in_process, start
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


def test_run_in_process_runs_steps_for_an_in_process_multi_step(monkeypatch):
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        steps_module,
        "run_steps",
        lambda config, job_type: (calls.append(job_type), config)[1],
    )

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.plan_execution["prepcompute"].executor == Executor.IN_PROCESS

    result = run_in_process(workflow, "prepcompute", {"general": {}})

    assert calls == ["prepcompute"]
    assert result == {"general": {}}


def test_run_in_process_rejects_a_batch_multi_step():
    from esm_runscripts.pplan import plan as plan_module

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.plan_execution["compute"].executor == Executor.BATCH

    with pytest.raises(ValueError, match="compute"):
        run_in_process(workflow, "compute", {"general": {}})


def test_advance_dispatches_the_triggered_in_process_multi_step(monkeypatch):
    # "tidy" triggers "prepcompute" (IN_PROCESS -- next cycle's bootstrap),
    # which itself triggers "compute" (BATCH) -- the cascade dispatches
    # both, but doesn't chase past "compute" (fire-and-forget).
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        steps_module,
        "run_steps",
        lambda config, job_type: (calls.append(job_type), config)[1],
    )
    batch_calls = []
    monkeypatch.setitem(
        dispatch_module._DISPATCHERS,
        plan_module.Executor.BATCH,
        lambda workflow, multi_step_name, config: batch_calls.append(multi_step_name) or config,
    )

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.resubmit_triggers == {
        "prepexp": ["prepcompute"],
        "prepcompute": ["compute"],
        "compute": ["tidy"],
        "tidy": ["prepcompute"],
    }

    result = advance(workflow, "tidy", {"general": {}})

    assert calls == ["prepcompute"]
    assert batch_calls == ["compute"]
    assert result == {"prepcompute": {"general": {}}, "compute": {"general": {}}}


def test_advance_returns_empty_dict_for_a_multi_step_that_triggers_nothing(monkeypatch):
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    monkeypatch.setattr(steps_module, "run_steps", lambda config, job_type: config)

    workflow = plan_module.Workflow.default(_default_config())

    # "compute" (BATCH) has no registered strategy here to monkeypatch --
    # use "tidy"'s own would-be target set to empty instead by asking about
    # a multi_step with no trigger in the default chain: none exists except
    # the ones already covered above, so assert directly against a
    # multi_step not wired into resubmit_triggers at all.
    assert advance(workflow, "nonexistent", {"general": {}}) == {}


def test_advance_dispatches_a_batch_trigger_target(monkeypatch):
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setitem(
        dispatch_module._DISPATCHERS,
        plan_module.Executor.BATCH,
        lambda workflow, multi_step_name, config: calls.append(multi_step_name) or config,
    )
    # "tidy" fans out to both "postprocessing" (BATCH, mocked above) and
    # "prepcompute" (IN_PROCESS, dispatched for real) -- mock step
    # resolution for the latter so it doesn't need a full real config.
    monkeypatch.setattr(steps_module, "run_steps", lambda config, job_type: config)

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
    assert workflow.plan_execution["postprocessing"].executor == Executor.BATCH

    result = advance(workflow, "tidy", {"marker": True})

    assert "postprocessing" in calls
    assert result["postprocessing"] == {"marker": True}


def test_advance_raises_when_an_executor_has_no_registered_strategy(monkeypatch):
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    monkeypatch.delitem(dispatch_module._DISPATCHERS, plan_module.Executor.BATCH)
    # "tidy" also fans out to "prepcompute" (IN_PROCESS) -- advance()
    # dispatches that one for real, which cascades straight into its own
    # trigger "compute" (BATCH, now unregistered) and raises there, before
    # the fan-out even reaches "postprocessing"; mock step resolution for
    # the IN_PROCESS leg.
    monkeypatch.setattr(steps_module, "run_steps", lambda config, job_type: config)

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

    with pytest.raises(NotImplementedError, match="compute"):
        advance(workflow, "tidy", {"general": {}})


def test_advance_dispatches_every_target_in_an_n_way_fan_out(monkeypatch):
    # Real config-building never produces two IN_PROCESS targets off one
    # trigger (only prepcompute/tidy ever get that executor, and they
    # don't share a trigger) -- construct the Workflow directly to unit
    # test advance()'s fan-out loop itself, independent of whether today's
    # config-building code happens to produce this shape yet.
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        steps_module,
        "run_steps",
        lambda config, job_type: (calls.append(job_type), config)[1],
    )

    multi_steps = {
        name: plan_module.MultiStep(name=name, step_of_plan=name)
        for name in ("alpha", "beta", "gamma")
    }
    multi_step_execution = {
        name: plan_module.MultiStepExecution() for name in multi_steps
    }
    plans = {
        name: plan_module.Plan(name=name) for name in multi_steps
    }
    plan_execution = {
        name: plan_module.PlanExecution(
            multi_step_names=[name], executor=plan_module.Executor.IN_PROCESS
        )
        for name in multi_steps
    }
    workflow = plan_module.Workflow(
        entry_point="alpha",
        resubmit_triggers={"alpha": ["beta", "gamma"]},
        plans=plans,
        plan_execution=plan_execution,
        multi_steps=multi_steps,
        multi_step_execution=multi_step_execution,
    )

    result = advance(workflow, "alpha", {"general": {}})

    assert set(calls) == {"beta", "gamma"}
    assert result == {
        "beta": {"general": {}},
        "gamma": {"general": {}},
    }


def test_start_dispatches_the_entry_point(monkeypatch):
    # entry_point "prepexp" cascades through "prepcompute" (both
    # IN_PROCESS) and stops at "compute" (BATCH, fire-and-forget).
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        steps_module, "run_steps", lambda config, job_type: calls.append(job_type) or config
    )
    batch_calls = []
    monkeypatch.setitem(
        dispatch_module._DISPATCHERS,
        plan_module.Executor.BATCH,
        lambda workflow, multi_step_name, config: batch_calls.append(multi_step_name) or config,
    )

    workflow = plan_module.Workflow.default(_default_config())
    assert workflow.entry_point == "prepexp"

    result = start(workflow, {"general": {}})

    assert calls == ["prepexp", "prepcompute"]
    assert batch_calls == ["compute"]
    assert result == {
        "prepexp": {"general": {}},
        "prepcompute": {"general": {}},
        "compute": {"general": {}},
    }


def test_advance_bumps_current_date_only_after_exit_point_not_before(monkeypatch, tmp_path):
    """Regression for a real bug: cli._run_observe used to advance
    current_date/run_number *before* dispatching anything, so "tidy" (meant
    to clean up the cycle that just finished) ran against the next cycle's
    not-yet-created directories. Now the advance happens inside the
    cascade itself, right after "tidy" (workflow.exit_point) dispatches,
    and before "prepcompute" (the next cycle) does."""
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    seen_dates = {}
    monkeypatch.setattr(
        steps_module,
        "run_steps",
        lambda config, job_type: seen_dates.setdefault(
            job_type, config["general"]["current_date"]
        )
        or config,
    )
    monkeypatch.setitem(
        dispatch_module._DISPATCHERS,
        plan_module.Executor.BATCH,
        lambda workflow, multi_step_name, config: seen_dates.setdefault(
            multi_step_name, config["general"]["current_date"]
        )
        or config,
    )

    class _FakeDate(int):
        def format(self, **kwargs):
            return str(int(self))

        def output(self):
            return str(int(self))

        def __add__(self, other):
            return _FakeDate(int(self) + int(other))

    config = _default_config()
    config["general"].update(
        {
            "current_date": _FakeDate(1),
            "delta_date": 1,
            "next_date": 2,
            "final_date": 99,
            "run_number": 1,
            "expid": "test",
            "setup_name": "fesom",
            "experiment_scripts_dir": str(tmp_path),
            "command_line_config": {},
        }
    )

    workflow = plan_module.Workflow.default(config)
    dispatch_module.advance(workflow, "compute", config)

    assert seen_dates["tidy"] == 1
    assert seen_dates["prepcompute"] == 2
    assert seen_dates["compute"] == 2
    assert config["general"]["current_date"] == 2
    assert config["general"]["run_number"] == 2


def test_start_raises_when_the_entry_points_executor_has_no_registered_strategy(monkeypatch):
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.pplan import plan as plan_module

    monkeypatch.delitem(dispatch_module._DISPATCHERS, plan_module.Executor.IN_PROCESS)

    workflow = plan_module.Workflow.default(_default_config())

    with pytest.raises(NotImplementedError, match="prepexp"):
        start(workflow, {"general": {}})
