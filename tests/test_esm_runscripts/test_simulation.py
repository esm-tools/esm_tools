from esm_runscripts.simulation_config import ConfigBuilder
from esm_runscripts.sim_objects import Simulation


class _ConfigSetupLike(dict):
    """Minimal stand-in for ``esm_parser.ConfigSetup``.

    ``Simulation.from_config`` calls ``store_prev_objects`` (moved verbatim from
    ``SimulationSetup``), which sets ``self.config.prev_objects = [...]`` as an
    *attribute* and extends ``config["general"]["system_components"]``. A plain
    ``dict`` cannot carry attributes, so a bare dict is not a faithful fixture --
    the real ``ConfigBuilder`` chain yields an ``esm_parser.ConfigSetup`` (a dict
    subclass that allows attribute assignment) with ``system_components`` already
    populated. ``esm_parser.ConfigSetup`` itself can only be constructed by
    reading real model config files, which is far too heavy for a unit test, so
    we use this equivalently-shaped lightweight subclass instead.
    """


def _minimal_config(wf_engine="legacy"):
    user_config = _ConfigSetupLike({
        "general": {
            "setup_name": "dummy", "expid": "test", "valid_model_names": [],
            "wf_engine": wf_engine, "jobtype": "prepare",
            "system_components": [],
        }
    })
    return ConfigBuilder.from_user_config(user_config).build()


def test_from_config_legacy_never_builds_run_state(monkeypatch):
    import esm_runscripts.prepare as prepare
    monkeypatch.setattr(prepare, "run_job", lambda config: config)
    sim = Simulation.from_config(_minimal_config("legacy"))
    assert not hasattr(sim, "run_state")
    assert not hasattr(sim, "plan")


def test_from_config_unknown_wf_engine_defaults_to_legacy(monkeypatch):
    import esm_runscripts.prepare as prepare
    monkeypatch.setattr(prepare, "run_job", lambda config: config)
    sim = Simulation.from_config(_minimal_config(wf_engine=None))
    assert not hasattr(sim, "plan")  # must take the legacy branch, not pplan, on a missing/falsy wf_engine


def test_pplan_tail_steps_called_in_recipe_order(monkeypatch):
    # NOTE: adapted from the brief's verbatim sketch in two ways, both driven
    # by reading the real code rather than guessing from names:
    #
    # 1. ``restore_protected_last_minute_changes`` is added as a sixth,
    #    first-called step. It sits between ``assemble_workflow`` and
    #    ``add_submission_info`` in the legacy recipe
    #    (configs/esm_software/esm_runscripts/esm_runscripts.yaml), and Task
    #    2's ``RunStateBuilder.with_last_minute_changes()`` only calls
    #    ``last_minute.apply_last_minute_changes`` -- never the restore half.
    #    See the investigation write-up in the task-4 report for the full
    #    reasoning.
    # 2. That function lives in ``esm_runscripts.last_minute``, not
    #    ``esm_runscripts.prepare`` (confirmed: ``prepare.py`` defines no such
    #    name), so it is monkeypatched on its real module.
    # 3. The stub's signature is ``_run_pplan_tail_steps(self, config)`` (an
    #    instance method, already fixed by Task 3), so it is called on a
    #    ``Simulation()`` instance rather than as a bare function.
    calls = []
    import esm_runscripts.prepare as prepare
    import esm_runscripts.last_minute as last_minute
    monkeypatch.setattr(
        last_minute, "restore_protected_last_minute_changes",
        lambda config: (calls.append("restore_protected_last_minute_changes"), config)[1],
    )
    for name in ["add_submission_info", "initialize_batch_system", "initialize_coupler",
                 "set_logfile", "check_config_for_warnings_errors"]:
        monkeypatch.setattr(prepare, name, lambda config, name=name: (calls.append(name), config)[1])
    Simulation._run_pplan_tail_steps(Simulation(), {"general": {}})
    assert calls == ["restore_protected_last_minute_changes", "add_submission_info",
                      "initialize_batch_system", "initialize_coupler",
                      "set_logfile", "check_config_for_warnings_errors"]


def test_pplan_branch_runs_real_crash_path_without_raising(tmp_path, monkeypatch):
    """Regression test for the Task 2 <-> Task 4 cross-task seam bug.

    ``RunStateBuilder.from_config`` deepcopies ``config.data`` and, by design,
    the per-invocation facts it computes (dates, calendar, folders, and -- the
    one that bites -- ``general.modify_config_memo``) land ONLY on
    ``self.run_state.data``, never on ``self.config``. The downstream consumers
    in ``Simulation.from_config``'s ``pplan`` branch (``Workflow.resolve`` and
    ``_run_pplan_tail_steps``) must therefore read ``self.run_state.data``.

    The original bug passed ``self.config`` instead. The very first tail step,
    ``last_minute.restore_protected_last_minute_changes``, does
    ``if config["general"]["modify_config_memo"]:`` -- an unconditional
    subscript of a key that ``with_last_minute_changes()`` sets only on the
    run_state deepcopy. Against ``self.config`` that is a guaranteed KeyError on
    every ``--wf-engine=pplan`` run.

    This test drives the whole real ``pplan`` branch. The bug-critical trio is
    genuinely UNSTUBBED:

    * ``last_minute.apply_last_minute_changes`` (via
      ``RunStateBuilder.with_last_minute_changes()``) -- the step that attaches
      ``modify_config_memo`` to the run_state deepcopy, and ONLY to it.
    * ``plan.Workflow.resolve`` / ``Workflow.default`` -- the real resolver.
    * ``last_minute.restore_protected_last_minute_changes`` -- the exact crash
      site, which reads ``modify_config_memo`` and then deletes it.

    So the regression guard is sharp: if a future change re-points either
    consumer back at ``self.config`` (which never receives the memo key), the
    real ``restore_protected_last_minute_changes`` raises KeyError here and the
    test fails -- exactly the class of bug (wrong object handed to a downstream
    consumer) the over-mocked ``test_pplan_tail_steps_called_in_recipe_order``
    could not catch.

    The only things stubbed are the chain steps that genuinely need a real
    ``esm_parser.ConfigSetup`` (``choose_blocks`` needs ``config._blackdict``,
    ``finalize_config`` needs ``config.finalize()``), real ``esm_calendar``
    date arithmetic (calendar/folders/prev_date/parent_info), or a real HPC
    environment (the batch-system / coupler tail steps, including
    ``find_openmp``, which needs real model ``omp_num_threads`` sections
    this minimal fixture doesn't build) -- the brief's explicitly-sanctioned
    "minimum necessary" patches.
    """
    import esm_runscripts.prepare as prepare
    import esm_runscripts.simulation_config as simulation_config

    passthrough = lambda config: config

    # Chain/tail steps that need a real ConfigSetup, real esm_calendar dates,
    # or a real HPC environment -- the minimum-necessary patches. The
    # bug-critical trio (apply/restore last-minute changes, Workflow.resolve)
    # is deliberately NOT in this list.
    for name in [
        "resolve_some_choose_blocks",  # needs config._blackdict
        "_initialize_calendar",        # needs esm_calendar dates
        "_add_all_folders",            # needs resolved calendar
        "set_prev_date",               # needs resolved calendar
        "set_parent_info",             # needs resolved calendar
        "finalize_config",             # needs config.finalize()
        "add_submission_info",         # needs a real batch system
        "initialize_batch_system",     # needs a real batch system
        "initialize_coupler",          # needs real coupler / standalone info
        "check_config_for_warnings_errors",  # walks every section for triggers
    ]:
        monkeypatch.setattr(prepare, name, passthrough)

    # find_openmp needs real model sections (omp_num_threads) this minimal
    # fixture doesn't build; stub it the same way as the other real-pipeline
    # steps above, now that simulation_config.py imports the class directly
    # (BatchSystem) rather than via the package's shadowed `batch_system`
    # attribute.
    monkeypatch.setattr(
        simulation_config.BatchSystem, "find_openmp", staticmethod(passthrough)
    )

    config = _ConfigSetupLike({
        "general": {
            "setup_name": "dummy",
            "expid": "test",
            "valid_model_names": [],
            "wf_engine": "pplan",
            "jobtype": "prepare",
            "system_components": [],
            "experiment_dir": str(tmp_path),
            "experiment_log_dir": str(tmp_path),
            "initial_date": "18500101",
        },
        "computer": {
            # Workflow.default reads computer.partitions.compute.name
            "partitions": {"compute": {"name": "mycompute"}},
        },
    })
    sim_config = ConfigBuilder.from_user_config(config).build()

    # The whole point: this must NOT raise. Pre-fix, the real
    # restore_protected_last_minute_changes raised KeyError here.
    sim = Simulation.from_config(sim_config)

    # The pplan branch was actually taken (not the legacy fallback).
    assert hasattr(sim, "run_state")
    assert hasattr(sim, "plan")

    # Proof the real crash path ran on run_state.data, not vacuously:
    # with_last_minute_changes() set general.modify_config_memo on the
    # run_state deepcopy, and the real restore_protected_last_minute_changes
    # consumed and deleted it. If restore had received self.config (no memo),
    # it would have raised instead of reaching this assertion.
    assert "modify_config_memo" not in sim.run_state.data["general"]
    # self.config never carried the memo -- it is the ConfigBuilder product,
    # which RunState's facts are deliberately never synced back into.
    assert "modify_config_memo" not in sim.config["general"]
    # The real resolver produced the default prepcompute -> compute -> tidy plan.
    assert sim.plan.entry_point == "prepcompute"


def test_run_is_the_renamed_dispatch_method():
    # The dispatch method is now ``run`` (renamed from ``__call__``). Note that
    # ``hasattr(SomeClass, "__call__")`` is True for *every* class (the metaclass
    # ``type`` makes classes themselves callable), so the meaningful check that
    # ``Simulation`` defines no explicit ``__call__`` of its own is against
    # ``__dict__``.
    assert hasattr(Simulation, "run")
    assert "__call__" not in Simulation.__dict__
