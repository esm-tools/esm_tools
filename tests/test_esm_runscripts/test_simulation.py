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


def test_from_config_legacy_never_builds_plan(monkeypatch):
    import esm_runscripts.prepare as prepare
    monkeypatch.setattr(prepare, "run_job", lambda config: config)
    sim = Simulation.from_config(_minimal_config("legacy"))
    assert not hasattr(sim, "plan")


def test_from_config_unknown_wf_engine_defaults_to_legacy(monkeypatch):
    import esm_runscripts.prepare as prepare
    monkeypatch.setattr(prepare, "run_job", lambda config: config)
    sim = Simulation.from_config(_minimal_config(wf_engine=None))
    assert not hasattr(sim, "plan")  # must take the legacy branch, not pplan, on a missing/falsy wf_engine


def test_pplan_tail_steps_called_in_recipe_order(monkeypatch):
    # ``restore_protected_last_minute_changes`` lives in
    # ``esm_runscripts.last_minute``, not ``prepare`` -- it is monkeypatched
    # on its own module, not ``prepare``, and is called first because
    # ``RunStateBuilder.with_last_minute_changes()`` only applies last-minute
    # changes, never restores/deletes the memo this step consumes.
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


def test_pplan_branch_runs_full_crash_path_without_raising(tmp_path, monkeypatch):
    """Drives the whole ``pplan`` branch with the last-minute-changes /
    ``Workflow.resolve`` / restore-last-minute-changes trio unstubbed, to
    guard against handing a downstream consumer the wrong config object.

    ``RunStateBuilder.from_config`` deepcopies ``config.data`` before
    computing the per-invocation facts (dates, calendar, folders, and --
    the one that bites -- ``general.modify_config_memo``). If
    ``Workflow.resolve``/``_run_pplan_tail_steps`` were ever pointed at the
    pre-``RunStateBuilder`` config instead of its output, the first tail
    step (``last_minute.restore_protected_last_minute_changes``, which
    unconditionally subscripts ``config["general"]["modify_config_memo"]``)
    would raise ``KeyError`` on every ``--wf-engine=pplan`` run, since that
    key only exists on the ``RunStateBuilder`` deepcopy.

    Only stubbed: chain steps needing a real ``esm_parser.ConfigSetup``
    (``choose_blocks`` needs ``config._blackdict``, ``finalize_config``
    needs ``config.finalize()``), real ``esm_calendar`` date arithmetic, or
    a real HPC environment (batch-system / coupler tail steps, including
    ``find_openmp``, which needs model ``omp_num_threads`` sections this
    minimal fixture doesn't build).
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

    # find_openmp needs model sections (omp_num_threads) this minimal
    # fixture doesn't build; stub it like the other steps above, via
    # simulation_config.py's direct BatchSystem import rather than the
    # package's shadowed `batch_system` attribute.
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

    # The whole point: this must not raise KeyError.
    sim = Simulation.from_config(sim_config)

    # The pplan branch was taken (not the legacy fallback).
    assert hasattr(sim, "plan")

    # with_last_minute_changes() set general.modify_config_memo on the
    # RunStateBuilder deepcopy, and restore_protected_last_minute_changes
    # consumed and deleted it before the result became self.config -- so
    # this assertion only passes if the tail steps ran on that deepcopy's
    # output, not on an un-enriched config (which never gets the memo key).
    assert "modify_config_memo" not in sim.config["general"]
    assert sim.plan.entry_point == "prepcompute"


def test_run_is_the_renamed_dispatch_method():
    # The dispatch method is now ``run`` (renamed from ``__call__``). Note that
    # ``hasattr(SomeClass, "__call__")`` is True for *every* class (the metaclass
    # ``type`` makes classes themselves callable), so the meaningful check that
    # ``Simulation`` defines no explicit ``__call__`` of its own is against
    # ``__dict__``.
    assert hasattr(Simulation, "run")
    assert "__call__" not in Simulation.__dict__
