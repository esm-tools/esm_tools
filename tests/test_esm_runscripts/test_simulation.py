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


def test_run_is_the_renamed_dispatch_method():
    # The dispatch method is now ``run`` (renamed from ``__call__``). Note that
    # ``hasattr(SomeClass, "__call__")`` is True for *every* class (the metaclass
    # ``type`` makes classes themselves callable), so the meaningful check that
    # ``Simulation`` defines no explicit ``__call__`` of its own is against
    # ``__dict__``.
    assert hasattr(Simulation, "run")
    assert "__call__" not in Simulation.__dict__
