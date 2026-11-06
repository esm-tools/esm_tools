"""
Tests for SimulationSetup as a deprecated pass-through shim to Simulation.

These tests encode the exact call shapes used by the three callers this
refactor plan does NOT touch:

- src/esm_master/esm_master.py: ``SimulationSetup(user_config=user_config)``
- src/esm_runscripts/resubmit.py: ``SimulationSetup(command_line_config)``
  (positional), then calling the instance via ``__call__``
- tests/test_esm_catalog/fixtures/fake_experiment/resolver.py:
  ``SimulationSetup(command_line_config=command_line_config).config``

Deviations from the task brief's verbatim Step-1 test sketch (documented here
since the brief specified the tests close to verbatim, same convention as
Task 3's report):

1. The brief's minimal runscript/user_config fixtures (``general.setup_name:
   dummy`` with no ``jobtype``) are not runnable as-is: ``init_interactive_info``
   (step 3 of the old ``__init__``, unchanged by this task) reads
   ``config["general"]["jobtype"]`` unconditionally, and there is no real
   "dummy" setup under ``configs/setups`` for
   ``config_initialization.get_total_config_from_user_config`` to load (every
   real setup there is a full climate-model coupling, far too heavy for a
   unit test). Fixtures below add ``jobtype``/``version`` and monkeypatch
   ``get_total_config_from_user_config`` with a lightweight attribute-carrying
   stand-in (``_ConfigSetupLike``, same device Task 3's tests used for the
   same reason), so these tests exercise the real
   ``SimulationSetup -> ConfigBuilder -> Simulation.from_config`` wiring
   under test here without needing a real model setup. ``prepare.run_job``
   (the legacy "resolve ESM-Tools syntax" recipe, step 12 of the old
   ``__init__``) is stubbed for all four tests, not only the one the brief's
   sketch stubs it for: its real recipe calls
   ``esm_parser.choose_blocks(config, blackdict=config._blackdict)``, which
   needs a real ``esm_parser.ConfigSetup`` (``_ConfigSetupLike`` has no
   ``_blackdict``) -- again, real-model-setup-dependent machinery unrelated
   to what these tests verify (the shim's wiring, not the legacy recipe
   engine, which has its own test coverage elsewhere).
2. ``test_simulation_setup_is_still_callable`` additionally monkeypatches
   ``prepexp.run_job``, ``assembler.run_job``, and ``resubmit.maybe_resubmit``
   on top of the ``prepare.run_job`` stub (the brief's sketch only mentions
   the latter). ``Simulation.run``
   unconditionally calls ``prepexp.run_job`` and then, for jobtype
   "prepare" (not prepcompute/tidy/viz/observe), ``self.assembler()`` and
   ``resubmit.maybe_resubmit`` -- all of which walk the same real,
   filesystem/model-dependent recipe machinery as point 1. None of this is
   part of what this test is verifying (that ``SimulationSetup`` is still
   callable, i.e. ``__call__`` forwards to ``run`` without raising
   "object not callable"), so it is stubbed out rather than faked up.
"""

import tempfile
import warnings

import pytest
from loguru import logger

from esm_runscripts import config_initialization
from esm_runscripts.sim_objects import SimulationSetup


class _ConfigSetupLike(dict):
    """Minimal attribute-carrying dict stand-in for esm_parser.ConfigSetup.

    See module docstring, deviation 1.
    """


def _fake_get_total_config_from_user_config(user_config):
    general = dict(user_config.get("general", {}))
    general.setdefault("valid_model_names", [])
    general.setdefault("system_components", [])
    general.setdefault("base_dir", "/tmp/esm-experiments")
    general["experiment_dir"] = f"{general['base_dir']}/{general['expid']}"
    general.setdefault("original_command", "esm_runscripts test.yaml")
    general.setdefault("iterative_coupled_model", "")
    general.setdefault("run_datestamp", "19000101")
    general.setdefault("run_number", 1)
    general.setdefault("current_date", "19000101")
    log_dir = tempfile.mkdtemp(prefix="esm-sim-setup-test-log-")
    general.setdefault("experiment_log_dir", log_dir)
    general.setdefault("thisrun_log_dir", log_dir)
    general.setdefault("jobid", "0")
    general.setdefault("command_line_config", {})
    computer = {"jobtype": general.get("jobtype")}
    return _ConfigSetupLike({"general": general, "computer": computer})


@pytest.fixture(autouse=True)
def _bypass_heavy_config_expansion(monkeypatch):
    """Bypass the filesystem/model-dependent config expansion and legacy
    "prepare" recipe steps.

    See module docstring, deviation 1.
    """
    monkeypatch.setattr(
        config_initialization,
        "get_total_config_from_user_config",
        _fake_get_total_config_from_user_config,
    )
    import esm_runscripts.prepare as prepare

    monkeypatch.setattr(prepare, "run_job", lambda config: config)


def test_simulation_setup_warns_on_construction(tmp_path):
    runscript = tmp_path / "test.yaml"
    runscript.write_text(
        "general:\n  setup_name: dummy\n  expid: test\n  jobtype: prepare\n"
        "  version: '1'\n"
    )
    command_line_config = {"runscript_abspath": str(runscript)}
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        SimulationSetup(command_line_config=command_line_config)
    assert any("SimulationSetup" in str(w.message) for w in caught)


def test_simulation_setup_user_config_path_still_works():
    # mirrors esm_master.py's exact call shape: SimulationSetup(user_config=user_config)
    user_config = {
        "general": {
            "setup_name": "dummy",
            "expid": "test",
            "valid_model_names": [],
            "jobtype": "prepare",
            "version": "1",
        }
    }
    setup = SimulationSetup(user_config=user_config)
    assert setup.config["general"]["expid"] == "test"


def test_simulation_setup_keeps_command_line_config_alongside_user_config():
    # the pre-shim __init__ used both inputs when both were given
    user_config = {
        "general": {
            "setup_name": "dummy",
            "expid": "test",
            "valid_model_names": [],
            "jobtype": "prepare",
            "version": "1",
        }
    }
    command_line_config = {"jobtype": "prepare", "last_jobtype": "command_line"}
    setup = SimulationSetup(
        command_line_config=command_line_config, user_config=user_config
    )
    assert setup.config["general"]["command_line_config"] == command_line_config


def test_simulation_setup_positional_command_line_config_still_works(tmp_path):
    # mirrors resubmit.py's exact call shape: SimulationSetup(command_line_config), positional
    runscript = tmp_path / "test.yaml"
    runscript.write_text(
        "general:\n  setup_name: dummy\n  expid: test\n  jobtype: prepare\n"
        "  version: '1'\n"
    )
    setup = SimulationSetup({"runscript_abspath": str(runscript)})
    assert setup.config["general"]["expid"] == "test"


def test_simulation_setup_is_still_callable(tmp_path, monkeypatch):
    import esm_runscripts.assembler as assembler
    import esm_runscripts.prepexp as prepexp
    import esm_runscripts.resubmit as resubmit
    from esm_runscripts.logfiles import SmartSink

    monkeypatch.setattr(prepexp, "run_job", lambda config: config)
    monkeypatch.setattr(assembler, "run_job", lambda config: config)
    monkeypatch.setattr(resubmit, "maybe_resubmit", lambda config: None)

    # Real production CLI startup (cli.py -> logfiles.initialize_logging /
    # set_progress_level) attaches these to the loguru logger singleton once
    # at process start; a unit test importing sim_objects directly never runs
    # that bootstrap.
    if not hasattr(logger, "stdout_sink"):
        monkeypatch.setattr(
            logger,
            "stdout_sink",
            SmartSink(print_in_stdout=False, task_log_files=[]),
            raising=False,
        )
    if not hasattr(logger, "progress"):
        logger.level("PROGRESS", no=15)
        logger.__class__.progress = lambda self, message, *a, **kw: self.log(
            "PROGRESS", message, *a, **kw
        )

    runscript = tmp_path / "test.yaml"
    runscript.write_text(
        "general:\n  setup_name: dummy\n  expid: test\n  jobtype: prepare\n"
        "  version: '1'\n"
    )
    setup = SimulationSetup(
        command_line_config={
            "runscript_abspath": str(runscript),
            "use_venv": False,
            "original_command": "esm_runscripts test.yaml",
        }
    )
    setup(kill_after_submit=False)  # must not raise AttributeError: object not callable
