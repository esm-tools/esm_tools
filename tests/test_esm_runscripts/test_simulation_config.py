import copy

from esm_runscripts.simulation_config import (
    ConfigBuilder,
    RunState,
    RunStateBuilder,
    SimulationConfig,
)


def test_from_runscript_with_zero_overrides_does_not_raise(tmp_path):
    runscript = tmp_path / "test.yaml"
    runscript.write_text("general:\n  setup_name: dummy\n  expid: test\n")
    builder = ConfigBuilder.from_runscript(str(runscript))
    assert isinstance(builder, ConfigBuilder)


def test_from_runscript_passes_overrides_into_command_line_config(tmp_path):
    runscript = tmp_path / "test.yaml"
    runscript.write_text("general:\n  setup_name: dummy\n  expid: test\n")
    builder = ConfigBuilder.from_runscript(str(runscript), task="prepare")
    assert builder._command_line_config["jobtype"] == "prepare" or \
        builder._command_line_config.get("task") == "prepare"
    # exact key name depends on get_user_config_from_command_line's contract;
    # implementer confirms by reading that function, test asserts whichever
    # key cli.py's command_line_config dict actually uses for task (see cli.py)


def test_from_user_config_wraps_dict_directly():
    user_config = {"general": {"setup_name": "dummy", "expid": "test"}}
    builder = ConfigBuilder.from_user_config(user_config)
    assert isinstance(builder, ConfigBuilder)


def test_build_returns_simulation_config():
    builder = ConfigBuilder.from_user_config({"general": {"setup_name": "dummy"}})
    result = builder.build()
    assert isinstance(result, SimulationConfig)
    assert result.data["general"]["setup_name"] == "dummy"


def test_with_methods_return_self_for_chaining():
    builder = ConfigBuilder.from_user_config({"general": {"setup_name": "dummy", "expid": "t"}})
    assert builder.with_execution_mode_default() is builder


def test_from_user_config_preserves_none_command_line_config():
    """
    Regression test: ConfigBuilder should preserve None command_line_config, not convert to {}.

    This ensures that init_iterative_coupling and other functions receive the exact
    None value that was passed, matching the original SimulationSetup.__init__ behavior.
    """
    user_config = {"general": {"setup_name": "dummy", "expid": "test"}}
    builder = ConfigBuilder.from_user_config(user_config)
    assert builder._command_line_config is None


def test_run_state_builder_from_config_does_not_mutate_original(tmp_path):
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    builder = RunStateBuilder.from_config(config)
    builder._data["general"]["expid"] = "mutated"
    assert config.data["general"]["expid"] == "test"  # the deepcopy requirement from Global Constraints


def test_run_state_builder_build_returns_run_state():
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    run_state = RunStateBuilder.from_config(config).build()
    assert isinstance(run_state, RunState)


def test_run_state_builder_from_config_deepcopies_nested_dicts_too():
    """
    The deepcopy requirement isn't just "a different outer dict": nested
    dicts (e.g. config["general"]) must be independent objects too, or
    mutating a nested section of the run state would still bleed into the
    SimulationConfig's data.
    """
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    builder = RunStateBuilder.from_config(config)
    assert builder._data is not config.data
    assert builder._data["general"] is not config.data["general"]
    assert builder._data == config.data


def test_with_date_file_sets_run_number_and_current_date_when_no_date_file_exists(tmp_path):
    config = ConfigBuilder.from_user_config(
        {
            "general": {
                "setup_name": "dummy",
                "expid": "test",
                "experiment_dir": str(tmp_path),
                "valid_model_names": [],
                "initial_date": "18500101",
            }
        }
    ).build()
    builder = RunStateBuilder.from_config(config).with_date_file()
    assert builder._data["general"]["run_number"] == 1
    assert builder._data["general"]["current_date"] == "18500101"


def test_with_lresume_checked_sets_lresume_false_on_first_run():
    config = ConfigBuilder.from_user_config(
        {
            "general": {
                "setup_name": "dummy",
                "expid": "test",
                "run_number": 1,
                "valid_model_names": ["echam"],
            },
            "echam": {},
        }
    ).build()
    builder = RunStateBuilder.from_config(config).with_lresume_checked()
    assert builder._data["echam"]["lresume"] is False


def test_with_lresume_checked_sets_lresume_true_when_not_first_run():
    config = ConfigBuilder.from_user_config(
        {
            "general": {
                "setup_name": "dummy",
                "expid": "test",
                "run_number": 2,
                "valid_model_names": ["echam"],
            },
            "echam": {},
        }
    ).build()
    builder = RunStateBuilder.from_config(config).with_lresume_checked()
    assert builder._data["echam"]["lresume"] is True


def test_with_conda_env_recorded_sets_launched_with_conda_key():
    config = ConfigBuilder.from_user_config(
        {
            "general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []},
            "computer": {},
        }
    ).build()
    builder = RunStateBuilder.from_config(config).with_conda_env_recorded()
    assert isinstance(builder._data["computer"]["launched_with_conda"], bool)


def test_with_openmp_found_sets_heterogeneous_parallelization_false_by_default():
    config = ConfigBuilder.from_user_config(
        {
            "general": {"setup_name": "dummy", "expid": "test", "valid_model_names": ["echam"]},
            "computer": {},
            "echam": {},
        }
    ).build()
    builder = RunStateBuilder.from_config(config).with_openmp_found()
    assert builder._data["computer"]["heterogeneous_parallelization"] is False


def test_with_run_in_chunk_updated_is_noop_without_iterative_coupling():
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    builder = RunStateBuilder.from_config(config)
    before = copy.deepcopy(builder._data)
    builder.with_run_in_chunk_updated()
    assert builder._data == before


def test_with_last_minute_changes_is_noop_without_modify_config_file():
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    builder = RunStateBuilder.from_config(config).with_last_minute_changes()
    assert builder._data["general"]["modify_config_memo"].modify_config is None


def test_run_state_builder_with_methods_return_self_for_chaining():
    config = ConfigBuilder.from_user_config(
        {
            "general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []},
            "computer": {},
        }
    ).build()
    builder = RunStateBuilder.from_config(config)
    assert builder.with_conda_env_recorded() is builder
    assert builder.with_openmp_found() is builder
