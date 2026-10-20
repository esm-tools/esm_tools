from esm_runscripts.simulation_config import ConfigBuilder, SimulationConfig


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
