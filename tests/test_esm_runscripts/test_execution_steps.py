"""Tests for native, lazy step resolution/execution."""

from esm_runscripts.execution import steps as steps_module
from esm_runscripts.pplan.plan import Step


def test_resolve_steps_returns_step_objects(monkeypatch):
    from esm_runscripts import helpers

    monkeypatch.setattr(
        helpers,
        "get_recipe_step_names",
        lambda config, job_type, recipe_name: ["_read_date_file", "check_model_lresume"],
    )

    result = steps_module.resolve_steps({"general": {}}, "prepare")

    assert result == [Step(name="_read_date_file"), Step(name="check_model_lresume")]


def test_resolve_steps_uses_job_type_plus_recipe_suffix(monkeypatch):
    from esm_runscripts import helpers

    calls = []
    monkeypatch.setattr(
        helpers,
        "get_recipe_step_names",
        lambda config, job_type, recipe_name: calls.append((job_type, recipe_name)) or [],
    )

    steps_module.resolve_steps({"general": {}}, "compute")

    assert calls == [("compute", "compute_recipe")]


def test_run_steps_is_a_noop_for_an_empty_steps_list(monkeypatch):
    from esm_runscripts import helpers

    monkeypatch.setattr(helpers, "get_recipe_step_names", lambda *a, **k: [])

    config = {"general": {"setup_name": "awicm3"}}
    result = steps_module.run_steps(config, "prepare")

    assert result is config


def test_run_steps_resolves_and_runs_via_esm_plugin_manager(monkeypatch):
    # run_steps is a thin adapter over esm_plugin_manager -- the same
    # read_plugin_information + check_plugin_availability +
    # work_through_recipe sequence helpers.evaluate already uses for the
    # legacy engine. Verify the adapter wiring (recipe shape, call order)
    # without running real plugin functions.
    from esm_runscripts import helpers

    monkeypatch.setattr(
        helpers,
        "get_recipe_step_names",
        lambda config, job_type, recipe_name: ["_read_date_file", "check_model_lresume"],
    )

    calls = []

    def fake_read_plugin_information(plugins_bare, recipe):
        calls.append(("read_plugin_information", plugins_bare, recipe))
        return {"fake_plugins": True}

    def fake_check_plugin_availability(plugins):
        calls.append(("check_plugin_availability", plugins))

    def fake_work_through_recipe(recipe, plugins, config):
        calls.append(("work_through_recipe", recipe, plugins, config))
        return {**config, "ran": True}

    import esm_plugin_manager
    monkeypatch.setattr(
        esm_plugin_manager, "read_plugin_information", fake_read_plugin_information
    )
    monkeypatch.setattr(
        esm_plugin_manager, "check_plugin_availability", fake_check_plugin_availability
    )
    monkeypatch.setattr(
        esm_plugin_manager, "work_through_recipe", fake_work_through_recipe
    )

    config = {"general": {"setup_name": "awicm3"}}
    result = steps_module.run_steps(config, "prepare")

    assert result == {"general": {"setup_name": "awicm3"}, "ran": True}
    assert [name for name, *_ in calls] == [
        "read_plugin_information",
        "check_plugin_availability",
        "work_through_recipe",
    ]
    recipe = calls[0][2]
    assert recipe == {
        "recipe": ["_read_date_file", "check_model_lresume"],
        "job_type": "prepare",
    }


def test_run_steps_sets_relevant_filetypes_for_prepcompute_compute_tidy(monkeypatch):
    # Native parity shim for legacy's run_job(config) (prepcompute.py/
    # compute.py/tidy.py), which sets general.relevant_filetypes before
    # ever calling helpers.evaluate -- native dispatch calls straight into
    # the recipe, bypassing that wrapper, so run_steps must set it itself
    # or a step reading it (log_used_files) raises KeyError.
    import esm_plugin_manager

    from esm_runscripts import helpers

    monkeypatch.setattr(helpers, "get_recipe_step_names", lambda *a, **k: ["log_used_files"])
    monkeypatch.setattr(esm_plugin_manager, "read_plugin_information", lambda *a, **k: {})
    monkeypatch.setattr(esm_plugin_manager, "check_plugin_availability", lambda *a, **k: None)
    seen_filetypes = {}
    monkeypatch.setattr(
        esm_plugin_manager,
        "work_through_recipe",
        lambda recipe, plugins, config: seen_filetypes.setdefault(
            recipe["job_type"], config["general"]["relevant_filetypes"]
        )
        or config,
    )

    for job_type, expected in {
        "prepcompute": ["bin", "config", "forcing", "input", "restart_in"],
        "compute": ["bin", "config", "forcing", "input", "restart_in"],
        "tidy": ["log", "mon", "outdata", "restart_out", "ignore", "unknown"],
    }.items():
        config = {"general": {"setup_name": "awicm3"}}
        steps_module.run_steps(config, job_type)
        assert config["general"]["relevant_filetypes"] == expected
        assert seen_filetypes[job_type] == expected


def test_run_steps_does_not_set_relevant_filetypes_for_other_job_types(monkeypatch):
    from esm_runscripts import helpers

    monkeypatch.setattr(helpers, "get_recipe_step_names", lambda *a, **k: [])

    config = {"general": {"setup_name": "awicm3"}}
    steps_module.run_steps(config, "prepexp")

    assert "relevant_filetypes" not in config["general"]


def test_run_steps_and_run_job_agree_on_relevant_filetypes():
    # The whole point of filetype_groups.py: legacy's run_job() (called by
    # the legacy engine) and native run_steps() (called by pplan dispatch)
    # must read the exact same single source, not two copies that can
    # drift apart.
    from esm_runscripts import compute, prepcompute, tidy
    from esm_runscripts.execution.steps import _RELEVANT_FILETYPES_BY_JOB_TYPE

    for job_type, module in (
        ("prepcompute", prepcompute),
        ("compute", compute),
        ("tidy", tidy),
    ):
        assert list(module._RELEVANT_FILETYPES) == list(
            _RELEVANT_FILETYPES_BY_JOB_TYPE[job_type]
        )
