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
