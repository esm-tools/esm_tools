"""Tests for the read-only pydantic view over an assembled workflow graph."""

import copy

import yaml

from esm_runscripts import workflow
from esm_runscripts.plan import Executor, Plan, Step, SubPlan, Workflow, run_steps


def _assembled_config(workflow_block):
    """Run a raw ``general.workflow`` block through complete_plans + order_plans.

    ``workflow.py``'s legacy functions still read/write
    ``config["general"]["workflow"]``; ``Workflow.from_config`` reads the
    top-level ``config["workflow"]`` instead, so copy it up after assembly.
    """
    gw = {"plans": {}, **copy.deepcopy(workflow_block)}
    config = {"general": {"workflow": gw}}
    workflow.translate_legacy_workflow_keys(config)
    workflow.complete_plans(config)
    workflow.order_plans(config)
    config["workflow"] = config["general"]["workflow"]
    return config


def test_workflow_from_config_default_chain(shared_datadir):
    block = yaml.safe_load(
        (shared_datadir / "workflow_default_new.yaml").read_text()
    )
    wf = Workflow.from_config(_assembled_config(block))

    assert isinstance(wf, Workflow)
    assert wf.entry_point == "prepcompute"
    assert set(wf.sub_plans) == {"prepcompute", "compute", "tidy"}

    compute = wf.sub_plans["compute"]
    assert isinstance(compute, SubPlan)
    assert compute.preceded_by == ["prepcompute"]
    assert compute.parent_plan == "compute"

    parent = wf.parent_plan_of("compute")
    assert isinstance(parent, Plan)
    assert parent.name == "compute"
    assert "compute" in parent.sub_plan_names


def test_workflow_from_config_coupled_chain(shared_datadir):
    block = yaml.safe_load(
        (shared_datadir / "workflow_coupled_new.yaml").read_text()
    )
    wf = Workflow.from_config(_assembled_config(block))

    assert wf.entry_point == "couple_in"
    assert wf.resubmit_triggers == {"couple_out": ["couple_in"]}
    assert wf.sub_plans["compute"].nproc == 1

    # Walking the ordering chain via preceded_by should reach every sub-plan.
    order = [wf.entry_point]
    remaining = set(wf.sub_plans) - {wf.entry_point}
    while remaining:
        nxt = next(
            name
            for name in remaining
            if wf.sub_plans[name].preceded_by == [order[-1]]
        )
        order.append(nxt)
        remaining.remove(nxt)
    assert order == ["couple_in", "prepcompute", "compute", "tidy", "couple_out"]


def test_workflow_and_plan_repr_are_readable(shared_datadir):
    block = yaml.safe_load(
        (shared_datadir / "workflow_default_new.yaml").read_text()
    )
    wf = Workflow.from_config(_assembled_config(block))

    assert "first=prepcompute" in repr(wf)
    assert "resubmit_triggers={'tidy': ['prepcompute']}" in repr(wf)
    assert "compute" in repr(wf.sub_plans["compute"])


def test_models_are_frozen(shared_datadir):
    block = yaml.safe_load(
        (shared_datadir / "workflow_default_new.yaml").read_text()
    )
    wf = Workflow.from_config(_assembled_config(block))

    compute = wf.sub_plans["compute"]
    try:
        compute.nproc = 99
        raised = False
    except Exception:
        raised = True
    assert raised, "SubPlan should be immutable (frozen)"


def test_subplan_defaults_for_new_fields():
    sub_plan = SubPlan(name="prepare", parent_plan="initialization")

    assert sub_plan.steps == []
    assert sub_plan.kind == "esm_subplan"


def test_plan_defaults_for_new_fields():
    cluster = Plan(name="initialization")

    assert cluster.executor is None
    assert cluster.cycling is None


def test_run_steps_is_a_noop_for_an_empty_steps_list():
    sub_plan = SubPlan(name="prepare", parent_plan="initialization")
    config = {"general": {"setup_name": "awicm3"}}

    result = run_steps(sub_plan, config)

    assert result is config


def test_run_steps_resolves_and_runs_via_esm_plugin_manager(monkeypatch):
    # run_steps is a thin adapter over esm_plugin_manager -- the same
    # read_plugin_information + check_plugin_availability +
    # work_through_recipe sequence helpers.evaluate already uses for the
    # legacy engine. Verify the adapter wiring (recipe shape, call order)
    # without running real plugin functions.
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

    sub_plan = SubPlan(
        name="prepare",
        parent_plan="initialization",
        steps=[Step(name="_read_date_file"), Step(name="check_model_lresume")],
    )
    config = {"general": {"setup_name": "awicm3"}}

    result = run_steps(sub_plan, config)

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


def test_workflow_default_synthesizes_prepcompute_compute_tidy():
    config = {
        "general": {
            "valid_model_names": ["fesom", "oifs"],
            "submit_to_batch_system": True,
        },
        "fesom": {"nproc": 128},
        "oifs": {"nproca": 8, "nprocb": 16},
        "computer": {"partitions": {"compute": {"name": "compute"}}},
    }

    wf = Workflow.default(config)

    assert wf.entry_point == "prepcompute"
    assert wf.resubmit_triggers == {"tidy": ["prepcompute"]}
    assert set(wf.sub_plans) == {"prepcompute", "compute", "tidy"}
    assert wf.sub_plans["compute"].nproc == 128 + 8 * 16
    assert wf.sub_plans["compute"].preceded_by == ["prepcompute"]
    assert wf.sub_plans["compute"].launch_mode == "background"
    assert wf.plans["compute"].run_on_queue == "compute"
    assert wf.plans["compute"].executor == Executor.BATCH
    assert wf.plans["prepcompute"].executor == Executor.IN_PROCESS
    assert wf.plans["tidy"].executor == Executor.IN_PROCESS
    assert wf.sub_plans["tidy"].preceded_by == ["compute"]
    assert wf.parent_plan_of("compute").name == "compute"
    # Each sub-plan is its own singleton cluster.
    assert set(wf.plans) == {"prepcompute", "compute", "tidy"}


def _no_custom_workflow_config():
    return {
        "general": {
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
        },
        "fesom": {"nproc": 128},
        "computer": {"partitions": {"compute": {"name": "compute"}}},
    }


def test_workflow_resolve_uses_default_when_no_custom_block():
    wf = Workflow.resolve(_no_custom_workflow_config())

    assert wf.entry_point == "prepcompute"


def test_workflow_resolve_raises_when_general_workflow_defines_its_own_entry_point():
    # A genuine attempt to replace the root chain (not just add a sub-plan
    # alongside it) is still out of scope -- this must keep failing loudly.
    config = _no_custom_workflow_config()
    config["general"]["workflow"] = {"entry_point": "something_else", "plans": {}}

    try:
        Workflow.resolve(config)
        raised = False
    except NotImplementedError:
        raised = True
    assert raised, "resolve() should refuse to replace the root chain entirely"


def test_workflow_resolve_does_not_raise_for_an_empty_supplemental_block():
    # A workflow: block with no actual sub_plans in it (no entry_point/plans
    # either) has nothing to merge or replace -- it's not a reason to fail.
    config = _no_custom_workflow_config()
    config["general"]["workflow"] = {"sub_plans": {}}

    wf = Workflow.resolve(config)

    assert wf.entry_point == "prepcompute"


def test_subplan_coerces_bool_with_provenance():
    # Real cluster crash: esm_parser YAML values aren't plain bool/int/str --
    # they're wrapped (e.g. BoolWithProvenance) to carry provenance info.
    # BoolWithProvenance fakes isinstance(v, bool) == True in pure Python
    # but isn't the literal C-level bool pydantic-core's Rust validator
    # checks, so an isinstance-based coercion guard silently passes it
    # through unconverted and pydantic-core rejects it (bool_type error).
    from esm_parser.provenance import BoolWithProvenance

    sub_plan = SubPlan(
        name="postprocessing",
        parent_plan="postprocessing",
        concurrent=BoolWithProvenance(True, provenance=None),
    )
    assert sub_plan.concurrent is True
    assert type(sub_plan.concurrent) is bool


def test_plan_accepts_none_with_provenance():
    # A YAML ``null`` arrives as NoneWithProvenance, which pydantic rejects
    # for Optional[str] fields unless the provenance is stripped first.
    from esm_parser.provenance import NoneWithProvenance

    plan = Plan(
        name="postprocessing",
        run_on_queue=NoneWithProvenance(None, provenance=None),
    )
    assert plan.run_on_queue is None


def test_workflow_resolve_merges_a_real_supplemental_sub_plan():
    # The actual bug this test guards against: configs/setups/awicm3/
    # awicm3.yaml's choose_general.postprocessing: True: workflow: {...}
    # block (legacy vocab: subjobs/run_after/next_run_triggered_by) must
    # merge into the default chain as an extra sub-plan, not raise.
    config = _no_custom_workflow_config()
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

    wf = Workflow.resolve(config)

    assert wf.entry_point == "prepcompute"
    assert "postprocessing" in wf.sub_plans
    assert wf.sub_plans["postprocessing"].preceded_by == ["tidy"]
    assert "postprocessing" in wf.plans
    assert wf.plans["postprocessing"].run_on_queue == "pp"
    assert wf.plans["postprocessing"].executor == Executor.BATCH
    # Fan-out: tidy still triggers the next cycle AND the new sub-plan.
    assert wf.resubmit_triggers["tidy"] == ["prepcompute", "postprocessing"]


def test_collect_extra_sub_plans_defaults_executor_to_local_when_omitted():
    # Matches the legacy dict engine's own default: workflow.py sets
    # plan_conf["submit_to_batch_system"] = False when the key is absent,
    # and resubmit.py's get_submission_type reads the same False default --
    # a supplemental sub-plan that doesn't mention submit_to_batch_system
    # at all must get Executor.LOCAL, not Executor.BATCH.
    config = _no_custom_workflow_config()
    config["fesom"]["workflow"] = {
        "next_run_triggered_by": "tidy",
        "subjobs": {
            "inline_postprocessing": {
                "run_after": "tidy",
                "nproc": 1,
            }
        },
    }

    wf = Workflow.resolve(config)

    assert wf.plans["inline_postprocessing"].executor == Executor.LOCAL
