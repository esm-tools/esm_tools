"""Tests for the read-only pydantic view over an assembled workflow graph."""

import copy

import yaml

from esm_runscripts import workflow
from esm_runscripts.plan import Plan, Step, SubPlan, Workflow, run_steps


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

    assert cluster.runs_on is None
    assert cluster.cycling is None


def test_run_steps_logs_each_step_and_does_not_mutate_config():
    sub_plan = SubPlan(
        name="prepare",
        parent_plan="initialization",
        steps=[Step(name="_read_date_file"), Step(name="check_model_lresume")],
    )
    config = {"general": {"setup_name": "awicm3"}}

    result = run_steps(sub_plan, config)

    assert result is config
    assert result == {"general": {"setup_name": "awicm3"}}


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
    assert wf.sub_plans["compute"].run_on_queue == "compute"
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
        submit_to_batch_system=BoolWithProvenance(True, provenance=None),
    )
    assert sub_plan.submit_to_batch_system is True
    assert type(sub_plan.submit_to_batch_system) is bool


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
    assert wf.sub_plans["postprocessing"].run_on_queue == "pp"
    assert "postprocessing" in wf.plans
    # Fan-out: tidy still triggers the next cycle AND the new sub-plan.
    assert wf.resubmit_triggers["tidy"] == ["prepcompute", "postprocessing"]
