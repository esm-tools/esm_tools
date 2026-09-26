"""Tests for the read-only pydantic view over an assembled workflow graph."""

import copy

import yaml

from esm_runscripts import workflow
from esm_runscripts.plan import Plan, SubPlan, Workflow


def _assembled_config(workflow_block):
    """Run a raw ``general.workflow`` block through complete_plans + order_plans."""
    gw = {"plans": {}, **copy.deepcopy(workflow_block)}
    config = {"general": {"workflow": gw}}
    workflow.translate_legacy_workflow_keys(config)
    workflow.complete_plans(config)
    workflow.order_plans(config)
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
    assert compute.preceded_by == "prepcompute"
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
    assert wf.exit_point == "couple_out"
    assert wf.sub_plans["compute"].nproc == 1

    # Walking the ordering chain via preceded_by should reach every sub-plan.
    order = [wf.entry_point]
    remaining = set(wf.sub_plans) - {wf.entry_point}
    while remaining:
        nxt = next(
            name
            for name in remaining
            if wf.sub_plans[name].preceded_by == order[-1]
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
    assert "last=tidy" in repr(wf)
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
