"""Tests for the read-only pydantic view over an assembled workflow graph."""

import copy

import yaml

from esm_runscripts import workflow
from esm_runscripts.pplan.plan import (
    Executor,
    MultiStep,
    MultiStepExecution,
    Plan,
    PlanExecution,
    Workflow,
)


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
    assert set(wf.multi_steps) == {"prepcompute", "compute", "tidy"}

    compute = wf.multi_steps["compute"]
    assert isinstance(compute, MultiStep)
    assert compute.preceded_by == ["prepcompute"]
    assert compute.step_of_plan == "compute"

    parent = wf.parent_plan_of("compute")
    assert isinstance(parent, Plan)
    assert parent.name == "compute"
    assert "compute" in wf.plan_execution["compute"].multi_step_names


def test_workflow_from_config_coupled_chain(shared_datadir):
    block = yaml.safe_load(
        (shared_datadir / "workflow_coupled_new.yaml").read_text()
    )
    wf = Workflow.from_config(_assembled_config(block))

    assert wf.entry_point == "couple_in"
    assert wf.resubmit_triggers == {"couple_out": ["couple_in"]}
    assert wf.multi_step_execution["compute"].nproc == 1

    # Walking the ordering chain via preceded_by should reach every sub-plan.
    order = [wf.entry_point]
    remaining = set(wf.multi_steps) - {wf.entry_point}
    while remaining:
        nxt = next(
            name
            for name in remaining
            if wf.multi_steps[name].preceded_by == [order[-1]]
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
    assert "compute" in repr(wf.multi_steps["compute"])


def test_models_are_frozen(shared_datadir):
    block = yaml.safe_load(
        (shared_datadir / "workflow_default_new.yaml").read_text()
    )
    wf = Workflow.from_config(_assembled_config(block))

    compute = wf.multi_steps["compute"]
    try:
        compute.preceded_by = []
        raised = False
    except Exception:
        raised = True
    assert raised, "MultiStep should be immutable (frozen)"


def test_multi_step_defaults_for_new_fields():
    multi_step = MultiStep(name="prepare", step_of_plan="initialization")
    execution = MultiStepExecution()

    assert multi_step.preceded_by == []
    assert execution.kind == "esm_multistep"


def test_plan_defaults_for_new_fields():
    cluster = Plan(name="initialization")
    execution = PlanExecution()

    assert execution.executor is None
    assert execution.cycling is None




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

    assert wf.entry_point == "prepexp"
    assert wf.resubmit_triggers == {
        "prepexp": ["prepcompute"],
        "tidy": ["prepcompute"],
    }
    assert set(wf.multi_steps) == {"prepexp", "prepcompute", "compute", "tidy"}
    assert wf.multi_step_execution["compute"].nproc == 128 + 8 * 16
    assert wf.multi_steps["prepcompute"].preceded_by == ["prepexp"]
    assert wf.multi_steps["compute"].preceded_by == ["prepcompute"]
    assert wf.multi_step_execution["compute"].launch_mode == "background"
    assert wf.plan_execution["compute"].run_on_queue == "compute"
    assert wf.plan_execution["compute"].executor == Executor.BATCH
    assert wf.plan_execution["prepexp"].executor == Executor.IN_PROCESS
    assert wf.plan_execution["prepcompute"].executor == Executor.IN_PROCESS
    assert wf.plan_execution["tidy"].executor == Executor.IN_PROCESS
    assert wf.multi_steps["tidy"].preceded_by == ["compute"]
    assert wf.parent_plan_of("compute").name == "compute"
    # Each sub-plan is its own singleton cluster.
    assert set(wf.plans) == {"prepexp", "prepcompute", "compute", "tidy"}


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

    assert wf.entry_point == "prepexp"


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
    # A workflow: block with no actual multi_steps in it (no entry_point/plans
    # either) has nothing to merge or replace -- it's not a reason to fail.
    config = _no_custom_workflow_config()
    config["general"]["workflow"] = {"multi_steps": {}}

    wf = Workflow.resolve(config)

    assert wf.entry_point == "prepexp"


def test_plan_execution_accepts_none_with_provenance():
    # A YAML ``null`` arrives as NoneWithProvenance, which pydantic rejects
    # for Optional[str] fields unless the provenance is stripped first.
    from esm_parser.provenance import NoneWithProvenance

    execution = PlanExecution(
        run_on_queue=NoneWithProvenance(None, provenance=None),
    )
    assert execution.run_on_queue is None


def test_workflow_resolve_merges_a_real_supplemental_multi_step():
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

    assert wf.entry_point == "prepexp"
    assert "postprocessing" in wf.multi_steps
    assert wf.multi_steps["postprocessing"].preceded_by == ["tidy"]
    assert "postprocessing" in wf.plans
    assert wf.plan_execution["postprocessing"].run_on_queue == "pp"
    assert wf.plan_execution["postprocessing"].executor == Executor.BATCH
    # Fan-out: tidy still triggers the next cycle AND the new sub-plan.
    assert wf.resubmit_triggers["tidy"] == ["prepcompute", "postprocessing"]
    # prepexp's own trigger (bootstrapping prepcompute) stays untouched.
    assert wf.resubmit_triggers["prepexp"] == ["prepcompute"]


def test_collect_extra_multi_steps_defaults_executor_to_local_when_omitted():
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

    assert wf.plan_execution["inline_postprocessing"].executor == Executor.LOCAL
