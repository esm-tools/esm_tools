"""Read-only pydantic view onto an assembled workflow graph (P-Plan nouns).

``workflow.py`` assembles and mutates the workflow as a plain nested dict
(``config["general"]["workflow"]``) -- that stays the execution engine's
working representation, unchanged by this module. ``Plan.from_config``
snapshots the *result* of that assembly (after
:func:`esm_runscripts.workflow.assemble_workflow`, or at minimum
``complete_plans`` + ``order_plans``) into typed, read-only objects so calling
code can navigate it as ``sim.plan.sub_plans["compute"].nproc`` instead of
threading dict-key strings through by hand.

This is the "models" step of the vocabulary rename's staged plan (see
esm-tools/esm_tools#1519): rename -> models -> REPL -> execution/provenance.
The REPL and execution/provenance layers are future work; this module only
covers structural read access.
"""

from __future__ import annotations

from typing import Dict, List, Literal, Optional

from loguru import logger
from pydantic import BaseModel, ConfigDict


class Step(BaseModel):
    """A single recipe step (``p-plan:Step``) -- a function or bash script
    that makes up a :class:`SubPlan`, run sequentially."""

    model_config = ConfigDict(frozen=True)

    name: str

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        return f"<Step {self.name}>"


class SubPlan(BaseModel):
    """A phase/jobtype recipe (``p-plan:MultiStep``).

    The finer-grained level inside a :class:`Plan` cluster -- e.g.
    ``prepcompute``, ``compute``, ``tidy``, or a coupling phase like
    ``couple_in``.
    """

    model_config = ConfigDict(frozen=True)

    name: str
    parent_plan: str
    nproc: int = 1
    preceded_by: List[str] = []
    run_only: Optional[str] = None
    submit_to_batch_system: bool = False
    run_on_queue: Optional[str] = None
    batch_or_shell: Optional[str] = None
    steps: List[Step] = []
    kind: Literal["esm_subplan", "bash"] = "esm_subplan"

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = [f"nproc={self.nproc}"]
        if self.preceded_by:
            bits.append(f"preceded_by={','.join(self.preceded_by)}")
        if self.submit_to_batch_system:
            bits.append("submit_to_batch_system=True")
        return f"<SubPlan {self.name}  {' '.join(bits)}>"


class Plan(BaseModel):
    """A batch-submission cluster (``p-plan:Plan`` + ``esm:Cluster``), or the
    root workflow itself.

    Clusters (non-root) carry ``sub_plans`` (the phase names batched into this
    one job) and are looked up by name in the root's :attr:`Workflow.plans`.
    The *root* Plan is represented by :class:`Workflow` instead, since it has a
    different shape (``plans`` + ``sub_plans`` pools, no ``preceded_by`` of its
    own).
    """

    model_config = ConfigDict(frozen=True)

    name: str
    sub_plan_names: List[str] = []
    preceded_by: List[str] = []
    next_submit: List[str] = []
    nproc: Optional[int] = None
    batch_or_shell: Optional[str] = None
    submit_to_batch_system: bool = False
    order_in_cluster: Optional[str] = None
    runs_on: Optional[Literal["login_node", "sbatch"]] = None
    cycling: Optional[dict] = None

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = []
        if self.nproc is not None:
            bits.append(f"nproc={self.nproc}")
        if self.preceded_by:
            bits.append(f"preceded_by={','.join(self.preceded_by)}")
        return f"<Plan {self.name}  {' '.join(bits)}>"


class Workflow(BaseModel):
    """The root plan (``p-plan:Plan``): the whole per-chunk workflow.

    Two parallel namespaces, matching ``config["workflow"]``:

    * :attr:`plans` -- clusters, keyed by cluster name.
    * :attr:`sub_plans` -- phases, keyed by phase name (which may be
      section-qualified, e.g. ``prepcompute_echam``, for a model-specific
      phase collected from a coupled model's own workflow block).
    """

    model_config = ConfigDict(frozen=True)

    entry_point: str
    resubmit_triggers: Dict[str, List[str]] = {}
    plans: Dict[str, Plan]
    sub_plans: Dict[str, SubPlan]

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        triggers = (
            f"  resubmit_triggers={self.resubmit_triggers}"
            if self.resubmit_triggers
            else ""
        )
        return f"<Plan 'workflow'  first={self.entry_point}{triggers}>"

    def parent_plan_of(self, sub_plan_name: str) -> Plan:
        """The cluster (:class:`Plan`) that a sub-plan is batched into."""
        return self.plans[self.sub_plans[sub_plan_name].parent_plan]

    @classmethod
    def from_config(cls, config: dict) -> "Workflow":
        """Snapshot ``config["workflow"]`` (top-level, not nested under
        ``general``) into a :class:`Workflow`.

        Expects the assembled shape: every plan has ``sub_plans`` (list of
        names) and ``preceded_by`` resolved (except the entry point), as
        produced by ``complete_plans`` + ``order_plans`` in ``workflow.py``.
        """
        gw_config = config["workflow"]

        sub_plans = {
            name: SubPlan(
                name=name,
                **_only_known_fields(
                    SubPlan, conf, exclude=frozenset({"preceded_by"})
                ),
                preceded_by=_as_list(conf.get("preceded_by")),
            )
            for name, conf in gw_config["sub_plans"].items()
        }
        plans = {
            name: Plan(
                name=name,
                sub_plan_names=conf.get("sub_plans", []),
                **_only_known_fields(
                    Plan, conf, exclude=frozenset({"sub_plans", "preceded_by"})
                ),
                preceded_by=_as_list(conf.get("preceded_by")),
            )
            for name, conf in gw_config["plans"].items()
        }

        # workflow.py's assembled dict is still single-trigger (exit_point
        # always resubmits to entry_point); resubmit_triggers generalizes
        # that shape for the native engine, so wrap it as a one-entry dict
        # here rather than teaching workflow.py the new vocabulary.
        exit_point = gw_config["exit_point"]
        return cls(
            entry_point=gw_config["entry_point"],
            resubmit_triggers={exit_point: [gw_config["entry_point"]]},
            plans=plans,
            sub_plans=sub_plans,
        )

    @classmethod
    def default(cls, config: dict) -> "Workflow":
        """Native equivalent of ``workflow.init_total_workflow``'s default
        case: synthesizes the ``prepcompute -> compute -> tidy`` chain
        directly as pydantic objects, for when no custom ``workflow:``
        block exists in the runscript.

        Each sub-plan is its own singleton cluster (matching the
        documented legacy intent -- "subjobs without a cluster should
        become their own cluster",
        ``examples/workflows/example_workflow.yaml``). Does not cover
        per-model workflow merging, ordering/completion validation, or the
        ``newrun`` self-resubmission wiring that
        ``workflow.assemble_workflow`` also does -- out of scope for this
        pass. Also does not check for a pre-existing custom ``workflow:``
        block before synthesizing -- callers must decide when this applies
        (see ``Workflow.from_config`` for the custom-block path).
        """
        tasks = 0
        for model in config["general"]["valid_model_names"]:
            model_conf = config[model]
            if "nproc" in model_conf:
                tasks += model_conf["nproc"]
            elif "nproca" in model_conf and "nprocb" in model_conf:
                tasks += model_conf["nproca"] * model_conf["nprocb"]
                if "nprocar" in model_conf and "nprocbr" in model_conf:
                    if (
                        model_conf["nprocar"] != "remove_from_namelist"
                        and model_conf["nprocbr"] != "remove_from_namelist"
                    ):
                        tasks += model_conf["nprocar"] * model_conf["nprocbr"]

        sub_plans = {
            "prepcompute": SubPlan(
                name="prepcompute", parent_plan="prepcompute", nproc=1
            ),
            "compute": SubPlan(
                name="compute",
                parent_plan="compute",
                nproc=tasks,
                preceded_by=["prepcompute"],
                submit_to_batch_system=config["general"].get(
                    "submit_to_batch_system", True
                ),
                run_on_queue=config["computer"]["partitions"]["compute"]["name"],
            ),
            "tidy": SubPlan(
                name="tidy", parent_plan="tidy", nproc=1, preceded_by=["compute"]
            ),
        }
        plans = {
            name: Plan(
                name=name,
                sub_plan_names=[name],
                nproc=sub_plan.nproc,
                preceded_by=sub_plan.preceded_by,
                submit_to_batch_system=sub_plan.submit_to_batch_system,
            )
            for name, sub_plan in sub_plans.items()
        }

        return cls(
            entry_point="prepcompute",
            resubmit_triggers={"tidy": ["prepcompute"]},
            plans=plans,
            sub_plans=sub_plans,
        )

    @classmethod
    def resolve(cls, config: dict) -> "Workflow":
        """Pick the right constructor for ``config``.

        If no section anywhere defines a raw ``workflow:`` block, the
        common case (e.g. the AWICM3 test runscript), use
        :meth:`default`. If one exists, fail loudly rather than silently
        ignoring it -- :meth:`default` would otherwise synthesize the
        generic chain and discard the user's actual configuration without
        a word. Resolving a custom block natively isn't implemented yet
        (that's the rest of ``workflow.assemble_workflow``'s job: per-model
        merging, ordering/completion validation, ``newrun`` wiring -- out
        of scope per this pass's agreed scope).
        """
        if _has_custom_workflow_block(config):
            raise NotImplementedError(
                "A custom 'workflow:' block was found in this runscript, but "
                "the native pplan engine only supports the default "
                "prepcompute/compute/tidy synthesis today (Workflow.default). "
                "Resolving a custom workflow block natively is not "
                "implemented yet."
            )
        return cls.default(config)


def _has_custom_workflow_block(config: dict) -> bool:
    """Whether any section of ``config`` defines a raw ``workflow:`` block.

    Same detection shape as ``workflow.translate_legacy_workflow_keys``'s
    walk over ``config.values()`` -- a section can be ``general`` or any
    model name; only ``dict``-valued ``workflow`` keys count.
    """
    return any(
        isinstance(section, dict) and isinstance(section.get("workflow"), dict)
        for section in config.values()
    )


def _as_list(value) -> List[str]:
    """Normalize a legacy single-string ``preceded_by`` (or ``None``) into
    the native list shape. ``workflow.py``'s assembled dicts still store a
    single string per plan; a list is passed through unchanged, so this
    also tolerates a future caller that already provides one."""
    if value is None:
        return []
    if isinstance(value, list):
        return value
    return [value]


def _only_known_fields(model, conf: dict, exclude: frozenset = frozenset()) -> dict:
    """Filter a raw config dict down to keys ``model`` actually declares.

    The engine's working dicts (``workflow.py``) carry bookkeeping fields
    (e.g. ``called_from``, ``run_only`` merged defaults) beyond what the
    read-only view needs; silently drop anything the model doesn't declare
    rather than erroring on every future engine-side field addition.
    """
    known = set(model.model_fields) - {"name"} - exclude
    return {k: v for k, v in conf.items() if k in known}


def run_steps(sub_plan: SubPlan, config: dict) -> dict:
    """Stub executor for a :class:`SubPlan`'s :attr:`steps`.

    Logs each step name in order and returns ``config`` unchanged -- proves
    the native pplan execution mechanism without resolving or calling any
    real plugin function yet (that's ``esm_plugin_manager``'s job, for a
    later pass).
    """
    for step in sub_plan.steps:
        logger.info(f"[pplan stub] would run: {step.name}")
    return config
