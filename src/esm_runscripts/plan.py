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

from typing import Annotated, Dict, List, Literal, Optional

from loguru import logger
from pydantic import BaseModel, BeforeValidator, ConfigDict


def _coerce_bool(value):
    """Coerce esm_parser's ``BoolWithProvenance`` (and similar truthy/falsy
    wrappers) into a real ``bool``.

    ``BoolWithProvenance`` wraps a bool with YAML-provenance tracking but
    is not itself a ``bool`` subclass, so pydantic's lax bool validator
    rejects it outright (``bool_type`` error) even though the value is
    unambiguously ``True``/``False``. Config values sourced straight from a
    runscript/setup YAML (e.g. a choose-block's
    ``submit_to_batch_system: True``) carry this wrapper; literals built in
    Python (``Workflow.default``'s ``.get(..., True)``) don't -- so this
    must tolerate both.
    """
    # `BoolWithProvenance` makes `isinstance(v, bool)` lie (True) via its own
    # `__subclasshook__`-style trickery, even though it is not the literal
    # C-level `bool` type pydantic-core's Rust validator checks against --
    # an `isinstance` guard here would silently pass the wrapper through
    # unconverted. `type(value) is bool` is the only reliable check.
    if type(value) is bool:
        return value
    return bool(value)


ProvenanceBool = Annotated[bool, BeforeValidator(_coerce_bool)]


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
    submit_to_batch_system: ProvenanceBool = False
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
    submit_to_batch_system: ProvenanceBool = False
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
        """Build the default chain, then fold in any per-model supplemental
        ``workflow:`` blocks (e.g. ``configs/setups/awicm3/awicm3.yaml``'s
        ``choose_general.postprocessing: True: workflow: {...}``, or
        similar blocks in ``oifs.yaml``/``icon.yaml``/``foci.yaml``/etc.).

        These are not "a custom workflow the user wrote to replace the
        default chain" -- they're framework-authored, added by a
        component/setup's own config via a choose-block once some flag
        (``postprocessing``, ``tripyview``, ...) resolves true, and they
        only ever *add* one or more extra sub-plans alongside the default
        prepcompute/compute/tidy chain (via ``resubmit_triggers`` fan-out),
        never replace it. Translates the legacy vocabulary first (pure
        rename, reusing ``workflow.translate_legacy_workflow_keys`` --
        not the dict engine's structural assembly) so ``subjobs``/
        ``run_after``/``next_run_triggered_by`` read the same as native
        P-Plan nouns.

        Only raises if ``general.workflow`` itself tries to define its
        own ``entry_point``/``plans`` -- i.e. genuinely replace the root
        chain, not just add a sub-plan to it. That case is still not
        implemented natively.
        """
        from .workflow import translate_legacy_workflow_keys

        translate_legacy_workflow_keys(config)

        general_workflow = config.get("general", {}).get("workflow")
        if isinstance(general_workflow, dict) and (
            "entry_point" in general_workflow or "plans" in general_workflow
        ):
            raise NotImplementedError(
                "general.workflow defines its own 'entry_point'/'plans', "
                "trying to replace the native default chain entirely -- "
                "resolving that natively is not implemented yet."
            )

        wf = cls.default(config)
        extra_sub_plans, extra_plans, extra_triggers = _collect_extra_sub_plans(
            config
        )
        if not extra_sub_plans:
            return wf

        merged_triggers = {name: list(targets) for name, targets in wf.resubmit_triggers.items()}
        for trigger_point, names in extra_triggers.items():
            bucket = merged_triggers.setdefault(trigger_point, [])
            for name in names:
                if name not in bucket:
                    bucket.append(name)

        return cls(
            entry_point=wf.entry_point,
            resubmit_triggers=merged_triggers,
            plans={**wf.plans, **extra_plans},
            sub_plans={**wf.sub_plans, **extra_sub_plans},
        )


def _collect_extra_sub_plans(config: dict):
    """Collect extra sub-plans from every section's (post-translation)
    ``workflow.sub_plans`` block -- e.g. a setup's framework-authored
    ``postprocessing``/``tripyview`` sub-plan. Each becomes its own
    singleton cluster (matching :meth:`Workflow.default`'s convention),
    wired into the chain via ``resubmit_triggers`` fan-out off of each
    sub-plan's own ``preceded_by``, rather than any special-casing of
    ``next_iteration_trigger`` (redundant with ``preceded_by`` for this
    single-sub-plan-per-block shape, and ``preceded_by`` is the more
    granular, reliable source).

    Returns ``(extra_sub_plans, extra_plans, extra_triggers)``.
    """
    extra_sub_plans: Dict[str, SubPlan] = {}
    extra_plans: Dict[str, Plan] = {}
    extra_triggers: Dict[str, List[str]] = {}

    for section in config.values():
        if not isinstance(section, dict):
            continue
        w_config = section.get("workflow")
        if not isinstance(w_config, dict):
            continue
        for name, conf in w_config.get("sub_plans", {}).items():
            preceded_by = _as_list(conf.get("preceded_by"))
            sub_plan = SubPlan(
                name=name,
                parent_plan=name,
                preceded_by=preceded_by,
                **_only_known_fields(
                    SubPlan, conf, exclude=frozenset({"preceded_by"})
                ),
            )
            extra_sub_plans[name] = sub_plan
            extra_plans[name] = Plan(
                name=name,
                sub_plan_names=[name],
                nproc=sub_plan.nproc,
                preceded_by=preceded_by,
                submit_to_batch_system=sub_plan.submit_to_batch_system,
            )
            for predecessor in preceded_by:
                extra_triggers.setdefault(predecessor, []).append(name)

    return extra_sub_plans, extra_plans, extra_triggers


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
