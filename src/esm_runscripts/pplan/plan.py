"""Read-only pydantic view onto an assembled workflow graph (P-Plan vocabulary).

``workflow.py`` assembles and mutates the workflow as a plain nested dict
(``config["general"]["workflow"]``) -- that stays the execution engine's
working representation, unchanged by this module. ``Workflow.from_config``
snapshots the *result* of that assembly (after
:func:`esm_runscripts.workflow.assemble_workflow`, or at minimum
``complete_plans`` + ``order_plans``) into typed, read-only objects so calling
code can navigate it as ``sim.plan.sub_plans["compute"].nproc`` instead of
threading dict-key strings through by hand.

``Step``/``SubPlan.steps``/``run_steps()`` model a sub-plan's individual
recipe steps, resolved and run through ``esm_plugin_manager`` exactly like
the legacy engine's recipes. No ``SubPlan`` built here is given a
non-empty ``steps`` list yet, though -- the per-invocation work that
exists today still runs through ``RunStateBuilder``'s hardcoded method
chain instead (``simulation_config.py``); wiring real step lists onto
``prepcompute``/``compute``/``tidy`` is future work.
"""

from __future__ import annotations

import enum
from typing import Dict, List, Optional

import esm_plugin_manager
import esm_tools
from esm_parser.pydantic_models import ProvenanceFreeModel
from esm_tools._compat import StrEnum
from pydantic import ConfigDict


class SubPlanKind(StrEnum):
    """What a :class:`SubPlan` fundamentally is, orthogonal to how or where
    it runs (that's :attr:`SubPlan.launch_mode` and the :class:`Plan` it
    belongs to)."""

    ESM_SUBPLAN = enum.auto()
    """A Python recipe -- steps resolved and executed one at a time through
    the plugin mechanism (e.g. an ``observe`` sub-plan's
    ``init_observe_logs`` -> ``wait_and_observe`` -> ``wake_up_call``)."""

    BASH = enum.auto()
    """A raw script file, run directly -- not driven through Python steps."""


class LaunchMode(StrEnum):
    """How a :class:`SubPlan` launches relative to the rest of its running
    job script (POSIX job-control vocabulary: does it block the script, or
    get kicked off and watched independently)."""

    FOREGROUND = enum.auto()
    """Blocks the job script -- runs as the next instruction, and the
    script waits for it to finish."""

    BACKGROUND = enum.auto()
    """Launched without blocking the job script -- ``&``, PID captured
    (e.g. ``{{SUBJOB_COMMAND}} & {{SUBJOB_NAME}}_PID=$!``). Whether and how
    anything watches that PID afterward is a separate concern, not implied
    by this value."""


class Executor(StrEnum):
    """Where/how a :class:`Plan`'s work actually runs."""

    IN_PROCESS = enum.auto()
    """No new OS process at all -- the currently-running ``esm_runscripts``
    process calls straight into Python for the next sub-plan."""

    LOCAL = enum.auto()
    """Spawns a new process, run directly (no scheduler)."""

    BATCH = enum.auto()
    """Spawns a new process, submitted to whatever scheduler
    ``computer.batch_system`` names (SLURM, PBS, ...) -- never a literal
    scheduler command name, since esm_tools supports more than one."""


class OrderInCluster(StrEnum):
    """How a cluster's sub-plans share their allocation (legacy
    ``workflow.py``'s ``order_in_cluster``, controlling whether
    :attr:`Plan.nproc` is the max or the sum of its sub-plans' ``nproc``)."""

    SEQUENTIAL = enum.auto()
    """Sub-plans run one after another, same allocation -- ``Plan.nproc``
    is the max of its sub-plans' ``nproc``. Legacy default."""

    CONCURRENT = enum.auto()
    """Sub-plans run side by side, sharing one allocation -- backgrounded
    (``&``) with a trailing ``wait``. ``Plan.nproc`` is the sum of its
    sub-plans' ``nproc``."""


class Step(ProvenanceFreeModel):
    """A single recipe step (``p-plan:Step``) -- a function or bash script
    that makes up a :class:`SubPlan`, run sequentially."""

    model_config = ConfigDict(frozen=True)

    name: str

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        return f"<Step {self.name}>"


class SubPlan(ProvenanceFreeModel):
    """A phase/jobtype recipe (``p-plan:MultiStep``).

    The finer-grained level inside a :class:`Plan` cluster -- e.g.
    ``prepcompute``, ``compute``, ``tidy``, or a coupling phase like
    ``couple_in``. ``step_of_plan`` is ``p-plan:isStepOfPlan`` (domain
    ``Step``, range ``Plan``) -- MultiStep is also a Step, so this is the
    property that names which cluster it's batched into.
    """

    model_config = ConfigDict(frozen=True)

    name: str
    step_of_plan: str
    nproc: int = 1
    preceded_by: List[str] = []
    steps: List[Step] = []
    kind: SubPlanKind = SubPlanKind.ESM_SUBPLAN
    launch_mode: Optional[LaunchMode] = None
    # Legacy's conditional-skip mechanism (workflow.skip_plan: run_only,
    # skip_chunk_number, skip_run_number) is cycling-condition territory --
    # belongs in Plan.cycling once that's shaped, not modeled here.

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = [f"nproc={self.nproc}"]
        if self.preceded_by:
            bits.append(f"preceded_by={','.join(self.preceded_by)}")
        if self.launch_mode:
            bits.append(f"launch_mode={self.launch_mode.value}")
        return f"<SubPlan {self.name}  {' '.join(bits)}>"


class Plan(ProvenanceFreeModel):
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
    order_in_cluster: Optional[OrderInCluster] = None
    executor: Optional[Executor] = None
    run_on_queue: Optional[str] = None
    cycling: Optional[dict] = None

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = []
        if self.nproc is not None:
            bits.append(f"nproc={self.nproc}")
        if self.preceded_by:
            bits.append(f"preceded_by={','.join(self.preceded_by)}")
        return f"<Plan {self.name}  {' '.join(bits)}>"


class Workflow(ProvenanceFreeModel):
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
        return self.plans[self.sub_plans[sub_plan_name].step_of_plan]

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
                step_of_plan=conf.get("parent_plan", name),
                **_only_known_fields(
                    SubPlan,
                    conf,
                    exclude=frozenset({"preceded_by", "step_of_plan"}),
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
                name="prepcompute", step_of_plan="prepcompute", nproc=1
            ),
            "compute": SubPlan(
                name="compute",
                step_of_plan="compute",
                nproc=tasks,
                preceded_by=["prepcompute"],
                launch_mode=LaunchMode.BACKGROUND,
            ),
            "tidy": SubPlan(
                name="tidy", step_of_plan="tidy", nproc=1, preceded_by=["compute"]
            ),
        }

        # Matches legacy's own default dispatch (resubmit.get_submission_type):
        # prepcompute/tidy run in-process (the daemon calls straight into
        # Python) unless something marks the whole run batch-submitted;
        # compute always spawns, either local or scheduled.
        compute_executor = (
            Executor.BATCH
            if config["general"].get("submit_to_batch_system", True)
            else Executor.LOCAL
        )
        plans = {
            "prepcompute": Plan(
                name="prepcompute",
                sub_plan_names=["prepcompute"],
                nproc=sub_plans["prepcompute"].nproc,
                preceded_by=sub_plans["prepcompute"].preceded_by,
                executor=Executor.IN_PROCESS,
            ),
            "compute": Plan(
                name="compute",
                sub_plan_names=["compute"],
                nproc=sub_plans["compute"].nproc,
                preceded_by=sub_plans["compute"].preceded_by,
                executor=compute_executor,
                run_on_queue=config["computer"]["partitions"]["compute"]["name"],
            ),
            "tidy": Plan(
                name="tidy",
                sub_plan_names=["tidy"],
                nproc=sub_plans["tidy"].nproc,
                preceded_by=sub_plans["tidy"].preceded_by,
                executor=Executor.IN_PROCESS,
            ),
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
        from ..workflow import translate_legacy_workflow_keys

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
                step_of_plan=name,
                preceded_by=preceded_by,
                **_only_known_fields(
                    SubPlan,
                    conf,
                    exclude=frozenset({"preceded_by", "step_of_plan"}),
                ),
            )
            extra_sub_plans[name] = sub_plan
            # run_on_queue/submit_to_batch_system are cluster-level facts
            # (same Plan, same batch submission) -- read off the raw conf,
            # not the SubPlan, which no longer carries them. Default False,
            # matching the legacy dict engine's own default for a sub-plan
            # that omits the key (workflow.py: "if 'submit_to_batch_system'
            # not in plan_conf: plan_conf['submit_to_batch_system'] = False";
            # resubmit.py's get_submission_type same default) -- unlike
            # Workflow.default()'s "compute" sub-plan below, which legacy
            # always batch-submits by default.
            executor = (
                Executor.BATCH
                if conf.get("submit_to_batch_system", False)
                else Executor.LOCAL
            )
            extra_plans[name] = Plan(
                name=name,
                sub_plan_names=[name],
                nproc=sub_plan.nproc,
                preceded_by=preceded_by,
                executor=executor,
                run_on_queue=conf.get("run_on_queue"),
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
    """Resolve and run a :class:`SubPlan`'s :attr:`steps`, in order.

    Reuses ``esm_plugin_manager`` wholesale -- the same
    ``read_plugin_information`` + ``check_plugin_availability`` +
    ``work_through_recipe`` sequence ``helpers.evaluate`` already uses for
    the legacy engine's recipes -- rather than re-resolving step names to
    callables natively. Every step function already has the uniform
    ``fn(config) -> config`` signature (the same shape every
    ``RunStateBuilder.with_*()`` method wraps), and there is no separate
    success/failure return value to model: a step either returns normally
    (success) or raises (failure propagates to the caller), exactly like
    the legacy engine.
    """
    if not sub_plan.steps:
        return config

    recipe = {
        "recipe": [step.name for step in sub_plan.steps],
        "job_type": sub_plan.name,
    }
    plugins_bare = (
        esm_tools.get_config_filepath()
        + "/esm_software/esm_runscripts/esm_plugins.yaml"
    )
    plugins = esm_plugin_manager.read_plugin_information(plugins_bare, recipe)
    esm_plugin_manager.check_plugin_availability(plugins)
    return esm_plugin_manager.work_through_recipe(recipe, plugins, config)
