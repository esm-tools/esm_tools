"""Read-only pydantic view onto an assembled workflow graph.

``workflow.py`` assembles and mutates the workflow as a plain nested dict
(``config["general"]["workflow"]``) -- that stays the execution engine's
working representation, unchanged by this module. ``Workflow.from_config``
snapshots the *result* of that assembly (after
:func:`esm_runscripts.workflow.assemble_workflow`, or at minimum
``complete_plans`` + ``order_plans``) into typed, read-only objects so calling
code can navigate it as ``sim.plan.multi_steps["compute"].preceded_by`` instead
of threading dict-key strings through by hand.

``Plan``/``MultiStep``/``Step`` map exactly onto the real P-Plan ontology
(``http://purl.org/net/p-plan#``) -- only the properties that ontology
actually defines (``isStepOfPlan``, ``isPrecededBy``). Everything esm-
specific about *how* a sub-plan actually runs (process count, executor,
queue, launch mode, ...) lives separately in :class:`PlanExecution`/
:class:`MultiStepExecution` -- openly esm-invented vocabulary, not claimed
to align to any ontology (none has been selected for that side yet).

``Step``/``MultiStep.steps``/``run_steps()`` model a sub-plan's individual
recipe steps, resolved and run through ``esm_plugin_manager`` exactly like
the legacy engine's recipes. No ``MultiStep`` built here is given a
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


class MultiStepKind(StrEnum):
    """What a :class:`MultiStep` fundamentally is, orthogonal to how or
    where it runs (that's :class:`MultiStepExecution`)."""

    ESM_MULTISTEP = enum.auto()
    """A Python recipe -- steps resolved and executed one at a time through
    the plugin mechanism (e.g. an ``observe`` sub-plan's
    ``init_observe_logs`` -> ``wait_and_observe`` -> ``wake_up_call``)."""

    BASH = enum.auto()
    """A raw script file, run directly -- not driven through Python steps."""


class LaunchMode(StrEnum):
    """How a sub-plan launches relative to the rest of its running job
    script (POSIX job-control vocabulary: does it block the script, or get
    kicked off and watched independently)."""

    FOREGROUND = enum.auto()
    """Blocks the job script -- runs as the next instruction, and the
    script waits for it to finish."""

    BACKGROUND = enum.auto()
    """Launched without blocking the job script -- ``&``, PID captured
    (e.g. ``{{SUBJOB_COMMAND}} & {{SUBJOB_NAME}}_PID=$!``). Whether and how
    anything watches that PID afterward is a separate concern, not implied
    by this value."""


class Executor(StrEnum):
    """Where/how a cluster's work actually runs."""

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
    ``workflow.py``'s ``order_in_cluster``, controlling whether a cluster's
    total ``nproc`` is the max or the sum of its sub-plans' ``nproc``)."""

    SEQUENTIAL = enum.auto()
    """Sub-plans run one after another, same allocation -- cluster
    ``nproc`` is the max of its sub-plans' ``nproc``. Legacy default."""

    CONCURRENT = enum.auto()
    """Sub-plans run side by side, sharing one allocation -- backgrounded
    (``&``) with a trailing ``wait``. Cluster ``nproc`` is the sum of its
    sub-plans' ``nproc``."""


class Step(ProvenanceFreeModel):
    """A single recipe step (``p-plan:Step``) -- a function or bash script
    that makes up a :class:`MultiStep`, run sequentially."""

    model_config = ConfigDict(frozen=True)

    name: str

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        return f"<Step {self.name}>"


class MultiStep(ProvenanceFreeModel):
    """A phase/jobtype recipe (``p-plan:MultiStep``) -- the finer-grained
    level inside a :class:`Plan` cluster, e.g. ``prepcompute``, ``compute``,
    ``tidy``, or a coupling phase like ``couple_in``.

    Only the two real P-Plan properties that apply to a ``MultiStep`` (also
    a ``Step``) are modeled here: ``step_of_plan`` (``isStepOfPlan``, domain
    ``Step``, range ``Plan`` -- which cluster it's batched into) and
    ``preceded_by`` (``isPrecededBy``, ``Step``-``Step``). Everything about
    how it actually runs is :class:`MultiStepExecution`, looked up
    separately by name.
    """

    model_config = ConfigDict(frozen=True)

    name: str
    step_of_plan: str
    preceded_by: List[str] = []
    steps: List[Step] = []

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = []
        if self.preceded_by:
            bits.append(f"preceded_by={','.join(self.preceded_by)}")
        return f"<MultiStep {self.name}  {' '.join(bits)}>"


class MultiStepExecution(ProvenanceFreeModel):
    """esm-specific execution metadata for a :class:`MultiStep`, looked up
    by name in :attr:`Workflow.multi_step_execution`. Not ontology vocabulary
    -- no ontology has been selected for this side yet."""

    model_config = ConfigDict(frozen=True)

    nproc: int = 1
    kind: MultiStepKind = MultiStepKind.ESM_MULTISTEP
    launch_mode: Optional[LaunchMode] = None
    # Legacy's conditional-skip mechanism (workflow.skip_plan: run_only,
    # skip_chunk_number, skip_run_number) is cycling-condition territory --
    # belongs in PlanExecution.cycling once that's shaped, not modeled here.


class Plan(ProvenanceFreeModel):
    """A batch-submission cluster (``p-plan:Plan``), or the root workflow
    itself.

    Looked up by name in the root's :attr:`Workflow.plans`. The *root* Plan
    is represented by :class:`Workflow` instead, since it has a different
    shape (``plans`` + ``multi_steps`` pools, no ``preceded_by`` of its own).
    Only ``preceded_by`` is modeled here -- the cluster-level fact that one
    cluster runs after another (for ordering clusters relative to each
    other, not a direct P-Plan property at the ``Plan`` level; P-Plan's own
    ``isPrecededBy`` is defined on ``Step``, not ``Plan``). Everything else
    about a cluster's execution is :class:`PlanExecution`.
    """

    model_config = ConfigDict(frozen=True)

    name: str
    preceded_by: List[str] = []

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = []
        if self.preceded_by:
            bits.append(f"preceded_by={','.join(self.preceded_by)}")
        return f"<Plan {self.name}  {' '.join(bits)}>"


class PlanExecution(ProvenanceFreeModel):
    """esm-specific execution metadata for a :class:`Plan`, looked up by
    name in :attr:`Workflow.plan_execution`. Not ontology vocabulary -- no
    ontology has been selected for this side yet."""

    model_config = ConfigDict(frozen=True)

    multi_step_names: List[str] = []
    next_submit: List[str] = []
    nproc: Optional[int] = None
    order_in_cluster: Optional[OrderInCluster] = None
    executor: Optional[Executor] = None
    run_on_queue: Optional[str] = None
    cycling: Optional[dict] = None


class Workflow(ProvenanceFreeModel):
    """The root plan (``p-plan:Plan``): the whole per-chunk workflow.

    Four parallel namespaces, all keyed by the same sub-plan/cluster names:

    * :attr:`plans` -- clusters' ontology-mapped shape.
    * :attr:`plan_execution` -- clusters' esm execution metadata.
    * :attr:`multi_steps` -- phases' ontology-mapped shape (names may be
      section-qualified, e.g. ``prepcompute_echam``, for a model-specific
      phase collected from a coupled model's own workflow block).
    * :attr:`multi_step_execution` -- phases' esm execution metadata.
    """

    model_config = ConfigDict(frozen=True)

    entry_point: str
    resubmit_triggers: Dict[str, List[str]] = {}
    plans: Dict[str, Plan]
    plan_execution: Dict[str, PlanExecution]
    multi_steps: Dict[str, MultiStep]
    multi_step_execution: Dict[str, MultiStepExecution]

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        triggers = (
            f"  resubmit_triggers={self.resubmit_triggers}"
            if self.resubmit_triggers
            else ""
        )
        return f"<Plan 'workflow'  first={self.entry_point}{triggers}>"

    def parent_plan_of(self, multi_step_name: str) -> Plan:
        """The cluster (:class:`Plan`) that a sub-plan is batched into."""
        return self.plans[self.multi_steps[multi_step_name].step_of_plan]

    def execution_of(self, multi_step_name: str) -> PlanExecution:
        """The execution metadata of the cluster a sub-plan is batched
        into."""
        return self.plan_execution[self.multi_steps[multi_step_name].step_of_plan]

    @classmethod
    def from_config(cls, config: dict) -> "Workflow":
        """Snapshot ``config["workflow"]`` (top-level, not nested under
        ``general``) into a :class:`Workflow`.

        Expects the assembled shape: every plan has ``multi_steps`` (list of
        names) and ``preceded_by`` resolved (except the entry point), as
        produced by ``complete_plans`` + ``order_plans`` in ``workflow.py``.
        """
        gw_config = config["workflow"]

        multi_steps = {}
        multi_step_execution = {}
        for name, conf in gw_config["sub_plans"].items():
            preceded_by = _as_list(conf.get("preceded_by"))
            multi_steps[name] = MultiStep(
                name=name,
                step_of_plan=conf.get("parent_plan", name),
                preceded_by=preceded_by,
                **_only_known_fields(
                    MultiStep,
                    conf,
                    exclude=frozenset({"preceded_by", "step_of_plan"}),
                ),
            )
            multi_step_execution[name] = MultiStepExecution(
                **_only_known_fields(MultiStepExecution, conf)
            )

        plans = {}
        plan_execution = {}
        for name, conf in gw_config["plans"].items():
            preceded_by = _as_list(conf.get("preceded_by"))
            plans[name] = Plan(
                name=name,
                preceded_by=preceded_by,
                **_only_known_fields(
                    Plan, conf, exclude=frozenset({"preceded_by"})
                ),
            )
            plan_execution[name] = PlanExecution(
                multi_step_names=conf.get("sub_plans", []),
                **_only_known_fields(
                    PlanExecution, conf, exclude=frozenset({"multi_step_names"})
                ),
            )

        # workflow.py's assembled dict is still single-trigger (exit_point
        # always resubmits to entry_point); resubmit_triggers generalizes
        # that shape for the native engine, so wrap it as a one-entry dict
        # here rather than teaching workflow.py the new vocabulary.
        exit_point = gw_config["exit_point"]
        return cls(
            entry_point=gw_config["entry_point"],
            resubmit_triggers={exit_point: [gw_config["entry_point"]]},
            plans=plans,
            plan_execution=plan_execution,
            multi_steps=multi_steps,
            multi_step_execution=multi_step_execution,
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

        multi_steps = {
            "prepcompute": MultiStep(name="prepcompute", step_of_plan="prepcompute"),
            "compute": MultiStep(
                name="compute",
                step_of_plan="compute",
                preceded_by=["prepcompute"],
            ),
            "tidy": MultiStep(
                name="tidy", step_of_plan="tidy", preceded_by=["compute"]
            ),
        }
        multi_step_execution = {
            "prepcompute": MultiStepExecution(nproc=1),
            "compute": MultiStepExecution(
                nproc=tasks, launch_mode=LaunchMode.BACKGROUND
            ),
            "tidy": MultiStepExecution(nproc=1),
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
                name="prepcompute", preceded_by=multi_steps["prepcompute"].preceded_by
            ),
            "compute": Plan(
                name="compute", preceded_by=multi_steps["compute"].preceded_by
            ),
            "tidy": Plan(name="tidy", preceded_by=multi_steps["tidy"].preceded_by),
        }
        plan_execution = {
            "prepcompute": PlanExecution(
                multi_step_names=["prepcompute"],
                nproc=multi_step_execution["prepcompute"].nproc,
                executor=Executor.IN_PROCESS,
            ),
            "compute": PlanExecution(
                multi_step_names=["compute"],
                nproc=multi_step_execution["compute"].nproc,
                executor=compute_executor,
                run_on_queue=config["computer"]["partitions"]["compute"]["name"],
            ),
            "tidy": PlanExecution(
                multi_step_names=["tidy"],
                nproc=multi_step_execution["tidy"].nproc,
                executor=Executor.IN_PROCESS,
            ),
        }

        return cls(
            entry_point="prepcompute",
            resubmit_triggers={"tidy": ["prepcompute"]},
            plans=plans,
            plan_execution=plan_execution,
            multi_steps=multi_steps,
            multi_step_execution=multi_step_execution,
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
        (
            extra_multi_steps,
            extra_multi_step_execution,
            extra_plans,
            extra_plan_execution,
            extra_triggers,
        ) = _collect_extra_multi_steps(config)
        if not extra_multi_steps:
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
            plan_execution={**wf.plan_execution, **extra_plan_execution},
            multi_steps={**wf.multi_steps, **extra_multi_steps},
            multi_step_execution={
                **wf.multi_step_execution,
                **extra_multi_step_execution,
            },
        )


def _collect_extra_multi_steps(config: dict):
    """Collect extra sub-plans from every section's (post-translation)
    ``workflow.multi_steps`` block -- e.g. a setup's framework-authored
    ``postprocessing``/``tripyview`` sub-plan. Each becomes its own
    singleton cluster (matching :meth:`Workflow.default`'s convention),
    wired into the chain via ``resubmit_triggers`` fan-out off of each
    sub-plan's own ``preceded_by``, rather than any special-casing of
    ``next_iteration_trigger`` (redundant with ``preceded_by`` for this
    single-sub-plan-per-block shape, and ``preceded_by`` is the more
    granular, reliable source).

    Returns ``(extra_multi_steps, extra_multi_step_execution, extra_plans,
    extra_plan_execution, extra_triggers)``.
    """
    extra_multi_steps: Dict[str, MultiStep] = {}
    extra_multi_step_execution: Dict[str, MultiStepExecution] = {}
    extra_plans: Dict[str, Plan] = {}
    extra_plan_execution: Dict[str, PlanExecution] = {}
    extra_triggers: Dict[str, List[str]] = {}

    for section in config.values():
        if not isinstance(section, dict):
            continue
        w_config = section.get("workflow")
        if not isinstance(w_config, dict):
            continue
        for name, conf in w_config.get("sub_plans", {}).items():
            preceded_by = _as_list(conf.get("preceded_by"))
            extra_multi_steps[name] = MultiStep(
                name=name,
                step_of_plan=name,
                preceded_by=preceded_by,
                **_only_known_fields(
                    MultiStep,
                    conf,
                    exclude=frozenset({"preceded_by", "step_of_plan"}),
                ),
            )
            execution = MultiStepExecution(
                **_only_known_fields(MultiStepExecution, conf)
            )
            extra_multi_step_execution[name] = execution
            # run_on_queue/submit_to_batch_system are cluster-level facts
            # (same Plan, same batch submission) -- read off the raw conf,
            # not the MultiStep/MultiStepExecution. Default False, matching
            # the legacy dict engine's own default for a sub-plan that
            # omits the key (workflow.py: "if 'submit_to_batch_system' not
            # in plan_conf: plan_conf['submit_to_batch_system'] = False";
            # resubmit.py's get_submission_type same default) -- unlike
            # Workflow.default()'s "compute" sub-plan, which legacy always
            # batch-submits by default.
            executor = (
                Executor.BATCH
                if conf.get("submit_to_batch_system", False)
                else Executor.LOCAL
            )
            extra_plans[name] = Plan(name=name, preceded_by=preceded_by)
            extra_plan_execution[name] = PlanExecution(
                multi_step_names=[name],
                nproc=execution.nproc,
                executor=executor,
                run_on_queue=conf.get("run_on_queue"),
            )
            for predecessor in preceded_by:
                extra_triggers.setdefault(predecessor, []).append(name)

    return (
        extra_multi_steps,
        extra_multi_step_execution,
        extra_plans,
        extra_plan_execution,
        extra_triggers,
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


def run_steps(multi_step: MultiStep, config: dict) -> dict:
    """Resolve and run a :class:`MultiStep`'s :attr:`steps`, in order.

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
    if not multi_step.steps:
        return config

    recipe = {
        "recipe": [step.name for step in multi_step.steps],
        "job_type": multi_step.name,
    }
    plugins_bare = (
        esm_tools.get_config_filepath()
        + "/esm_software/esm_runscripts/esm_plugins.yaml"
    )
    plugins = esm_plugin_manager.read_plugin_information(plugins_bare, recipe)
    esm_plugin_manager.check_plugin_availability(plugins)
    return esm_plugin_manager.work_through_recipe(recipe, plugins, config)
