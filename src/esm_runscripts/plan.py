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

from typing import Dict, List, Optional

from pydantic import BaseModel, ConfigDict


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
    preceded_by: Optional[str] = None
    run_only: Optional[str] = None
    submit_to_batch_system: bool = False
    run_on_queue: Optional[str] = None
    batch_or_shell: Optional[str] = None

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = [f"nproc={self.nproc}"]
        if self.preceded_by:
            bits.append(f"preceded_by={self.preceded_by}")
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
    preceded_by: Optional[str] = None
    next_submit: List[str] = []
    nproc: Optional[int] = None
    batch_or_shell: Optional[str] = None
    submit_to_batch_system: bool = False
    order_in_cluster: Optional[str] = None

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        bits = []
        if self.nproc is not None:
            bits.append(f"nproc={self.nproc}")
        if self.preceded_by:
            bits.append(f"preceded_by={self.preceded_by}")
        return f"<Plan {self.name}  {' '.join(bits)}>"


class Workflow(BaseModel):
    """The root plan (``p-plan:Plan``): the whole per-chunk workflow.

    Two parallel namespaces, matching ``config["general"]["workflow"]``:

    * :attr:`plans` -- clusters, keyed by cluster name.
    * :attr:`sub_plans` -- phases, keyed by phase name (which may be
      section-qualified, e.g. ``prepcompute_echam``, for a model-specific
      phase collected from a coupled model's own workflow block).
    """

    model_config = ConfigDict(frozen=True)

    entry_point: str
    exit_point: str
    next_iteration_trigger: Optional[str] = None
    plans: Dict[str, Plan]
    sub_plans: Dict[str, SubPlan]

    def __repr__(self) -> str:  # pragma: no cover - cosmetic
        trigger = f"  next_iteration_trigger={self.next_iteration_trigger}" if self.next_iteration_trigger else ""
        return (
            f"<Plan 'workflow'  first={self.entry_point}  "
            f"last={self.exit_point}{trigger}>"
        )

    def parent_plan_of(self, sub_plan_name: str) -> Plan:
        """The cluster (:class:`Plan`) that a sub-plan is batched into."""
        return self.plans[self.sub_plans[sub_plan_name].parent_plan]

    @classmethod
    def from_config(cls, config: dict) -> "Workflow":
        """Snapshot ``config["general"]["workflow"]`` into a :class:`Workflow`.

        Expects the assembled shape: every plan has ``sub_plans`` (list of
        names) and ``preceded_by`` resolved (except the entry point), as
        produced by ``complete_plans`` + ``order_plans`` in ``workflow.py``.
        """
        gw_config = config["general"]["workflow"]

        sub_plans = {
            name: SubPlan(name=name, **_only_known_fields(SubPlan, conf))
            for name, conf in gw_config["sub_plans"].items()
        }
        plans = {
            name: Plan(
                name=name,
                sub_plan_names=conf.get("sub_plans", []),
                **_only_known_fields(Plan, conf, exclude=frozenset({"sub_plans"})),
            )
            for name, conf in gw_config["plans"].items()
        }

        return cls(
            entry_point=gw_config["entry_point"],
            exit_point=gw_config["exit_point"],
            next_iteration_trigger=gw_config.get("next_iteration_trigger"),
            plans=plans,
            sub_plans=sub_plans,
        )


def _only_known_fields(model, conf: dict, exclude: frozenset = frozenset()) -> dict:
    """Filter a raw config dict down to keys ``model`` actually declares.

    The engine's working dicts (``workflow.py``) carry bookkeeping fields
    (e.g. ``called_from``, ``run_only`` merged defaults) beyond what the
    read-only view needs; silently drop anything the model doesn't declare
    rather than erroring on every future engine-side field addition.
    """
    known = set(model.model_fields) - {"name"} - exclude
    return {k: v for k, v in conf.items() if k in known}
