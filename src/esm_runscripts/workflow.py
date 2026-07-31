import copy
import sys
import warnings
from typing import Dict

import deprecation
import esm_parser
import esm_tools
import esm_tools.esm_warnings  # noqa: F401  installs the deprecation visibility filter
from loguru import logger


class WorkflowError(Exception):
    """Base class for errors raised while assembling the workflow graph."""


class UnknownPlanError(WorkflowError):
    """A plan orders itself after a predecessor that is not a known plan."""


_LEGACY_KEYS_SPEC: dict = esm_parser.yaml_file_to_dict(
    f"{esm_tools.get_config_filepath()}"
    "/esm_software/esm_runscripts/workflow_legacy_keys.yaml"
)
"""Legacy ``general.workflow`` key spec, loaded from
``configs/esm_software/esm_runscripts/workflow_legacy_keys.yaml``: the rename
table plus its deprecation schedule, kept as data rather than inline here."""

_LEGACY_WORKFLOW_KEYS: Dict[str, str] = _LEGACY_KEYS_SPEC["renames"]
"""Maps each legacy ``general.workflow`` key to its current P-Plan spelling.
Applied once at ingest by :func:`translate_legacy_workflow_keys`. ``run_before``
is deliberately absent -- it is the inverse of ``preceded_by`` and is flipped
onto the target sub-plan by :func:`_flip_legacy_run_before`, not a plain rename."""

_KEYS_DEPRECATED_SINCE: str = _LEGACY_KEYS_SPEC["deprecated_since"]
"""esm_tools version in which the legacy keys became deprecated."""

_KEYS_REMOVED_IN: str = _LEGACY_KEYS_SPEC["removed_in"]
"""esm_tools version in which support for the legacy keys will be removed."""


def _rename_legacy_key(container: dict, old: str) -> None:
    """Rename a legacy ``old`` key to its current spelling in ``container``.

    The current spelling comes from :data:`_LEGACY_WORKFLOW_KEYS`, the single
    source of truth. Warns once per call site (the ``default`` filter installed
    by :mod:`esm_tools.esm_warnings` dedups on message + line number); a key
    rewritten from more than one site -- e.g. ``subjobs``, translated both at
    the top level and inside each plan -- therefore warns once per site.

    If both the legacy key and its current spelling are present, their values
    are *merged* rather than dropping the legacy one: dict values are
    deep-merged with the explicit current key winning on conflict; for a
    non-dict current value, the current one wins.
    """
    if not isinstance(container, dict) or old not in container:
        return
    new = _LEGACY_WORKFLOW_KEYS[old]
    warnings.warn(
        deprecation.DeprecatedWarning(
            old, _KEYS_DEPRECATED_SINCE, _KEYS_REMOVED_IN, f"Use '{new}' instead."
        ),
        stacklevel=2,
    )
    legacy_value = container.pop(old)
    if new not in container:
        container[new] = legacy_value
    elif isinstance(container[new], dict) and isinstance(legacy_value, dict):
        # Keep legacy entries; let the explicit current key win on conflict.
        esm_parser.dict_merge(legacy_value, container[new])
        container[new] = legacy_value
    # else: a non-dict current value wins; the legacy value is dropped.


def translate_legacy_workflow_keys(config: dict) -> dict:
    """Normalise legacy workflow key names to the current P-Plan nouns.

    Cheap, one-pass, behaviour-preserving: walks every ``<model>.workflow``
    block and rewrites the renamed keys in place, driven by
    :data:`_LEGACY_WORKFLOW_KEYS`. Covers the structural nouns, the
    workflow-level ordering keys (``first_task_in_queue`` -> ``entry_point``
    etc.) and the ordering relations. ``run_after`` maps straight to the single
    P-Plan ordering property ``preceded_by``; its inverse ``run_before`` is
    flipped onto the target sub-plan by :func:`_flip_legacy_run_before`.
    """
    plans_key = _LEGACY_WORKFLOW_KEYS["subjob_clusters"]  # "plans"
    sub_plans_key = _LEGACY_WORKFLOW_KEYS["subjobs"]  # "sub_plans"

    for model_config in config.values():
        if not isinstance(model_config, dict):
            continue
        w_config = model_config.get("workflow")
        if not isinstance(w_config, dict):
            continue

        # Top-level containers and workflow-level ordering keys.
        _rename_legacy_key(w_config, "subjob_clusters")
        _rename_legacy_key(w_config, "subjobs")
        _rename_legacy_key(w_config, "first_task_in_queue")
        _rename_legacy_key(w_config, "last_task_in_queue")
        _rename_legacy_key(w_config, "next_run_triggered_by")

        # Per sub-plan: parent pointer and ordering keys.
        sub_plans = w_config.get(sub_plans_key)
        if isinstance(sub_plans, dict):
            for sub_plan in sub_plans.values():
                _rename_legacy_key(sub_plan, "subjob_cluster")
                _rename_legacy_key(sub_plan, "run_after")
            _flip_legacy_run_before(sub_plans)

        # Inner subjobs list -> sub_plans inside each plan.
        plans = w_config.get(plans_key)
        if isinstance(plans, dict):
            for plan in plans.values():
                _rename_legacy_key(plan, "subjobs")

    return config


def _flip_legacy_run_before(sub_plans: dict) -> None:
    """Rewrite the legacy inverse ``run_before`` into the canonical ``preceded_by``.

    P-Plan has a single ordering property (``isPrecededBy``); ``run_before`` is
    its inverse. ``X.run_before: Y`` ("X runs before Y") is the same edge as
    ``Y.preceded_by: X``, so we move the assertion to the target sub-plan and
    drop the inverse key. Runs after the ``run_after`` rename so both directions
    have collapsed onto ``preceded_by``.
    """
    for name, sub_plan in list(sub_plans.items()):
        if not isinstance(sub_plan, dict) or "run_before" not in sub_plan:
            continue
        target = sub_plan.pop("run_before")
        warnings.warn(
            deprecation.DeprecatedWarning(
                "run_before",
                _KEYS_DEPRECATED_SINCE,
                _KEYS_REMOVED_IN,
                "Use 'preceded_by' on the target sub-plan instead.",
            ),
            stacklevel=2,
        )
        if isinstance(sub_plans.get(target), dict):
            sub_plans[target]["preceded_by"] = name


def skip_plan(plan, config):
    gw_config = config["general"]["workflow"]
    plan_conf = gw_config["plans"][plan]

    """
    print(f"run_only {plan_conf.get('run_only', 'Error') }")
    print(f"skip_chunk_number {plan_conf.get('skip_chunk_number', -999)}")
    print(f"skip_run_number {plan_conf.get('skip_run_number', -999)}")
    print(f"chunk_number {config['general'].get('chunk_number', -998)}")
    print(f"run_number {config['general'].get('run_number', -998)}")
    print(f"last_run_in_chunk {config['general']['last_run_in_chunk']}")
    print(f"first_run_in_chunk {config['general']['first_run_in_chunk']}")
    """

    if plan_conf.get("run_only", "Error") == "last_run_in_chunk" and not config[
        "general"
    ].get("last_run_in_chunk", False):
        return True
    if plan_conf.get("run_only", "Error") == "first_run_in_chunk" and not config[
        "general"
    ].get("first_run_in_chunk", False):
        return True
    if plan_conf.get("skip_chunk_number", -999) == config["general"].get(
        "chunk_number", -998
    ):
        return True
    if plan_conf.get("skip_run_number", -999) == config["general"].get(
        "run_number", -998
    ):
        return True

    return False


def assemble_workflow(config):
    #
    config = translate_legacy_workflow_keys(config)
    config = init_total_workflow(config)
    config = collect_all_workflow_information(config)
    config = complete_plans(config)
    config = order_plans(config)
    config = prepend_newrun_job(config)

    if config["general"]["jobtype"] == "unknown":
        config["general"]["command_line_config"]["jobtype"] = config["general"][
            "workflow"
        ]["entry_point"]
        config["general"]["jobtype"] = config["general"]["workflow"][
            "entry_point"
        ]

    return config


def display_nicely(config):
    esm_parser.pprint_config(config["general"]["workflow"])
    return config


def prepend_newrun_job(config):
    gw_config = config["general"]["workflow"]
    first_plan_name = gw_config["entry_point"]
    first_plan = gw_config["plans"][first_plan_name]

    if not first_plan.get("batch_or_shell", "Error") == "SimulationSetup":

        last_plan_name = gw_config["exit_point"]
        last_plan = gw_config["plans"][last_plan_name]

        new_first_plan = {
            "newrun": {
                "called_from": last_plan_name,
                "preceded_by": last_plan_name,
                "next_submit": [first_plan_name],
                "sub_plans": ["newrun_general"],
                "batch_or_shell": "SimulationSetup",
            }
        }

        last_plan["next_submit"].append("newrun")
        last_plan["next_submit"].remove(first_plan_name)

        first_plan["called_from"] = "newrun"

        gw_config["entry_point"] = "newrun"

        new_sub_plan = {
            "newrun_general": {
                "nproc": 1,
                "called_from": last_plan_name,
                "preceded_by": last_plan_name,
                "next_submit": [first_plan_name],
                "parent_plan": "newrun",
            }
        }

        gw_config["plans"].update(new_first_plan)
        gw_config["sub_plans"].update(new_sub_plan)

    return config

    #


def order_plans(config):
    gw_config = config["general"]["workflow"]

    for plan in gw_config["plans"]:
        if "next_submit" not in gw_config["plans"][plan]:
            gw_config["plans"][plan]["next_submit"] = []

    for plan in gw_config["plans"]:
        plan_conf = gw_config["plans"][plan]
        if "preceded_by" not in plan_conf:
            continue

        predecessor = plan_conf["preceded_by"]
        if predecessor not in gw_config["plans"]:
            # preceded_by may name a model-scoped sub_plan (e.g.
            # ``prepcompute_general``); resolve it to the plan that owns it.
            sub_plan_conf = gw_config.get("sub_plans", {}).get(predecessor, {})
            if sub_plan_conf.get("parent_plan") in gw_config["plans"]:
                predecessor = sub_plan_conf["parent_plan"]
        if predecessor not in gw_config["plans"]:
            raise UnknownPlanError(
                f"Plan {plan!r} is preceded_by {predecessor!r}, "
                "which is not a known plan."
            )

        if plan not in gw_config["plans"][predecessor]["next_submit"]:
            gw_config["plans"][predecessor]["next_submit"].append(plan)
        plan_conf["called_from"] = predecessor

        # Carry the entry/exit points along the ordering chain.
        if predecessor == gw_config["exit_point"]:
            gw_config["exit_point"] = plan
        if plan == gw_config["entry_point"]:
            gw_config["entry_point"] = predecessor

    # A plan with no predecessor is only valid once it is the entry point (the
    # entry may have been carried onto it by the loop above).
    for plan in gw_config["plans"]:
        if (
            "preceded_by" not in gw_config["plans"][plan]
            and plan != gw_config.get("entry_point")
        ):
            logger.error(f"Don't know when to execute plan {plan}.")
            logger.error(gw_config)
            sys.exit(-1)

    if "next_iteration_trigger" in gw_config:
        gw_config["exit_point"] = gw_config["next_iteration_trigger"]

    entry_name = gw_config["entry_point"]
    entry_plan = gw_config["plans"][entry_name]
    exit_name = gw_config["exit_point"]
    exit_plan = gw_config["plans"][exit_name]

    if entry_name not in exit_plan.get("next_submit", ["Error"]):
        exit_plan["next_submit"].append(entry_name)
    if exit_name not in entry_plan.get("called_from", ["Error"]):
        entry_plan["called_from"] = exit_name

    return config


def complete_plans(config):
    gw_config = config["general"]["workflow"]

    # First, complete the matching sub-plans <-> plans

    for sub_plan in gw_config["sub_plans"]:
        plan = gw_config["sub_plans"][sub_plan]["parent_plan"]
        if plan not in gw_config["plans"]:
            gw_config["plans"][plan] = {}

        if "sub_plans" not in gw_config["plans"][plan]:
            gw_config["plans"][plan]["sub_plans"] = []

        gw_config["plans"][plan]["sub_plans"].append(sub_plan)

    # Then, complete the resource information per plan
    # determine whether a plan is to be submitted to a batch system

    for plan in gw_config["plans"]:
        nproc_sum = nproc_max = 0
        plan_conf = gw_config["plans"][plan]
        for sub_plan in plan_conf["sub_plans"]:
            sub_plan_conf = gw_config["sub_plans"][sub_plan]

            plan_conf = merge_single_entry_if_possible(
                "submit_to_batch_system", sub_plan_conf, plan_conf
            )
            plan_conf = merge_single_entry_if_possible(
                "order_in_cluster", sub_plan_conf, plan_conf
            )

            if sub_plan_conf.get("submit_to_batch_system", False):
                plan_conf["batch_or_shell"] = "batch"
            elif sub_plan_conf.get("script", False):
                plan_conf["batch_or_shell"] = "shell"

            plan_conf = merge_single_entry_if_possible(
                "run_on_queue", sub_plan_conf, plan_conf
            )
            plan_conf = merge_single_entry_if_possible(
                "preceded_by", sub_plan_conf, plan_conf
            )
            plan_conf = merge_single_entry_if_possible(
                "run_only", sub_plan_conf, plan_conf
            )
            plan_conf = merge_single_entry_if_possible(
                "skip_run_number", sub_plan_conf, plan_conf
            )
            plan_conf = merge_single_entry_if_possible(
                "skip_chunk_number", sub_plan_conf, plan_conf
            )

            nproc_sum += sub_plan_conf.get("nproc", 1)
            nproc_max = max(sub_plan_conf.get("nproc", 1), nproc_max)

        if "submit_to_batch_system" not in plan_conf:
            plan_conf["submit_to_batch_system"] = False
        else:
            if "run_on_queue" not in plan_conf:
                logger.error(
                    f"Information on target queue is missing in plan {plan_conf}."
                )
                sys.exit(-1)

        if not plan_conf.get("batch_or_shell", False):
            plan_conf["batch_or_shell"] = "SimulationSetup"

        if "order_in_cluster" not in plan_conf:
            plan_conf["order_in_cluster"] = "sequential"

        if plan_conf["order_in_cluster"] == "concurrent":
            nproc = nproc_sum
        else:
            nproc = nproc_max
        plan_conf["nproc"] = nproc

    return config


def merge_single_entry_if_possible(entry, sourceconf, targetconf):
    if entry in sourceconf:
        if entry in targetconf and not sourceconf[entry] == targetconf[entry]:
            logger.error(f"Mismatch found in {entry} for plan {targetconf}")
            sys.exit(-1)
        targetconf[entry] = sourceconf[entry]
    return targetconf


def init_total_workflow(config):
    # add compute, tidy etc information already here!

    tasks = 0
    for model in config["general"]["valid_model_names"]:
        if "nproc" in config[model]:
            tasks += config[model]["nproc"]
        elif "nproca" in config[model] and "nprocb" in config[model]:
            tasks += config[model]["nproca"] * config[model]["nprocb"]
            if "nprocar" in config[model] and "nprocbr" in config[model]:
                if (
                    config[model]["nprocar"] != "remove_from_namelist"
                    and config[model]["nprocbr"] != "remove_from_namelist"
                ):
                    tasks += config[model]["nprocar"] * config[model]["nprocbr"]

    prepcompute = {
        "prepcompute": {
            "nproc": 1,
        }
    }

    compute = {
        "compute": {
            "nproc": tasks,
            "preceded_by": "prepcompute",
            "submit_to_batch_system": config["general"].get(
                "submit_to_batch_system", True
            ),
            "run_on_queue": config["computer"]["partitions"]["compute"]["name"],
        }
    }

    # das ist nur vorübergehend
    tidy = {
        "tidy": {
            "nproc": 1,
            "preceded_by": "compute",
        }
    }

    if "workflow" not in config["general"]:
        config["general"]["workflow"] = {}
    if "plans" not in config["general"]["workflow"]:
        config["general"]["workflow"]["plans"] = {}
    if "sub_plans" not in config["general"]["workflow"]:
        config["general"]["workflow"]["sub_plans"] = prepcompute
        config["general"]["workflow"]["sub_plans"].update(compute)
        config["general"]["workflow"]["sub_plans"].update(tidy)
    else:
        if "prepcompute" not in config["general"]["workflow"]["sub_plans"]:
            config["general"]["workflow"]["sub_plans"].update(prepcompute)
        if "compute" not in config["general"]["workflow"]["sub_plans"]:
            config["general"]["workflow"]["sub_plans"].update(compute)
        if "tidy" not in config["general"]["workflow"]["sub_plans"]:
            config["general"]["workflow"]["sub_plans"].update(tidy)
    if "exit_point" not in config["general"]["workflow"]:
        config["general"]["workflow"]["exit_point"] = "tidy"
    if "entry_point" not in config["general"]["workflow"]:
        config["general"]["workflow"]["entry_point"] = "prepcompute"

    if "next_iteration_trigger" not in config["general"]["workflow"]:
        config["general"]["workflow"]["next_iteration_trigger"] = "tidy"

    return config


def collect_all_workflow_information(config):

    for model in config:
        if "workflow" in config[model]:
            w_config = config[model]["workflow"]
            gw_config = config["general"]["workflow"]

            if "plans" in w_config:
                for plan in w_config["plans"]:
                    if plan in gw_config["plans"]:
                        gw_config["plans"][plan] = merge_if_possible(
                            w_config["plans"][plan],
                            gw_config["plans"][plan],
                        )
                    else:
                        gw_config["plans"][plan] = copy.deepcopy(
                            w_config["plans"][plan],
                        )

            if "sub_plans" in w_config:
                ref_config = copy.deepcopy(w_config)
                for sub_plan in list(copy.deepcopy(w_config["sub_plans"])):

                    # sub-plans (other than plans) should be model specific
                    model_sub_plan = f"{sub_plan}_{model}"
                    gw_config["sub_plans"][model_sub_plan] = copy.deepcopy(
                        w_config["sub_plans"][sub_plan]
                    )
                    if sub_plan in gw_config["sub_plans"]:
                        del gw_config["sub_plans"][sub_plan]
                    # make sure preceded_by refers to that model-specific sub-plan
                    for other_sub_plan in gw_config["sub_plans"]:
                        other_conf = gw_config["sub_plans"][other_sub_plan]
                        if other_conf.get("preceded_by") == sub_plan:
                            other_conf["preceded_by"] = model_sub_plan

                    # if not in another plan, each sub-plan gets its own
                    if "parent_plan" not in gw_config["sub_plans"][model_sub_plan]:
                        gw_config["sub_plans"][model_sub_plan]["parent_plan"] = sub_plan

            if "next_iteration_trigger" in w_config:
                if not gw_config["next_iteration_trigger"] in [
                    "tidy",
                    w_config["next_iteration_trigger"],
                ]:
                    logger.error(
                        "Mismatch found setting next_iteration_trigger for workflow."
                    )
                    sys.exit(-1)
                else:
                    gw_config["next_iteration_trigger"] = w_config[
                        "next_iteration_trigger"
                    ]

    return config


def merge_if_possible(source, target):
    for entry in source:
        if entry in target:
            if not source[entry] == target[entry]:
                logger.error(
                    f"Mismatch while trying to merge plans {source} into {target}"
                )
                sys.exit(-1)
        else:
            target[entry] = source[entry]
    return target
