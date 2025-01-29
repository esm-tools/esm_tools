import copy
import sys

import yaml
from loguru import logger

import esm_parser


def should_skip_cluster(cluster, config):
    """
    Determine whether a specific cluster should be skipped based on the provided configuration.

    Parameters
    ----------
    cluster : str
        The name of the cluster to check.
    config : dict
        A dictionary containing various configuration settings.

    Returns
    -------
    bool
        True if the cluster should be skipped, False otherwise.

    Notes
    -----
    The function evaluates several conditions to decide if the cluster should be skipped:
    1. If `run_only` in the cluster configuration is set to "last_run_in_chunk" and `last_run_in_chunk` in the general configuration is not True.
    2. If `run_only` in the cluster configuration is set to "first_run_in_chunk" and `first_run_in_chunk` in the general configuration is not True.
    3. If `skip_chunk_number` in the cluster configuration matches `chunk_number` in the general configuration.
    4. If `skip_run_number` in the cluster configuration matches `run_number` in the general configuration.

    If none of these conditions are met, the function returns False, indicating that the cluster should not be skipped.
    """
    general_config = config["general"]
    workflow_config = general_config["workflow"]
    cluster_config = workflow_config["subjob_clusters"][cluster]

    run_only_on = cluster_config.get("run_only")

    if run_only_on == "last_run_in_chunk" and not general_config.get(
        "last_run_in_chunk", False
    ):
        return True

    if run_only_on == "first_run_in_chunk" and not general_config.get(
        "first_run_in_chunk", False
    ):
        return True

    skip_chunk_number = cluster_config.get("skip_chunk_number")
    if skip_chunk_number == general_config.get("chunk_number"):
        return True

    skip_run_number = cluster_config.get("skip_run_number")
    if skip_run_number == general_config.get("run_number"):
        return True

    return False


def assemble_workflow(config):
    config = init_total_workflow(config)
    config = collect_all_workflow_information(config)
    config = complete_clusters(config)
    config = order_clusters(config)
    config = prepend_newrun_job(config)
    config = handle_unknown_jobtype(config)
    return config


def handle_unknown_jobtype(config):
    """
    Update the jobtype in the configuration if it is set to 'unknown'.

    Args:
        config (dict): The configuration dictionary.

    Returns:
        dict: The updated configuration dictionary.
    """
    if config["general"]["jobtype"] == "unknown":
        first_task = config["general"]["workflow"]["first_task_in_queue"]
        config["general"]["command_line_config"]["jobtype"] = first_task
        config["general"]["jobtype"] = first_task
    return config


def display_nicely(config):
    esm_parser.pprint_config(config["general"]["workflow"])
    return config


def prepend_newrun_job(config):
    gw_config = config["general"]["workflow"]
    first_cluster_name = gw_config["first_task_in_queue"]
    first_cluster = gw_config["subjob_clusters"][first_cluster_name]

    if not first_cluster.get("batch_or_shell", "Error") == "Simulation":
        last_cluster_name = gw_config["last_task_in_queue"]
        last_cluster = gw_config["subjob_clusters"][last_cluster_name]

        new_first_cluster_name = "newrun"
        new_first_cluster = {
            "newrun": {
                "called_from": last_cluster_name,
                "run_before": first_cluster_name,
                "next_submit": [first_cluster_name],
                "subjobs": ["newrun_general"],
                "batch_or_shell": "Simulation",
            }
        }

        last_cluster["next_submit"].append("newrun")
        last_cluster["next_submit"].remove(first_cluster_name)

        first_cluster["called_from"] = "newrun"

        gw_config["first_task_in_queue"] = "newrun"

        new_subjob = {
            "newrun_general": {
                "nproc": 1,
                "called_from": last_cluster_name,
                "run_before": first_cluster_name,
                "next_submit": [first_cluster_name],
                "subjob_cluster": "newrun",
            }
        }

        gw_config["subjob_clusters"].update(new_first_cluster)
        gw_config["subjobs"].update(new_subjob)

    return config

    #


def order_clusters(config):
    gw_config = config["general"]["workflow"]

    initialize_next_submit(gw_config)
    validate_and_set_dependencies(gw_config)
    handle_next_run_triggered_by_and_last_task_in_queue(gw_config)
    ensure_first_and_last_clusters_linked(gw_config)

    return config


def initialize_next_submit(gw_config):
    for subjob_cluster in gw_config["subjob_clusters"]:
        if "next_submit" not in gw_config["subjob_clusters"][subjob_cluster]:
            gw_config["subjob_clusters"][subjob_cluster]["next_submit"] = []


def validate_and_set_dependencies(gw_config):
    for subjob_cluster in gw_config["subjob_clusters"]:
        cluster_config = gw_config["subjob_clusters"][subjob_cluster]
        if "run_after" not in cluster_config and "run_before" not in cluster_config:
            log_and_exit(
                f"Don't know when to execute cluster {subjob_cluster}.", gw_config
            )

        if "run_after" in cluster_config:
            validate_run_after(cluster_config, subjob_cluster, gw_config)
        if "run_before" in cluster_config:
            validate_run_before(cluster_config, subjob_cluster, gw_config)


def validate_run_after(cluster_config, subjob_cluster, gw_config):
    if "run_before" in cluster_config:
        log_and_exit(
            f"Specifying both run_after and run_before for cluster {subjob_cluster} may lead to problems. Please choose.",
            gw_config,
        )

    calling_cluster = cluster_config["run_after"]
    if calling_cluster not in gw_config["subjob_clusters"]:
        log_and_exit(
            f"Validate run after -- Unknown cluster {calling_cluster}.", gw_config
        )

    append_to_next_submit(gw_config, calling_cluster, subjob_cluster)
    cluster_config["called_from"] = calling_cluster

    if calling_cluster == gw_config["last_task_in_queue"]:
        gw_config["last_task_in_queue"] = subjob_cluster


def validate_run_before(cluster_config, subjob_cluster, gw_config):
    called_cluster = cluster_config["run_before"]
    if called_cluster not in gw_config["subjob_clusters"]:
        log_and_exit(
            f"Validate run before -- Unknown cluster {called_cluster}.", gw_config
        )

    append_to_next_submit(gw_config, subjob_cluster, called_cluster)
    gw_config["subjob_clusters"][called_cluster]["called_from"] = subjob_cluster

    if called_cluster == gw_config["first_task_in_queue"]:
        gw_config["first_task_in_queue"] = subjob_cluster


def append_to_next_submit(gw_config, source_cluster, target_cluster):
    if (
        target_cluster
        not in gw_config["subjob_clusters"][source_cluster]["next_submit"]
    ):
        gw_config["subjob_clusters"][source_cluster]["next_submit"].append(
            target_cluster
        )


def ensure_first_and_last_clusters_linked(gw_config):
    first_cluster_name = gw_config["first_task_in_queue"]
    first_cluster = gw_config["subjob_clusters"][first_cluster_name]
    last_cluster_name = gw_config["last_task_in_queue"]
    last_cluster = gw_config["subjob_clusters"][last_cluster_name]

    if first_cluster_name not in last_cluster.get("next_submit", ["Error"]):
        last_cluster["next_submit"].append(first_cluster_name)
    if last_cluster_name not in first_cluster.get("called_from", ["Error"]):
        first_cluster["called_from"] = last_cluster_name


def log_and_exit(message, gw_config):
    logger.error(message)
    esm_parser.pprint_config(gw_config)
    sys.exit(-1)


# -----------------------------------------------------------------------------
def complete_clusters(config):
    gw_config = config["general"]["workflow"]

    complete_subjob_cluster_assignments(gw_config)
    complete_resource_information(gw_config)

    return config


def complete_subjob_cluster_assignments(gw_config):
    """
    Assigns subjobs to their respective subjob clusters in the workflow configuration.

    Parameters
    ----------
    gw_config : dict
        The general workflow configuration dictionary containing subjobs and subjob clusters.

    Notes
    -----
    This function iterates over all subjobs in the workflow configuration and assigns each subjob
    to its specified subjob cluster. If the subjob cluster does not already exist in the configuration,
    it is created. Each subjob is then appended to the list of subjobs within its respective subjob cluster.
    """
    for subjob in gw_config["subjobs"]:
        subjob_cluster = gw_config["subjobs"][subjob]["subjob_cluster"]
        if subjob_cluster not in gw_config["subjob_clusters"]:
            gw_config["subjob_clusters"][subjob_cluster] = {}

        if "subjobs" not in gw_config["subjob_clusters"][subjob_cluster]:
            gw_config["subjob_clusters"][subjob_cluster]["subjobs"] = []

        gw_config["subjob_clusters"][subjob_cluster]["subjobs"].append(subjob)


def complete_resource_information(gw_config):
    """
    Completes the resource information for each subjob cluster in the workflow configuration.

    Parameters
    ----------
    gw_config : dict
        The general workflow configuration dictionary containing subjobs and subjob clusters.

    Notes
    -----
    This function iterates over all subjob clusters in the workflow configuration and performs the following tasks:
    - Merges individual configuration entries from subjobs into their respective subjob clusters.
    - Determines whether each subjob cluster should be submitted to a batch system or run as a shell script.
    - Ensures that necessary information such as target queue and order in cluster is present.
    - Calculates the total number of processors required for each subjob cluster based on the order in cluster.
    - Sets default values for missing configuration entries.
    """
    for subjob_cluster in gw_config["subjob_clusters"]:
        clusterconf = gw_config["subjob_clusters"][subjob_cluster]
        nproc_sum, nproc_max = calculate_nproc(clusterconf, gw_config)
        set_default_subjob_values(clusterconf)
        clusterconf["nproc"] = (
            nproc_sum if clusterconf["order_in_cluster"] == "concurrent" else nproc_max
        )


def calculate_nproc(clusterconf, gw_config):
    """
    Calculates the total and maximum number of processors required for a subjob cluster.

    Parameters
    ----------
    clusterconf : dict
        The configuration dictionary for a specific subjob cluster.
    gw_config : dict
        The general workflow configuration dictionary containing subjobs and subjob clusters.

    Returns
    -------
    nproc_sum : int
        The sum of processors required for all subjobs in the cluster.
    nproc_max : int
        The maximum number of processors required for any single subjob in the cluster.
    """
    nproc_sum = nproc_max = 0
    for subjob in clusterconf["subjobs"]:
        subjobconf = gw_config["subjobs"][subjob]
        merge_subjob_entries(clusterconf, subjobconf)
        nproc_sum += subjobconf.get("nproc", 1)
        nproc_max = max(subjobconf.get("nproc", 1), nproc_max)
    return nproc_sum, nproc_max


def merge_subjob_entries(clusterconf, subjobconf):
    """
    Merges individual configuration entries from a subjob into its respective subjob cluster.

    Parameters
    ----------
    clusterconf : dict
        The configuration dictionary for a specific subjob cluster.
    subjobconf : dict
        The configuration dictionary for a specific subjob.
    """
    keys_to_merge = [
        "submit_to_batch_system",
        "order_in_cluster",
        "run_on_queue",
        "run_after",
        "run_before",
        "run_only",
        "skip_run_number",
        "skip_chunk_number",
    ]
    for key in keys_to_merge:
        clusterconf = merge_single_entry_if_possible(key, subjobconf, clusterconf)

    if subjobconf.get("submit_to_batch_system", False):
        clusterconf["batch_or_shell"] = "batch"
    elif subjobconf.get("script", False):
        clusterconf["batch_or_shell"] = "shell"


def set_default_subjob_values(clusterconf):
    """
    Sets default values for missing configuration entries in a subjob cluster.

    Parameters
    ----------
    clusterconf : dict
        The configuration dictionary for a specific subjob cluster.
    """
    if "submit_to_batch_system" not in clusterconf:
        clusterconf["submit_to_batch_system"] = False
    else:
        if "run_on_queue" not in clusterconf:
            logger.error(
                f"Information on target queue is missing in cluster {clusterconf}."
            )
            sys.exit(-1)

    if not clusterconf.get("batch_or_shell", False):
        clusterconf["batch_or_shell"] = "Simulation"

    if "order_in_cluster" not in clusterconf:
        clusterconf["order_in_cluster"] = "sequential"


# -----------------------------------------------------------------------------


def merge_single_entry_if_possible(entry, sourceconf, targetconf):
    if entry in sourceconf:
        if entry in targetconf and not sourceconf[entry] == targetconf[entry]:
            logger.error(f"Mismatch found in {entry} for cluster {targetconf}")
            sys.exit(-1)
        targetconf[entry] = sourceconf[entry]
    return targetconf


def calculate_tasks(config):
    """
    Calculate the total number of tasks based on the configuration.

    Parameters
    ----------
    config : dict
        The configuration dictionary containing model information.

    Returns
    -------
    int
        The total number of tasks calculated from the model configurations.
    """
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
    return tasks


def create_prepcompute_stage():
    """
    Create the prepcompute stage configuration for the workflow manager.

    Returns
    -------
    dict
        The configuration dictionary for the prepcompute stage.
    """
    return {
        "prepcompute": {
            "nproc": 1,
            "run_before": "compute",
        }
    }


def create_compute_stage(tasks, config):
    """
    Create the compute stage configuration for the workflow manager

    Parameters
    ----------
    tasks : int
        The total number of tasks to be used in the compute stage.
    config : dict
        The configuration dictionary containing general settings.

    Returns
    -------
    dict
        The configuration dictionary for the compute stage.
    """
    return {
        "compute": {
            "nproc": tasks,
            "run_before": "tidy",
            "submit_to_batch_system": config["general"].get(
                "submit_to_batch_system", True
            ),
            "run_on_queue": config["computer"]["partitions"]["compute"]["name"],
        }
    }


def create_tidy_stage():
    """
    Create the tidy stage configuration for the workflow manager

    Returns
    -------
    dict
        The configuration dictionary for the tidy stage.
    """
    return {
        "tidy": {
            "nproc": 1,
            "run_after": "compute",
        }
    }


def init_total_workflow(config):
    """
    Initialize and configure the total workflow based on the given configuration.

    Parameters
    ----------
    config : dict
        The configuration dictionary containing workflow settings.

    Returns
    -------
    dict
        The updated configuration dictionary with the initialized workflow.
    """
    tasks = calculate_tasks(config)

    prepcompute = create_prepcompute_stage()
    compute = create_compute_stage(tasks, config)
    tidy = create_tidy_stage()

    workflow = config["general"].setdefault("workflow", {})
    workflow.setdefault("subjob_clusters", {})
    subjobs = workflow.setdefault("subjobs", {})

    subjobs.update(prepcompute)
    subjobs.update(compute)
    subjobs.update(tidy)

    workflow.setdefault("last_task_in_queue", "tidy")
    workflow.setdefault("first_task_in_queue", "prepcompute")
    workflow.setdefault("next_run_triggered_by", "tidy")

    return config


def merge_subjobs(w_config, gw_config, model):
    """
    Merge subjobs from model-specific workflow configuration into the general workflow configuration.

    Parameters
    ----------
    w_config : dict
        The model-specific workflow configuration.
    gw_config : dict
        The general workflow configuration.
    model : str
        The name of the model.
    """
    logger.critical(f"{model=}")
    if "subjobs" in w_config:
        for subjob in list(copy.deepcopy(w_config["subjobs"])):
            logger.critical(subjob)
            gw_config["subjobs"][subjob + "_" + model] = copy.deepcopy(
                w_config["subjobs"][subjob]
            )
            if subjob in gw_config["subjobs"]:
                del gw_config["subjobs"][subjob]
            update_run_references(gw_config, subjob, model)
            assign_subjob_cluster(gw_config, subjob, model)


def update_run_references(gw_config, subjob, model):
    """
    Update run_after and run_before references to be model-specific.

    Parameters
    ----------
    gw_config : dict
        The general workflow configuration.
    subjob : str
        The name of the subjob.
    model : str
        The name of the model.
    """
    for other_subjob in gw_config["subjobs"]:
        if "run_after" in gw_config["subjobs"][other_subjob]:
            if gw_config["subjobs"][other_subjob]["run_after"] == subjob:
                logger.critical("Updating run_after 001")
                logger.critical("Old value: ")
                logger.critical(gw_config["subjobs"][other_subjob]["run_after"])
                gw_config["subjobs"][other_subjob]["run_after"] = f"{subjob}_{model}"
                logger.critical(
                    f"gw_config['subjobs']['{other_subjob}']['run_after'] = {subjob}_{model}"
                )
        if "run_before" in gw_config["subjobs"][other_subjob]:
            if gw_config["subjobs"][other_subjob]["run_before"] == subjob:
                logger.critical("Updating run_before 001")
                logger.critical("Old value: ")
                logger.critical(gw_config["subjobs"][other_subjob]["run_before"])
                gw_config["subjobs"][other_subjob]["run_before"] = f"{subjob}_{model}"
                logger.critical(
                    f"gw_config['subjobs']['{other_subjob}']['run_before'] = {subjob}_{model}"
                )


def assign_subjob_cluster(gw_config, subjob, model):
    """
    Assign each subjob to a subjob cluster if not already assigned.

    Parameters
    ----------
    gw_config : dict
        The general workflow configuration.
    subjob : str
        The name of the subjob.
    model : str
        The name of the model.
    """
    if "subjob_cluster" not in gw_config["subjobs"][f"{subjob}_{model}"]:
        gw_config["subjobs"][f"{subjob}_{model}"]["subjob_cluster"] = subjob


def handle_next_run_triggered_by_and_last_task_in_queue(gw_config):
    """
    Handle the next_run_triggered_by key in the workflow configuration.

    Parameters
    ----------
    gw_config : dict
        The general workflow configuration.
    """
    if "next_run_triggered_by" in gw_config:
        gw_config["last_task_in_queue"] = gw_config["next_run_triggered_by"]


# --------------------------------------------------------------------------------
def collect_all_workflow_information(config):
    """
    Aggregates workflow configurations from all models into the general workflow config,
    handling subjob renaming and reference updates.
    """
    for model_name in config:
        if "workflow" not in config[model_name]:
            continue

        model_wf = config[model_name]["workflow"]
        general_wf = config["general"]["workflow"]

        # Merge clusters first as subjobs might depend on them
        merge_subjob_clusters(model_wf, general_wf)
        process_model_subjobs(model_wf, general_wf, model_name)
        handle_next_run_trigger(model_wf, general_wf)

    return config


def merge_subjob_clusters(source_wf, target_wf):
    """Merge subjob clusters from model workflow into general workflow"""
    for cluster_name, cluster_config in source_wf.get("subjob_clusters", {}).items():
        if cluster_name in target_wf["subjob_clusters"]:
            target_wf["subjob_clusters"][cluster_name] = safe_merge(
                cluster_config, target_wf["subjob_clusters"][cluster_name]
            )
        else:
            target_wf["subjob_clusters"][cluster_name] = copy.deepcopy(cluster_config)


def process_model_subjobs(source_wf, target_wf, model_name):
    """Process and merge subjobs with model-specific naming and references"""
    if "subjobs" not in source_wf:
        return

    rename_map = create_rename_mapping(source_wf["subjobs"], model_name)
    create_model_specific_subjobs(source_wf, target_wf, model_name, rename_map)
    update_workflow_references(target_wf, rename_map)
    resolve_references_to_clusters(target_wf)


def resolve_references_to_clusters(workflow_config):
    """Convert subjob references in dependencies to their parent clusters"""
    subjob_to_cluster = {
        subjob: conf["subjob_cluster"]
        for subjob, conf in workflow_config["subjobs"].items()
    }

    # Update references in ALL SUBJOBS
    for subjob_conf in workflow_config["subjobs"].values():
        for ref_type in ["run_after", "run_before"]:
            if ref_type in subjob_conf:
                subjob_conf[ref_type] = subjob_to_cluster.get(
                    subjob_conf[ref_type], subjob_conf[ref_type]
                )

    # Update references in CLUSTERS
    for cluster_conf in workflow_config["subjob_clusters"].values():
        for ref_type in ["run_after", "run_before", "next_submit"]:
            if ref_type in cluster_conf:
                if isinstance(cluster_conf[ref_type], list):
                    cluster_conf[ref_type] = [
                        subjob_to_cluster.get(name, name)
                        for name in cluster_conf[ref_type]
                    ]
                else:
                    cluster_conf[ref_type] = subjob_to_cluster.get(
                        cluster_conf[ref_type], cluster_conf[ref_type]
                    )


def create_rename_mapping(subjobs, model_name):
    """Create mapping from original subjob names to model-specific names"""
    return {orig: f"{orig}_{model_name}" for orig in subjobs}


def create_model_specific_subjobs(source_wf, target_wf, model_name, rename_map):
    """Create renamed subjob entries in general workflow"""
    for orig_name, new_name in rename_map.items():
        target_wf["subjobs"][new_name] = copy.deepcopy(source_wf["subjobs"][orig_name])

        # Remove original entry if present in general workflow
        if orig_name in target_wf["subjobs"]:
            del target_wf["subjobs"][orig_name]

        # Ensure cluster assignment
        if "subjob_cluster" not in target_wf["subjobs"][new_name]:
            target_wf["subjobs"][new_name]["subjob_cluster"] = orig_name


def update_workflow_references(target_wf, rename_map):
    """Update references throughout workflow to use renamed subjobs"""
    # Update references in all subjobs
    for subjob_config in target_wf["subjobs"].values():
        update_references_in_config(subjob_config, rename_map)

    # Update references in clusters
    for cluster_config in target_wf["subjob_clusters"].values():
        update_references_in_config(cluster_config, rename_map)


def update_references_in_config(config, rename_map):
    """Update references in a single configuration block"""
    for ref_type in ["run_after", "run_before", "called_from"]:
        if ref_type in config:
            config[ref_type] = rename_map.get(config[ref_type], config[ref_type])


def handle_next_run_trigger(source_wf, target_wf):
    """Handle next_run_triggered_by inheritance with validation"""
    if "next_run_triggered_by" in source_wf:
        new_trigger = source_wf["next_run_triggered_by"]
        current_trigger = target_wf.get("next_run_triggered_by", "tidy")

        if new_trigger != current_trigger and current_trigger != "tidy":
            raise WorkflowMergeError(
                f"Conflicting next_run_triggered_by: {current_trigger} vs {new_trigger}"
            )

        target_wf["next_run_triggered_by"] = new_trigger


class WorkflowMergeError(Exception):
    """Exception for workflow configuration merge conflicts"""


def safe_merge(source, target):
    """Safely merge two configurations with conflict checking"""
    merged = copy.deepcopy(target)
    for key, value in source.items():
        if key in merged and merged[key] != value:
            raise WorkflowMergeError(
                f"Conflict in key '{key}': {merged[key]} vs {value}"
            )
        merged[key] = copy.deepcopy(value)
    return merged


# --------------------------------------------------------------------------------


def merge_if_possible(source, target, exit_on_mismatch=True):
    """
    Merge source dictionary into target dictionary, ensuring no conflicts.

    Parameters
    ----------
    source : dict
        The source dictionary to merge from.
    target : dict
        The target dictionary to merge into.
    exit_on_mismatch : bool, optional
        Whether to exit if a mismatch is found, by default True

    Returns
    -------
    dict
        The updated target dictionary with merged entries from the source.
    """
    for key, value in source.items():
        if key in target and target[key] != value:
            logger.error(
                f"Mismatch while trying to merge subjob_clusters {source} into {target}"
            )
            if exit_on_mismatch:
                sys.exit(1)
        target[key] = value
    return target
