import sys, copy, os
import esm_parser


def skip_cluster(cluster, config):
    gw_config = config["general"]["workflow"]
    clusterconf = gw_config["subjob_clusters"][cluster]

    """
    print(f"run_only {clusterconf.get('run_only', 'Error') }")
    print(f"skip_chunk_number {clusterconf.get('skip_chunk_number', -999)}")
    print(f"skip_run_number {clusterconf.get('skip_run_number', -999)}")
    print(f"chunk_number {config['general'].get('chunk_number', -998)}")
    print(f"run_number {config['general'].get('run_number', -998)}")
    print(f"last_run_in_chunk {config['general']['last_run_in_chunk']}")
    print(f"first_run_in_chunk {config['general']['first_run_in_chunk']}")
    """

    if clusterconf.get("run_only", "Error") == "last_run_in_chunk" and not config[
        "general"
    ].get("last_run_in_chunk", False):
        return True
    if clusterconf.get("run_only", "Error") == "first_run_in_chunk" and not config[
        "general"
    ].get("first_run_in_chunk", False):
        return True
    if clusterconf.get("skip_chunk_number", -999) == config["general"].get(
        "chunk_number", -998
    ):
        return True
    if clusterconf.get("skip_run_number", -999) == config["general"].get(
        "run_number", -998
    ):
        return True

    return False


def assemble_workflow(config):
    #
    config = init_total_workflow(config)
    config = collect_all_workflow_information(config)
    config = complete_clusters(config)
    config = order_clusters(config)
    config = prepend_newrun_job(config)

    if config["general"]["jobtype"] == "unknown":
        config["general"]["command_line_config"]["jobtype"] = config["general"][
            "workflow"
        ]["first_task_in_queue"]
        config["general"]["jobtype"] = config["general"]["workflow"][
            "first_task_in_queue"
        ]

    return config


def display_nicely(config):
    esm_parser.pprint_config(config["general"]["workflow"])
    return config


def prepend_newrun_job(config):
    gw_config = config["general"]["workflow"]
    first_cluster_name = gw_config["first_task_in_queue"]
    first_cluster = gw_config["subjob_clusters"][first_cluster_name]

    if not first_cluster.get("batch_or_shell", "Error") == "SimulationSetup":

        last_cluster_name = gw_config["last_task_in_queue"]
        last_cluster = gw_config["subjob_clusters"][last_cluster_name]

        new_first_cluster_name = "newrun"
        new_first_cluster = {
            "newrun": {
                "called_from": last_cluster_name,
                "run_before": first_cluster_name,
                "next_submit": [first_cluster_name],
                "subjobs": ["newrun_general"],
                "batch_or_shell": "SimulationSetup",
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

    for subjob_cluster in gw_config["subjob_clusters"]:
        if not "next_submit" in gw_config["subjob_clusters"][subjob_cluster]:
            gw_config["subjob_clusters"][subjob_cluster]["next_submit"] = []

    for subjob_cluster in gw_config["subjob_clusters"]:
        if not "run_after" in gw_config["subjob_clusters"][subjob_cluster]:
            if not ("run_before" in gw_config["subjob_clusters"][subjob_cluster]):

                print(f"Don't know when to execute cluster {subjob_cluster}.")
                print(gw_config)
                sys.exit(-1)

        if "run_after" in gw_config["subjob_clusters"][subjob_cluster]:
            if "run_before" in gw_config["subjob_clusters"][subjob_cluster]:
                print(
                    f"Specifying both run_after and run_before for cluster {subjob_cluster} may lead to problems."
                )
                print(f"Please choose.")
                sys.exit(-1)
            if (
                not gw_config["subjob_clusters"][subjob_cluster]["run_after"]
                in gw_config["subjob_clusters"]
            ):
                print(f"Unknown cluster {subjob_cluster['run_after']}.")
                sys.exit(-1)

            calling_cluster = gw_config["subjob_clusters"][subjob_cluster]["run_after"]

            if (
                not subjob_cluster
                in gw_config["subjob_clusters"][calling_cluster]["next_submit"]
            ):
                gw_config["subjob_clusters"][calling_cluster]["next_submit"].append(
                    subjob_cluster
                )
            gw_config["subjob_clusters"][subjob_cluster][
                "called_from"
            ] = calling_cluster

            if calling_cluster == gw_config["last_task_in_queue"]:
                gw_config["last_task_in_queue"] = subjob_cluster

        if "run_before" in gw_config["subjob_clusters"][subjob_cluster]:
            if (
                not gw_config["subjob_clusters"][subjob_cluster]["run_before"]
                in gw_config["subjob_clusters"]
            ):
                print(f"Unknown cluster {subjob_cluster['run_before']}.")
                sys.exit(-1)

            called_cluster = gw_config["subjob_clusters"][subjob_cluster]["run_before"]

            if (
                not called_cluster
                in gw_config["subjob_clusters"][subjob_cluster]["next_submit"]
            ):
                gw_config["subjob_clusters"][subjob_cluster]["next_submit"].append(
                    called_cluster
                )
            gw_config["subjob_clusters"][called_cluster]["called_from"] = subjob_cluster

            if called_cluster == gw_config["first_task_in_queue"]:
                gw_config["first_task_in_queue"] = subjob_cluster

    if "next_run_triggered_by" in gw_config:
        gw_config["last_task_in_queue"] = gw_config["next_run_triggered_by"]

    first_cluster_name = gw_config["first_task_in_queue"]
    first_cluster = gw_config["subjob_clusters"][first_cluster_name]
    last_cluster_name = gw_config["last_task_in_queue"]
    last_cluster = gw_config["subjob_clusters"][last_cluster_name]

    if not first_cluster_name in last_cluster.get("next_submit", ["Error"]):
        last_cluster["next_submit"].append(first_cluster_name)
    if not last_cluster_name in first_cluster.get("called_from", ["Error"]):
        first_cluster["called_from"] = last_cluster_name

    return config


def complete_clusters(config):
    gw_config = config["general"]["workflow"]

    # First, complete the matching subjobs <-> clusters

    for subjob in gw_config["subjobs"]:
        subjob_cluster = gw_config["subjobs"][subjob]["subjob_cluster"]
        if not subjob_cluster in gw_config["subjob_clusters"]:
            gw_config["subjob_clusters"][subjob_cluster] = {}

        if not "subjobs" in gw_config["subjob_clusters"][subjob_cluster]:
            gw_config["subjob_clusters"][subjob_cluster]["subjobs"] = []

        gw_config["subjob_clusters"][subjob_cluster]["subjobs"].append(subjob)

    # Then, complete the resource information per cluster
    # determine whether a cluster is to be submitted to a batch system

    for subjob_cluster in gw_config["subjob_clusters"]:
        nproc_sum = nproc_max = 0
        clusterconf = gw_config["subjob_clusters"][subjob_cluster]
        for subjob in clusterconf["subjobs"]:
            subjobconf = gw_config["subjobs"][subjob]

            clusterconf = merge_single_entry_if_possible(
                "submit_to_batch_system", subjobconf, clusterconf
            )
            clusterconf = merge_single_entry_if_possible(
                "order_in_cluster", subjobconf, clusterconf
            )

            if subjobconf.get("submit_to_batch_system", False):
                clusterconf["batch_or_shell"] = "batch"
            elif subjobconf.get("script", False):
                clusterconf["batch_or_shell"] = "shell"

            clusterconf = merge_single_entry_if_possible(
                "run_on_queue", subjobconf, clusterconf
            )
            clusterconf = merge_single_entry_if_possible(
                "run_after", subjobconf, clusterconf
            )
            clusterconf = merge_single_entry_if_possible(
                "run_before", subjobconf, clusterconf
            )
            clusterconf = merge_single_entry_if_possible(
                "run_only", subjobconf, clusterconf
            )
            clusterconf = merge_single_entry_if_possible(
                "skip_run_number", subjobconf, clusterconf
            )
            clusterconf = merge_single_entry_if_possible(
                "skip_chunk_number", subjobconf, clusterconf
            )

            nproc_sum += subjobconf.get("nproc", 1)
            nproc_max = max(subjobconf.get("nproc", 1), nproc_max)

        if not "submit_to_batch_system" in clusterconf:
            clusterconf["submit_to_batch_system"] = False
        else:
            if not "run_on_queue" in clusterconf:
                print(
                    f"Information on target queue is missing in cluster {clusterconf}."
                )
                sys.exit(-1)

        if not clusterconf.get("batch_or_shell", False):
            clusterconf["batch_or_shell"] = "SimulationSetup"

        if not "order_in_cluster" in clusterconf:
            clusterconf["order_in_cluster"] = "sequential"

        if clusterconf["order_in_cluster"] == "concurrent":
            nproc = nproc_sum
        else:
            nproc = nproc_max
        clusterconf["nproc"] = nproc

    return config


def merge_single_entry_if_possible(entry, sourceconf, targetconf):
    if entry in sourceconf:
        if entry in targetconf and not sourceconf[entry] == targetconf[entry]:
            print(f"Mismatch found in {entry} for cluster {targetconf}")
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
            "run_before": "compute",
        }
    }

    compute = {
        "compute": {
            "nproc": tasks,
            "run_before": "tidy",
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
            "run_after": "compute",
        }
    }

    if not "workflow" in config["general"]:
        config["general"]["workflow"] = {}
    if not "subjob_clusters" in config["general"]["workflow"]:
        config["general"]["workflow"]["subjob_clusters"] = {}
    if not "subjobs" in config["general"]["workflow"]:
        config["general"]["workflow"]["subjobs"] = prepcompute
        config["general"]["workflow"]["subjobs"].update(compute)
        config["general"]["workflow"]["subjobs"].update(tidy)

    if not "last_task_in_queue" in config["general"]["workflow"]:
        config["general"]["workflow"]["last_task_in_queue"] = "tidy"
    if not "first_task_in_queue" in config["general"]["workflow"]:
        config["general"]["workflow"]["first_task_in_queue"] = "prepcompute"

    if not "next_run_triggered_by" in config["general"]["workflow"]:
        config["general"]["workflow"]["next_run_triggered_by"] = "tidy"

    return config


def collect_all_workflow_information(config):

    for model in config:
        if "workflow" in config[model]:
            w_config = config[model]["workflow"]
            gw_config = config["general"]["workflow"]

            if "subjob_clusters" in w_config:
                for cluster in w_config["subjob_clusters"]:
                    if cluster in gw_config["subjob_clusters"]:
                        gw_config["subjob_clusters"][cluster] = merge_if_possible(
                            w_config["subjob_clusters"][cluster],
                            gw_config["subjob_clusters"][cluster],
                        )
                    else:
                        gw_config["subjob_clusters"][cluster] = copy.deepcopy(
                            w_config["subjob_clusters"][cluster],
                        )

            if "subjobs" in w_config:
                ref_config = copy.deepcopy(w_config)
                for subjob in list(copy.deepcopy(w_config["subjobs"])):

                    # subjobs (other than clusters) should be model specific
                    gw_config["subjobs"][subjob + "_" + model] = copy.deepcopy(
                        w_config["subjobs"][subjob]
                    )
                    if subjob in gw_config["subjobs"]:
                        del gw_config["subjobs"][subjob]
                    # make sure that the run_after and run_before refer to that cluster
                    for other_subjob in gw_config["subjobs"]:
                        if "run_after" in gw_config["subjobs"][other_subjob]:
                            if (
                                gw_config["subjobs"][other_subjob]["run_after"]
                                == subjob
                            ):
                                gw_config["subjobs"][other_subjob][
                                    "run_after"
                                ] == subjob + "_" + model
                        if "run_before" in gw_config["subjobs"][other_subjob]:
                            if (
                                gw_config["subjobs"][other_subjob]["run_before"]
                                == subjob
                            ):
                                gw_config["subjobs"][other_subjob][
                                    "run_before"
                                ] == subjob + "_" + model

                    # if not in another cluster, each subjob gets its own
                    if (
                        not "subjob_cluster"
                        in gw_config["subjobs"][subjob + "_" + model]
                    ):
                        gw_config["subjobs"][subjob + "_" + model][
                            "subjob_cluster"
                        ] = subjob  # + "_" + model

            if "next_run_triggered_by" in w_config:
                if not gw_config["next_run_triggered_by"] in [
                    "tidy",
                    w_config["next_run_triggered_by"],
                ]:
                    print(f"Mismatch found setting next_run_triggered_by for workflow.")
                    sys.exit(-1)
                else:
                    gw_config["next_run_triggered_by"] = w_config[
                        "next_run_triggered_by"
                    ]

    return config


def merge_if_possible(source, target):
    for entry in source:
        if entry in target:
            if not source[entry] == target[entry]:
                print(
                    f"Mismatch while trying to merge subjob_clusters {source} into {target}"
                )
                sys.exit(-1)
        else:
            target[entry] = source[entry]
    return target
