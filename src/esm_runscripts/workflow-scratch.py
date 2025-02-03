import copy
import os
import sys

from loguru import logger

import esm_parser


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
