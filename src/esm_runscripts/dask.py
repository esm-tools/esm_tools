import json
import os
import subprocess
import time

import esm_parser

from loguru import logger
from enum import IntEnum

import dask.distributed as daskd

def wait_for_dask_status(dask_scheduler_json, target_status, timeout, poll_interval, description):
    """Poll get_dask_cluster_status until status >= target_status or timeout."""
    elapsed = 0
    status, n_workers = get_dask_cluster_status(dask_scheduler_json)
    while status < target_status and elapsed < timeout:
        time.sleep(poll_interval)
        elapsed += poll_interval
        status, n_workers = get_dask_cluster_status(dask_scheduler_json)
    if status >= target_status:
        logger.debug(f"{description} succeeded after {elapsed:.1f}s")
    else:
        logger.warning(f"{description} timed out after {timeout}s (status: {status.name})")
    return (status, n_workers)


class DaskStatus(IntEnum):
    MISSING_JSON = 0
    SCHEDULER_ERROR = 1
    WORKERS_ERROR = 2
    NO_WORKERS = 3
    RUNNING = 4
    TESTED = 5

def initialize_dask_cluster(config):
    _, node = esm_parser.determine_computer_and_node_from_hostname()
    if not uses_dask(config) or node == "login_nodes":
        return config

    ini_dask_cluster(config)

    return config

def uses_dask(config):
    dask_config = config.get("dask", {})
    active_dask_actions = []
    for action in dask_config.get("actions", []):
        if config["general"].get(action) == "dask":
            active_dask_actions.append(action)
    return len(active_dask_actions) > 0

def test_dask():
    return True

def get_dask_cluster_status(dask_scheduler_json, client_timeout=0.01, test=False):
    n_workers = 0
    # Read tcp address from dask_scheduler_json
    if os.path.isfile(dask_scheduler_json):
        with open(dask_scheduler_json, "r") as f:
            scheduler_info = json.load(f)
            tcp_address = scheduler_info.get("address", None)
    else:
        logger.debug(f"Missing dask scheduler json file {dask_scheduler_json}")
        return (DaskStatus.MISSING_JSON, n_workers)

    try:
        client = daskd.Client(tcp_address, timeout=client_timeout)
    except Exception as e:
        logger.debug(f"Could not connect to dask scheduler at {tcp_address}: {e}")
        return (DaskStatus.SCHEDULER_ERROR, n_workers)

    try:
        n_workers = len(client.scheduler_info().get("workers", []))
    except Exception as e:
        logger.debug(f"Could not get dask workers info from scheduler at {tcp_address}: {e}")
        client.close()
        return (DaskStatus.WORKERS_ERROR, n_workers)

    if n_workers == 0:
        logger.debug(f"No dask workers connected to scheduler at {tcp_address}")
        client.close()
        return (DaskStatus.NO_WORKERS, n_workers)
    elif test:
        status = DaskStatus.RUNNING
        try:
            client.submit(test_dask).result()
            logger.debug(f"Dask test task succeeded on scheduler at {tcp_address} with {n_workers} workers")
            status = DaskStatus.TESTED
        except Exception as e:
            logger.debug(f"Dask test task failed on scheduler at {tcp_address}: {e}")
        client.close()
        return (status, n_workers)
    else:
        logger.debug(f"Dask cluster is running with {n_workers} workers connected to scheduler at {tcp_address}")
        client.close()
        return (DaskStatus.RUNNING, n_workers)

def ini_dask_cluster(config):

    logger.debug(f"{time.ctime()} | Start dask cluster initialization")
    # Load parameters
    dask_config = config.get("dask", {})
    dask_scheduler_json = dask_config["scheduler_json"]
    log_scheduler = f'{config["general"]["thisrun_log_dir"]}/dask_scheduler.log'

    nnodes = int(os.getenv(config["computer"]["nnodes_envvar"], 1))

    init_scheduler_cmd = config["dask"].get("init_scheduler_cmd")
    init_workers_cmd = config["dask"].get("init_workers_cmd")

    # Substitute placeholders in scheduler commands
    placeholders = [
        ("@nodes@", nnodes),
    ]

    for param, value in placeholders:
        init_workers_cmd = init_workers_cmd.replace(param, str(value))

    scheduler_timeout = dask_config.get("scheduler_timeout", 5)
    workers_timeout = dask_config.get("workers_timeout", 5)
    poll_interval = dask_config.get("poll_interval", 0.5)

    # Check whether there is already a dask scheduler started
    logger.debug(f"{time.ctime()} | Checking for existing dask scheduler")
    dask_status, n_workers = get_dask_cluster_status(dask_scheduler_json)

    # Run init_scheduler_cmd with subprocess and print output into a log file
    if dask_status <= DaskStatus.SCHEDULER_ERROR:
        logger.debug(f"Starting dask scheduler with: {init_scheduler_cmd}")
        process = subprocess.Popen(
            f"{init_scheduler_cmd} > {log_scheduler} 2>&1",
            shell=True,
            preexec_fn=os.setpgrp,
        )

        # Wait for the scheduler to become reachable
        wait_for_dask_status(
            dask_scheduler_json,
            target_status=DaskStatus.NO_WORKERS,
            timeout=scheduler_timeout,
            poll_interval=poll_interval,
            description="Dask scheduler startup",
        )

    elif dask_status > DaskStatus.NO_WORKERS and n_workers > 0:
        logger.debug("Skipping initialization of new dask scheduler.")
        return config

    # Start the dask workers
    logger.debug(f"Starting dask workers with: {init_workers_cmd}")
    process = subprocess.Popen(
        f"{init_workers_cmd} >> {log_scheduler} 2>&1",
        shell=True,
        preexec_fn=os.setpgrp,
    )

    dask_status, n_workers = get_dask_cluster_status(dask_scheduler_json)

    logger.info(f"Dask cluster status: {dask_status.name}, number of workers: {n_workers}")
    logger.debug(f"{time.ctime()} | End dask cluster initialization")
    return config
