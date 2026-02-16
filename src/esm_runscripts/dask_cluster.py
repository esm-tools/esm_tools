import json
import os
import subprocess
import time
from enum import IntEnum

import dask.distributed as daskd
from loguru import logger

import esm_parser


def wait_for_dask_status(
    dask_scheduler_json,
    target_status,
    timeout,
    poll_interval,
    description,
    client_timeout=0.01,
):
    """
    Poll ``get_dask_cluster_status`` until the cluster reaches
    ``target_status`` or the ``timeout`` expires.

    Parameters
    ----------
    dask_scheduler_json : str
        Path to the dask scheduler JSON file.
    target_status : DaskStatus
        Minimum status level to wait for.
    timeout : float
        Maximum seconds to wait before giving up.
    poll_interval : float
        Seconds between status checks.
    description : str
        Label used in log messages (e.g. ``"Dask scheduler startup"``).

    Returns
    -------
    status : DaskStatus
        The last observed cluster status.
    n_workers : int
        Number of workers connected at the time of the last check.
    """
    elapsed = 0
    status, n_workers = get_dask_cluster_status(
        dask_scheduler_json, client_timeout=client_timeout
    )
    while status < target_status and elapsed < timeout:
        time.sleep(poll_interval)
        elapsed += poll_interval
        status, n_workers = get_dask_cluster_status(
            dask_scheduler_json, client_timeout=client_timeout
        )
    if status >= target_status:
        logger.debug(f"{description} succeeded after {elapsed:.1f}s")
    else:
        logger.warning(
            f"{description} timed out after {timeout}s (status: {status.name})"
        )
    return (status, n_workers)


class DaskStatus(IntEnum):
    """
    Status levels for a dask cluster, ordered by readiness.

    The integer ordering allows comparison: a cluster with status >= RUNNING
    is ready to accept work.
    """

    MISSING_JSON = 0
    SCHEDULER_ERROR = 1
    WORKERS_ERROR = 2
    NO_WORKERS = 3
    RUNNING = 4
    TESTED = 5


def initialize_dask_cluster(config):
    """
    Entry point for dask cluster initialization.

    Starts a dask scheduler and workers as background processes when dask is
    enabled in the config and the current node is a compute node. Does nothing
    on login nodes or when dask is not configured.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary (read-only).

    Returns
    -------
    config : dict
        The unchanged simulation configuration.
    """
    _, node = esm_parser.determine_computer_and_node_from_hostname()
    if not uses_dask(config) or node == "login_nodes":
        return config

    ini_dask_cluster(config)

    return config


def uses_dask(config):
    """
    Check whether any action in ``config["dask"]["actions"]`` is set to
    ``"dask"`` in the general configuration.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary.

    Returns
    -------
    bool
        ``True`` if at least one action uses dask.
    """
    dask_config = config.get("dask", {})
    active_dask_actions = []
    for action in dask_config.get("actions", []):
        if config["general"].get(action) == "dask":
            active_dask_actions.append(action)
    return len(active_dask_actions) > 0


def test_dask():
    """Trivial function submitted to the cluster to verify workers are responsive."""
    return True


def get_dask_cluster_status(
    dask_scheduler_json, client_timeout=0.01, submit_test_task=False
):
    """
    Probe the health of a dask cluster by reading its scheduler JSON,
    connecting to the scheduler, and optionally submitting a test task.

    This function is called repeatedly during polling loops, so it logs
    only at trace level.

    Parameters
    ----------
    dask_scheduler_json : str
        Path to the dask scheduler JSON file containing the TCP address.
    client_timeout : float, optional
        Timeout in seconds for connecting to the scheduler (default 0.01).
    submit_test_task : bool, optional
        If ``True``, submit a test task to verify the cluster can execute
        work (default ``False``).

    Returns
    -------
    status : DaskStatus
        The current cluster status.
    n_workers : int
        Number of workers connected to the scheduler.
    """
    n_workers = 0
    # Read tcp address from dask_scheduler_json
    if os.path.isfile(dask_scheduler_json):
        with open(dask_scheduler_json, "r") as f:
            scheduler_info = json.load(f)
            tcp_address = scheduler_info.get("address", None)
    else:
        logger.trace(f"Missing dask scheduler json file {dask_scheduler_json}")
        return (DaskStatus.MISSING_JSON, n_workers)

    try:
        client = daskd.Client(tcp_address, timeout=client_timeout)
    except Exception as e:
        logger.trace(f"Could not connect to dask scheduler at {tcp_address}: {e}")
        return (DaskStatus.SCHEDULER_ERROR, n_workers)

    try:
        n_workers = len(client.scheduler_info().get("workers", []))
    except Exception as e:
        logger.trace(f"Could not get dask workers info: {e}")
        client.close()
        return (DaskStatus.WORKERS_ERROR, n_workers)

    if n_workers == 0:
        logger.trace(f"No dask workers connected to scheduler at {tcp_address}")
        client.close()
        return (DaskStatus.NO_WORKERS, n_workers)
    elif submit_test_task:
        status = DaskStatus.RUNNING
        try:
            client.submit(test_dask).result()
            logger.trace(
                f"Dask test task succeeded on scheduler at {tcp_address} with {n_workers} workers"
            )
            status = DaskStatus.TESTED
        except Exception as e:
            logger.trace(f"Dask test task failed: {e}")
        client.close()
        return (status, n_workers)
    else:
        logger.trace(f"Dask cluster running with {n_workers} workers at {tcp_address}")
        client.close()
        return (DaskStatus.RUNNING, n_workers)


def shutdown_dask_cluster(config):
    """
    Gracefully shut down the dask scheduler and workers.

    Connects to the running scheduler (if reachable) and calls
    ``Client.shutdown()``, which terminates all workers and the scheduler
    process. If the cluster is not running, the function is a no-op.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary.

    Returns
    -------
    config : dict
        The unchanged simulation configuration.
    """
    dask_config = config.get("dask", {})
    dask_scheduler_json = dask_config.get("scheduler_json")
    if not dask_scheduler_json:
        return config

    client_timeout = dask_config.get("client_timeout", 0.01)
    status, _ = get_dask_cluster_status(
        dask_scheduler_json, client_timeout=client_timeout
    )
    if status < DaskStatus.NO_WORKERS:
        logger.debug("No dask cluster to shut down.")
        return config

    try:
        with open(dask_scheduler_json, "r") as f:
            scheduler_info = json.load(f)
            tcp_address = scheduler_info.get("address")
        client = daskd.Client(tcp_address, timeout=2)
        client.shutdown()
        logger.info("Dask cluster shut down successfully.")
    except Exception as e:
        logger.warning(f"Could not shut down dask cluster: {e}")

    return config


def ini_dask_cluster(config):
    """
    Start a dask scheduler and workers as background processes.

    If a scheduler is already running and has workers, the function returns
    early. Otherwise it launches the scheduler command, waits for it to
    become reachable, then launches the workers command. Commands and
    timeouts are read from ``config["dask"]``.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary (read-only).

    Returns
    -------
    config : dict
        The unchanged simulation configuration.
    """
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

    client_timeout = dask_config.get("client_timeout", 0.01)
    scheduler_timeout = dask_config.get("scheduler_timeout", 5)
    workers_timeout = dask_config.get(
        "workers_timeout", dask_config.get("workers_timeout", 5)
    )
    poll_interval = dask_config.get("poll_interval", 0.5)

    # Check whether there is already a dask scheduler started
    dask_status, n_workers = get_dask_cluster_status(
        dask_scheduler_json, client_timeout=client_timeout
    )

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
            client_timeout=client_timeout,
        )

    elif dask_status > DaskStatus.WORKERS_ERROR:
        logger.debug(
            "Skipping initialization of new dask scheduler and workers since an active "
            f"cluster is already running (status: {n_workers} workers running)"
        )
        return config

    # Start the dask workers
    logger.debug(f"Starting dask workers with: {init_workers_cmd}")
    process = subprocess.Popen(
        f"{init_workers_cmd} >> {log_scheduler} 2>&1",
        shell=True,
        preexec_fn=os.setpgrp,
    )

    dask_status, n_workers = get_dask_cluster_status(
        dask_scheduler_json, client_timeout=client_timeout
    )

    logger.info(
        f"Dask cluster status: {dask_status.name}, number of workers: {n_workers}"
    )
    return config
