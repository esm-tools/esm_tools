import os
import sys
import time

import psutil
from loguru import logger

from . import database_actions, helpers, logfiles


def run_job(config):
    helpers.evaluate(config, "observe", "observe_recipe")
    return config


def init_observe_logs(config):
    # which job am I spying on?
    observe_job = config["general"]["jobtype"]
    actual_job = observe_job.replace("observe_", "")
    called_from = actual_job
    # called_from = config["general"]["last_jobtype"]

    del config["general"]["command_line_config"]["launcher_pid"]

    exp_log_path = (
        config["general"]["experiment_log_dir"]
        + config["general"]["expid"]
        + "_"
        + config["general"]["setup_name"]
        + "_"
        + called_from
        + "_"
        + config["general"]["run_datestamp"]
        + "_"
        + str(config["general"]["jobid"])
        + ".log"
    )
    log_in_run = (
        config["general"]["thisrun_log_dir"]
        + config["general"]["expid"]
        + "_"
        + config["general"]["setup_name"]
        + "_"
        + called_from
        + "_"
        + str(config["general"]["jobid"])
        + ".log"
    )

    if os.path.isfile(exp_log_path):
        os.symlink(exp_log_path, log_in_run)

    logger.info(called_from)
    logger.info(exp_log_path)

    logger.debug("observing job initialized")
    logger.debug(f'attaching to process {str(config["general"]["launcher_pid"])}')
    logger.debug(f"Called from a {called_from} job")
    return config


def wait_and_observe(config):
    # `submitted` is True for batch (compute) jobs launched into a SLURM
    # allocation. Coupling subjobs (couple_in / couple_out) instead run their
    # heavy work as a backgrounded process on the login node (`<function> &`),
    # so `submitted` is False and this observe step would otherwise return
    # immediately -- triggering the next job (prepcompute) while couple_in is
    # still running. For a light coupling that finishes before the compute job
    # clears the queue this is harmless, but a heavy couple_in (e.g. ice2fesom
    # regenerating a submesh + OASIS weights, ~15 min) then has compute read
    # half-written coupling files. So also block whenever the monitored PID is a
    # live local process, regardless of `submitted`.
    monitor_pid = config["general"].get("launcher_pid")
    try:
        have_live_local_job = psutil.pid_exists(int(monitor_pid))
    except (TypeError, ValueError):
        have_live_local_job = False
    if config["general"]["submitted"] or have_live_local_job:
        thistime = 0
        error_check_list = assemble_error_list(config)
        while job_is_still_running(config):
            logger.debug("still running")
            config["general"]["next_test_time"] = thistime
            config = check_for_errors(config)
            thistime = thistime + 10
            time.sleep(10)
        thistime = thistime + 100000000
        config["general"]["next_test_time"] = thistime
        config = check_for_errors(config)
    config = check_coupling_failure(config)
    return config


def check_coupling_failure(config):
    """Stop the workflow when a coupling subjob failed.

    A coupling function (ice2fesom, fesom2ice, ...) is backgrounded by the generated
    .run script, and this observe job is NOT its parent -- so it can never see the
    exit code. ``assemble_error_list`` additionally returns an EMPTY error list for
    every non-compute job, so the log is not even scanned for keywords. A failing
    coupling step therefore used to advance the workflow silently, and the next leg
    would run on stale or half-written coupling files (observed: remap_oasis_restart
    correctly refused to write a mis-sized OASIS restart, yet the compute leg started
    anyway on an un-remapped restart).

    The coupling functions drop a ``.coupling_failed`` sentinel in COUPLE_DIR via
    ``couple_fail``. Honour it here and abort.
    """
    jobtype = config["general"].get("jobtype", "")
    if not jobtype.startswith("observe_couple"):
        return config
    couple_dir = config["general"].get("experiment_couple_dir")
    if not couple_dir:
        return config
    # concurrent mode namespaces the sentinel per chain; the bare name still
    # trips every chain (fail-stop)
    sentinels = [os.path.join(couple_dir, ".coupling_failed")]
    if config["general"].get("coupling_mode", "serial") == "concurrent":
        sentinels.append(
            os.path.join(
                couple_dir, ".coupling_failed." + config["general"]["setup_name"]
            )
        )
    for sentinel in sentinels:
        if os.path.isfile(sentinel):
            try:
                with open(sentinel) as fid:
                    reason = fid.read().strip()
            except OSError:
                reason = "(unreadable sentinel)"
            os.remove(sentinel)
            logger.error("=" * 78)
            logger.error(f"COUPLING STEP FAILED: {reason}")
            logger.error(f"({jobtype}) -- refusing to advance the workflow.")
            logger.error("=" * 78)
            sys.exit(1)
    return config


def wake_up_call(config):
    if hasattr(logger, "stdout_sink"):
        # Flush the content of te stdout sink and set print_in_stdout to True
        # so that the next messages will be printed in stdout, instead of buffered
        logger.stdout_sink.flush_to_stdout()
        logger.stdout_sink.print_in_stdout = True
        if (
            config["general"]["jobtype"] == "observe_compute"
            and not config["general"]["task_log_files"]
        ):
            # Observe log is always created to be used as a buffer while the
            # compute job is running in parallel to the observe job. If
            # ``task_log_files`` is ``False`` we delete the observe log file
            # once the log buffer is not needed anymore, after having flushed
            # its content to stdout.
            os.remove(logger.stdout_sink.path)
    logger.debug("job ended, starting to tidy up now \n")
    return config


def assemble_error_list(config):
    gconfig = config["general"]

    thisjob = config["general"]["jobtype"]
    called_from = thisjob.replace("observe_", "")
    if not called_from == "compute":
        config["general"]["error_list"] = []
        return config

    known_methods = ["warn", "kill"]

    stdout = (
        gconfig["thisrun_log_dir"]
        + "/"
        + gconfig["expid"]
        + "_"
        + config["general"]["setup_name"]
        + "_"
        + called_from
        + "_"
        + gconfig["jobid"]
        + ".log"
    )

    error_list = [
        ("error", stdout, "warn", 60, 60, "keyword error detected, watch out")
    ]

    for model in config:
        if "check_error" in config[model]:
            for trigger in config[model]["check_error"]:
                search_file = stdout
                method = "warn"
                frequency = 60
                message = "keyword " + trigger + " detected, watch out"
                if isinstance(config[model]["check_error"][trigger], dict):
                    if "file" in config[model]["check_error"][trigger]:
                        search_file = config[model]["check_error"][trigger]["file"]
                        if search_file == "stdout" or search_file == "stderr":
                            search_file = stdout
                        elif "@jobid@" in search_file:
                            search_file = search_file.replace(
                                "@jobid@", config["general"]["jobid"]
                            )
                    if "method" in config[model]["check_error"][trigger]:
                        method = config[model]["check_error"][trigger]["method"]
                        if method not in known_methods:
                            method = "warn"
                    if "message" in config[model]["check_error"][trigger]:
                        message = config[model]["check_error"][trigger]["message"]
                    if "frequency" in config[model]["check_error"][trigger]:
                        frequency = config[model]["check_error"][trigger]["frequency"]
                        try:
                            frequency = int(frequency)
                        except:
                            frequency = 60
                elif isinstance(config[model]["check_error"][trigger], str):
                    pass
                else:
                    continue
                error_list.append(
                    (trigger, search_file, method, frequency, frequency, message)
                )
    config["general"]["error_list"] = error_list
    return config


def check_for_errors(config):
    thisjob = config["general"]["jobtype"]
    called_from = thisjob.replace("observe_", "")
    if not called_from == "compute":
        return config

    new_list = []
    error_check_list = config["general"]["error_list"]
    time = config["general"]["next_test_time"]
    for (
        trigger,
        search_file,
        method,
        next_check,
        frequency,
        message,
    ) in error_check_list:
        warned = 0
        if next_check <= time:
            if os.path.isfile(search_file):
                with open(search_file) as origin_file:
                    for line in origin_file:
                        if trigger.upper() in line.upper():
                            if method == "warn":
                                warned = 1
                                logger.warning(f"WARNING: {message}")
                                break
                            elif method == "kill":
                                cancel_job = f"scancel {config['general']['jobid']}"
                                logger.error(f"ERROR: {message}")
                                logger.error("Will kill the run now...")
                                logger.stdout_sink.flush_to_stdout()
                                database_actions.database_entry_crashed(config)
                                os.system(cancel_job)
                                sys.exit(42)
            next_check += frequency
        if warned == 0:
            new_list.append(
                (trigger, search_file, method, next_check, frequency, message)
            )
    config["general"]["error_list"] = new_list
    return config


def job_is_still_running(config):
    # launcher_pid arrives from the CLI as a string (argparse has no type=int)
    # and defaults to -666 when absent; cast defensively so a missing/odd value
    # reports "not running" instead of raising inside the observe loop.
    try:
        return psutil.pid_exists(int(config["general"]["launcher_pid"]))
    except (TypeError, ValueError):
        return False
