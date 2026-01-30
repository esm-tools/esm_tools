"""
Rule: initialize_experiment_log

Creates and initializes the experiment log file with header information.
This log tracks the progress of all runs within the experiment.

Event emitted: logfile_initialized
Depends on: run_directories_ready
"""

from pathlib import Path
from datetime import datetime
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, get_experiment_log_path

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
LOG_DIR = paths["log_dir"]
EXPID = paths["expid"]
SETUP_NAME = paths["setup_name"]
DATESTAMP = paths["datestamp"]

# Compute log file path
LOG_FILE = get_experiment_log_path(paths)


rule initialize_experiment_log:
    """Initialize the experiment log file."""
    input:
        EVENT_DIR / "run_directories_ready.yaml"
    output:
        log_file=LOG_FILE,
        event=EVENT_DIR / "logfile_initialized.yaml"
    message:
        "Initializing experiment log for {EXPID}"
    run:
        # Ensure log directory exists
        LOG_DIR.mkdir(parents=True, exist_ok=True)

        # Check if this is a new experiment or continuation
        is_new = not Path(output.log_file).exists() or \
                 Path(output.log_file).stat().st_size == 0

        with open(output.log_file, "a") as f:
            timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

            if is_new:
                # Write header for new experiment
                f.write("=" * 80 + "\n")
                f.write(f"Experiment Log: {EXPID}\n")
                f.write(f"Setup: {SETUP_NAME}\n")
                f.write(f"Created: {timestamp}\n")
                f.write("=" * 80 + "\n\n")
                f.write(f"{timestamp} | {EXPID} | Experiment initialized\n")
            else:
                f.write(f"\n{'-' * 40}\n")

            # Log the current run start
            f.write(f"{timestamp} | {EXPID} | {DATESTAMP} | [prepexp] Run started\n")

        # Emit event
        events.emit(
            "logfile_initialized",
            "initialize_experiment_log",
            stage="prepexp",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            log_file=str(LOG_FILE),
            is_new_experiment=is_new,
        )

        status = "created" if is_new else "appended to"
        print(f"Experiment log {status}: {LOG_FILE}")
