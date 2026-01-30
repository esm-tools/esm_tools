"""
Rule: cleanup_run_directory

Optionally cleans up the thisrun work directory after results have been
archived. The cleanup behavior is controlled by configuration settings.

Event emitted: run_directory_cleaned
Depends on: results_archived

This is the final rule in the tidy stage (and the entire workflow).
"""

from pathlib import Path
import shutil
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, write_log_entry, get_experiment_log_path
from files import clean_directory

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
WORK_DIR = paths["work_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EXPERIMENT_DIR = paths["experiment_dir"]
LOG_DIR = paths["log_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

# Get cleanup settings from config
CLEAN_WORK = config.get("tidy", {}).get("clean_work", False)
KEEP_PATTERNS = config.get("tidy", {}).get("keep_patterns", ["*.log", "*.nc"])


rule cleanup_run_directory:
    """Clean up work directory after archiving."""
    input:
        EVENT_DIR / "results_archived.yaml"
    output:
        event=EVENT_DIR / "run_directory_cleaned.yaml"
    message:
        "Cleaning up run directory for {EXPID}"
    run:
        removed_count = 0
        kept_count = 0

        if CLEAN_WORK and WORK_DIR.exists():
            print(f"Cleaning work directory: {WORK_DIR}")
            print(f"Keeping patterns: {KEEP_PATTERNS}")

            # Count files before cleanup
            before_count = sum(1 for _ in WORK_DIR.rglob("*") if _.is_file())

            # Clean directory, keeping specified patterns
            removed_count = clean_directory(
                WORK_DIR,
                keep_patterns=KEEP_PATTERNS,
                remove_empty=True
            )

            # Count files after cleanup
            after_count = sum(1 for _ in WORK_DIR.rglob("*") if _.is_file())
            kept_count = after_count

            print(f"Removed {removed_count} files, kept {kept_count}")
        else:
            if not CLEAN_WORK:
                print("Work directory cleanup disabled in config")
            else:
                print(f"Work directory does not exist: {WORK_DIR}")

        # Write final log entry
        log_path = get_experiment_log_path(paths)
        write_log_entry(
            log_path,
            f"Run completed. Files archived: yes, Work cleaned: {CLEAN_WORK}",
            EXPID,
            DATESTAMP,
            stage="tidy"
        )

        # Emit event - this marks the end of the entire workflow
        events.emit(
            "run_directory_cleaned",
            "cleanup_run_directory",
            stage="tidy",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            work_dir=str(WORK_DIR),
            cleaned=CLEAN_WORK,
            removed_count=removed_count,
            kept_count=kept_count,
            tidy_complete=True,  # Mark tidy as complete
            workflow_complete=True,  # Mark entire workflow as complete
        )

        print(f"Tidy stage complete for {EXPID}")
        print(f"=" * 60)
        print(f"Workflow complete!")
        print(f"Experiment: {EXPID}")
        print(f"Run: {DATESTAMP}")
        print(f"Results: {EXPERIMENT_DIR}")
        print(f"=" * 60)
