"""
Rule: prepare_run_directories

Creates the run-specific directory structure (thisrun dirs) with all
standard filetype subdirectories.

Event emitted: run_directories_ready
Depends on: experiment_directories_ready
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, ALL_FILETYPES, create_directory_structure

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EXPERIMENT_DIR = paths["experiment_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EVENT_DIR = paths["event_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule prepare_run_directories:
    """Create directory structure for this specific run."""
    input:
        EVENT_DIR / "experiment_directories_ready.yaml"
    output:
        flag=THISRUN_DIR / ".thisrun_dirs_created",
        event=EVENT_DIR / "run_directories_ready.yaml"
    message:
        "Preparing run directories for {EXPID} / {DATESTAMP}"
    run:
        # Create thisrun directory with all filetype subdirectories
        created_dirs = create_directory_structure(THISRUN_DIR, ALL_FILETYPES)

        # Write flag file for Snakemake
        Path(output.flag).touch()

        # Emit event
        events.emit(
            "run_directories_ready",
            "prepare_run_directories",
            stage="prepexp",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            thisrun_dir=str(THISRUN_DIR),
            directories=[str(d) for d in created_dirs],
            directory_count=len(created_dirs),
        )

        print(f"Created {len(created_dirs)} directories in {THISRUN_DIR}")
