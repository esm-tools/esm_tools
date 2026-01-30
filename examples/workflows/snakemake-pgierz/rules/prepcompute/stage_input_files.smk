"""
Rule: stage_input_files

Copies or links input files from their source locations to the thisrun
intermediate directories. This is the "staging" step that prepares files
for the model run.

Event emitted: input_files_staged
Depends on: input_sources_validated
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, get_all_file_lists
from files import copy_files_from_config

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule stage_input_files:
    """Stage input files to thisrun directories."""
    input:
        EVENT_DIR / "input_sources_validated.yaml"
    output:
        event=EVENT_DIR / "input_files_staged.yaml"
    message:
        "Staging input files for {EXPID}"
    run:
        # Get all file lists from config
        file_lists = get_all_file_lists(config)

        total_copied = []
        total_failed = []

        # Process each file type
        for filetype, files in file_lists.items():
            if not files:
                continue

            # Determine target base directory for this filetype
            target_base = THISRUN_DIR

            # Copy files
            result = copy_files_from_config(
                files,
                target_base,
                method="copy"  # Default to copy; files can override with "method" key
            )

            total_copied.extend(result["copied"])
            total_failed.extend(result["failed"])

            if result["copied"]:
                print(f"Staged {len(result['copied'])} {filetype} files")

        # Report results
        if total_failed:
            print(f"WARNING: Failed to stage {len(total_failed)} files")
        print(f"Total: {len(total_copied)} files staged to {THISRUN_DIR}")

        # Emit event
        events.emit(
            "input_files_staged",
            "stage_input_files",
            stage="prepcompute",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            thisrun_dir=str(THISRUN_DIR),
            staged_count=len(total_copied),
            failed_count=len(total_failed),
            staged_files=[str(f) for f in total_copied],
        )
