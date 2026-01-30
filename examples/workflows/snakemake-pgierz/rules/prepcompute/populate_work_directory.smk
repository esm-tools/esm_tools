"""
Rule: populate_work_directory

Copies all necessary files from thisrun intermediate directories to the
work directory where the model will actually execute. This is the final
staging step before model execution.

Event emitted: work_directory_ready
Depends on: namelists_configured
"""

from pathlib import Path
import shutil
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, INPUT_FILETYPES
from files import collect_files_from_directory

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_DIR = paths["thisrun_dir"]
WORK_DIR = paths["work_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule populate_work_directory:
    """Populate work directory with all input files."""
    input:
        EVENT_DIR / "namelists_configured.yaml"
    output:
        marker=WORK_DIR / ".ready",
        event=EVENT_DIR / "work_directory_ready.yaml"
    message:
        "Populating work directory for {EXPID}"
    run:
        # Create work directory
        WORK_DIR.mkdir(parents=True, exist_ok=True)

        total_files = []

        # Copy files from each relevant filetype directory
        filetypes_to_copy = ["bin", "config", "input", "restart", "forcing"]

        for filetype in filetypes_to_copy:
            source_dir = THISRUN_DIR / filetype

            if source_dir.exists() and any(source_dir.iterdir()):
                # Copy all files from this directory to work
                # Using symlinks by default to save space
                copied = collect_files_from_directory(
                    source_dir,
                    WORK_DIR,
                    patterns=["*"],
                    method="link"  # Symlink to save space
                )
                total_files.extend(copied)

                if copied:
                    print(f"Linked {len(copied)} {filetype} files to work/")

        # Write marker file
        Path(output.marker).write_text(
            f"Work directory ready for {EXPID}\n"
            f"Run: {DATESTAMP}\n"
            f"Files: {len(total_files)}\n"
        )

        # Emit event
        events.emit(
            "work_directory_ready",
            "populate_work_directory",
            stage="prepcompute",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            work_dir=str(WORK_DIR),
            file_count=len(total_files),
            files=[str(f) for f in total_files[:50]],  # Limit to first 50 for event
        )

        print(f"Work directory ready with {len(total_files)} files at {WORK_DIR}")
