"""
Rule: archive_results

Archives results from the thisrun directory to the experiment directory.
Files are versioned with the run datestamp, and symlinks point to the
latest version of each file.

Event emitted: results_archived
Depends on: file_manifest_generated
"""

from pathlib import Path
import shutil
import os
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, OUTPUT_FILETYPES

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EXPERIMENT_DIR = paths["experiment_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

# Get archive settings from config
ARCHIVE_RESULTS = config.get("tidy", {}).get("archive_results", True)


rule archive_results:
    """Archive results to experiment directory."""
    input:
        EVENT_DIR / "file_manifest_generated.yaml"
    output:
        event=EVENT_DIR / "results_archived.yaml"
    message:
        "Archiving results for {EXPID}"
    run:
        archived_files = []
        skipped_files = []

        if not ARCHIVE_RESULTS:
            print("Archiving disabled in config")
        else:
            # Archive output directories to experiment level
            archive_filetypes = ["outdata", "restart", "log", "mon"]

            for filetype in archive_filetypes:
                src_dir = THISRUN_DIR / filetype
                dst_dir = EXPERIMENT_DIR / filetype

                if not src_dir.exists():
                    continue

                dst_dir.mkdir(parents=True, exist_ok=True)

                for src_file in src_dir.rglob("*"):
                    if not src_file.is_file():
                        continue

                    # Skip empty files
                    if src_file.stat().st_size == 0:
                        skipped_files.append(str(src_file))
                        continue

                    # Compute relative path and versioned name
                    rel_path = src_file.relative_to(src_dir)
                    base_name = rel_path.stem
                    suffix = rel_path.suffix
                    versioned_name = f"{base_name}_{DATESTAMP}{suffix}"

                    # Target paths
                    dst_versioned = dst_dir / rel_path.parent / versioned_name
                    dst_latest = dst_dir / rel_path

                    # Create parent directories
                    dst_versioned.parent.mkdir(parents=True, exist_ok=True)

                    # Copy/move to versioned path
                    shutil.copy2(src_file, dst_versioned)

                    # Update latest symlink
                    if dst_latest.is_symlink():
                        dst_latest.unlink()
                    elif dst_latest.exists():
                        # Rename existing to old datestamp if needed
                        pass  # For simplicity, we just overwrite the symlink

                    # Create symlink: latest -> versioned
                    try:
                        dst_latest.symlink_to(versioned_name)
                    except Exception:
                        # If symlink fails, just copy
                        shutil.copy2(dst_versioned, dst_latest)

                    archived_files.append(str(dst_versioned))

                if archived_files:
                    count = len([f for f in archived_files if filetype in f])
                    if count:
                        print(f"Archived {count} {filetype} files")

        print(f"Total: {len(archived_files)} files archived to {EXPERIMENT_DIR}")

        # Emit event
        events.emit(
            "results_archived",
            "archive_results",
            stage="tidy",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            experiment_dir=str(EXPERIMENT_DIR),
            archived_count=len(archived_files),
            skipped_count=len(skipped_files),
            archived_files=archived_files[:20],  # First 20 for event
        )
