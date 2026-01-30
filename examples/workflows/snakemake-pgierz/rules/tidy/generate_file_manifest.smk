"""
Rule: generate_file_manifest

Generates a complete manifest of all files used in the run, including
sources, intermediates, and targets. This audit log provides full
file provenance for reproducibility and debugging.

Event emitted: file_manifest_generated
Depends on: unknown_files_inventoried
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, ALL_FILETYPES
from files import list_directory_contents

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_DIR = paths["thisrun_dir"]
LOG_DIR = paths["log_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule generate_file_manifest:
    """Generate complete file manifest for the run."""
    input:
        EVENT_DIR / "unknown_files_inventoried.yaml"
    output:
        manifest=LOG_DIR / f"{EXPID}_complete_manifest_{DATESTAMP}.txt",
        event=EVENT_DIR / "file_manifest_generated.yaml"
    message:
        "Generating file manifest for {EXPID}"
    run:
        # Ensure log directory exists
        LOG_DIR.mkdir(parents=True, exist_ok=True)

        total_files = 0
        manifest_data = {}

        with open(output.manifest, "w") as f:
            f.write("=" * 80 + "\n")
            f.write(f"Complete File Manifest\n")
            f.write(f"Experiment: {EXPID}\n")
            f.write(f"Run: {DATESTAMP}\n")
            f.write(f"Directory: {THISRUN_DIR}\n")
            f.write("=" * 80 + "\n\n")

            # Inventory each filetype directory
            for filetype in ALL_FILETYPES:
                filetype_dir = THISRUN_DIR / filetype

                if filetype_dir.exists():
                    files = list_directory_contents(filetype_dir, relative=True)

                    if files:
                        f.write(f"[{filetype}] ({len(files)} files)\n")
                        f.write("-" * 40 + "\n")

                        for filepath in sorted(files):
                            full_path = filetype_dir / filepath
                            size = full_path.stat().st_size if full_path.exists() else 0
                            size_str = format_size(size)
                            f.write(f"  {filepath:<50} {size_str:>10}\n")

                        f.write("\n")
                        manifest_data[filetype] = files
                        total_files += len(files)

            # Summary
            f.write("=" * 80 + "\n")
            f.write(f"Total: {total_files} files across {len(manifest_data)} directories\n")

        print(f"Generated manifest with {total_files} files: {output.manifest}")

        # Emit event
        events.emit(
            "file_manifest_generated",
            "generate_file_manifest",
            stage="tidy",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            manifest_path=str(output.manifest),
            total_files=total_files,
            files_by_type={k: len(v) for k, v in manifest_data.items()},
        )


def format_size(size_bytes: int) -> str:
    """Format file size in human-readable form."""
    for unit in ["B", "KB", "MB", "GB"]:
        if size_bytes < 1024:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024
    return f"{size_bytes:.1f} TB"
