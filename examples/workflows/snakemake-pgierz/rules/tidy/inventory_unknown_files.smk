"""
Rule: inventory_unknown_files

Identifies and logs files in the work directory that aren't tracked by
the configuration. These "unknown" files may be intermediate outputs,
temporary files, or unexpected model outputs.

Event emitted: unknown_files_inventoried
Depends on: outputs_collected
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, get_all_file_lists
from files import list_directory_contents

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
WORK_DIR = paths["work_dir"]
THISRUN_DIR = paths["thisrun_dir"]
LOG_DIR = paths["log_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule inventory_unknown_files:
    """Inventory files not tracked by configuration."""
    input:
        EVENT_DIR / "outputs_collected.yaml"
    output:
        inventory=LOG_DIR / f"{EXPID}_unknown_files_{DATESTAMP}.txt",
        event=EVENT_DIR / "unknown_files_inventoried.yaml"
    message:
        "Inventorying unknown files for {EXPID}"
    run:
        unknown_files = []
        known_patterns = set()

        # Build set of known file patterns from config
        file_lists = get_all_file_lists(config)
        for filetype, files in file_lists.items():
            for f in files:
                target = f.get("target", "")
                # Add the target filename as a known pattern
                known_patterns.add(Path(target).name)

        # Add common known files
        known_patterns.update([
            ".ready",
            ".installed",
            "hostfile_srun",
            "namcouple",
            "coupling.xml",
        ])

        # Scan work directory for unknown files
        if WORK_DIR.exists():
            work_files = list_directory_contents(WORK_DIR, relative=True)

            for f in work_files:
                filename = Path(f).name
                if filename not in known_patterns:
                    unknown_files.append(f)

        # Write inventory
        LOG_DIR.mkdir(parents=True, exist_ok=True)
        with open(output.inventory, "w") as inv:
            inv.write(f"Unknown Files Inventory\n")
            inv.write(f"Experiment: {EXPID}\n")
            inv.write(f"Run: {DATESTAMP}\n")
            inv.write(f"Work directory: {WORK_DIR}\n")
            inv.write("=" * 60 + "\n\n")

            if unknown_files:
                inv.write(f"Found {len(unknown_files)} unknown files:\n\n")
                for f in sorted(unknown_files):
                    inv.write(f"  {f}\n")
            else:
                inv.write("No unknown files found.\n")

        # Copy unknown files to unknown/ directory for preservation
        unknown_dir = THISRUN_DIR / "unknown"
        if unknown_files:
            unknown_dir.mkdir(parents=True, exist_ok=True)
            import shutil
            for f in unknown_files:
                src = WORK_DIR / f
                dst = unknown_dir / f
                if src.exists():
                    dst.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(src, dst)

        print(f"Inventoried {len(unknown_files)} unknown files")

        # Emit event
        events.emit(
            "unknown_files_inventoried",
            "inventory_unknown_files",
            stage="tidy",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            unknown_count=len(unknown_files),
            unknown_files=unknown_files[:50],  # First 50 for event
            inventory_path=str(output.inventory),
        )
