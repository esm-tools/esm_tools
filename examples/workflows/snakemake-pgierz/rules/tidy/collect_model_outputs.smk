"""
Rule: collect_model_outputs

Collects model output files from the work directory back to the thisrun
intermediate directory structure. This preserves the outputs after a
model run completes.

In real usage, this runs after the SLURM job completes. For the demo,
it runs immediately after prepcompute.

Event emitted: outputs_collected
Depends on: configuration_snapshot_saved (or job completion marker)
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, OUTPUT_FILETYPES
from files import collect_files_from_directory

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
WORK_DIR = paths["work_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule collect_model_outputs:
    """Collect outputs from work directory."""
    input:
        # In production, this would wait for job completion
        # For demo, we depend on prepcompute completion
        EVENT_DIR / "configuration_snapshot_saved.yaml"
    output:
        event=EVENT_DIR / "outputs_collected.yaml"
    message:
        "Collecting model outputs for {EXPID}"
    run:
        total_collected = []

        # Output file types to collect
        output_patterns = {
            "outdata": ["*.nc", "*.grib", "*.bin"],
            "restart": ["*restart*", "*.rst"],
            "log": ["*.log", "*.out", "*.err"],
            "mon": ["*.mon", "*.monitor"],
        }

        # For the demo, we simulate collecting outputs by checking what's in work
        if WORK_DIR.exists():
            # Collect any files that appear to be outputs
            for filetype, patterns in output_patterns.items():
                target_dir = THISRUN_DIR / filetype

                for pattern in patterns:
                    for source_file in WORK_DIR.rglob(pattern):
                        if source_file.is_file():
                            # Copy to appropriate thisrun directory
                            rel_path = source_file.relative_to(WORK_DIR)
                            target_file = target_dir / rel_path
                            target_file.parent.mkdir(parents=True, exist_ok=True)

                            # Use move for outputs (they're no longer needed in work)
                            import shutil
                            shutil.copy2(source_file, target_file)
                            total_collected.append(str(target_file))

            if total_collected:
                print(f"Collected {len(total_collected)} output files")
            else:
                print("No output files to collect (demo mode)")
        else:
            print(f"Work directory not found (demo mode): {WORK_DIR}")

        # Emit event
        events.emit(
            "outputs_collected",
            "collect_model_outputs",
            stage="tidy",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            work_dir=str(WORK_DIR),
            collected_count=len(total_collected),
            collected_files=total_collected[:20],  # First 20 for event
        )
