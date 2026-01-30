"""
Rule: stage_runscript

Copies the runscript (experiment configuration YAML) to the thisrun scripts
directory. This ensures the exact configuration used for this run is preserved.

Event emitted: runscript_staged
Depends on: tools_installed

This is the final rule in the prepexp stage.
"""

from pathlib import Path
import shutil
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
SCRIPTS_DIR = paths["scripts_dir"]
THISRUN_SCRIPTS_DIR = paths["thisrun_scripts_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

# Get runscript path from config
RUNSCRIPT = config.get("general", {}).get("runscript", "experiment.yaml")
RUNSCRIPT_PATH = Path(RUNSCRIPT)


rule stage_runscript:
    """Stage the runscript to thisrun scripts directory."""
    input:
        EVENT_DIR / "tools_installed.yaml"
    output:
        # Copy to both experiment/scripts and thisrun/scripts
        exp_copy=SCRIPTS_DIR / RUNSCRIPT_PATH.name,
        thisrun_copy=THISRUN_SCRIPTS_DIR / RUNSCRIPT_PATH.name,
        event=EVENT_DIR / "runscript_staged.yaml"
    message:
        "Staging runscript for {EXPID} / {DATESTAMP}"
    run:
        # Ensure directories exist
        SCRIPTS_DIR.mkdir(parents=True, exist_ok=True)
        THISRUN_SCRIPTS_DIR.mkdir(parents=True, exist_ok=True)

        # Determine source path
        # First check if it's an absolute path
        if RUNSCRIPT_PATH.is_absolute() and RUNSCRIPT_PATH.exists():
            source = RUNSCRIPT_PATH
        # Then check relative to workflow directory
        elif (_workflow_dir / RUNSCRIPT_PATH).exists():
            source = _workflow_dir / RUNSCRIPT_PATH
        # Finally check current working directory
        elif RUNSCRIPT_PATH.exists():
            source = RUNSCRIPT_PATH.resolve()
        else:
            # For the demo, create a placeholder if source doesn't exist
            source = None

        if source:
            # Copy to experiment scripts (if not already there or different)
            if not Path(output.exp_copy).exists():
                shutil.copy2(source, output.exp_copy)
                print(f"Copied runscript to {output.exp_copy}")

            # Always copy to thisrun scripts
            shutil.copy2(source, output.thisrun_copy)
            print(f"Copied runscript to {output.thisrun_copy}")
        else:
            # Create placeholder for demo
            placeholder_content = (
                f"# Runscript placeholder for {EXPID}\n"
                f"# Original: {RUNSCRIPT}\n"
                f"# Date: {DATESTAMP}\n"
                f"#\n"
                f"# In production, the actual runscript would be copied here.\n"
            )
            Path(output.exp_copy).write_text(placeholder_content)
            Path(output.thisrun_copy).write_text(placeholder_content)
            print(f"Created placeholder runscript (original not found: {RUNSCRIPT})")

        # Emit event - this marks the end of prepexp stage
        events.emit(
            "runscript_staged",
            "stage_runscript",
            stage="prepexp",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            original_runscript=str(RUNSCRIPT),
            experiment_copy=str(output.exp_copy),
            thisrun_copy=str(output.thisrun_copy),
            prepexp_complete=True,  # Mark prepexp as complete
        )

        print(f"Prepexp stage complete for {EXPID}")
