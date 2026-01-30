"""
Rule: generate_dynamic_files

Creates files dynamically from content specified in the configuration.
This is used for coupling configs, hostfiles, or other runtime-generated files.

Event emitted: dynamic_files_generated
Depends on: runscript_staged (prepexp complete)
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

# Get files to create from config
CREATE_FILES = config.get("create_files", [])


rule generate_dynamic_files:
    """Generate files from config-specified content."""
    input:
        EVENT_DIR / "runscript_staged.yaml"
    output:
        event=EVENT_DIR / "dynamic_files_generated.yaml"
    message:
        "Generating dynamic files for {EXPID}"
    run:
        created_files = []

        for file_spec in CREATE_FILES:
            file_path = THISRUN_DIR / file_spec["path"]
            content = file_spec.get("content", "")

            # Create parent directory if needed
            file_path.parent.mkdir(parents=True, exist_ok=True)

            # Write content
            file_path.write_text(content)
            created_files.append(str(file_path))
            print(f"Created: {file_path}")

        # Emit event
        events.emit(
            "dynamic_files_generated",
            "generate_dynamic_files",
            stage="prepcompute",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            files_created=created_files,
            file_count=len(created_files),
        )

        if created_files:
            print(f"Generated {len(created_files)} dynamic files")
        else:
            print("No dynamic files to generate")
