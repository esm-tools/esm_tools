"""
Rule: validate_input_sources

Validates that all source files specified in the config exist before
attempting to copy them. Logs the file plan and reports any missing files.

Event emitted: input_sources_validated
Depends on: dynamic_files_generated
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, get_all_file_lists
from files import validate_sources

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
LOG_DIR = paths["log_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule validate_input_sources:
    """Validate that all source files exist."""
    input:
        EVENT_DIR / "dynamic_files_generated.yaml"
    output:
        manifest=LOG_DIR / f"{EXPID}_file_manifest_{DATESTAMP}.txt",
        event=EVENT_DIR / "input_sources_validated.yaml"
    message:
        "Validating input sources for {EXPID}"
    run:
        # Get all file lists from config
        file_lists = get_all_file_lists(config)

        all_valid = []
        all_missing = []
        manifest_lines = [
            f"File Manifest for {EXPID}",
            f"Run: {DATESTAMP}",
            "=" * 60,
            "",
        ]

        # Validate each file type
        for filetype, files in file_lists.items():
            if not files:
                continue

            manifest_lines.append(f"[{filetype}]")
            result = validate_sources(files)

            all_valid.extend(result["valid"])
            all_missing.extend(result["missing"])

            for f in files:
                source = f["source"]
                target = f["target"]
                status = "OK" if source in result["valid"] else "MISSING"
                manifest_lines.append(f"  {status}: {source} -> {target}")

            manifest_lines.append("")

        # Write manifest
        LOG_DIR.mkdir(parents=True, exist_ok=True)
        Path(output.manifest).write_text("\n".join(manifest_lines))
        print(f"Wrote file manifest to {output.manifest}")

        # Report results
        if all_missing:
            print(f"WARNING: {len(all_missing)} missing source files:")
            for f in all_missing:
                print(f"  - {f}")
        else:
            print(f"All {len(all_valid)} source files validated")

        # Emit event
        events.emit(
            "input_sources_validated",
            "validate_input_sources",
            stage="prepcompute",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            valid_count=len(all_valid),
            missing_count=len(all_missing),
            missing_files=all_missing,
            manifest_path=str(output.manifest),
        )
