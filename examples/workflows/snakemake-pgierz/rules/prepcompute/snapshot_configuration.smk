"""
Rule: snapshot_configuration

Writes the complete resolved configuration to a YAML file for reproducibility.
This "finished_config" file captures the exact state of all settings used
for this specific run.

Event emitted: configuration_snapshot_saved
Depends on: work_directory_ready

This is the final rule in the prepcompute stage.
"""

from pathlib import Path
from datetime import datetime
import yaml
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_CONFIG_DIR = paths["thisrun_config_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

# Output path for finished config
FINISHED_CONFIG = THISRUN_CONFIG_DIR / f"{EXPID}_finished_config.yaml"


rule snapshot_configuration:
    """Save configuration snapshot for reproducibility."""
    input:
        EVENT_DIR / "work_directory_ready.yaml"
    output:
        config_snapshot=FINISHED_CONFIG,
        event=EVENT_DIR / "configuration_snapshot_saved.yaml"
    message:
        "Saving configuration snapshot for {EXPID}"
    run:
        # Ensure config directory exists
        THISRUN_CONFIG_DIR.mkdir(parents=True, exist_ok=True)

        # Build the finished config with provenance information
        finished_config = {
            "_provenance": {
                "expid": EXPID,
                "datestamp": DATESTAMP,
                "snapshot_time": datetime.now().isoformat(),
                "workflow_version": "snakemake-event-driven-0.1",
            },
            # Include all config sections
            **dict(config),
        }

        # Add computed paths to provenance
        finished_config["_provenance"]["paths"] = {
            "experiment_dir": str(paths["experiment_dir"]),
            "thisrun_dir": str(paths["thisrun_dir"]),
            "work_dir": str(paths["work_dir"]),
        }

        # Write config snapshot
        with open(output.config_snapshot, "w") as f:
            # Write header comment
            f.write(f"# Finished Configuration Snapshot\n")
            f.write(f"# Experiment: {EXPID}\n")
            f.write(f"# Run: {DATESTAMP}\n")
            f.write(f"# Generated: {datetime.now().isoformat()}\n")
            f.write(f"#\n")
            f.write(f"# This file captures the complete resolved configuration\n")
            f.write(f"# used for this specific run, for reproducibility.\n")
            f.write(f"#\n\n")

            yaml.dump(finished_config, f, default_flow_style=False, sort_keys=False)

        print(f"Configuration snapshot saved to {output.config_snapshot}")

        # Emit event - this marks the end of prepcompute stage
        events.emit(
            "configuration_snapshot_saved",
            "snapshot_configuration",
            stage="prepcompute",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            config_path=str(output.config_snapshot),
            prepcompute_complete=True,  # Mark prepcompute as complete
        )

        print(f"Prepcompute stage complete for {EXPID}")
