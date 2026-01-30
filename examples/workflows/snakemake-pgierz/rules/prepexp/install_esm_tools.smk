"""
Rule: install_esm_tools

Copies ESM-Tools configuration templates to the experiment's scripts directory.
This provides a snapshot of the tools configuration at experiment creation time.

In production, this would copy from the installed esm_tools package. For the
demo, we create a placeholder structure.

Event emitted: tools_installed
Depends on: logfile_initialized
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
ESM_TOOLS_DIR = paths["esm_tools_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]


rule install_esm_tools:
    """Install ESM-Tools configs to experiment directory."""
    input:
        EVENT_DIR / "logfile_initialized.yaml"
    output:
        marker=ESM_TOOLS_DIR / ".installed",
        event=EVENT_DIR / "tools_installed.yaml"
    message:
        "Installing ESM-Tools configuration for {EXPID}"
    run:
        # Ensure esm_tools directory exists
        ESM_TOOLS_DIR.mkdir(parents=True, exist_ok=True)

        # In production, this would copy from installed esm_tools:
        #   esm_tools.copy_config_folder(ESM_TOOLS_DIR / "configs")
        #   esm_tools.copy_namelist_folder(ESM_TOOLS_DIR / "namelists")
        #
        # For the demo, we create placeholder directories
        configs_dir = ESM_TOOLS_DIR / "configs"
        namelists_dir = ESM_TOOLS_DIR / "namelists"

        configs_dir.mkdir(parents=True, exist_ok=True)
        namelists_dir.mkdir(parents=True, exist_ok=True)

        # Write a README explaining this is a demo
        readme = ESM_TOOLS_DIR / "README.txt"
        readme.write_text(
            f"ESM-Tools Configuration Snapshot\n"
            f"=================================\n"
            f"Experiment: {EXPID}\n"
            f"Date: {DATESTAMP}\n"
            f"\n"
            f"In production, this directory contains a copy of the esm_tools\n"
            f"configs and namelists that were used when the experiment was\n"
            f"first created. This provides reproducibility by preserving the\n"
            f"exact configuration state.\n"
            f"\n"
            f"For this demo, the directories are placeholders.\n"
        )

        # Write marker file
        Path(output.marker).write_text(f"Installed for {EXPID}\n")

        installed_items = ["configs/", "namelists/", "README.txt"]

        # Emit event
        events.emit(
            "tools_installed",
            "install_esm_tools",
            stage="prepexp",
            output_path=Path(output.event),
            expid=EXPID,
            esm_tools_dir=str(ESM_TOOLS_DIR),
            installed_items=installed_items,
        )

        print(f"ESM-Tools configuration installed to {ESM_TOOLS_DIR}")
