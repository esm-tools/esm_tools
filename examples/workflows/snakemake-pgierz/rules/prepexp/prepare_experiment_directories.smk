"""
Rule: prepare_experiment_directories

Creates the top-level experiment directory structure with all standard
filetype subdirectories (analysis/, bin/, config/, etc.).

Event emitted: experiment_directories_ready
Depends on: None (first rule in prepexp chain)
"""

import sys
from pathlib import Path

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import (ALL_FILETYPES, create_directory_structure, get_event_bus,
                    get_paths)

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EXPERIMENT_DIR = paths["experiment_dir"]
EVENT_DIR = paths["event_dir"]
EXPID = paths["expid"]


rule prepare_experiment_directories:
    """Create top-level experiment directory structure."""
    output:
        flag=EXPERIMENT_DIR / ".experiment_dirs_created",
        event=EVENT_DIR / "experiment_directories_ready.yaml"
    message:
        "Preparing experiment directories for {EXPID}"
    run:
        # Create experiment directory with all filetype subdirectories
        created_dirs = create_directory_structure(EXPERIMENT_DIR, ALL_FILETYPES)

        # Create special esm_tools subdirectory in scripts
        esm_tools_dir = EXPERIMENT_DIR / "scripts" / "esm_tools"
        esm_tools_dir.mkdir(parents=True, exist_ok=True)
        created_dirs.append(esm_tools_dir)

        # Create marker file at top of experiment tree
        marker_file = EXPERIMENT_DIR / ".top_of_exp_tree"
        marker_file.write_text(f"Top of experiment {EXPID}\n")

        # Write flag file for Snakemake
        Path(output.flag).touch()

        # Emit event
        events.emit(
            "experiment_directories_ready",
            "prepare_experiment_directories",
            stage="prepexp",
            output_path=Path(output.event),
            expid=EXPID,
            experiment_dir=str(EXPERIMENT_DIR),
            directories=[str(d) for d in created_dirs],
            directory_count=len(created_dirs),
        )

        print(f"Created {len(created_dirs)} directories in {EXPERIMENT_DIR}")



rule prepare_experiment_directories_new:
    ....

ruleorder: prepare_experiment_directories_new > prepare_experiment_directories
