"""
Rule: _create_setup_folders

Creates the top-level experiment directory structure.
Corresponds to prepexp recipe step 2.
"""
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from lib.common import get_paths, get_event_bus, ALL_FILETYPES

# Get paths and event bus
paths = get_paths(config)
events = get_event_bus(config)

EXPERIMENT_DIR = paths["experiment_dir"]
EVENT_DIR = paths["event_dir"]
EXPID = paths["expid"]

rule create_setup_folders:
    """Create top-level experiment directory structure."""
    output:
        flag=EXPERIMENT_DIR / ".experiment_dirs_created",
        event=EVENT_DIR / "experiment_directories_created.yaml"
    message:
        "Creating experiment directories for {EXPID}"
    run:
        # Get rule name from the rule object
        rule_name = str(rule)
        # Create experiment base directory
        EXPERIMENT_DIR.mkdir(parents=True, exist_ok=True)
        
        # Create experiment-level subdirectories for each filetype
        created_dirs = []
        for filetype in ALL_FILETYPES:
            filetype_dir = EXPERIMENT_DIR / filetype
            filetype_dir.mkdir(parents=True, exist_ok=True)
            created_dirs.append(str(filetype_dir))
            print(f"Created: {filetype_dir}")
        
        # Special: create scripts subdirectory for esm_tools configs
        scripts_esm_dir = EXPERIMENT_DIR / "scripts" / "esm_tools"
        scripts_esm_dir.mkdir(parents=True, exist_ok=True)
        created_dirs.append(str(scripts_esm_dir))
        print(f"Created: {scripts_esm_dir}")
        
        # Write flag file
        Path(output.flag).touch()
        
        # Emit event (creates timestamped file, but we also create a stable symlink)
        event_file = events.emit(
            "experiment_directories_created",
            rule_name,
            stage="prepexp",
            expid=EXPID,
            experiment_dir=str(EXPERIMENT_DIR),
            directories=created_dirs,
            directory_count=len(created_dirs)
        )
        
        # Create stable filename for Snakemake to track
        import shutil
        shutil.copy2(event_file, output.event)
