"""
Rule: _create_component_folders

Creates the run-specific directory structure (thisrun dirs).
Corresponds to prepexp recipe step 3.
"""
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from lib.common import get_paths, get_event_bus, ALL_FILETYPES

# Get paths and event bus
paths = get_paths(config)
events = get_event_bus(config)

EXPERIMENT_DIR = paths["experiment_dir"]
THISRUN_DIR = paths["thisrun_dir"]
EVENT_DIR = paths["event_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

rule create_component_folders:
    """Create directory structure for this specific run."""
    input:
        EXPERIMENT_DIR / ".experiment_dirs_created"
    output:
        flag=THISRUN_DIR / ".thisrun_dirs_created",
        event=EVENT_DIR / "thisrun_directories_created.yaml"
    message:
        "Creating run directories for {DATESTAMP}"
    run:
        # Get rule name from the rule object
        rule_name = str(rule)
        # Create thisrun base directory
        THISRUN_DIR.mkdir(parents=True, exist_ok=True)
        
        # Create thisrun-level subdirectories for each filetype
        created_dirs = []
        for filetype in ALL_FILETYPES:
            filetype_dir = THISRUN_DIR / filetype
            filetype_dir.mkdir(parents=True, exist_ok=True)
            created_dirs.append(str(filetype_dir))
            print(f"Created: {filetype_dir}")
        
        # Write flag file
        Path(output.flag).touch()
        
        # Emit event (creates timestamped file, but we also create a stable symlink)
        event_file = events.emit(
            "thisrun_directories_created",
            rule_name,
            stage="prepexp",
            expid=EXPID,
            datestamp=DATESTAMP,
            thisrun_dir=str(THISRUN_DIR),
            directories=created_dirs,
            directory_count=len(created_dirs)
        )
        
        # Create stable filename for Snakemake to track
        import shutil
        shutil.copy2(event_file, output.event)
