"""
Rule: configure_namelists

Applies configuration-specified modifications to namelist files.
This transforms template namelists into run-specific configurations.

In production, this would use f90nml to parse and modify Fortran namelists.
For the demo, we log what modifications would be applied.

Event emitted: namelists_configured
Depends on: input_files_staged
"""

from pathlib import Path
import sys

# Add lib directory to path
_workflow_dir = Path(workflow.basedir)
sys.path.insert(0, str(_workflow_dir / "lib"))

from common import get_paths, get_event_bus, get_file_list

# Get paths and event bus from config
paths = get_paths(config)
events = get_event_bus(config)

EVENT_DIR = paths["event_dir"]
THISRUN_CONFIG_DIR = paths["thisrun_config_dir"]
EXPID = paths["expid"]
DATESTAMP = paths["datestamp"]

# Get namelist modifications from config
NAMELIST_MODS = config.get("namelists", {})


rule configure_namelists:
    """Apply modifications to namelist files."""
    input:
        EVENT_DIR / "input_files_staged.yaml"
    output:
        event=EVENT_DIR / "namelists_configured.yaml"
    message:
        "Configuring namelists for {EXPID}"
    run:
        modified_namelists = []
        modifications_applied = {}

        # Get config files that need modification
        config_files = get_file_list(config, "config")

        for model_name, model_mods in NAMELIST_MODS.items():
            # Find corresponding config file
            namelist_path = THISRUN_CONFIG_DIR / f"namelist.{model_name}"

            # In production, this would use f90nml:
            #   import f90nml
            #   nml = f90nml.read(namelist_path)
            #   for group, settings in model_mods.items():
            #       if group not in nml:
            #           nml[group] = {}
            #       nml[group].update(settings)
            #   f90nml.write(nml, namelist_path)

            # For demo, log what would be modified
            print(f"Namelist modifications for {model_name}:")
            mod_list = []
            for group, settings in model_mods.items():
                for key, value in settings.items():
                    print(f"  [{group}] {key} = {value}")
                    mod_list.append(f"{group}.{key}={value}")

            modifications_applied[model_name] = mod_list

            # If namelist exists, append a comment showing modifications
            if namelist_path.exists():
                with open(namelist_path, "a") as f:
                    f.write(f"\n! Modifications applied for run {DATESTAMP}:\n")
                    for group, settings in model_mods.items():
                        for key, value in settings.items():
                            f.write(f"! [{group}] {key} = {value}\n")
                modified_namelists.append(str(namelist_path))
            else:
                # Create placeholder namelist for demo
                THISRUN_CONFIG_DIR.mkdir(parents=True, exist_ok=True)
                with open(namelist_path, "w") as f:
                    f.write(f"! Placeholder namelist for {model_name}\n")
                    f.write(f"! Run: {EXPID} / {DATESTAMP}\n")
                    f.write(f"!\n")
                    f.write(f"! Configured settings:\n")
                    for group, settings in model_mods.items():
                        f.write(f"&{group}\n")
                        for key, value in settings.items():
                            f.write(f"  {key} = {value}\n")
                        f.write(f"/\n\n")
                modified_namelists.append(str(namelist_path))
                print(f"Created placeholder namelist: {namelist_path}")

        # Emit event
        events.emit(
            "namelists_configured",
            "configure_namelists",
            stage="prepcompute",
            output_path=Path(output.event),
            expid=EXPID,
            datestamp=DATESTAMP,
            modified_namelists=modified_namelists,
            modifications=modifications_applied,
        )

        print(f"Configured {len(modified_namelists)} namelists")
