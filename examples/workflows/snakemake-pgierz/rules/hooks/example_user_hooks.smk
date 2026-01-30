"""
Example User Hooks

This file demonstrates how to create custom rules that hook into the
event-driven workflow. To use these hooks:

1. Copy this file to user_hooks.smk in the same directory
2. Uncomment and modify the rules you need
3. The hooks will be automatically loaded by the main Snakefile

Hook Points (Events):
--------------------
Every event in the workflow is a potential hook point. Your custom rules
can depend on any event and produce their own events.

Prepexp events:
- experiment_directories_ready
- run_directories_ready
- logfile_initialized
- tools_installed
- runscript_staged

Prepcompute events:
- dynamic_files_generated
- input_sources_validated
- input_files_staged
- namelists_configured
- work_directory_ready
- configuration_snapshot_saved

Tidy events:
- outputs_collected
- unknown_files_inventoried
- file_manifest_generated
- results_archived
- run_directory_cleaned

Example Usage:
--------------
To validate ocean namelists after they're configured:

    rule validate_ocean_namelist:
        input:
            EVENT_DIR / "namelists_configured.yaml"
            exp_config = "exp_config.yaml"
        output:
            EVENT_DIR / "ocean_namelist_validated.yaml"
        run:
            # Your validation logic here
            ...

Then add "ocean_namelist_validated.yaml" to the hooks.prepcompute_prerequisites
list in your experiment config.
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


# =============================================================================
# Example Hook: Custom Validation After Namelists
# =============================================================================
# Uncomment this rule to add custom validation after namelists are configured.

# rule validate_model_configuration:
#     """Custom validation of model configuration."""
#     input:
#         namelists=EVENT_DIR / "namelists_configured.yaml",
#         # Add other dependencies as needed
#     output:
#         event=EVENT_DIR / "model_configuration_validated.yaml"
#     message:
#         "Running custom validation for {EXPID}"
#     run:
#         # Your validation logic here
#         # Example: check that certain namelist values are within bounds
#         print("Running custom validation...")
#
#         # If validation fails, raise an exception
#         # if not valid:
#         #     raise ValueError("Validation failed: ...")
#
#         # Emit success event
#         events.emit(
#             "model_configuration_validated",
#             "validate_model_configuration",
#             stage="user_hook",
#             output_path=Path(output.event),
#             expid=EXPID,
#             validation_status="passed",
#         )
#         print("Custom validation passed")


# =============================================================================
# Example Hook: Custom Preprocessing Before Work Directory
# =============================================================================
# Uncomment to add custom preprocessing steps.

# rule custom_preprocessing:
#     """Custom preprocessing before work directory is populated."""
#     input:
#         EVENT_DIR / "input_files_staged.yaml"
#     output:
#         event=EVENT_DIR / "custom_preprocessing_done.yaml"
#     message:
#         "Running custom preprocessing for {EXPID}"
#     run:
#         # Your preprocessing logic here
#         # Example: generate additional input files, run pre-processing scripts
#         print("Running custom preprocessing...")
#
#         # Emit event
#         events.emit(
#             "custom_preprocessing_done",
#             "custom_preprocessing",
#             stage="user_hook",
#             output_path=Path(output.event),
#             expid=EXPID,
#         )


# =============================================================================
# Example Hook: Custom Postprocessing After Outputs Collected
# =============================================================================
# Uncomment to add custom postprocessing steps.

# rule custom_postprocessing:
#     """Custom postprocessing after model outputs are collected."""
#     input:
#         EVENT_DIR / "outputs_collected.yaml"
#     output:
#         event=EVENT_DIR / "custom_postprocessing_done.yaml"
#     message:
#         "Running custom postprocessing for {EXPID}"
#     run:
#         # Your postprocessing logic here
#         # Example: compute diagnostics, generate plots, run analysis
#         print("Running custom postprocessing...")
#
#         # Emit event
#         events.emit(
#             "custom_postprocessing_done",
#             "custom_postprocessing",
#             stage="user_hook",
#             output_path=Path(output.event),
#             expid=EXPID,
#         )


# =============================================================================
# Example Hook: Notification on Workflow Completion
# =============================================================================
# Uncomment to send notifications when workflow completes.

# rule notify_completion:
#     """Send notification when workflow completes."""
#     input:
#         EVENT_DIR / "run_directory_cleaned.yaml"
#     output:
#         event=EVENT_DIR / "notification_sent.yaml"
#     message:
#         "Sending completion notification for {EXPID}"
#     run:
#         # Your notification logic here
#         # Example: send email, Slack message, etc.
#         print(f"Workflow complete for {EXPID}!")
#
#         # Emit event
#         events.emit(
#             "notification_sent",
#             "notify_completion",
#             stage="user_hook",
#             output_path=Path(output.event),
#             expid=EXPID,
#             notification_type="completion",
#         )
