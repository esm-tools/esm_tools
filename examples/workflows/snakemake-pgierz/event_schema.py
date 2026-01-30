"""
Event schema definitions using Pydantic.

This provides structured, validated, and reconstructable event objects.
"""

from datetime import datetime
from typing import Optional, Dict, Any, List
from pathlib import Path

from pydantic import BaseModel, Field, ConfigDict


class WorkflowEvent(BaseModel):
    """Base class for all workflow events."""

    model_config = ConfigDict(
        # Allow arbitrary types like Path
        arbitrary_types_allowed=True,
        # Generate JSON schema
        json_schema_extra={"description": "Base workflow event"},
    )

    timestamp: datetime = Field(
        default_factory=datetime.now, description="When the event occurred"
    )
    event_type: str = Field(description="Type of event (e.g., 'directories_created')")
    rule_name: str = Field(
        description="Name of the Snakemake rule that emitted this event"
    )
    stage: Optional[str] = Field(
        default=None,
        description="Workflow stage (e.g., 'prepexp', 'prepcompute', 'tidy')",
    )
    expid: str = Field(description="Experiment ID")

    # Allow additional fields for extensibility
    extra_data: Dict[str, Any] = Field(
        default_factory=dict, description="Additional event-specific data"
    )


class DirectoriesCreatedEvent(WorkflowEvent):
    """Event emitted when directories are created."""

    event_type: str = Field(default="directories_created", description="Event type")

    directories: List[str] = Field(
        description="List of directory paths that were created"
    )
    directory_count: int = Field(description="Number of directories created")
    experiment_dir: Optional[str] = Field(
        default=None, description="Path to experiment directory"
    )
    thisrun_dir: Optional[str] = Field(
        default=None, description="Path to this run directory"
    )
    datestamp: Optional[str] = Field(
        default=None, description="Date stamp for this run"
    )


class FilesProcessedEvent(WorkflowEvent):
    """Event emitted when files are processed (copied, modified, etc.)."""

    operation: str = Field(description="Operation performed (copy, modify, link, etc.)")
    file_count: int = Field(description="Number of files processed")
    source_path: Optional[str] = Field(
        default=None, description="Source path for file operations"
    )
    target_path: Optional[str] = Field(
        default=None, description="Target path for file operations"
    )


class JobSubmittedEvent(WorkflowEvent):
    """Event emitted when a SLURM job is submitted."""

    event_type: str = Field(default="job_submitted", description="Event type")

    job_id: str = Field(description="SLURM job ID")
    slurm_script: str = Field(description="Path to SLURM script")
    partition: Optional[str] = Field(default=None, description="SLURM partition")
    nodes: Optional[int] = Field(default=None, description="Number of nodes requested")


class JobCompletedEvent(WorkflowEvent):
    """Event emitted when a SLURM job completes."""

    event_type: str = Field(default="job_completed", description="Event type")

    job_id: str = Field(description="SLURM job ID")
    exit_code: int = Field(description="Job exit code")
    state: str = Field(description="Final job state (COMPLETED, FAILED, etc.)")
    runtime_seconds: Optional[float] = Field(
        default=None, description="Job runtime in seconds"
    )


# Event type registry for deserialization
EVENT_TYPES = {
    "directories_created": DirectoriesCreatedEvent,
    "files_processed": FilesProcessedEvent,
    "job_submitted": JobSubmittedEvent,
    "job_completed": JobCompletedEvent,
    # Add more event types here as they're defined
}


def event_from_dict(data: Dict[str, Any]) -> WorkflowEvent:
    """Reconstruct an event object from a dictionary.

    Args:
        data: Event data dictionary (e.g., loaded from YAML)

    Returns:
        Appropriate WorkflowEvent subclass instance
    """
    event_type = data.get("event_type")
    event_class = EVENT_TYPES.get(event_type, WorkflowEvent)
    return event_class(**data)
