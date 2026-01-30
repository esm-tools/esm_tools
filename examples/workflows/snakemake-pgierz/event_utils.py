"""Simple file-based event system for Snakemake workflows.

This provides a minimal event bus using YAML files on disk instead of SQLite.
Events are both completion markers (for Snakemake) and audit logs (for humans).
"""

import yaml
from pathlib import Path
from datetime import datetime
from typing import Any, Dict, Optional, List, Union


class FileEventBus:
    """Minimal event bus using YAML files on disk."""

    def __init__(self, event_dir: str = "events"):
        """Initialize event bus with a directory for storing events.

        Args:
            event_dir: Directory path where event YAML files will be written
        """
        self.event_dir = Path(event_dir)
        self.event_dir.mkdir(parents=True, exist_ok=True)

    def emit(
        self, event_type: str, rule_name: str, stage: Optional[str] = None, **data
    ) -> Path:
        """Emit an event by writing a YAML file.

        Args:
            event_type: Type of event (e.g., 'directories_created', 'files_copied')
            rule_name: Name of the Snakemake rule emitting this event
            stage: Stage name (e.g., 'prepexp', 'prepcompute', 'tidy')
            **data: Additional event data to store

        Returns:
            Path to the created event file
        """
        timestamp = datetime.now().isoformat()
        event_data = {
            "timestamp": timestamp,
            "event_type": event_type,
            "rule_name": rule_name,
            "stage": stage,
            **data,
        }

        # Format timestamp for alphabetical sorting: YYYYMMDD-HHMMSS
        # This ensures `ls` shows events in chronological order
        now = datetime.now()
        sortable_timestamp = now.strftime("%Y%m%d-%H%M%S")

        # Include stage in filename if provided
        if stage:
            event_file = (
                self.event_dir / f"{sortable_timestamp}_{stage}_{event_type}.yaml"
            )
        else:
            event_file = self.event_dir / f"{sortable_timestamp}_{event_type}.yaml"

        with open(event_file, "w") as f:
            yaml.dump(event_data, f, default_flow_style=False)

        print(
            f"[EVENT] {stage}.{event_type}: {rule_name} -> {event_file.name}"
            if stage
            else f"[EVENT] {event_type}: {rule_name} -> {event_file.name}"
        )
        return event_file

    def has_event(self, event_type: str, **match_data) -> bool:
        """Check if an event exists matching criteria.

        Args:
            event_type: Type of event to search for
            **match_data: Key-value pairs that must match in event data

        Returns:
            True if matching event exists, False otherwise
        """
        for event_file in self.event_dir.glob(f"*_{event_type}.yaml"):
            with open(event_file) as f:
                event = yaml.safe_load(f)

            # Check if all match_data keys/values exist in this event
            if all(event.get(k) == v for k, v in match_data.items()):
                return True

        return False

    def latest_event(self, event_type: str) -> Optional[Dict[str, Any]]:
        """Get the most recent event of a given type.

        Args:
            event_type: Type of event to search for

        Returns:
            Event data dict, or None if no events of this type exist
        """
        events = sorted(
            self.event_dir.glob(f"*_{event_type}.yaml"),
            reverse=True,  # Most recent first
        )

        if not events:
            return None

        with open(events[0]) as f:
            return yaml.safe_load(f)

    def query_events(
        self, event_type: Optional[str] = None, limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Query events with optional filters.

        Args:
            event_type: Filter by event type (None = all types)
            limit: Maximum number of events to return

        Returns:
            List of event data dicts, sorted by timestamp (newest first)
        """
        if event_type:
            pattern = f"*_{event_type}.yaml"
        else:
            pattern = "*.yaml"

        event_files = sorted(self.event_dir.glob(pattern), reverse=True)[:limit]

        events = []
        for event_file in event_files:
            with open(event_file) as f:
                events.append(yaml.safe_load(f))

        return events

    def clear_events(self, event_type: Optional[str] = None):
        """Clear events from disk.

        Args:
            event_type: Clear only this event type (None = clear all)
        """
        if event_type:
            pattern = f"*_{event_type}.yaml"
        else:
            pattern = "*.yaml"

        for event_file in self.event_dir.glob(pattern):
            event_file.unlink()
            print(f"Cleared: {event_file.name}")
