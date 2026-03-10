"""
Centralized file operation tracking for esm_runscripts.

All file movements (copy, move, link) should go through the FileTracker
to ensure consistent logging and optional checksum computation.
"""

import hashlib
import os
import shutil
import threading
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Optional

import esm_parser
from loguru import logger


class FileOperation(Enum):
    """Types of file operations that can be tracked."""

    COPY = "copy"
    MOVE = "move"
    LINK = "link"
    HARDLINK = "hardlink"


@dataclass
class TrackedFile:
    """A single tracked file operation."""

    source: str
    destination: str
    operation: FileOperation
    checksum: Optional[str] = None
    phase: Optional[str] = None  # "prepare", "tidy", etc.
    filetype: Optional[str] = None  # "forcing", "restart", "outdata", etc.
    component: Optional[str] = None  # "fesom", "echam", etc.


class FileTracker:
    """
    Thread-safe file operation tracker.

    Usage:
        tracker = FileTracker(compute_checksums=True)
        tracker.copy("/src/file.nc", "/dst/file.nc", phase="prepare", filetype="forcing")
        tracker.move("/src/output.nc", "/dst/output.nc", phase="tidy")
        tracker.dump_yaml("/path/to/filelist.yaml")
    """

    def __init__(self, compute_checksums: bool = False):
        """
        Initialize the file tracker.

        Parameters
        ----------
        compute_checksums : bool
            If True, compute MD5 checksums for all tracked files.
        """
        self._operations: list[TrackedFile] = []
        self._lock = threading.Lock()
        self.compute_checksums = compute_checksums
        self._skip_provenance = True  # Marker to skip provenance tracking

    def _compute_checksum(self, path: str) -> Optional[str]:
        """Compute MD5 checksum of a file."""
        if not self.compute_checksums:
            return None
        if not os.path.isfile(path):
            return None
        try:
            with open(path, "rb") as f:
                return hashlib.md5(f.read()).hexdigest()
        except (IOError, OSError) as e:
            logger.warning(f"Could not compute checksum for {path}: {e}")
            return None

    def _track(
        self,
        source: str,
        destination: str,
        operation: FileOperation,
        checksum: Optional[str] = None,
        **metadata,
    ) -> None:
        """Add a tracked operation (thread-safe)."""
        with self._lock:
            self._operations.append(
                TrackedFile(
                    source=source,
                    destination=destination,
                    operation=operation,
                    checksum=checksum,
                    phase=metadata.get("phase"),
                    filetype=metadata.get("filetype"),
                    component=metadata.get("component"),
                )
            )

    def record(
        self,
        source: str,
        destination: str,
        operation: str,
        checksum: Optional[str] = None,
        **metadata,
    ) -> None:
        """
        Record a file operation that was already performed elsewhere.

        Use this for parallel execution (dask/threads) where workers perform
        the operation and return metadata, then the main process records it.

        Parameters
        ----------
        source : str
            Source file path.
        destination : str
            Destination file path.
        operation : str
            Operation type: "copy", "move", "link", or "hardlink".
        checksum : str, optional
            Pre-computed checksum (if None and compute_checksums is True,
            will attempt to compute from destination).
        **metadata
            Additional metadata (phase, filetype, component).
        """
        logger.info(f"DEBUG FileTracker.record: {operation} {source} -> {destination}")
        op = FileOperation(operation)
        if checksum is None and self.compute_checksums:
            checksum = self._compute_checksum(destination)
        self._track(source, destination, op, checksum, **metadata)
        logger.info(f"DEBUG FileTracker.record: Now have {len(self._operations)} operations")

    def record_many(self, results: list[dict], **metadata) -> None:
        """
        Record multiple file operations from parallel execution.

        Parameters
        ----------
        results : list[dict]
            List of dicts with keys: source, destination, operation, checksum (optional).
        **metadata
            Additional metadata to apply to all records (phase, filetype, component).
        """
        for result in results:
            self.record(
                source=result["source"],
                destination=result["destination"],
                operation=result["operation"],
                checksum=result.get("checksum"),
                **{**metadata, **result.get("metadata", {})},
            )

    def copy(self, src: str, dst: str, **metadata) -> None:
        """
        Copy a file and track the operation.

        Parameters
        ----------
        src : str
            Source file path.
        dst : str
            Destination file path.
        **metadata
            Additional metadata (phase, filetype, component).
        """
        shutil.copy2(src, dst)
        checksum = self._compute_checksum(dst)
        self._track(src, dst, FileOperation.COPY, checksum, **metadata)
        logger.debug(f"Copied {src} -> {dst}")

    def move(self, src: str, dst: str, **metadata) -> None:
        """
        Move a file and track the operation.

        Parameters
        ----------
        src : str
            Source file path.
        dst : str
            Destination file path.
        **metadata
            Additional metadata (phase, filetype, component).
        """
        os.rename(src, dst)
        checksum = self._compute_checksum(dst)
        self._track(src, dst, FileOperation.MOVE, checksum, **metadata)
        logger.debug(f"Moved {src} -> {dst}")

    def link(self, target: str, link_path: str, **metadata) -> None:
        """
        Create a symbolic link and track the operation.

        Parameters
        ----------
        target : str
            The file the link points to.
        link_path : str
            The path of the symbolic link to create.
        **metadata
            Additional metadata (phase, filetype, component).
        """
        os.symlink(target, link_path)
        # For symlinks, checksum the actual target file
        real_path = os.path.realpath(link_path)
        checksum = self._compute_checksum(real_path) if os.path.exists(real_path) else None
        self._track(target, link_path, FileOperation.LINK, checksum, **metadata)
        logger.debug(f"Linked {link_path} -> {target}")

    def hardlink(self, src: str, dst: str, **metadata) -> None:
        """
        Create a hard link and track the operation.

        Parameters
        ----------
        src : str
            Source file path.
        dst : str
            Destination file path (the new hard link).
        **metadata
            Additional metadata (phase, filetype, component).
        """
        os.link(src, dst)
        checksum = self._compute_checksum(dst)
        self._track(src, dst, FileOperation.HARDLINK, checksum, **metadata)
        logger.debug(f"Hardlinked {src} -> {dst}")

    def to_dict(self) -> dict:
        """
        Convert tracked operations to a dictionary.

        Returns
        -------
        dict
            Dictionary organized by operation type.
        """
        result = {
            "copy": [],
            "move": [],
            "link": [],
            "hardlink": [],
        }

        for op in self._operations:
            entry = {
                "source": op.source,
                "destination": op.destination,
            }
            if op.checksum:
                entry["checksum"] = op.checksum
            if op.phase:
                entry["phase"] = op.phase
            if op.filetype:
                entry["filetype"] = op.filetype
            if op.component:
                entry["component"] = op.component

            result[op.operation.value].append(entry)

        return result

    def to_dict_by_phase(self) -> dict:
        """
        Convert tracked operations to a dictionary organized by phase.

        Returns
        -------
        dict
            Dictionary organized by phase, then operation type.
        """
        result = {}

        for op in self._operations:
            phase = op.phase or "unknown"
            if phase not in result:
                result[phase] = {"copy": [], "move": [], "link": [], "hardlink": []}

            entry = {
                "source": op.source,
                "destination": op.destination,
            }
            if op.checksum:
                entry["checksum"] = op.checksum
            if op.filetype:
                entry["filetype"] = op.filetype
            if op.component:
                entry["component"] = op.component

            result[phase][op.operation.value].append(entry)

        return result

    def dump_yaml(self, path: str, by_phase: bool = False) -> None:
        """
        Write tracked operations to a YAML file.

        Parameters
        ----------
        path : str
            Output file path.
        by_phase : bool
            If True, organize output by phase.
        """
        logger.info(f"DEBUG FileTracker.dump_yaml: {len(self._operations)} operations, writing to {path}")
        data = self.to_dict_by_phase() if by_phase else self.to_dict()
        logger.info(f"DEBUG FileTracker.dump_yaml: data = {data}")
        esm_parser.yaml_dump(data, path)
        logger.info(f"File tracking log written to {path}")

    def clear(self) -> None:
        """Clear all tracked operations."""
        with self._lock:
            self._operations.clear()

    def __len__(self) -> int:
        """Return number of tracked operations."""
        return len(self._operations)

    def summary(self) -> dict:
        """
        Get a summary of tracked operations.

        Returns
        -------
        dict
            Count of operations by type.
        """
        counts = {"copy": 0, "move": 0, "link": 0, "hardlink": 0}
        for op in self._operations:
            counts[op.operation.value] += 1
        return counts

    def __getstate__(self) -> dict:
        """Return state for pickling (exclude the lock)."""
        state = self.__dict__.copy()
        # Remove the lock - it can't be pickled
        del state["_lock"]
        return state

    def __setstate__(self, state: dict) -> None:
        """Restore state from pickling (recreate the lock)."""
        self.__dict__.update(state)
        # Recreate the lock
        self._lock = threading.Lock()

    @classmethod
    def yaml_representer(cls, dumper, data):
        """YAML representer - serialize as placeholder string."""
        return dumper.represent_scalar("!FileTracker", f"<FileTracker: {len(data)} operations>")

    @classmethod
    def register_yaml_representer(cls):
        """Register the YAML representer with ruamel.yaml."""
        try:
            from ruamel.yaml import YAML
            yaml = YAML()
            yaml.representer.add_representer(cls, cls.yaml_representer)
        except ImportError:
            pass


# Register representer on module load
try:
    from ruamel.yaml import RoundTripRepresenter
    RoundTripRepresenter.add_representer(
        FileTracker,
        lambda dumper, data: dumper.represent_scalar(
            "!FileTracker", f"<FileTracker: {len(data)} operations>"
        )
    )
except ImportError:
    pass
