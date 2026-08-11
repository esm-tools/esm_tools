"""Legible vocabulary for the scan layer.

Internal immutable value objects (frozen dataclasses) plus a couple of string
aliases that recur across the walk, sourcing, parallel, and storage modules.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal, Optional

from upath import UPath

from esm_catalog.types import ComponentName

ScanPhase = Literal["sourcing", "reading", "writing"]
"""Which stage of a scan a :class:`ProgressEvent` reports."""

RunStamp = str
"""A run-segment stamp, e.g. '20000101-20001231' — one finished_config window."""

Md5 = str
"""A lowercase hex MD5 checksum."""


@dataclass(frozen=True)
class OutputFile:
    """One output file to scan, with its component and (if known) checksum.

    Produced by the sourcing layer from the experiment config, so the walk does
    not have to infer the component from the path.
    """

    path: UPath
    component: ComponentName
    md5: Optional[Md5] = None


@dataclass(frozen=True)
class ScanFailure:
    """A file the scan could not read; collected for the failures sidecar."""

    path: str
    error: str


@dataclass(frozen=True)
class ScanReport:
    """The outcome summary of a scan run."""

    scanned: int
    items: int
    skipped: int
    unsupported: int
    failures: tuple[ScanFailure, ...]


@dataclass(frozen=True)
class ProgressEvent:
    """A scan progress update for a CLI/UI to render.

    Emitted by :func:`scan_experiment` so the library can report progress
    without depending on any UI toolkit — the CLI turns these into a spinner
    and bar; a library caller can ignore them or log them.
    """

    phase: ScanPhase
    current: int = 0
    total: int = 0
    detail: str = ""
