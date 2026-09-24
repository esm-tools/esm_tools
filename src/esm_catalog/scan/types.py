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

Role = Literal["data", "restart"]
"""What an output file represents -- the STAC asset ``roles`` value it carries."""

Stream = str
"""The config-declared file-producing category a file belongs to (an
``outdata_targets``/``restart_out_files`` key, e.g. ``'echam_nc'``,
``'oce_restart'``) -- one Item per ``(component, stream)``, growing across
scans as more files of that stream are found. Not a CMIP ``variable_id``: a
single native output file can carry several variables."""


@dataclass(frozen=True)
class OutputFile:
    """One output file to scan: its component, stream, role, and (if known) checksum.

    Produced by the sourcing layer from the experiment config, so the walk does
    not have to infer the component from the path. ``stream`` identifies which
    growing Item this file's asset belongs to (see :data:`Stream`) -- fixed as
    ``'restart'`` for every restart file of a component (one Item covers every
    restart category together), or the ``outdata_targets`` key for data.
    ``category`` is the finer-grained semantic name within that Item (the raw
    ``restart_out_sources`` key, e.g. ``'oce_restart'``) -- used to build the
    asset key for a restart file; unused (``None``) for data, where the
    stream name already carries the semantic identity.
    """

    path: UPath
    component: ComponentName
    stream: Optional[Stream]
    role: Role = "data"
    category: Optional[str] = None
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
