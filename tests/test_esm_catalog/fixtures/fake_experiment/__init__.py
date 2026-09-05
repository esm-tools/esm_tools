"""Config-anchored synthetic experiment fixture for the scan layer.

:func:`build` resolves a real ESM-Tools runscript to its true component set and
output-file patterns (see :mod:`.resolver`), materializes concrete files on an
in-memory or on-disk :class:`Backend`, and returns a :class:`FakeExperiment` with
the :class:`Expected` values a scan should recover.
"""

from __future__ import annotations

from .base import Backend, DiskBackend, Expected, FakeExperiment, MemoryBackend
from .config_builder import Segment, build

__all__ = [
    "build",
    "Segment",
    "FakeExperiment",
    "Expected",
    "Backend",
    "MemoryBackend",
    "DiskBackend",
]
