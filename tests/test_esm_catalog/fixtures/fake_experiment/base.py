"""Backends and result contracts for the synthetic experiment fixture.

The writer lives in :mod:`.config_builder`; this module holds the pieces it
binds to:

* :class:`Backend` -- where the experiment tree is written and how it is torn
  down. :class:`MemoryBackend` (fsspec ``memory://``, exercises the reader's
  h5netcdf remote branch) and :class:`DiskBackend` (a ``tmp_path``, exercises the
  netCDF4 local branch) are interchangeable; the builder is backend-agnostic.
  ``cleanup()`` is uniform -- disk is a no-op (pytest GCs ``tmp_path``), memory
  removes its subtree from the process-global store.
* :class:`FakeExperiment` / :class:`Expected` -- the object handed to a test: the
  ``root`` UPath to scan, the raw ``datasets`` for object-level assertions, and
  the ``expected`` values (counts, run span, checksums) to assert against.
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Mapping

from upath import UPath

from esm_catalog.types import ComponentName

AbsPath = str
"""The ``str`` form of an output file's absolute path under ``exp_root`` -- the
key used throughout (``FakeExperiment.datasets``, ``Expected.md5_by_path``) so
object-layer and end-to-end layers refer to files the same way."""

Md5 = str
"""A lowercase hex MD5 checksum of a written file's bytes."""


class Backend(ABC):
    """Where an experiment tree is written, and how it is torn down.

    The builder is backend-agnostic: it writes through ``base / expid`` using
    ordinary :class:`~upath.UPath` operations, so the same code produces a
    ``memory://`` tree or an on-disk tree. Backends differ only in :attr:`base`
    and :meth:`cleanup`.
    """

    @property
    @abstractmethod
    def base(self) -> UPath:
        """The root UPath under which experiments are created (``base / expid``)."""

    @abstractmethod
    def cleanup(self) -> None:
        """Tear down whatever this backend created. Must be idempotent."""


@dataclass
class MemoryBackend(Backend):
    """An in-memory backend on fsspec's ``memory://`` filesystem.

    Exercises the reader's remote branch (h5netcdf over fsspec). The fsspec
    ``MemoryFileSystem`` store is process-global, so *token* MUST be unique per
    experiment (derive it from ``tmp_path.name`` in the fixture) to keep parallel
    tests from colliding, and :meth:`cleanup` MUST run or the store leaks across
    the session.
    """

    token: str

    @property
    def base(self) -> UPath:
        return UPath(f"memory://{self.token}")

    def cleanup(self) -> None:
        base = self.base
        try:
            base.fs.rm(base.path, recursive=True)
        except FileNotFoundError:
            pass


@dataclass
class DiskBackend(Backend):
    """An on-disk backend rooted at a pytest ``tmp_path``.

    Exercises the reader's local branch (netCDF4). :meth:`cleanup` is a no-op:
    pytest garbage-collects ``tmp_path`` on its own, and the footprint is a few MB
    of tiny-grid files.
    """

    root_path: UPath

    @property
    def base(self) -> UPath:
        return self.root_path

    def cleanup(self) -> None:
        """No-op: pytest reaps the tmp_path tree."""


@dataclass(frozen=True)
class Expected:
    """The values a test asserts a scan of the fixture should produce.

    Everything here is computed by the builder from what it actually wrote, so a
    test can assert against it without re-deriving counts by hand.
    """

    experiment_id: ExperimentId
    run_start: datetime
    run_end: datetime
    components: tuple[ComponentName, ...]
    ts_paths: tuple[AbsPath, ...]
    fx_paths: tuple[AbsPath, ...]
    md5_by_path: Mapping[AbsPath, Md5]
    # Per-component views, so a test can assert about one component without
    # filtering the flat lists by a magic subdir substring. Keyed by component
    # name; the flat ``ts_paths`` / ``fx_paths`` are their concatenation.
    ts_paths_by_component: Mapping[ComponentName, tuple[AbsPath, ...]] = field(
        default_factory=dict
    )
    fx_paths_by_component: Mapping[ComponentName, tuple[AbsPath, ...]] = field(
        default_factory=dict
    )
    # restart_out files each segment writes. They are real output but MUST be
    # excluded by ``output_files`` (they live in the tidy ``restart_out`` category,
    # never ``outdata``), so they are tracked separately and never counted in
    # ts/fx/item totals.
    restart_paths: tuple[AbsPath, ...] = ()

    @property
    def ts_file_count(self) -> int:
        """Number of time-varying files written (one STAC Item each)."""
        return len(self.ts_paths)

    @property
    def fx_file_count(self) -> int:
        """Number of time-invariant (``fx``) files written (one STAC Item each)."""
        return len(self.fx_paths)

    @property
    def item_count(self) -> int:
        """Total STAC Items a full scan should yield (ts + fx)."""
        return self.ts_file_count + self.fx_file_count

    def ts_paths_for(self, component: ComponentName) -> tuple[AbsPath, ...]:
        """The time-varying file paths written by one *component*."""
        return self.ts_paths_by_component.get(component, ())

    def fx_paths_for(self, component: ComponentName) -> tuple[AbsPath, ...]:
        """The time-invariant (``fx``) file paths written by one *component*."""
        return self.fx_paths_by_component.get(component, ())

    def counts_for(self, component: ComponentName) -> tuple[int, int]:
        """The ``(ts, fx)`` file counts for one *component*."""
        return len(self.ts_paths_for(component)), len(self.fx_paths_for(component))


@dataclass
class FakeExperiment:
    """A ready-to-scan synthetic experiment, plus its object layer and expectations.

    Attributes
    ----------
    root : UPath
        The experiment root -- pass straight to ``scan_experiment`` /
        ``source_experiment`` / ``output_files``.
    datasets : dict[AbsPath, xr.Dataset]
        Every output file's absolute path -> the in-memory dataset it was built
        from, for object-level assertions that need no scan.
    expected : Expected
        Counts, run span, and checksums to assert against.
    backend : Backend
        The backend that owns the tree; :meth:`cleanup` delegates to it.
    """

    root: UPath
    datasets: dict[AbsPath, xr.Dataset] = field(default_factory=dict)
    expected: Expected | None = None
    backend: Backend | None = None

    def cleanup(self) -> None:
        """Tear down the underlying backend (idempotent; safe in a fixture finalizer)."""
        if self.backend is not None:
            self.backend.cleanup()
