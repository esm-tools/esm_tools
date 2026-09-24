"""Shared fixtures for the esm_catalog STAC tests.

Zero-arg object builders (item, collection) and temp_nc live here as fixtures.
The parameterized builders (make_exp_metadata, make_file_metadata) and assert_valid are plain
functions in helpers.py, imported and called inline with per-test arguments.
"""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Collection, Extent, Item, SpatialExtent, TemporalExtent
from upath import UPath

from .fixtures.fake_experiment.base import (
    Backend,
    DiskBackend,
    FakeExperiment,
    MemoryBackend,
)
from .fixtures.fake_experiment.config_builder import build as _build


@pytest.fixture
def collection() -> Collection:
    """An empty pystac Collection, to exercise collection-level extensions directly."""
    return Collection(
        id="exp",
        description="test collection",
        extent=Extent(
            spatial=SpatialExtent(bboxes=[[-180.0, -90.0, 180.0, 90.0]]),
            temporal=TemporalExtent(intervals=[[None, None]]),
        ),
    )


@pytest.fixture
def item() -> Item:
    """A minimal pystac Item, to exercise item-level extensions directly."""
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


@pytest.fixture
def temp_nc(tmp_path) -> Path:
    """A throwaway .nc file on disk; make_item only needs the path to exist."""
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    return f


@pytest.fixture
def make_fake_experiment(tmp_path, request):
    """A factory yielding config-anchored :class:`FakeExperiment` objects.

    Yields ``_factory(**overrides)`` -> :class:`FakeExperiment`, forwarding to
    :func:`config_builder.build`. Each call defaults to a :class:`MemoryBackend`
    whose token is unique per test (``tmp_path.name`` plus a per-call counter, so
    repeated calls never collide on the process-global memory store), and registers
    the experiment's ``cleanup`` on the test finalizer.
    """
    built: list[FakeExperiment] = []

    def _factory(**overrides) -> FakeExperiment:
        if "backend" not in overrides:
            token = f"{tmp_path.name}-{len(built)}"
            overrides["backend"] = MemoryBackend(token=token)
        exp = _build(**overrides)
        built.append(exp)
        request.addfinalizer(exp.cleanup)
        return exp

    return _factory


@pytest.fixture
def fake_experiment_memory(tmp_path) -> FakeExperiment:
    """A ready default experiment on a per-test :class:`MemoryBackend`."""
    exp = _build(backend=MemoryBackend(token=tmp_path.name))
    yield exp
    exp.cleanup()


@pytest.fixture
def fake_experiment_disk(tmp_path) -> FakeExperiment:
    """A ready default experiment on a :class:`DiskBackend` rooted at ``tmp_path``."""
    exp = _build(backend=DiskBackend(UPath(tmp_path)))
    yield exp
    exp.cleanup()


@pytest.fixture(params=["memory", "disk"])
def fake_experiment(request, tmp_path) -> FakeExperiment:
    """A ready default experiment, parameterized across the memory and disk backends."""
    if request.param == "memory":
        backend: Backend = MemoryBackend(token=tmp_path.name)
    else:
        backend = DiskBackend(UPath(tmp_path))
    exp = _build(backend=backend)
    yield exp
    exp.cleanup()


@pytest.fixture
def serial_scan(monkeypatch):
    """Force ``scan_experiment`` to read in-process (serial), not via a process pool.

    The fsspec ``memory://`` store is process-global but is **not** inherited by
    *spawned* worker processes (the default start method on macOS), so a
    memory-backend scan fanned out over ``ProcessPoolExecutor`` reads zero files
    -- every worker sees an empty store. Patching ``parallel_map`` to a serial,
    in-process map keeps every read in the process that holds the store, which is
    the only way a ``memory://`` experiment can be scanned end-to-end.

    Disk-backed scans do **not** need this (spawned workers open real files), so
    request this fixture only in the memory backend's full-scan tests. It captures
    a genuine constraint: ``memory://`` is a single-process construct.
    """
    import esm_catalog.scan.ingest as ingest

    def _serial(inputs, worker, **_kwargs):
        return [worker(item) for item in inputs]

    monkeypatch.setattr(ingest, "parallel_map", _serial)
