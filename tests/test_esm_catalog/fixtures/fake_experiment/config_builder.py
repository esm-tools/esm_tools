"""Build a scannable experiment whose structure comes from real ESM-Tools config.

Where the hand-declared builder invents component names and filenames, this one
asks :mod:`.resolver` what a model actually writes (component set + per-component
``outdata_targets`` glob patterns) and materializes concrete files by filling
those patterns with a synthetic timeline. The Fortran that would normally emit
the concrete dated files is absent, so the fixture stands in for it: the
*patterns* are config-derived, the *date instances* are ours.

Runs are multi-segment (cold / restart / branchoff), each segment writing its
outdata and its restart_out files; the run span is the union across segments.
Streams named in ``fx_streams`` are written time-invariant (one file, no time
coordinate) to exercise the fx shard; the rest are time-varying.
"""

from __future__ import annotations

import hashlib
import tempfile
from dataclasses import dataclass
from datetime import datetime
from typing import Iterable, Literal, Mapping, Optional

import numpy as np
import xarray as xr
from ruamel.yaml import YAML

from esm_catalog.types import ExperimentId

from .base import Backend, Expected, FakeExperiment
from .resolver import resolve


def to_hdf5_bytes(ds: xr.Dataset) -> bytes:
    """Serialize *ds* to HDF5 netCDF bytes.

    Round-trips through a temp file with xarray's default engine (netCDF4, which
    produces HDF5): the reader's remote branch opens files with h5netcdf, which
    needs a real HDF5 container, and xarray cannot return bytes for h5netcdf
    without a path (scipy's netCDF3 in-memory fallback is not installed here).
    """
    with tempfile.NamedTemporaryFile(suffix=".nc") as tmp:
        ds.to_netcdf(tmp.name)
        tmp.seek(0)
        return tmp.read()

_RESOLVE_YEAR = 2000  # the placeholder year baked into resolved target patterns
SegmentKind = Literal["cold", "restart", "branchoff"]

# Synthetic scientific metadata for the finished_config. Real component metadata
# in the resolved config is heterogeneous (echam uses a `metadata` dict, fesom a
# `contact` string, jsbach `description`/`license_text`), which the sourcing layer
# does not uniformly parse; the fixture writes one clean block so the identity /
# license / contacts path is exercised. Structure comes from config; this does not.
_METADATA = {
    "Description": "Synthetic experiment for scan tests.",
    "Authors": ["Jane Modeller", "Karl Klima"],
    "Institute": "AWI",
    "License": "CC-BY-4.0",
}


@dataclass(frozen=True)
class Segment:
    """One run segment: the model year it covers and how it was started."""

    year: int
    kind: SegmentKind = "cold"


def _concrete_files(pattern: str, expid: str, year: int, months: int) -> list[str]:
    """Fill one config target *pattern* into concrete filenames for *year*.

    The placeholder year is always swapped for *year*. A pattern with no wildcard
    is then a single concrete file (a fixed coupler file, or an already-dated
    restart name). A wildcard pattern that embedded the year has its ``*`` filled
    with the first *months* months (monthly cadence); a year-less wildcard pattern
    is filled with the year itself (yearly cadence -> 1 file). *months* is a test
    knob: real runs are 12/yr, but a smaller value keeps the fixture tree small.
    """
    had_year = str(_RESOLVE_YEAR) in pattern
    stem = pattern.replace(str(_RESOLVE_YEAR), f"{year:04d}") if had_year else pattern
    if "*" not in stem:
        return [stem]
    tags = [f"{m:02d}" for m in range(1, months + 1)] if had_year else [f"{year:04d}"]
    return [_fill_last_star(stem, expid, tag) for tag in tags]


def _fill_last_star(stem: str, expid: str, tag: str) -> str:
    """Fill the last ``*`` (the date slot) with *tag*, any earlier ``*`` with expid."""
    star_count = stem.count("*")
    filled = stem.replace("*", expid, star_count - 1) if star_count > 1 else stem
    return filled[::-1].replace("*", tag[::-1], 1)[::-1]


def _dataset(seed: int, year: int, month: int) -> xr.Dataset:
    """A tiny seeded time+lat+lon dataset standing in for one output file.

    Carries a single monthly time step (so the reader classifies it time-varying)
    and a global ``frequency`` attribute (a lone step has no interval to measure).
    """
    rng = np.random.default_rng(seed)
    lat = np.linspace(-89.0, 89.0, 4)
    lon = np.linspace(-179.0, 179.0, 5)
    time = np.array(
        [np.datetime64(f"{year:04d}-{month:02d}-16")], dtype="datetime64[ns]"
    )
    data = rng.standard_normal((1, 4, 5)).astype("float32")
    ds = xr.Dataset(
        {"var": (("time", "lat", "lon"), data, {"units": "1"})},
        coords={"time": time, "lat": lat, "lon": lon},
    )
    ds.attrs["frequency"] = "mon"
    return ds


def _fx_dataset(seed: int) -> xr.Dataset:
    """A tiny seeded lat/lon-only dataset for a time-invariant (``fx``) file.

    The absence of a ``time`` coordinate is the signal the reader keys on to
    classify the file as ``fx`` (a mask/grid), routing it to the fx shard.
    """
    rng = np.random.default_rng(seed)
    lat = np.linspace(-89.0, 89.0, 4)
    lon = np.linspace(-179.0, 179.0, 5)
    data = rng.standard_normal((4, 5)).astype("float32")
    return xr.Dataset(
        {"mask": (("lat", "lon"), data, {"units": "1"})},
        coords={"lat": lat, "lon": lon},
    )


def build(
    *,
    backend: Backend,
    model: str = "awiesm-2.1",
    expid: ExperimentId = "FAKE",
    segments: tuple[Segment, ...] = (Segment(1850, "cold"),),
    fx_streams: Optional[Mapping[str, Iterable[str]]] = None,
    months: int = 1,
) -> FakeExperiment:
    """Build a config-anchored experiment tree on *backend*.

    Resolves *model* to its real component/target layout, then for each segment
    materializes every component's output files, writes a ``finished_config``
    carrying the real ``outdata_targets`` globs, and writes a tidy log listing the
    concrete files (the scan's authoritative file list).

    Parameters
    ----------
    fx_streams : mapping of component -> stream keys, optional
        Which ``outdata_targets`` streams are time-invariant (``fx``). The config
        does not record this (fx-ness is a property of the file content), so a test
        designates it here. An fx stream writes exactly one time-less file for the
        whole run (in the first segment), routed to the fx shard by the reader.
    """
    layout = resolve(model, expid)
    fx_designation = {c: set(streams) for c, streams in (fx_streams or {}).items()}
    root = backend.base / expid
    yaml = YAML(typ="safe")

    datasets: dict[str, xr.Dataset] = {}
    md5_by_path: dict[str, str] = {}
    ts_paths: list[str] = []
    ts_by_component: dict[str, list[str]] = {c: [] for c in layout.components}
    fx_paths: list[str] = []
    fx_by_component: dict[str, list[str]] = {c: [] for c in layout.components}
    restart_paths: list[str] = []

    for seg_index, segment in enumerate(segments):
        tidy: dict[str, dict] = {}
        for component in layout.components:
            outdata_dir = root / "outdata" / component
            outdata_entries: dict[str, dict] = {}
            fx_keys = fx_designation.get(component, set())
            for stream_key, pattern in layout.outdata_targets[component].items():
                if stream_key in fx_keys:
                    # Time-invariant: one file for the whole run, written once.
                    if seg_index != 0:
                        continue
                    filename = _concrete_files(pattern, expid, segment.year, months)[0]
                    abspath = outdata_dir / filename
                    abspath.parent.mkdir(parents=True, exist_ok=True)
                    ds = _fx_dataset(_seed(component, filename))
                    data = to_hdf5_bytes(ds)
                    abspath.write_bytes(data)
                    key = str(abspath)
                    md5 = hashlib.md5(data).hexdigest()
                    datasets[key] = ds
                    md5_by_path[key] = md5
                    fx_paths.append(key)
                    fx_by_component[component].append(key)
                    outdata_entries[filename] = {"destination": key, "checksum": md5}
                    continue
                for month, filename in _numbered(
                    _concrete_files(pattern, expid, segment.year, months)
                ):
                    abspath = outdata_dir / filename
                    abspath.parent.mkdir(parents=True, exist_ok=True)
                    ds = _dataset(_seed(component, filename), segment.year, month)
                    data = to_hdf5_bytes(ds)
                    abspath.write_bytes(data)
                    key = str(abspath)
                    md5 = hashlib.md5(data).hexdigest()
                    outdata_entries[filename] = {"destination": key, "checksum": md5}
                    if filename.endswith((".codes", ".idx")):
                        # GRIB sidecars: kept in the tidy manifest so the scan's
                        # sidecar exclusion is exercised, but never catalogued --
                        # so they are not part of the expected item count.
                        continue
                    datasets[key] = ds
                    md5_by_path[key] = md5
                    ts_paths.append(key)
                    ts_by_component[component].append(key)

            # restart_out: files the segment produces to seed the next run. They
            # are output, but the scan MUST exclude them -- they live in the tidy
            # ``restart_out`` category, never ``outdata``. A cold start still writes
            # them (it seeds the first restart); the kind is recorded for the oracle.
            restart_dir = root / "restart" / component
            restart_entries: dict[str, dict] = {}
            for pattern in layout.restart_out_targets.get(component, {}).values():
                for _, filename in _numbered(
                    _concrete_files(pattern, expid, segment.year, months)
                ):
                    abspath = restart_dir / filename
                    abspath.parent.mkdir(parents=True, exist_ok=True)
                    data = to_hdf5_bytes(
                        _dataset(_seed(component, filename), segment.year, 12)
                    )
                    abspath.write_bytes(data)
                    key = str(abspath)
                    restart_paths.append(key)
                    restart_entries[filename] = {
                        "destination": key,
                        "checksum": hashlib.md5(data).hexdigest(),
                    }

            tidy[component] = {
                "files": {"outdata": outdata_entries, "restart_out": restart_entries}
            }
        _write_finished_config(root, expid, segment, layout, yaml)
        _write_tidy_log(root, expid, segment, tidy, yaml)

    # Dedup by path, matching output_files: a fixed-name file (e.g. an oasis
    # coupler file with no date) is rewritten to the same path every segment, so
    # it is one catalogued file, not one per segment.
    years = [s.year for s in segments]
    expected = Expected(
        experiment_id=expid,
        run_start=datetime(min(years), 1, 1),
        run_end=datetime(max(years), 12, 31),
        components=tuple(layout.components),
        ts_paths=tuple(dict.fromkeys(ts_paths)),
        fx_paths=tuple(dict.fromkeys(fx_paths)),
        md5_by_path=dict(md5_by_path),
        ts_paths_by_component={
            k: tuple(dict.fromkeys(v)) for k, v in ts_by_component.items()
        },
        fx_paths_by_component={
            k: tuple(dict.fromkeys(v)) for k, v in fx_by_component.items()
        },
        restart_paths=tuple(dict.fromkeys(restart_paths)),
    )
    return FakeExperiment(
        root=root, datasets=datasets, expected=expected, backend=backend
    )


def _numbered(files: list[str]) -> list[tuple[int, str]]:
    """Pair each concrete file with a 1-based month index (for its time step)."""
    return [(i + 1, name) for i, name in enumerate(files)]


def _seed(component: str, filename: str) -> int:
    """A stable per-file RNG seed, so payloads are reproducible across runs."""
    return int(hashlib.md5(f"{component}/{filename}".encode()).hexdigest()[:8], 16)


def _write_finished_config(root, expid, segment, layout, yaml) -> None:
    """Write the segment's finished_config with the REAL outdata_targets globs."""
    doc: dict = {
        "general": {
            "expid": expid,
            "start_date": f"{segment.year:04d}-01-01",
            "end_date": f"{segment.year:04d}-12-31",
            "metadata": dict(_METADATA),
        }
    }
    for component in layout.components:
        doc[component] = {"outdata_targets": dict(layout.outdata_targets[component])}
    config_dir = root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    name = f"{expid}_finished_config.yaml_{segment.year:04d}0101-{segment.year:04d}1231"
    with (config_dir / name).open("w") as stream:
        yaml.dump(doc, stream)


def _write_tidy_log(root, expid, segment, tidy, yaml) -> None:
    """Write the segment's tidy log (the scan's authoritative concrete file list)."""
    log_dir = root / "log"
    log_dir.mkdir(parents=True, exist_ok=True)
    name = f"{expid}_file_operations_tidy_{segment.year:04d}0101-{segment.year:04d}1231.yaml"
    with (log_dir / name).open("w") as stream:
        yaml.dump(tidy, stream)
