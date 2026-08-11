"""End-to-end scan integration test (esm_catalog.scan.ingest).

Builds a minimal synthetic ESM-Tools experiment — a finished_config plus a
time-varying and a time-invariant NetCDF file — runs the real scan_experiment
(process-pool backend), and asserts the shards + collection are produced.
"""

from __future__ import annotations

import numpy as np
import xarray as xr
from ruamel.yaml import YAML
from upath import UPath

from esm_catalog.scan.ingest import scan_experiment
from esm_catalog.scan.workspace import catalog_dir
from esm_catalog.storage.geoparquet import fx_shard_name, read_shard

_EXPID = "exp-alpha"
_COORDS = {"lat": [-45.0, 0.0, 45.0], "lon": [0.0, 90.0, 180.0, 270.0]}


def _write_time_varying(path: UPath) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((2, 3, 4)))},
        coords={
            "time": [np.datetime64("2000-01-15"), np.datetime64("2000-02-15")],
            **_COORDS,
        },
    ).to_netcdf(str(path))


def _write_time_invariant(path: UPath) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    xr.Dataset({"sftlf": (("lat", "lon"), np.zeros((3, 4)))}, coords=_COORDS).to_netcdf(
        str(path)
    )


def _build_experiment(tmp_path) -> UPath:
    exp_root = UPath(tmp_path)
    ts_file = exp_root / "outdata" / "echam" / f"{_EXPID}_echam_200001.nc"
    fx_file = exp_root / "outdata" / "echam" / f"{_EXPID}_echam_fx.nc"
    _write_time_varying(ts_file)
    _write_time_invariant(fx_file)

    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {
        "general": {
            "expid": _EXPID,
            "start_date": "2000-01-01",
            "end_date": "2000-12-31",
        },
        "echam": {
            "outdata_targets": {"ts": str(ts_file), "fx": str(fx_file)},
            "metadata": {
                "Description": "synthetic",
                "Authors": ["Ada"],
                "License": "CC-BY-4.0",
            },
        },
    }
    with (config_dir / f"{_EXPID}_finished_config.yaml").open("w") as stream:
        YAML(typ="safe").dump(doc, stream)
    return exp_root


def test_scan_writes_shards_and_collection(tmp_path):
    exp_root = _build_experiment(tmp_path)

    report = scan_experiment(exp_root)

    assert report.scanned == 2
    assert report.items == 2
    assert report.failures == ()

    cdir = catalog_dir(exp_root)
    assert (cdir / "collection.json").exists()
    # the time-invariant field lands in the mutable fx shard...
    assert read_shard(cdir / "items" / fx_shard_name(_EXPID)).num_rows == 1
    # ...and the time-varying field in a single ts shard (the non-fx stac shard).
    ts_shards = [
        s
        for s in (cdir / "items").glob(f"{_EXPID}_stac_*.parquet")
        if s.name != fx_shard_name(_EXPID)
    ]
    assert len(ts_shards) == 1
    assert read_shard(ts_shards[0]).num_rows == 1


def test_scan_emits_progress_events(tmp_path):
    exp_root = _build_experiment(tmp_path)
    events = []

    report = scan_experiment(exp_root, on_progress=events.append)

    phases = [e.phase for e in events]
    ordered = [p for i, p in enumerate(phases) if i == 0 or phases[i - 1] != p]
    assert ordered == ["sourcing", "reading", "writing"]
    reading = [e for e in events if e.phase == "reading"]
    # one tick per file, reaching the total, and the last tick names its file
    assert reading[-1].current == reading[-1].total == report.scanned
    assert reading[-1].detail


def test_unsupported_format_is_skipped_not_failed(tmp_path):
    exp_root = _build_experiment(tmp_path)
    # A GRIB file: format is detected (magic bytes), but no reader is registered,
    # so it is unsupported -- a clean skip, never a read failure.
    grib = exp_root / "outdata" / "echam" / f"{_EXPID}_echam_grib_200001.grb"
    grib.write_bytes(b"GRIB\x00\x00\x00\x00padding")

    report = scan_experiment(exp_root)

    assert report.unsupported == 1
    assert report.failures == ()
    # The two NetCDF files are still catalogued; the GRIB file is not.
    assert report.items == 2


def test_scan_is_incremental(tmp_path):
    exp_root = _build_experiment(tmp_path)

    scan_experiment(exp_root)
    report = scan_experiment(exp_root)

    # the time-varying file is recorded and skipped; the fx file is always re-read.
    assert report.skipped >= 1
    assert report.scanned <= 1
