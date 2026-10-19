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

from esm_catalog.scan.format import needs_sniff
from esm_catalog.scan.ingest import (
    UNSUPPORTED_FILENAME,
    _fingerprint,
    _local_read_path,
    _read_output_file,
    scan_experiment,
)
from esm_catalog.scan.sourcing import source_files
from esm_catalog.scan.types import OutputFile
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


def test_reading_progress_names_the_in_flight_file_not_the_completed_one(tmp_path):
    exp_root = _build_experiment(tmp_path)
    # The process-pool backend collects results strictly in input order (see
    # ingest._tick), so the first "reading" tick always fires once todo[0]
    # returns -- at that point todo[1] is the file actually still in flight.
    todo = source_files(exp_root)
    assert len(todo) == 2
    events = []

    scan_experiment(exp_root, on_progress=events.append)

    first_tick = next(e for e in events if e.phase == "reading" and e.current == 1)
    assert first_tick.detail == todo[1].path.name


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

    # Not just a bare count -- the unsupported sidecar names which file.
    import json

    sidecar = catalog_dir(exp_root) / UNSUPPORTED_FILENAME
    assert json.loads(sidecar.read_text()) == [str(grib)]


def test_unsupported_sidecar_cleared_once_the_file_is_gone(tmp_path):
    exp_root = _build_experiment(tmp_path)
    grib = exp_root / "outdata" / "echam" / f"{_EXPID}_echam_grib_200001.grb"
    grib.write_bytes(b"GRIB\x00\x00\x00\x00padding")
    scan_experiment(exp_root)
    sidecar = catalog_dir(exp_root) / UNSUPPORTED_FILENAME
    assert sidecar.exists()

    grib.unlink()
    scan_experiment(exp_root)

    assert not sidecar.exists()


def test_scan_is_incremental(tmp_path):
    exp_root = _build_experiment(tmp_path)

    scan_experiment(exp_root)
    report = scan_experiment(exp_root)

    # the time-varying file is recorded and skipped; the fx file is always re-read.
    assert report.skipped >= 1
    assert report.scanned <= 1


# --- _local_read_path: fetch a remote file locally only when the reader needs it ---


class _FakeReader:
    def __init__(self, supports_remote: bool) -> None:
        self.supports_remote = supports_remote


def test_local_read_path_noop_for_local_path(tmp_path):
    f = tmp_path / "x.grb"
    f.write_bytes(b"GRIB")
    with _local_read_path(UPath(f), _FakeReader(False)) as out:
        assert str(out) == str(UPath(f))


def test_local_read_path_noop_when_reader_supports_remote():
    p = UPath("memory://local-read-path/x.nc")
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(b"netcdf-bytes")
    with _local_read_path(p, _FakeReader(True)) as out:
        assert out is p


def test_local_read_path_fetches_remote_file_when_reader_cannot_read_remote():
    p = UPath("memory://local-read-path/x.grb")
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(b"GRIB-bytes")
    with _local_read_path(p, _FakeReader(False)) as out:
        assert out.protocol in ("", "file")
        assert str(out).endswith(".grb")
        assert out.read_bytes() == b"GRIB-bytes"


# --- _fingerprint: reuse a caller-supplied stat() instead of a second one ---


def test_fingerprint_prefers_md5_over_any_stat():
    output_file = OutputFile(
        path=UPath("memory://fingerprint/never-touched.nc"),
        component="echam",
        stream="echam_nc",
        md5="deadbeef",
    )
    assert _fingerprint(output_file) == "deadbeef"


def test_fingerprint_reuses_supplied_stat_without_a_second_stat_call(tmp_path):
    f = tmp_path / "x.nc"
    f.write_bytes(b"hello")
    path = UPath(f)
    real_stat = path.stat()

    class _ExplodingPath:
        def stat(self):
            raise AssertionError("must not stat() again when a stat is supplied")

    output_file = OutputFile(path=_ExplodingPath(), component="echam", stream="echam_nc")
    fp = _fingerprint(output_file, stat=real_stat)
    assert fp == f"{real_stat.st_size}-{getattr(real_stat, 'st_mtime_ns', real_stat.st_mtime)}"


def test_fingerprint_stats_when_none_supplied(tmp_path):
    f = tmp_path / "x.nc"
    f.write_bytes(b"hello")
    output_file = OutputFile(path=UPath(f), component="echam", stream="echam_nc")
    assert _fingerprint(output_file) == _fingerprint(output_file, stat=UPath(f).stat())


# --- extension-less remote files: sniff and read must share one fetch ---


def test_extensionless_remote_file_sniffed_and_read_with_one_fetch(tmp_path, monkeypatch):
    # ECHAM/FESOM native output has no file extension -- detect() must sniff
    # it before a reader is even known. A remote file like this must share
    # one fetch between that sniff and the real read, not pay a separate
    # remote open for each.
    local = tmp_path / "src.nc"
    _write_time_varying(local)

    dest = UPath("memory://extensionless-fetch/exp-alpha_echam_200001")
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(local.read_bytes())
    assert needs_sniff(dest)

    calls = []
    path_cls = type(dest)
    real_read_bytes = path_cls.read_bytes

    def counting_read_bytes(self):
        if str(self) == str(dest):
            calls.append(str(self))
        return real_read_bytes(self)

    monkeypatch.setattr(path_cls, "read_bytes", counting_read_bytes)

    output_file = OutputFile(path=dest, component="echam", stream="echam_ts")
    result = _read_output_file(output_file)

    assert result.failure is None
    assert not result.unsupported
    assert result.file_metadata is not None
    assert len(calls) == 1
