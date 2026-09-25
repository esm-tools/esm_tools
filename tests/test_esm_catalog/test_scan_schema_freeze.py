"""Schema-freeze-per-stream + path-facet shortcut (esm_catalog.scan.ingest).

Confirmed live: an ECHAM stream's schema (variables/dims/geometry/format) is
the same across every monthly file; re-deriving it file by file, for tens of
thousands of files, is the actual bulk of scan cost. ``_triage`` decides,
per file, whether a real read is needed or a path-facet shortcut applies --
tested directly (pure, no I/O, so it is unaffected by the process pool's
start method). The end-to-end test below proves the wiring in
``scan_experiment`` actually uses the shortcut's datetime, not the real
file's content, for files it skips reading.
"""

from __future__ import annotations

import json
from datetime import datetime

import numpy as np
import xarray as xr
from ruamel.yaml import YAML
from upath import UPath

import esm_catalog.scan.ingest as ingest
from esm_catalog.scan.ingest import _ReadResult, _triage, _warn_on_schema_drift
from esm_catalog.scan.ingest import scan_experiment
from esm_catalog.scan.types import OutputFile
from esm_catalog.types import FileMetadata

_EXPID = "historical_c14_init"


def _echam_file(yyyymm: str, day: str = "01") -> OutputFile:
    path = UPath(f"/x/{_EXPID}_{yyyymm}.{day}_echam")
    return OutputFile(path=path, component="echam", stream="echam")


def test_triage_first_file_always_must_read_rest_shortcut_via_path_facets():
    files = [_echam_file(m) for m in ("200001", "200002", "200003")]

    triage = _triage(files, revalidate_every=0)

    assert triage.must_read == [files[0]]
    assert [f for f, _ in triage.facet_candidates] == files[1:]
    assert [dt for _, dt in triage.facet_candidates] == [
        datetime(2000, 2, 1),
        datetime(2000, 3, 1),
    ]


def test_triage_revalidate_every_forces_periodic_real_reads():
    files = [_echam_file(f"2000{m:02d}") for m in range(1, 7)]

    triage = _triage(files, revalidate_every=2)

    # occurrence 1 (first, always) + occurrences 2, 4, 6 (checkpoints)
    assert triage.must_read == [files[0], files[1], files[3], files[5]]
    assert [f for f, _ in triage.facet_candidates] == [files[2], files[4]]
    assert triage.revalidation_paths == {
        str(files[1].path),
        str(files[3].path),
        str(files[5].path),
    }


def test_triage_revalidate_every_zero_never_revalidates():
    files = [_echam_file(f"2000{m:02d}") for m in range(1, 11)]

    triage = _triage(files, revalidate_every=0)

    assert triage.must_read == [files[0]]
    assert triage.revalidation_paths == set()


def test_triage_falls_back_to_must_read_when_no_extractor_claims_the_path():
    # No path-facet provider is registered for "fesom" -- every file needs a
    # real read, including ones after the stream's first.
    files = [
        OutputFile(path=UPath(f"/x/foo_{i}.nc"), component="fesom", stream="oce")
        for i in range(3)
    ]

    triage = _triage(files, revalidate_every=0)

    assert triage.must_read == files
    assert triage.facet_candidates == []


def test_warn_on_schema_drift_logs_when_variables_differ(monkeypatch):
    warnings = []
    monkeypatch.setattr(
        ingest.logger, "warning", lambda *a, **k: warnings.append((a, k))
    )
    frozen = {
        ("echam", "echam"): FileMetadata(
            variable="temp",
            variables=[{"name": "temp"}],
            component="echam",
            stream="echam",
        )
    }
    fresh = FileMetadata(
        variable="salt", variables=[{"name": "salt"}], component="echam", stream="echam"
    )
    result = _ReadResult(_echam_file("200002"), fresh, None)

    _warn_on_schema_drift(result, frozen)

    assert len(warnings) == 1


def test_warn_on_schema_drift_silent_when_variables_match(monkeypatch):
    warnings = []
    monkeypatch.setattr(
        ingest.logger, "warning", lambda *a, **k: warnings.append((a, k))
    )
    frozen = {
        ("echam", "echam"): FileMetadata(
            variable="temp",
            variables=[{"name": "temp"}],
            component="echam",
            stream="echam",
        )
    }
    fresh = FileMetadata(
        variable="temp", variables=[{"name": "temp"}], component="echam", stream="echam"
    )
    result = _ReadResult(_echam_file("200002"), fresh, None)

    _warn_on_schema_drift(result, frozen)

    assert warnings == []


# --- End-to-end: scan_experiment actually uses the shortcut, not real content ---

_MONTHS = ["200001", "200002", "200003", "200004", "200005", "200006"]
_COORDS = {"lat": [-45.0, 0.0, 45.0], "lon": [0.0, 90.0, 180.0, 270.0]}


def _write_run_segment(exp_root: UPath, yyyymm: str) -> None:
    """One run segment's finished_config + its (always-January-content)
    ECHAM file, named per the real on-disk convention. Every file's actual
    NetCDF time coordinate is January -- if the scan quietly fell back to
    reading content for a "shortcut" file, its catalogued date would wrongly
    also read January instead of its own month."""
    year, month = int(yyyymm[:4]), int(yyyymm[4:])
    end_day = 28 if month == 2 else 30

    echam_file = exp_root / "outdata" / "echam" / f"{_EXPID}_{yyyymm}.01_echam"
    echam_file.parent.mkdir(parents=True, exist_ok=True)
    xr.Dataset(
        {"temp": (("time", "lat", "lon"), np.zeros((1, 3, 4)))},
        coords={"time": [np.datetime64("2000-01-15")], **_COORDS},
    ).to_netcdf(str(echam_file))

    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {
        "general": {
            "expid": _EXPID,
            "start_date": f"{year}-{month:02d}-01",
            "end_date": f"{year}-{month:02d}-{end_day}",
        },
        "echam": {"outdata_targets": {"echam": str(echam_file)}},
    }
    stamp = f"{year}{month:02d}01-{year}{month:02d}{end_day}"
    with (config_dir / f"{_EXPID}_finished_config.yaml_{stamp}").open("w") as stream:
        YAML(typ="safe").dump(doc, stream)


def test_scan_uses_path_facet_datetime_not_always_january_content(tmp_path):
    exp_root = UPath(tmp_path)
    for yyyymm in _MONTHS:
        _write_run_segment(exp_root, yyyymm)

    report = scan_experiment(exp_root, revalidate_every=0)

    # Scan shards one row per file; merging into one growing Item happens at
    # push time, not scan time -- all six months' files are catalogued.
    assert report.items == len(_MONTHS)
    assert report.failures == ()

    collection_doc = json.loads((exp_root / "catalog" / "collection.json").read_text())
    start, end = collection_doc["extent"]["temporal"]["interval"][0]
    # Real content is always January; the extent reaching June proves the
    # later files' dates came from their path, not a re-read of content.
    assert start.startswith("2000-01")
    assert end.startswith("2000-06")
