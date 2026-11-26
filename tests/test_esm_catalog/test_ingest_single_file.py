"""ingest_single_file: the shared engine behind the ``add``/``validate`` CLI
commands (esm_catalog.scan.ingest).

Reuses the exact same primitives (_try_path_facets, _read_output_file,
_synthesize_from_template) scan_experiment's batch loop is built from -- one
implementation of "ingest a file", not a separate one for these commands.
Confirmed via real ECHAM-convention filenames and genuinely unreadable
("garbage") file content, same style as test_scan_schema_freeze.py, so a
wrongly-triggered real read surfaces as a failure rather than silently
passing.
"""

from __future__ import annotations

from datetime import datetime

from upath import UPath

from esm_catalog.scan.ingest import ingest_single_file
from esm_catalog.scan.types import OutputFile
from esm_catalog.scan.workspace import WorkspaceState
from esm_catalog.types import FileMetadata


def _echam_output_file(yyyymm: str) -> OutputFile:
    path = UPath(f"/x/historical_c14_init_{yyyymm}.01_echam")
    return OutputFile(path=path, component="echam", stream="echam")


def _state_with_cached_echam_schema() -> WorkspaceState:
    state = WorkspaceState(experiment_id="historical_c14_init")
    state.schema_by_stream["echam|echam"] = FileMetadata(
        variable="temp",
        variables=[{"name": "temp"}],
        component="echam",
        stream="echam",
        frequency="mon",
    )
    return state


def test_force_read_always_reads_even_when_schema_is_cached(tmp_path):
    output_file = OutputFile(
        path=UPath(tmp_path / "historical_c14_init_200001.01_echam"),
        component="echam",
        stream="echam",
    )
    output_file.path.write_bytes(b"not a real grib or netcdf file")
    state = _state_with_cached_echam_schema()

    result = ingest_single_file(output_file, state, force_read=True)

    # Garbage content forces a real read to actually fail -- proving
    # force_read bypassed the cached shortcut rather than silently reusing it.
    assert result.failure is not None or result.unsupported


def test_add_shortcuts_via_cached_schema_without_opening_the_file(tmp_path):
    output_file = OutputFile(
        path=UPath(tmp_path / "historical_c14_init_200002.01_echam"),
        component="echam",
        stream="echam",
    )
    output_file.path.write_bytes(b"not a real grib or netcdf file")  # never opened
    state = _state_with_cached_echam_schema()

    result = ingest_single_file(output_file, state, force_read=False)

    assert result.failure is None
    assert result.unsupported is False
    assert result.file_metadata.variable == "temp"
    assert result.file_metadata.datetime_start == datetime(2000, 2, 1)


def test_add_falls_back_to_a_real_read_when_nothing_is_cached_yet(tmp_path):
    import numpy as np
    import xarray as xr

    output_file = OutputFile(
        path=UPath(tmp_path / "historical_c14_init_200001.01_echam"),
        component="echam",
        stream="echam",
    )
    xr.Dataset(
        {"tas": (("time",), np.zeros(1))},
        coords={"time": [np.datetime64("2000-01-15")]},
    ).to_netcdf(str(output_file.path))
    state = WorkspaceState(experiment_id="historical_c14_init")  # nothing cached

    result = ingest_single_file(output_file, state, force_read=False)

    assert result.failure is None
    assert result.file_metadata.variable == "tas"
    # A real read on a genuine first asset establishes the cache for next time.
    assert "echam|echam" in state.schema_by_stream


def test_add_falls_back_to_real_read_when_facet_resolves_a_different_stream(tmp_path):
    # The filename's own trailing "_co2" disagrees with the caller-given
    # stream "echam" -- not confident enough to trust the shortcut's date.
    output_file = OutputFile(
        path=UPath(tmp_path / "historical_c14_init_200001.01_co2"),
        component="echam",
        stream="echam",
    )
    output_file.path.write_bytes(b"not a real grib or netcdf file")
    state = _state_with_cached_echam_schema()

    result = ingest_single_file(output_file, state, force_read=False)

    assert result.failure is not None or result.unsupported


def test_never_touches_occurrences_since_check(tmp_path):
    output_file = OutputFile(
        path=UPath(tmp_path / "historical_c14_init_200002.01_echam"),
        component="echam",
        stream="echam",
    )
    output_file.path.write_bytes(b"not a real grib or netcdf file")
    state = _state_with_cached_echam_schema()
    state.occurrences_since_check["echam|echam"] = 7

    ingest_single_file(output_file, state, force_read=False)
    ingest_single_file(output_file, state, force_read=True)

    assert state.occurrences_since_check["echam|echam"] == 7
