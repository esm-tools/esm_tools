"""A pathologically slow file read must fail, not stall the whole scan."""

from __future__ import annotations

import time

from upath import UPath

from esm_catalog.scan import ingest
from esm_catalog.scan.types import OutputFile


def _make_output_file(tmp_path) -> OutputFile:
    path = tmp_path / "slow.grib"
    path.write_bytes(b"")
    return OutputFile(path=UPath(path), component="echam", stream="echam_nc")


def test_read_output_file_times_out_on_a_hung_reader(tmp_path, monkeypatch):
    monkeypatch.setattr(ingest, "_READ_TIMEOUT_SECONDS", 1)
    monkeypatch.setattr(ingest, "detect", lambda path: "grib")

    class _HungReader:
        def read(self, path):
            time.sleep(5)
            raise AssertionError("should have been interrupted by the timeout")

    monkeypatch.setattr(ingest, "reader_for", lambda file_format: _HungReader())

    output_file = _make_output_file(tmp_path)
    start = time.monotonic()
    result = ingest._read_output_file(output_file)
    elapsed = time.monotonic() - start

    assert elapsed < 3
    assert result.file_metadata is None
    assert result.unsupported is False
    assert result.failure is not None
    assert "timed out" in result.failure.error


def test_read_output_file_completes_normally_within_timeout(tmp_path, monkeypatch):
    monkeypatch.setattr(ingest, "_READ_TIMEOUT_SECONDS", 5)
    monkeypatch.setattr(ingest, "detect", lambda path: "grib")

    class _FastReader:
        def read(self, path):
            return {
                "variable": "tas",
                "variables": [{"name": "tas", "units": "K", "dimensions": []}],
                "format": "grib",
            }

    monkeypatch.setattr(ingest, "reader_for", lambda file_format: _FastReader())

    output_file = _make_output_file(tmp_path)
    result = ingest._read_output_file(output_file)

    assert result.failure is None
    assert result.unsupported is False
    assert result.file_metadata is not None
    assert result.file_metadata.variable == "tas"
