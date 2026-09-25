"""The scan-read timeout result: what a hung file is recorded as.

The actual timeout enforcement (killing and replacing the worker pool) is
generic and tested in test_scan_parallel.py; this only pins the shape of the
result :func:`~esm_catalog.scan.ingest._on_read_timeout` hands back.
"""

from __future__ import annotations

from upath import UPath

from esm_catalog.scan import ingest
from esm_catalog.scan.types import OutputFile


def test_on_read_timeout_records_a_failure_not_a_crash(tmp_path):
    path = tmp_path / "slow.grib"
    path.write_bytes(b"")
    output_file = OutputFile(path=UPath(path), component="echam", stream="echam_nc")

    result = ingest._on_read_timeout(output_file)

    assert result.output_file == output_file
    assert result.file_metadata is None
    assert result.unsupported is False
    assert result.failure is not None
    assert "timed out" in result.failure.error
    assert str(ingest._READ_TIMEOUT_SECONDS) in result.failure.error
