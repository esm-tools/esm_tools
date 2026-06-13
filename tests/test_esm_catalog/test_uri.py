"""Unit tests for the neutral URI helpers (no scan/ dependency)."""

from __future__ import annotations

from pathlib import Path

from esm_catalog.uri import _has_protocol, to_uri


def test_has_protocol_detects_remote_and_local():
    assert _has_protocol("ssh://host/path") is True
    assert _has_protocol("s3://bucket/key") is True
    assert _has_protocol("/local/abs/path") is False


def test_to_uri_local_path_is_file_uri(tmp_path):
    f = tmp_path / "data.nc"
    f.write_bytes(b"x")
    assert to_uri(f) == f"file://{Path(f).resolve()}"
