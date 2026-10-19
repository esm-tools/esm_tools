"""Opaque binary restart dumps (esm_catalog.scan.model_plugins.readers.opaque).

LPJ-GUESS's per-task ``.state`` restart dump and FESOM's ``iceberg.restart``
have no public, parseable structure -- confirmed live against a real
AWI-ESM3 run: the former is a 43MB headerless binary blob, the latter is 0
bytes whenever the iceberg module produced nothing for the segment. Both
should still be catalogued (path/checksum/size), just without content
metadata.
"""

from __future__ import annotations

from upath import UPath

from esm_catalog.scan.format import FileFormat, UnknownFormatError, detect
from esm_catalog.scan.model_plugins.readers.opaque import (
    OpaqueBinaryReader,
    claim_by_suffix,
)
from esm_catalog.scan.reader import reader_for


def test_claims_lpjguess_state_only_for_its_component():
    assert claim_by_suffix(".state", "lpj_guess") == FileFormat.opaque_binary
    assert claim_by_suffix(".state", "fesom") is None
    assert claim_by_suffix(".state", None) is None


def test_claims_fesom_restart_and_ism_only_for_its_component():
    assert claim_by_suffix(".restart", "fesom") == FileFormat.opaque_binary
    assert claim_by_suffix(".ism", "fesom") == FileFormat.opaque_binary
    assert claim_by_suffix(".restart", "lpj_guess") is None
    assert claim_by_suffix(".ism", None) is None


def test_reader_for_resolves_to_opaque_binary_reader():
    assert isinstance(reader_for(FileFormat.opaque_binary), OpaqueBinaryReader)


def test_empty_fesom_iceberg_restart_is_catalogued_not_unsupported(tmp_path):
    path = UPath(tmp_path / "iceberg.restart")
    path.write_bytes(b"")

    assert detect(path, "fesom") == FileFormat.opaque_binary
    meta = OpaqueBinaryReader().read(path)
    assert meta.format == str(FileFormat.opaque_binary)


def test_fesom_iceberg_ism_variant_is_catalogued(tmp_path):
    path = UPath(tmp_path / "iceberg.restart.ISM")
    path.write_bytes(b"")

    assert detect(path, "fesom") == FileFormat.opaque_binary


def test_lpjguess_state_dump_is_catalogued_without_being_opened(tmp_path):
    path = UPath(tmp_path / "0.state")
    path.write_bytes(b"\x00\x00\x00\xa0RUI\xc0")

    assert detect(path, "lpj_guess") == FileFormat.opaque_binary
    meta = OpaqueBinaryReader().read(path)
    assert meta.format == str(FileFormat.opaque_binary)
    assert meta.variable is None
    assert meta.variables == []


def test_detect_rejects_without_the_right_component(tmp_path):
    path = UPath(tmp_path / "iceberg.restart")
    path.write_bytes(b"")

    for component in (None, "lpj_guess"):
        try:
            detect(path, component)
        except UnknownFormatError:
            continue
        raise AssertionError(f"expected UnknownFormatError for component={component!r}")
