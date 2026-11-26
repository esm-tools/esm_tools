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
    assert claim_by_suffix(".bin", "lpj_guess") == FileFormat.opaque_binary
    assert claim_by_suffix(".state", "fesom") is None
    assert claim_by_suffix(".state", None) is None


def test_claims_fesom_restart_and_ism_only_for_its_component():
    assert claim_by_suffix(".restart", "fesom") == FileFormat.opaque_binary
    assert claim_by_suffix(".ism", "fesom") == FileFormat.opaque_binary
    assert claim_by_suffix(".restart", "lpj_guess") is None
    assert claim_by_suffix(".ism", None) is None


def test_reader_for_resolves_to_opaque_binary_reader():
    assert isinstance(reader_for(FileFormat.opaque_binary), OpaqueBinaryReader)


def test_claims_oifs_per_rank_restart_chunks_by_suffix_shape_not_literal_value():
    """OIFS's own restart dump has no fixed suffix -- confirmed live across
    578 files in one real segment, suffix varies per MPI rank
    (``.100_192``, ``.0001``, ...) or is absent entirely (``rcf``,
    ``waminfo<stamp>``). Matched by shape (pure digits/underscore, or
    empty), never colliding with a real OIFS ``.nc`` output.
    """
    for suffix in (".100_192", ".10_192", ".0001", ".0192", ""):
        assert claim_by_suffix(suffix, "oifs") == FileFormat.opaque_binary
    assert claim_by_suffix(".nc", "oifs") is None
    assert claim_by_suffix(".100_192", "fesom") is None
    assert claim_by_suffix(".100_192", None) is None


def test_empty_fesom_iceberg_restart_is_catalogued_not_unsupported(tmp_path):
    path = UPath(tmp_path / "iceberg.restart")
    path.write_bytes(b"")

    assert detect(path, "fesom") == FileFormat.opaque_binary
    meta = OpaqueBinaryReader().read(path)
    assert meta.format == str(FileFormat.opaque_binary)
    assert meta.geometry is not None
    assert meta.bbox is not None


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


def test_opaque_restart_becomes_a_valid_fx_item_not_a_null_geometry(tmp_path):
    """A no-datetime opaque restart is placed as an fx item (item.py's
    ``is_fx`` path) -- ``geometry=None`` is spec-legal on a pystac Item but
    the geoparquet writer's WKB conversion crashes on it (confirmed live:
    AttributeError: 'NoneType' object has no attribute 'get'), so the fx
    path specifically must never see a None geometry from this reader.
    """
    from datetime import datetime, timezone

    from esm_catalog.item import make_item

    from .helpers import make_exp_metadata

    path = UPath(tmp_path / "0.state")
    path.write_bytes(b"\x00\x00\x00\xa0RUI\xc0")
    meta = OpaqueBinaryReader().read(path)
    meta.component = "lpj_guess"
    meta.role = "restart"

    exp_metadata = make_exp_metadata(
        run_start=datetime(1850, 1, 1, tzinfo=timezone.utc),
        run_end=datetime(1851, 12, 31, tzinfo=timezone.utc),
    )
    item = make_item(path, meta, exp_metadata)

    assert item.properties.get("frequency") == "fx"
    assert item.geometry is not None


def test_oifs_restart_chunk_is_catalogued_end_to_end(tmp_path):
    for name in (
        "BLS18500101000000_000364230000.100_192",
        "srf000003650000.0001",
        "rcf",
        "waminfo00000365",
    ):
        path = UPath(tmp_path / name)
        path.write_bytes(b"\x00\x08\x1f\x00")
        assert detect(path, "oifs") == FileFormat.opaque_binary


def test_real_oifs_netcdf_output_is_not_shadowed_by_the_rank_claim(tmp_path):
    """A genuine OIFS .nc output file must still resolve to netcdf -- the
    opaque claimer must never swallow it just because it also matches on
    component='oifs'.
    """
    path = UPath(tmp_path / "atm_remapped_1m_pl_t_1m_pl_1850-1850.nc")
    path.write_bytes(b"CDF\x01" + b"\x00" * 20)

    assert detect(path, "oifs") == FileFormat.netcdf


def test_detect_rejects_without_the_right_component(tmp_path):
    path = UPath(tmp_path / "iceberg.restart")
    path.write_bytes(b"")

    for component in (None, "lpj_guess"):
        try:
            detect(path, component)
        except UnknownFormatError:
            continue
        raise AssertionError(f"expected UnknownFormatError for component={component!r}")
