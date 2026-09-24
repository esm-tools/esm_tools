"""Scan tests over the config-anchored fake_experiment fixture.

The fixture resolves a real ESM-Tools runscript (awiesm-2.1) to its true
component set and output-file patterns, then materializes concrete files. These
tests assert the scan recovers that real structure -- the four real components
(including jsbach, which the earlier hand-declared fixture missed), real echam
stream filenames, restart exclusion, fx routing, and the multi-segment run span
-- rather than hand-declared stand-ins.
"""

from __future__ import annotations

from datetime import datetime

from upath import UPath

from esm_catalog.scan.ingest import scan_experiment
from esm_catalog.scan.sourcing import output_files, source_experiment
from esm_catalog.scan.workspace import catalog_dir
from esm_catalog.storage.geoparquet import item_ids, read_shard

# The single fx shard is named ``<expid>_stac_fx.parquet``; glob for it rather
# than reconstruct the experiment's id.
_FX_SHARD_GLOB = "*_stac_fx.parquet"

from .fixtures.fake_experiment.config_builder import Segment

_REAL_COMPONENTS = {"echam", "fesom", "oasis3mct", "jsbach"}


# --------------------------------------------------------------------------- #
# identity + structure                                                         #
# --------------------------------------------------------------------------- #


def test_all_four_real_components_are_present(fake_experiment_disk):
    """The resolved layout yields the real component set, jsbach included."""
    components = set(fake_experiment_disk.expected.components)
    assert components == _REAL_COMPONENTS
    assert "jsbach" in components  # the component the hand-declared fixture missed


def test_identity_license_and_contacts(fake_experiment_disk):
    meta = source_experiment(fake_experiment_disk.root)
    assert meta.experiment_id == "FAKE"
    assert meta.data_license == "CC-BY-4.0"
    assert {c.name for c in meta.contacts} == {"Jane Modeller", "Karl Klima"}


def test_echam_filenames_are_real_stream_names(fake_experiment_disk):
    """echam files carry the real ``<expid>_<YYYYMM>.01_<stream>`` naming."""
    import re

    echam = fake_experiment_disk.expected.ts_paths_for("echam")
    assert echam
    pattern = re.compile(r"FAKE_\d{6}\.01_")
    assert all(pattern.search(p) for p in echam)


def test_output_files_match_the_oracle(fake_experiment_disk):
    files = output_files(fake_experiment_disk.root)
    assert len(files) == fake_experiment_disk.expected.item_count


# --------------------------------------------------------------------------- #
# restart exclusion                                                           #
# --------------------------------------------------------------------------- #


def test_restart_files_are_never_catalogued(make_fake_experiment):
    """restart_out files (a whole category) stay out of the catalog."""
    exp = make_fake_experiment(
        segments=(Segment(1850, "cold"), Segment(1851, "restart"))
    )
    catalogued = {str(f.path) for f in output_files(exp.root)}
    assert exp.expected.restart_paths
    assert catalogued.isdisjoint(exp.expected.restart_paths)


def test_name_innocent_oasis_restarts_are_excluded(make_fake_experiment):
    """oasis restart files (areas.nc / masks.nc) have no 'restart' in the name.

    A name-based filter would keep them; only reading solely the tidy ``outdata``
    category (never ``restart_out``) excludes them, so this pins that behaviour.
    """
    exp = make_fake_experiment(segments=(Segment(1850, "cold"),))
    innocent = [
        p
        for p in exp.expected.restart_paths
        if p.endswith("areas.nc") or p.endswith("masks.nc")
    ]
    assert innocent  # the fixture really does produce them
    catalogued = {str(f.path) for f in output_files(exp.root)}
    assert catalogued.isdisjoint(innocent)


# --------------------------------------------------------------------------- #
# fx routing + multi-segment run span                                         #
# --------------------------------------------------------------------------- #


def test_fx_stream_routes_to_the_fx_shard(make_fake_experiment, serial_scan):
    """A stream designated fx is written time-invariant and lands in the fx shard."""
    from .fixtures.fake_experiment.resolver import resolve

    fesom_key = next(iter(resolve("awiesm-2.1", "FAKE").outdata_targets["fesom"]))
    exp = make_fake_experiment(fx_streams={"fesom": [fesom_key]})

    scan_experiment(exp.root)

    (fx_shard,) = (catalog_dir(exp.root) / "items").glob(_FX_SHARD_GLOB)
    fx_ids = item_ids(read_shard(fx_shard))
    assert len(fx_ids) == exp.expected.fx_file_count == 1


def test_run_span_is_the_union_across_segments(make_fake_experiment):
    exp = make_fake_experiment(
        segments=(
            Segment(1850, "cold"),
            Segment(1851, "restart"),
            Segment(1852, "branchoff"),
        )
    )
    meta = source_experiment(exp.root)
    assert meta.run_start == datetime(1850, 1, 1)
    assert meta.run_end == datetime(1852, 12, 31)


# --------------------------------------------------------------------------- #
# end-to-end scan, both backends                                              #
# --------------------------------------------------------------------------- #


def test_disk_scan_catalogues_every_file(fake_experiment_disk):
    report = scan_experiment(fake_experiment_disk.root)
    assert report.items == fake_experiment_disk.expected.item_count
    assert report.failures == ()


def test_memory_scan_catalogues_every_file(fake_experiment_memory, serial_scan):
    report = scan_experiment(fake_experiment_memory.root)
    assert report.items == fake_experiment_memory.expected.item_count
    assert report.failures == ()


def test_incremental_scan_skips_catalogued_ts_files(fake_experiment_disk):
    scan_experiment(fake_experiment_disk.root)
    report = scan_experiment(fake_experiment_disk.root)
    assert report.skipped == fake_experiment_disk.expected.ts_file_count


def test_remote_experiment_writes_catalog_to_local_dir(
    fake_experiment_memory, serial_scan, tmp_path
):
    """A remote (memory://) experiment can write its catalog to a local dir,
    leaving the experiment root untouched."""
    catalog = UPath(tmp_path)
    report = scan_experiment(fake_experiment_memory.root, catalog=catalog)

    assert report.items == fake_experiment_memory.expected.item_count
    assert (catalog / "collection.json").exists()
    assert list((catalog / "items").glob(_FX_SHARD_GLOB))
    # the experiment root stays catalog-free -- nothing written back over the wire
    assert not catalog_dir(fake_experiment_memory.root).exists()
