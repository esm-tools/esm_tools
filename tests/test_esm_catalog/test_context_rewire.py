"""CollectionContext regains collection_title for the scan layer."""

from __future__ import annotations

from pathlib import Path

from esm_catalog.context import CollectionContext
from esm_catalog.scan.context import resolve_context, scan_all_namelists


def test_context_has_collection_title_default():
    ctx = CollectionContext(experiment_id="e", component="c", collection_id="e")
    assert ctx.collection_title == ""


def test_context_collection_title_settable():
    ctx = CollectionContext(
        experiment_id="e", component="c", collection_id="e", collection_title="My Exp"
    )
    assert ctx.collection_title == "My Exp"


def test_resolve_context_from_path():
    p = Path("/data/experiments/exp-alpha/outdata/echam/tas_200001.nc")
    ctx = resolve_context(p)
    assert ctx.experiment_id == "exp-alpha"
    assert ctx.component == "echam"
    assert ctx.collection_id == "exp-alpha"   # Option A
    assert ctx.collection_title == "exp-alpha"


def test_scan_all_namelists(tmp_path):
    cfg = tmp_path / "config" / "echam"
    cfg.mkdir(parents=True)
    (cfg / "namelist.echam").write_text("&runctl\n delta_time = 450\n/\n")
    by_comp = scan_all_namelists(tmp_path)
    assert by_comp["echam"]["namelist.echam"]["runctl"]["delta_time"] == 450
