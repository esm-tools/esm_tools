"""scan_tree walks a run dir and builds in-memory STAC (no DuckDB)."""

from __future__ import annotations

import numpy as np
import pandas as pd
import xarray as xr

from esm_catalog.scan.ingest import scan_tree


def _make_run(root):
    out = root / "experiments" / "exp-alpha" / "outdata" / "echam"
    out.mkdir(parents=True)
    times = pd.date_range("2000-01-01", periods=2, freq="MS")
    ds = xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((2, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    )
    ds.to_netcdf(out / "tas_200001.nc")
    return root


def test_scan_tree_builds_collection_and_items(tmp_path):
    _make_run(tmp_path)
    cat = scan_tree(tmp_path)
    assert len(cat["collections"]) == 1
    assert cat["collections"][0]["id"] == "exp-alpha"
    assert len(cat["items"]) == 1
    item = cat["items"][0]
    assert item["type"] == "Feature"
    assert item["properties"]["component"] == "echam"
    assert item["collection"] == "exp-alpha"
