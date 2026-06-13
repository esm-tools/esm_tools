"""`esm-catalog scan` walks a dir and emits a STAC catalog JSON."""

from __future__ import annotations

import json

import numpy as np
import pandas as pd
import xarray as xr
from click.testing import CliRunner

from esm_catalog.cli import main


def test_cli_scan_outputs_json(tmp_path):
    out = tmp_path / "experiments" / "exp-alpha" / "outdata" / "echam"
    out.mkdir(parents=True)
    times = pd.date_range("2000-01-01", periods=1, freq="MS")
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    ).to_netcdf(out / "tas_200001.nc")

    target = tmp_path / "cat.json"
    res = CliRunner().invoke(main, ["scan", str(tmp_path), "--output", str(target)])
    assert res.exit_code == 0, res.output
    cat = json.loads(target.read_text())
    assert cat["collections"][0]["id"] == "exp-alpha"
    assert len(cat["items"]) == 1
