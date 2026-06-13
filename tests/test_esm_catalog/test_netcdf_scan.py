"""scan_netcdf extracts STAC-relevant metadata from a real NetCDF file."""

from __future__ import annotations

import numpy as np
import pandas as pd
import xarray as xr

from esm_catalog.scan.netcdf import scan_netcdf


def _make_nc(path):
    times = pd.date_range("2000-01-01", periods=3, freq="MS")
    ds = xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((3, 2, 2), dtype="float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    )
    ds["tas"].attrs["standard_name"] = "air_temperature"
    ds["tas"].attrs["units"] = "K"
    ds.attrs["Conventions"] = "CF-1.8"
    ds.to_netcdf(path)


def test_scan_netcdf_basic(tmp_path):
    f = tmp_path / "tas_200001.nc"
    _make_nc(f)
    md = scan_netcdf(f)
    assert md["format"] == "netcdf"
    assert md["variable"] == "tas"
    assert md["conventions"] == "CF-1.8"
    assert md["bbox"] == [0.0, -45.0, 90.0, 45.0]
    assert md["datetime_start"].year == 2000
    assert md["file_size"] > 0
