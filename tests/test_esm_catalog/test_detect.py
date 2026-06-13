"""scan_file dispatches NetCDF correctly."""

from __future__ import annotations

import numpy as np
import xarray as xr

from esm_catalog.scan.detect import scan_file


def test_scan_file_netcdf_by_suffix(tmp_path):
    f = tmp_path / "x.nc"
    xr.Dataset({"v": ("t", np.arange(3))}).to_netcdf(f)
    md = scan_file(f)
    assert md["format"] == "netcdf"
