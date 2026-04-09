"""Shared fixtures for esm_catalog tests."""

import numpy as np
import pytest
import xarray as xr


# Suppress benign binary-compatibility warning triggered by mixed conda/pip
# C extensions (e.g. netCDF4 + pandas) loading numpy internals in the same
# process.  All tests pass; this is purely cosmetic noise.
def pytest_configure(config):
    config.addinivalue_line(
        "filterwarnings",
        "ignore:numpy.ndarray size changed:RuntimeWarning",
    )

from esm_catalog.scan.context import CollectionContext
from esm_catalog.storage.duckdb import CatalogDB


# ---------------------------------------------------------------------------
# File fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def fesom_nc(tmp_path):
    """Synthetic FESOM-style NetCDF in the ESM-Tools path convention.

    Path: tmp/experiments/basic-001/outdata/fesom/ssh.fesom.185001.01.nc
    """
    nc_dir = tmp_path / "experiments" / "basic-001" / "outdata" / "fesom"
    nc_dir.mkdir(parents=True)
    path = nc_dir / "ssh.fesom.185001.01.nc"

    times = np.array(
        ["1850-01-31", "1850-02-28", "1850-03-31"], dtype="datetime64[ns]"
    )
    ds = xr.Dataset(
        {
            "ssh": xr.DataArray(
                np.random.rand(3, 8, 16).astype("float32"),
                dims=["time", "lat", "lon"],
                attrs={
                    "standard_name": "sea_surface_height_above_geoid",
                    "long_name": "Sea Surface Height",
                    "units": "m",
                },
            )
        },
        coords={
            "time": times,
            "lat": np.linspace(-90, 90, 8),
            "lon": np.linspace(-180, 180, 16),
        },
        attrs={"Conventions": "CF-1.8", "institution": "AWI"},
    )
    ds.to_netcdf(path)
    return path


@pytest.fixture()
def echam_nc(tmp_path):
    """Synthetic ECHAM-style NetCDF (no lat/lon → global bbox fallback)."""
    nc_dir = tmp_path / "experiments" / "pi-ctrl" / "outdata" / "echam"
    nc_dir.mkdir(parents=True)
    path = nc_dir / "temp.echam.195001.nc"

    ds = xr.Dataset(
        {
            "temp": xr.DataArray(
                np.random.rand(6, 19, 64).astype("float32"),
                dims=["time", "lat", "lon"],
                attrs={"standard_name": "air_temperature", "units": "K"},
            )
        },
        coords={
            "time": np.array(
                ["1950-01-31", "1950-02-28", "1950-03-31",
                 "1950-04-30", "1950-05-31", "1950-06-30"],
                dtype="datetime64[ns]",
            ),
            "lat": np.linspace(-87.159, 87.159, 19),
            "lon": np.linspace(0, 354.375, 64),
        },
    )
    ds.to_netcdf(path)
    return path


@pytest.fixture()
def esm_config():
    """Minimal ESM-Tools finished_config dict."""
    return {
        "general": {
            "expid": "basic-001",
            "pi_name": "Paul Gierz",
            "pi_orcid": "0000-0003-3163-4650",
        },
        "fesom": {
            "outdata_dir": "/work/user/experiments/basic-001/outdata/fesom",
        },
    }


# ---------------------------------------------------------------------------
# Storage fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def db(tmp_path):
    """Open CatalogDB in a temp directory; closes after the test."""
    with CatalogDB(tmp_path / "catalog.duckdb") as catalog:
        yield catalog


# ---------------------------------------------------------------------------
# STAC object fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def ctx():
    """A CollectionContext for basic-001 / fesom."""
    return CollectionContext(
        experiment_id="basic-001",
        component="fesom",
        collection_id="basic-001-fesom",
        collection_title="basic-001 / fesom",
    )


@pytest.fixture()
def sample_metadata():
    """Metadata dict as returned by scan_netcdf."""
    from datetime import datetime, timezone

    return {
        "variable": "ssh",
        "variables": [
            {
                "name": "ssh",
                "standard_name": "sea_surface_height_above_geoid",
                "units": "m",
                "dimensions": ["time", "lat", "lon"],
            }
        ],
        "cf_parameters": [
            {"name": "sea_surface_height_above_geoid", "variable": "ssh", "unit": "m"}
        ],
        "dimensions": {
            "time": {"type": "temporal", "extent": ["1850-01-31", "1850-03-31"]},
            "lat": {"type": "spatial", "axis": "y", "extent": [-90.0, 90.0]},
            "lon": {"type": "spatial", "axis": "x", "extent": [-180.0, 180.0]},
        },
        "bbox": [-180.0, -90.0, 180.0, 90.0],
        "geometry": {
            "type": "Polygon",
            "coordinates": [[
                [-180.0, -90.0], [180.0, -90.0],
                [180.0, 90.0], [-180.0, 90.0], [-180.0, -90.0],
            ]],
        },
        "datetime_start": datetime(1850, 1, 31, tzinfo=timezone.utc),
        "datetime_end": datetime(1850, 3, 31, tzinfo=timezone.utc),
        "datetime_str": "185001",
        "file_size": 131072,
        "conventions": "CF-1.8",
        "format": "netcdf",
    }


@pytest.fixture()
def sample_item(sample_metadata, ctx, fesom_nc):
    """A fully constructed STAC Item dict."""
    from esm_catalog.stac.item import make_item
    return make_item(fesom_nc, sample_metadata, ctx)
