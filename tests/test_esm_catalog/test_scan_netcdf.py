"""Tests for the NetCDF reader of the scan layer.

Small NetCDF files are synthesized with xarray in ``tmp_path`` so each test
exercises real extraction against a real file. The central case is the
time-invariant (``fx``) file: it must yield *no* ``frequency`` and *no*
temporal range, since that omission is what marks a mask/grid downstream.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import xarray as xr
from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.reader import reader_for
from esm_catalog.scan.readers.netcdf import NetCDFReader


def _write(dataset: xr.Dataset, path) -> UPath:
    """Write *dataset* to *path* and return it as a UPath."""
    dataset.to_netcdf(path)
    return UPath(path)


def test_time_lat_lon_dataset(tmp_path):
    """A monthly time+lat+lon file yields the full complement of metadata."""
    times = pd.date_range("2000-01-01", periods=3, freq="MS")
    lat = np.linspace(-89.0, 89.0, 4)
    lon = np.linspace(-179.0, 179.0, 5)
    data = np.zeros((3, 4, 5), dtype="float32")
    dataset = xr.Dataset(
        {"tas": (("time", "lat", "lon"), data, {"units": "K"})},
        coords={"time": times, "lat": lat, "lon": lon},
    )

    metadata = NetCDFReader().read(_write(dataset, tmp_path / "tas_200001.nc"))

    assert metadata["variable"] == "tas"
    assert [v["name"] for v in metadata["variables"]] == ["tas"]
    assert metadata["format"] == "netcdf"
    assert metadata["dimensions"]["time"]["type"] == "temporal"
    assert metadata["dimensions"]["lat"]["axis"] == "y"
    assert metadata["dimensions"]["lon"]["axis"] == "x"

    bbox = metadata["bbox"]
    assert bbox[0] >= -180 and bbox[2] <= 180
    assert bbox[1] >= -90 and bbox[3] <= 90

    assert metadata["datetime_start"].year == 2000
    assert metadata["datetime_start"] < metadata["datetime_end"]
    assert metadata["frequency"] == "mon"
    assert metadata["datetime_str"] == "200001"


def test_time_invariant_dataset_has_no_frequency_or_time(tmp_path):
    """A lat/lon-only file (fx) omits frequency and the temporal range."""
    lat = np.linspace(-89.0, 89.0, 4)
    lon = np.linspace(-179.0, 179.0, 5)
    mask = np.ones((4, 5), dtype="float32")
    dataset = xr.Dataset(
        {"sftlf": (("lat", "lon"), mask)},
        coords={"lat": lat, "lon": lon},
    )

    metadata = NetCDFReader().read(_write(dataset, tmp_path / "sftlf_fx.nc"))

    assert "frequency" not in metadata
    assert "datetime_start" not in metadata
    assert "datetime_end" not in metadata
    assert metadata["variable"] == "sftlf"
    assert metadata["bbox"][0] >= -180


def test_multi_variable_dataset_lists_all(tmp_path):
    """Every data variable appears in ``variables`` with its CF attributes."""
    times = pd.date_range("2000-01-01", periods=3, freq="D")
    lat = np.linspace(-89.0, 89.0, 3)
    lon = np.linspace(-179.0, 179.0, 3)
    shape = (3, 3, 3)
    dataset = xr.Dataset(
        {
            "tas": (
                ("time", "lat", "lon"),
                np.zeros(shape, "float32"),
                {"units": "K", "standard_name": "air_temperature"},
            ),
            "pr": (
                ("time", "lat", "lon"),
                np.zeros(shape, "float32"),
                {"units": "kg m-2 s-1", "long_name": "precipitation"},
            ),
        },
        coords={"time": times, "lat": lat, "lon": lon},
    )

    metadata = NetCDFReader().read(_write(dataset, tmp_path / "multi_20000101.nc"))

    names = {v["name"] for v in metadata["variables"]}
    assert names == {"tas", "pr"}
    by_name = {v["name"]: v for v in metadata["variables"]}
    assert by_name["tas"]["standard_name"] == "air_temperature"
    assert by_name["pr"]["long_name"] == "precipitation"
    assert metadata["frequency"] == "day"


def test_empty_dataset_uses_unknown_variable_and_carries_geometry(tmp_path):
    """A coords-only file has no data variable -> the ``unknown`` fallback fires."""
    dataset = xr.Dataset(
        coords={"lat": np.linspace(-89.0, 89.0, 3), "lon": np.linspace(-179.0, 179.0, 3)}
    )

    metadata = NetCDFReader().read(_write(dataset, tmp_path / "empty-200001.nc"))

    assert metadata["variable"] == "unknown"
    assert metadata["geometry"]["type"] == "Polygon"


def test_single_timestep_has_no_frequency(tmp_path):
    """One time value cannot be spaced, so no frequency is claimed (not guessed).

    The old reader read a frequency off the filename stamp here; the filename is
    never consulted now, and a lone step gives ``infer_freq`` nothing to work on.
    """
    dataset = xr.Dataset(
        {"tas": (("time", "lat"), np.zeros((1, 2), "float32"))},
        coords={"time": pd.date_range("2000-01-01", periods=1), "lat": [0.0, 1.0]},
    )

    metadata = NetCDFReader().read(_write(dataset, tmp_path / "tas-200001.nc"))

    assert "frequency" not in metadata
    # a single decoded timestamp still stamps the item id from the data, not the name
    assert metadata["datetime_str"] == "200001"


def test_reader_is_registered():
    """The netcdf format resolves to a remote-capable NetCDFReader."""
    reader = reader_for(FileFormat.netcdf)
    assert isinstance(reader, NetCDFReader)
    assert reader.supports_remote is True


def test_drop_surrogates_makes_metadata_utf8_encodable():
    """Non-UTF-8 attribute bytes (lone surrogates) are stripped from the metadata.

    Fortran-written NetCDF attributes surface as lone surrogates that pyarrow
    rejects when serialising a stac-geoparquet shard; the reader round-trips them
    away. (Such bytes can't be written through netCDF4, which validates UTF-8, so
    this exercises the sanitiser the reader applies to its output directly.)
    """
    from esm_catalog.scan.readers.netcdf import _drop_surrogates

    dirty = {
        "variable": "deg\udc80C",
        "variables": [{"name": "tas", "long_name": "near-surface \udcfftemp"}],
        "bbox": [-180.0, -90.0, 180.0, 90.0],
    }

    clean = _drop_surrogates(dirty)

    # every string now encodes to UTF-8 (would raise before the fix)
    clean["variable"].encode("utf-8")
    clean["variables"][0]["long_name"].encode("utf-8")
    assert "\udc80" not in clean["variable"]
    assert clean["bbox"] == dirty["bbox"]  # non-strings pass through unchanged
