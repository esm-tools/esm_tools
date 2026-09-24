"""Unit tests for the NetCDF reader's pure extraction helpers.

The integration tests in ``test_scan_netcdf`` drive :class:`NetCDFReader` over
real files; these pin the individual helpers directly. Coordinates are found by
their CF attributes through :mod:`cf_xarray` (bare names are stamped first with
``guess_coord_axis``), the frequency comes from :func:`xarray.infer_freq` mapped
to a CMIP code, and neither the frequency nor the id stamp is ever read off the
filename.
"""

from __future__ import annotations

from datetime import datetime, timezone

import cf_xarray  # noqa: F401 - registers the .cf accessor
import numpy as np
import pandas as pd
import pytest
import xarray as xr

from esm_catalog.scan.readers.netcdf.coords import (
    _GLOBAL_BBOX,
    _bbox_to_polygon,
    _cf_coord,
    _extract_bbox,
)
from esm_catalog.scan.readers.netcdf.dimensions import _extract_dimensions, _to_python
from esm_catalog.scan.readers.netcdf.frequency import _cf_frequency, _infer_frequency
from esm_catalog.scan.readers.netcdf.timeaxis import _time_coord, _to_utc_datetime
from esm_catalog.scan.readers.netcdf.variables import _extract_variables


def _guess(dataset: xr.Dataset) -> xr.Dataset:
    """Stamp CF axis attrs onto bare names, as the reader does before extracting."""
    return dataset.cf.guess_coord_axis()


# --------------------------------------------------------------------------- #
# _cf_frequency — pandas offset alias -> CMIP frequency code                    #
# --------------------------------------------------------------------------- #


@pytest.mark.parametrize(
    "alias, expected",
    [
        ("30min", "subhr"),  # under an hour
        ("h", "1hr"),
        ("2h", "2hr"),  # generic sub-daily hours
        ("3h", "3hr"),
        ("6h", "6hr"),
        ("12h", "12hr"),
        ("D", "day"),
        ("2D", "day"),  # multi-day still 'day'
        ("MS", "mon"),  # month-start
        ("ME", "mon"),  # month-end
        ("YS", "yr"),  # year-start (anchored name must still match)
        ("YE", "yr"),  # year-end
        ("W", None),  # weekly has no CMIP code
    ],
)
def test_cf_frequency_maps_offsets_to_cmip(alias, expected):
    assert _cf_frequency(alias) == expected


# --------------------------------------------------------------------------- #
# _infer_frequency — end to end over a dataset's time axis                      #
# --------------------------------------------------------------------------- #


def _time_dataset(freq: str, periods: int = 4) -> xr.Dataset:
    times = pd.date_range("2000-01-01", periods=periods, freq=freq)
    return _guess(
        xr.Dataset(
            {"v": (("time",), np.zeros(periods, "float32"))}, coords={"time": times}
        )
    )


@pytest.mark.parametrize(
    "freq, expected",
    [("h", "1hr"), ("3h", "3hr"), ("6h", "6hr"), ("D", "day"), ("MS", "mon"), ("YS", "yr")],
)
def test_infer_frequency_from_regular_axis(freq, expected):
    assert _infer_frequency(_time_dataset(freq)) == expected


def test_infer_frequency_prefers_global_attribute():
    dataset = _time_dataset("D")
    dataset.attrs["frequency"] = "Mon"
    assert _infer_frequency(dataset) == "mon"  # attribute wins, lower-cased


def test_infer_frequency_none_without_time_axis():
    dataset = xr.Dataset({"v": (("x",), np.zeros(3, "float32"))})
    assert _infer_frequency(dataset) is None


def test_infer_frequency_none_for_single_timestep():
    assert _infer_frequency(_time_dataset("D", periods=1)) is None


def test_infer_frequency_none_for_irregular_axis():
    """A non-uniform axis is left unnamed rather than guessed."""
    times = pd.to_datetime(["2000-01-01", "2000-01-02", "2000-01-05"])
    dataset = _guess(
        xr.Dataset({"v": (("time",), np.zeros(3, "float32"))}, coords={"time": times})
    )
    assert _infer_frequency(dataset) is None


# --------------------------------------------------------------------------- #
# _time_coord — CF time axis, whatever it is named                             #
# --------------------------------------------------------------------------- #


def test_time_coord_finds_untidy_name():
    """A time axis named ``T`` (old FESOM) is found by its CF attributes."""
    coord = xr.DataArray(
        pd.date_range("2000-01-01", periods=2),
        dims="T",
        attrs={"standard_name": "time", "axis": "T"},
    )
    dataset = xr.Dataset({"v": (("T",), np.zeros(2, "float32"))}, coords={"T": coord})
    assert _time_coord(dataset).name == "T"


def test_time_coord_none_when_absent():
    dataset = xr.Dataset({"v": (("x",), np.zeros(2, "float32"))})
    assert _time_coord(dataset) is None


# --------------------------------------------------------------------------- #
# _to_utc_datetime — datetime64 scalar and cftime object                       #
# --------------------------------------------------------------------------- #


def test_to_utc_datetime_from_datetime64():
    result = _to_utc_datetime(np.datetime64("2001-02-03T04:05:06"))
    assert result == datetime(2001, 2, 3, 4, 5, 6, tzinfo=timezone.utc)
    assert result.tzinfo == timezone.utc


def test_to_utc_datetime_from_cftime():
    cftime = pytest.importorskip("cftime")
    result = _to_utc_datetime(cftime.DatetimeNoLeap(1500, 6, 7, 8, 9, 10))
    assert (result.year, result.month, result.day) == (1500, 6, 7)
    assert (result.hour, result.minute, result.second) == (8, 9, 10)
    assert result.tzinfo == timezone.utc


# --------------------------------------------------------------------------- #
# _bbox_to_polygon / _extract_bbox / _cf_coord — geometry and fallbacks        #
# --------------------------------------------------------------------------- #


def test_bbox_to_polygon_is_closed_ring_in_ccw_corner_order():
    poly = _bbox_to_polygon([-10.0, -5.0, 10.0, 5.0])
    assert poly == {
        "type": "Polygon",
        "coordinates": [
            [
                [-10.0, -5.0],
                [10.0, -5.0],
                [10.0, 5.0],
                [-10.0, 5.0],
                [-10.0, -5.0],
            ]
        ],
    }


def _lat_lon_dataset(lat, lon) -> xr.Dataset:
    return _guess(
        xr.Dataset(
            {"v": (("lat", "lon"), np.zeros((len(lat), len(lon)), "float32"))},
            coords={
                "lat": np.asarray(lat, "float64"),
                "lon": np.asarray(lon, "float64"),
            },
        )
    )


def test_extract_bbox_exact_values():
    dataset = _lat_lon_dataset([-40.0, -10.0, 20.0], [-30.0, 0.0, 60.0])
    bbox, geometry = _extract_bbox(dataset)
    assert bbox == [-30.0, -40.0, 60.0, 20.0]  # [west, south, east, north]
    assert geometry == _bbox_to_polygon(bbox)


def test_extract_bbox_out_of_range_falls_back_to_global():
    dataset = _lat_lon_dataset([-100.0, 100.0], [0.0, 10.0])  # lat beyond +/-90
    bbox, geometry = _extract_bbox(dataset)
    assert bbox == _GLOBAL_BBOX
    assert geometry == _bbox_to_polygon(_GLOBAL_BBOX)


def test_extract_bbox_requires_both_lat_and_lon():
    """With latitude but no longitude, fall back to global."""
    lat_only = _guess(
        xr.Dataset({"v": (("lat",), np.zeros(2, "float32"))}, coords={"lat": [0.0, 1.0]})
    )
    assert _extract_bbox(lat_only)[0] == _GLOBAL_BBOX


def test_extract_bbox_accepts_exact_domain_limits():
    """Coordinates exactly on +/-90, +/-180 are in range (inclusive bounds)."""
    dataset = _lat_lon_dataset([-90.0, 90.0], [-180.0, 180.0])
    bbox, _ = _extract_bbox(dataset)
    assert bbox == [-180.0, -90.0, 180.0, 90.0]  # accepted, NOT the global fallback


def test_extract_bbox_no_coords_falls_back_to_global():
    dataset = xr.Dataset({"v": (("x",), np.zeros(3, "float32"))})
    assert _extract_bbox(dataset)[0] == _GLOBAL_BBOX


def test_cf_coord_returns_values_when_present_else_none():
    dataset = _lat_lon_dataset([1.0, 2.0], [3.0, 4.0])
    assert list(_cf_coord(dataset, "latitude")) == [1.0, 2.0]
    assert _cf_coord(dataset, "vertical") is None  # no vertical coordinate


# --------------------------------------------------------------------------- #
# _extract_dimensions — extents, units, and axis classification                #
# --------------------------------------------------------------------------- #


def test_extract_dimensions_spatial_extent_unit_and_axes():
    dataset = _guess(
        xr.Dataset(
            {"v": (("lev", "lat", "lon"), np.zeros((2, 3, 3), "float32"))},
            coords={
                "lat": ("lat", np.array([-45.0, 0.0, 45.0]), {"units": "degrees_north"}),
                "lon": np.array([0.0, 90.0, 180.0]),
                "lev": np.array([10.0, 20.0]),
            },
        )
    )
    dims = _extract_dimensions(dataset)

    assert dims["lat"]["type"] == "spatial"
    assert dims["lat"]["axis"] == "y"
    assert dims["lat"]["extent"] == [-45.0, 45.0]
    assert dims["lat"]["unit"] == "degrees_north"
    assert dims["lon"]["axis"] == "x"
    assert dims["lon"]["extent"] == [0.0, 180.0]
    assert dims["lev"]["axis"] == "z"
    # guess_coord_axis stamps a CF unit onto a bare longitude, so it carries one
    assert dims["lon"]["unit"] == "degrees_east"


def test_extract_dimensions_unclassified_dim_is_spatial_without_axis():
    """A dimension cf cannot classify stays plain ``spatial``."""
    dataset = xr.Dataset({"v": (("bnds",), np.zeros(2, "float32"))})
    dims = _extract_dimensions(dataset)
    assert dims["bnds"]["type"] == "spatial"
    assert "axis" not in dims["bnds"]


def test_extract_dimensions_untidy_time_name_is_temporal():
    """A time axis named ``ocean_time`` is classified temporal by cf_xarray."""
    times = pd.date_range("2000-01-01", periods=2, freq="D")
    dataset = _guess(
        xr.Dataset(
            {"v": (("ocean_time",), np.zeros(2, "float32"))},
            coords={"ocean_time": times},
        )
    )
    dims = _extract_dimensions(dataset)
    assert dims["ocean_time"]["type"] == "temporal"


def test_extract_dimensions_temporal_extent_is_iso():
    times = pd.date_range("2000-01-01", periods=3, freq="D")
    dataset = _guess(
        xr.Dataset({"v": (("time",), np.zeros(3, "float32"))}, coords={"time": times})
    )
    dims = _extract_dimensions(dataset)
    assert dims["time"]["type"] == "temporal"
    assert dims["time"]["extent"][0].startswith("2000-01-01")
    assert dims["time"]["extent"][1].startswith("2000-01-03")


# --------------------------------------------------------------------------- #
# _extract_variables / _to_python                                              #
# --------------------------------------------------------------------------- #


def test_extract_variables_captures_every_cf_attr_and_dimensions():
    """Each of the four recognized CF attrs is copied, plus the dimension list."""
    dataset = xr.Dataset(
        {
            "tas": (
                ("time", "lat"),
                np.zeros((2, 2), "float32"),
                {
                    "standard_name": "air_temperature",
                    "long_name": "Near-Surface Air Temperature",
                    "units": "K",
                    "description": "2m temperature",
                    "ignored_attr": "not copied",
                },
            )
        },
        coords={"time": pd.date_range("2000-01-01", periods=2), "lat": [0.0, 1.0]},
    )
    (entry,) = _extract_variables(dataset)
    assert entry == {
        "name": "tas",
        "standard_name": "air_temperature",
        "long_name": "Near-Surface Air Temperature",
        "units": "K",
        "description": "2m temperature",
        "dimensions": ["time", "lat"],
    }


def test_to_python_unwraps_numpy_scalar():
    result = _to_python(np.float64(3.5))
    assert result == 3.5
    assert isinstance(result, float)
    assert _to_python("plain") == "plain"


def test_unidentified_dimension_has_no_type():
    # A dimension with no CF axis (e.g. FESOM's unstructured 'elem') is a bare
    # index: it keeps its extent but must NOT be labelled 'spatial'/'temporal'.
    dataset = xr.Dataset(
        coords={
            "time": ("time", pd.date_range("1850-03-31", periods=1)),
            "elem": ("elem", np.arange(0, 100)),
        }
    )
    dataset["time"].attrs["standard_name"] = "time"

    dims = _extract_dimensions(dataset)
    assert dims["time"]["type"] == "temporal"
    assert dims["elem"]["extent"] == [0, 99]
    assert "type" not in dims["elem"]
