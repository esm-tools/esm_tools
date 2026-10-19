"""The OIFS/XIOS enricher (esm_catalog.scan.model_plugins.enrichers.netcdf.oifs).

Confirmed live against a real AWI-CM3 v3.2 run (OIFS via XIOS): XIOS output
carries two time axes cf_xarray refuses to disambiguate.
"""

from __future__ import annotations

import numpy as np
import xarray as xr
from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.model_plugins.enrichers.netcdf.oifs import enrich_metadata


def _xios_dataset():
    time_counter = np.array(
        ["2000-01-16T12:00:00", "2000-02-15T00:00:00"], dtype="datetime64[ns]"
    )
    time_counter_bounds = np.array(
        [
            [np.datetime64("2000-01-01"), np.datetime64("2000-02-01")],
            [np.datetime64("2000-02-01"), np.datetime64("2000-03-01")],
        ],
        dtype="datetime64[ns]",
    )
    return xr.Dataset(
        {
            "oifs_ttrc": (("time_counter",), [1.0, 2.0]),
            "time_counter_bounds": (
                ("time_counter", "axis_nbounds"),
                time_counter_bounds,
            ),
        },
        coords={
            "time_counter": (
                "time_counter",
                time_counter,
                {"bounds": "time_counter_bounds"},
            ),
            "time_centered": ("time_counter", time_counter),
        },
    )


def test_xios_dual_axis_picks_time_counter_and_drops_bounds():
    ds = _xios_dataset()
    metadata = {
        "variables": [{"name": "oifs_ttrc"}, {"name": "time_counter_bounds"}],
        "variable": "oifs_ttrc",
    }

    enrich_metadata(UPath("oifs_ttrc_200001.nc"), FileFormat.netcdf, metadata, [ds])

    assert metadata["datetime_start"] is not None
    assert metadata["datetime_end"] is not None
    assert metadata["frequency"] == "mon"
    assert [v["name"] for v in metadata["variables"]] == ["oifs_ttrc"]


def test_bounds_variable_dropped_as_primary_variable_falls_back_to_kept():
    ds = _xios_dataset()
    metadata = {
        "variables": [{"name": "time_counter_bounds"}, {"name": "oifs_ttrc"}],
        "variable": "time_counter_bounds",
    }

    enrich_metadata(UPath("oifs_ttrc_200001.nc"), FileFormat.netcdf, metadata, [ds])

    assert metadata["variable"] == "oifs_ttrc"


def test_noop_for_non_netcdf_format():
    ds = _xios_dataset()
    metadata = {"variables": [{"name": "time_counter_bounds"}]}

    enrich_metadata(UPath("x.grb"), FileFormat.grib, metadata, [ds])

    assert metadata["variables"] == [{"name": "time_counter_bounds"}]


def test_noop_when_metadata_already_has_a_time_range():
    ds = _xios_dataset()
    metadata = {"datetime_start": "already-set", "variables": []}

    enrich_metadata(UPath("oifs_ttrc_200001.nc"), FileFormat.netcdf, metadata, [ds])

    assert metadata["datetime_start"] == "already-set"
    assert "datetime_end" not in metadata
