"""The FESOM2 enricher (esm_catalog.scan.model_plugins.enrichers.netcdf.fesom).

Confirmed live against a real AWI-CM3 v3.2 run: FESOM annual fields are
single-step, so the data itself carries no spacing to infer a frequency from.
"""

from __future__ import annotations

import xarray as xr
from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.model_plugins.enrichers.netcdf.fesom import enrich_metadata


def test_fesom_single_step_annual_frequency_from_namelist_io(tmp_path):
    outdata = tmp_path / "outdata" / "fesom"
    outdata.mkdir(parents=True)
    config = tmp_path / "config" / "fesom"
    config.mkdir(parents=True)
    (config / "namelist.io").write_text("io_list = 'a_ice', 1, 'y', 4,\n")

    ds = xr.Dataset({"a_ice": (("time",), [1.0])})
    metadata = {"variables": []}

    enrich_metadata(
        UPath(outdata / "a_ice.fesom.2000.01.nc"), FileFormat.netcdf, metadata, [ds]
    )

    assert metadata["frequency"] == "yr"


def test_fesom_frequency_unset_when_variable_not_in_namelist_io(tmp_path):
    outdata = tmp_path / "outdata" / "fesom"
    outdata.mkdir(parents=True)
    config = tmp_path / "config" / "fesom"
    config.mkdir(parents=True)
    (config / "namelist.io").write_text("io_list = 'a_ice', 1, 'y', 4,\n")

    ds = xr.Dataset({"Redi_K": (("time",), [1.0])})
    metadata = {"variables": []}

    enrich_metadata(
        UPath(outdata / "Redi_K.fesom.2000.01.nc"), FileFormat.netcdf, metadata, [ds]
    )

    assert "frequency" not in metadata


def test_noop_for_non_netcdf_format(tmp_path):
    metadata = {"variables": []}
    enrich_metadata(UPath("x.grb"), FileFormat.grib, metadata, [])
    assert "frequency" not in metadata


def test_noop_when_frequency_already_set(tmp_path):
    outdata = tmp_path / "outdata" / "fesom"
    outdata.mkdir(parents=True)
    config = tmp_path / "config" / "fesom"
    config.mkdir(parents=True)
    (config / "namelist.io").write_text("io_list = 'a_ice', 1, 'y', 4,\n")

    metadata = {"variables": [], "frequency": "mon"}
    enrich_metadata(
        UPath(outdata / "a_ice.fesom.2000.01.nc"), FileFormat.netcdf, metadata, []
    )

    assert metadata["frequency"] == "mon"
