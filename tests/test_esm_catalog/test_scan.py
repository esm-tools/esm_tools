"""Tests for scan/netcdf.py, scan/grib.py, scan/detect.py, and scan/context.py."""

from pathlib import Path
from unittest.mock import patch

import numpy as np
import pytest
import xarray as xr

from esm_catalog.scan.context import CollectionContext, resolve_context
from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
from esm_catalog.scan.grib import _update_bbox
from esm_catalog.scan.netcdf import scan_netcdf


# ---------------------------------------------------------------------------
# scan/netcdf.py
# ---------------------------------------------------------------------------

class TestScanNetcdf:
    def test_returns_expected_keys(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        for key in ("variable", "variables", "cf_parameters", "dimensions",
                    "bbox", "geometry", "datetime_start", "datetime_end",
                    "datetime_str", "file_size", "format"):
            assert key in meta, f"missing key: {key}"

    def test_primary_variable(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert meta["variable"] == "ssh"

    def test_cf_parameters_extracted(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert len(meta["cf_parameters"]) == 1
        assert meta["cf_parameters"][0]["name"] == "sea_surface_height_above_geoid"
        assert meta["cf_parameters"][0]["unit"] == "m"

    def test_bbox_within_bounds(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        lon_min, lat_min, lon_max, lat_max = meta["bbox"]
        assert -180 <= lon_min <= lon_max <= 180
        assert -90 <= lat_min <= lat_max <= 90

    def test_geometry_is_polygon(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert meta["geometry"]["type"] == "Polygon"
        coords = meta["geometry"]["coordinates"][0]
        assert len(coords) == 5  # closed ring
        assert coords[0] == coords[-1]

    def test_datetime_range_extracted(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert meta["datetime_start"] is not None
        assert meta["datetime_end"] is not None
        assert meta["datetime_start"] <= meta["datetime_end"]

    def test_datetime_str_format(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert len(meta["datetime_str"]) == 6  # YYYYMM
        assert meta["datetime_str"].isdigit()

    def test_file_size_positive(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert meta["file_size"] > 0

    def test_format_is_netcdf(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        assert meta["format"] == "netcdf"

    def test_cube_dimensions_populated(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        dims = meta["dimensions"]
        assert "time" in dims
        assert "lat" in dims
        assert "lon" in dims
        assert dims["time"]["type"] == "temporal"
        assert dims["lat"]["type"] == "spatial"

    def test_global_bbox_fallback(self, tmp_path):
        """File without lat/lon coordinates should return global bbox."""
        nc_path = tmp_path / "no_coords.nc"
        ds = xr.Dataset(
            {"data": xr.DataArray(np.ones((5, 10)), dims=["x", "y"])}
        )
        ds.to_netcdf(nc_path)
        meta = scan_netcdf(nc_path)
        assert meta["bbox"] == [-180.0, -90.0, 180.0, 90.0]

    def test_variables_list_has_dimensions(self, fesom_nc):
        meta = scan_netcdf(fesom_nc)
        var = next(v for v in meta["variables"] if v["name"] == "ssh")
        assert "time" in var["dimensions"]


# ---------------------------------------------------------------------------
# scan/detect.py
# ---------------------------------------------------------------------------

class TestScanFile:
    def test_dispatches_to_netcdf_for_nc(self, fesom_nc):
        meta = scan_file(fesom_nc)
        assert meta["format"] == "netcdf"

    def test_dispatches_to_netcdf_for_nc4(self, tmp_path):
        nc4 = tmp_path / "data.nc4"
        ds = xr.Dataset({"x": xr.DataArray([1.0, 2.0], dims=["t"])})
        ds.to_netcdf(nc4)
        meta = scan_file(nc4)
        assert meta["format"] == "netcdf"

    def test_dispatches_to_netcdf_via_magic_no_extension(self, tmp_path, fesom_nc):
        """A NetCDF file renamed to have no extension is detected via HDF5 magic bytes."""
        import shutil
        no_ext = tmp_path / "basic-001_185001.01_fesom"
        shutil.copy(fesom_nc, no_ext)
        meta = scan_file(no_ext)
        assert meta["format"] == "netcdf"

    def test_unsupported_format_raises(self, tmp_path):
        txt = tmp_path / "data.txt"
        txt.write_text("not a climate file")
        with pytest.raises(UnsupportedFormatError):
            scan_file(txt)

    def test_unknown_extension_raises(self, tmp_path):
        f = tmp_path / "file.xyz"
        f.touch()
        with pytest.raises(UnsupportedFormatError):
            scan_file(f)

    def test_error_message_contains_suffix(self, tmp_path):
        f = tmp_path / "file.hdf4"
        f.touch()
        with pytest.raises(UnsupportedFormatError, match=r"\.hdf4"):
            scan_file(f)

    def test_extension_less_non_climate_file_raises(self, tmp_path):
        """Extension-less file with unrecognised magic bytes raises UnsupportedFormatError."""
        f = tmp_path / "basic-001_185001.01_echam"
        f.write_bytes(b"\x00\x00\x00\x00")
        with pytest.raises(UnsupportedFormatError):
            scan_file(f)


# ---------------------------------------------------------------------------
# scan/grib.py — longitude normalisation
# ---------------------------------------------------------------------------

class TestUpdateBbox:
    def _make_ds(self, lons, lats):
        """Build a minimal xarray Dataset with the given lon/lat coordinates."""
        import xarray as xr
        return xr.Dataset(coords={"longitude": lons, "latitude": lats})

    def test_standard_180_convention_unchanged(self):
        ds = self._make_ds(np.array([-180.0, -90.0, 0.0, 90.0, 180.0]),
                           np.array([-90.0, 0.0, 90.0]))
        bbox = _update_bbox(ds, [-180.0, -90.0, 180.0, 90.0])
        assert bbox[0] >= -180.0
        assert bbox[2] <= 180.0

    def test_0_360_convention_normalised(self):
        """Longitudes in 0–360 range (ECHAM) are folded to −180–180."""
        ds = self._make_ds(np.array([0.0, 90.0, 180.0, 270.0, 358.125]),
                           np.array([-90.0, 0.0, 90.0]))
        bbox = _update_bbox(ds, [-180.0, -90.0, 180.0, 90.0])
        assert bbox[0] >= -180.0
        assert bbox[2] <= 180.0

    def test_echam_global_grid_produces_valid_bbox(self):
        """Simulated ECHAM T63 global grid (0–358.125°) gives a valid GeoJSON bbox."""
        lons = np.linspace(0.0, 358.125, 128)
        lats = np.linspace(-87.159, 87.159, 64)
        ds = self._make_ds(lons, lats)
        bbox = _update_bbox(ds, [-180.0, -90.0, 180.0, 90.0])
        lon_min, lat_min, lon_max, lat_max = bbox
        assert -180.0 <= lon_min <= lon_max <= 180.0
        assert -90.0 <= lat_min <= lat_max <= 90.0

    def test_no_coords_returns_current_bbox(self):
        import xarray as xr
        ds = xr.Dataset()
        original = [-180.0, -90.0, 180.0, 90.0]
        assert _update_bbox(ds, original) == original


# ---------------------------------------------------------------------------
# scan/context.py
# ---------------------------------------------------------------------------

class TestResolveContextFromPath:
    def test_resolves_experiment_and_component(self, fesom_nc):
        ctx = resolve_context(fesom_nc)
        assert ctx.experiment_id == "basic-001"
        assert ctx.component == "fesom"
        assert ctx.collection_id == "basic-001-fesom"

    def test_resolves_echam_component(self, echam_nc):
        ctx = resolve_context(echam_nc)
        assert ctx.experiment_id == "pi-ctrl"
        assert ctx.component == "echam"
        assert ctx.collection_id == "pi-ctrl-echam"

    def test_collection_id_is_experiment_dash_component(self, fesom_nc):
        ctx = resolve_context(fesom_nc)
        assert ctx.collection_id == f"{ctx.experiment_id}-{ctx.component}"

    def test_collection_title_readable(self, fesom_nc):
        ctx = resolve_context(fesom_nc)
        assert ctx.experiment_id in ctx.collection_title
        assert ctx.component in ctx.collection_title

    def test_raises_on_unresolvable_path(self, tmp_path):
        orphan = tmp_path / "some" / "random" / "file.nc"
        orphan.parent.mkdir(parents=True)
        orphan.touch()
        with pytest.raises(ValueError, match="Cannot resolve"):
            resolve_context(orphan)

    def test_never_returns_none_collection_id(self, fesom_nc):
        ctx = resolve_context(fesom_nc)
        assert ctx.collection_id is not None
        assert ctx.collection_id != ""

    def test_creates_collection_in_db_on_first_encounter(self, fesom_nc, db):
        assert not db.collection_exists("basic-001-fesom")
        resolve_context(fesom_nc, db=db)
        assert db.collection_exists("basic-001-fesom")

    def test_does_not_duplicate_collection(self, fesom_nc, db):
        resolve_context(fesom_nc, db=db)
        resolve_context(fesom_nc, db=db)  # second call — must not raise
        cols, n = db.search_collections()
        assert n == 1


class TestResolveContextFromConfig:
    def test_uses_expid_from_config(self, fesom_nc, esm_config):
        # Point outdata_dir at fesom_nc's parent so the component is found
        esm_config["fesom"]["outdata_dir"] = str(fesom_nc.parent)
        ctx = resolve_context(fesom_nc, config=esm_config)
        assert ctx.experiment_id == "basic-001"
        assert ctx.component == "fesom"

    def test_falls_back_to_path_when_config_has_no_matching_outdata(
        self, fesom_nc, esm_config
    ):
        # outdata_dir points somewhere unrelated → falls back to path parsing
        esm_config["fesom"]["outdata_dir"] = "/some/other/path"
        ctx = resolve_context(fesom_nc, config=esm_config)
        assert ctx.experiment_id == "basic-001"
        assert ctx.component == "fesom"

    def test_none_config_uses_path_strategy(self, fesom_nc):
        ctx = resolve_context(fesom_nc, config=None)
        assert ctx.experiment_id == "basic-001"


class TestCollectionContext:
    def test_dataclass_fields(self):
        ctx = CollectionContext(
            experiment_id="test",
            component="ocean",
            collection_id="test-ocean",
            collection_title="test / ocean",
        )
        assert ctx.experiment_id == "test"
        assert ctx.component == "ocean"
        assert ctx.collection_id == "test-ocean"
