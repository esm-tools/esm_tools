import json
from datetime import datetime
from functools import lru_cache
from pathlib import Path

import numpy as np
import pyfesom2 as pf2
import pystac
import xarray as xr
from shapely.geometry import Polygon, box, mapping

# -----------------------------
# CONFIG
# -----------------------------
FESOM_MeshPath = "/albedo/pool/FESOM2/core2/"

# Fallback bbox (use mesh bbox if you have it)
FALLBACK_BBOX = [-180.0, -90.0, 180.0, 90.0]


# -----------------------------
# Helpers
# -----------------------------
def extract_datetime(ds):
    """Extract the first datetime from an xarray Dataset.

    Args:
        ds: xarray.Dataset containing a 'time' coordinate/variable.

    Returns:
        datetime object of the first time value, or None if 'time' is not present.
    """
    if "time" not in ds:
        return None
    time = ds["time"].values
    return np.datetime64(time[0]).astype("datetime64[ms]").item()


@lru_cache(maxsize=None)
def fesom2_geometry(meshpath=FESOM_MeshPath):
    """Load FESOM2 mesh and compute bounding box and GeoJSON geometry.

    This function is cached to avoid reloading the same mesh multiple times.

    Args:
        meshpath: Path to FESOM2 mesh directory. Defaults to FESOM_MeshPath.

    Returns:
        tuple: (bbox, geometry) where:
            - bbox: List of [min_lon, min_lat, max_lon, max_lat]
            - geometry: GeoJSON-like dict representing the bounding box polygon
    """
    mesh = pf2.load_mesh(meshpath)
    lon_vals = mesh.x2
    lat_vals = mesh.y2
    bbox = [
        float(np.nanmin(lon_vals)),
        float(np.nanmin(lat_vals)),
        float(np.nanmax(lon_vals)),
        float(np.nanmax(lat_vals)),
    ]
    geometry = mapping(box(*bbox))
    return bbox, geometry


def extract_cf_parameters(ds):
    """Extract CF convention parameters from xarray Dataset variables.

    Scans all data variables for CF 'standard_name' and 'units' attributes
    to build STAC CF extension parameter objects. For FESOM data, also
    extracts 'description' and 'long_name' attributes.

    Args:
        ds: xarray.Dataset with CF-compliant variable attributes.

    Returns:
        list: List of dicts with 'name' (required) and 'unit' (optional) keys,
              following the STAC CF extension schema.
    """
    cf_parameters = []
    for var_name, da in ds.data_vars.items():
        # Try standard CF convention first
        standard_name = da.attrs.get("standard_name")

        # Fall back to FESOM-specific attributes
        if not standard_name:
            # FESOM uses 'description' or 'long_name' instead of 'standard_name'
            standard_name = da.attrs.get("description") or da.attrs.get("long_name")

        if not standard_name:
            continue

        param = {
            "name": standard_name,
            "variable": var_name,  # Include the actual variable name
        }

        unit = da.attrs.get("units")
        if unit:
            param["unit"] = unit

        cf_parameters.append(param)
    return cf_parameters


def create_stac_item(nc_file, ds, bbox, geometry):
    """Create a STAC Item from a FESOM NetCDF file.

    Builds a complete STAC Item with:
    - Basic metadata (id, geometry, bbox, datetime)
    - FESOM-specific properties (model, grid, conventions)
    - CF extension with parameter metadata
    - NetCDF data asset with CF parameters

    Args:
        nc_file: Path object pointing to the NetCDF file.
        ds: xarray.Dataset opened from nc_file.
        bbox: Bounding box as [min_lon, min_lat, max_lon, max_lat].
        geometry: GeoJSON-like geometry dict.

    Returns:
        tuple: (item, datetime) where:
            - item: pystac.Item with CF extension and data asset
            - datetime: Extracted datetime from the dataset
    """
    dt = extract_datetime(ds)

    item = pystac.Item(
        id=nc_file.stem,
        geometry=geometry,
        bbox=bbox,
        datetime=dt,
        properties={
            "model": "FESOM",
            "grid": "unstructured-mesh",
            "conventions": ds.attrs.get("Conventions", "CF-UGRID"),
        },
    )

    cf_parameters = extract_cf_parameters(ds)

    if cf_parameters:
        item.stac_extensions = [
            "https://stac-extensions.github.io/cf/v0.2.0/schema.json"
        ]
        item.properties["cf:parameter"] = cf_parameters

    asset = pystac.Asset(
        href=str(nc_file.resolve()),
        media_type="application/x-netcdf",
        roles=["data"],
        title="FESOM NetCDF output",
        extra_fields={
            "xarray:open_kwargs": {
                "engine": "netcdf4",
                "decode_times": True,
            },
            "xarray:storage_options": {},
        },
    )

    if cf_parameters:
        asset.extra_fields["cf:parameter"] = cf_parameters

    item.add_asset("data", asset)

    return item, dt


def get_mesh_geometry(ds):
    """Extract mesh geometry from a FESOM Dataset with fallback handling.

    Attempts to:
    1. Read mesh path from Dataset attributes (ds.FESOM_MeshPath)
    2. Load mesh geometry using fesom2_geometry()
    3. Fall back to default mesh path and global bbox if either step fails

    Args:
        ds: xarray.Dataset potentially containing FESOM_MeshPath attribute.

    Returns:
        tuple: (bbox, geometry) as returned by fesom2_geometry()
    """
    try:
        meshpath = ds.FESOM_MeshPath
    except Exception:
        meshpath = FESOM_MeshPath

    try:
        bbox, geometry = fesom2_geometry(meshpath)
    except Exception:
        bbox, geometry = FALLBACK_BBOX, mapping(box(*FALLBACK_BBOX))

    return bbox, geometry


def initialize_catalog_and_collection(catalog_id, collection_id):
    """Initialize STAC Catalog and Collection structure.

    Creates a catalog with a single collection for FESOM output.
    The collection's spatial extent is derived from the default mesh,
    and temporal extent is initialized as unbounded (will be updated later).

    Args:
        catalog_id: Identifier for the STAC Catalog.
        collection_id: Identifier for the STAC Collection.

    Returns:
        tuple: (catalog, collection) where:
            - catalog: pystac.Catalog with collection already added as child
            - collection: pystac.Collection with initial extents
    """
    catalog = pystac.Catalog(
        id=catalog_id,
        description="STAC catalog for FESOM mesh-based NetCDF output",
    )

    try:
        bbox, geometry = fesom2_geometry()
    except Exception:
        bbox, geometry = FALLBACK_BBOX, mapping(box(*FALLBACK_BBOX))

    collection = pystac.Collection(
        id=collection_id,
        description="FESOM unstructured mesh climate model output",
        extent=pystac.Extent(
            spatial=pystac.SpatialExtent([bbox]),
            temporal=pystac.TemporalExtent([[None, None]]),
        ),
        license="proprietary",
    )

    catalog.add_child(collection)

    # Add titles to links for STAC compliance
    for link in catalog.links:
        if link.rel == "child" and not link.title:
            link.title = "FESOM Collection"

    for link in collection.links:
        if link.rel == "root" and not link.title:
            link.title = "FESOM Catalog"
        elif link.rel == "parent" and not link.title:
            link.title = "FESOM Catalog"

    return catalog, collection


def build_fesom_stac(
    data_dir,
    output_dir="stac",
    catalog_id="fesom-catalog",
    collection_id="fesom-collection",
):
    """Build a STAC catalog from FESOM NetCDF output files.

    Processes all .nc files in the specified directory and creates a complete
    STAC catalog with:
    - One catalog containing one collection
    - One STAC Item per NetCDF file with CF extension
    - Spatial extents from FESOM mesh geometry
    - Temporal extents from file timestamps
    - CF parameter metadata extracted from variable attributes

    The catalog is saved as a self-contained structure with relative links.

    Args:
        data_dir: Directory containing FESOM NetCDF files (*.nc).
        output_dir: Directory where STAC catalog will be written. Default: "stac".
        catalog_id: Identifier for the STAC Catalog. Default: "fesom-catalog".
        collection_id: Identifier for the STAC Collection. Default: "fesom-collection".

    Returns:
        None. Writes catalog structure to output_dir and prints confirmation.

    Example:
        >>> build_fesom_stac(
        ...     data_dir="/path/to/fesom/output",
        ...     output_dir="my_stac_catalog"
        ... )
        Processing file1.nc
        Processing file2.nc
        ...
        FESOM STAC catalog written to: /full/path/to/my_stac_catalog
    """
    data_dir = Path(data_dir)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    catalog, collection = initialize_catalog_and_collection(catalog_id, collection_id)
    all_datetimes = []

    for nc_file in sorted(data_dir.glob("*.nc")):
        print(f"Processing {nc_file.name}")

        ds = xr.open_dataset(nc_file, decode_times=True)
        bbox, geometry = get_mesh_geometry(ds)

        item, dt = create_stac_item(nc_file, ds, bbox, geometry)

        try:
            item.validate()
        except Exception:
            pass

        collection.add_item(item)

        # Add titles to item links for STAC compliance
        for link in item.links:
            if link.rel == "root" and not link.title:
                link.title = "FESOM Catalog"
            elif link.rel == "collection" and not link.title:
                link.title = "FESOM Collection"
            elif link.rel == "parent" and not link.title:
                link.title = "FESOM Collection"

        all_datetimes.append(dt)
        ds.close()

    if all_datetimes:
        collection.extent.temporal.intervals = [
            [min(all_datetimes), max(all_datetimes)]
        ]

    # Add titles to collection's item links for STAC compliance
    for link in collection.links:
        if link.rel == "item" and not link.title:
            # Use target item's ID to create a meaningful title
            if link.target:
                item_id = link.target.id
                link.title = f"FESOM Item: {item_id}"

    catalog.normalize_and_save(
        root_href=str(output_dir),
        catalog_type=pystac.CatalogType.SELF_CONTAINED,
    )

    # Add titles to any remaining links without titles (created during normalize_and_save)
    for link in catalog.links:
        if link.rel == "root" and not link.title:
            link.title = "FESOM Catalog"

    for link in collection.links:
        if link.rel == "parent" and not link.title:
            link.title = "FESOM Catalog"

    for item in collection.get_all_items():
        for link in item.links:
            if link.rel == "parent" and not link.title:
                link.title = "FESOM Collection"

    # Save again to persist the title changes
    catalog.save(catalog_type=pystac.CatalogType.SELF_CONTAINED)

    print(f"\nFESOM STAC catalog written to: {output_dir.resolve()}")
