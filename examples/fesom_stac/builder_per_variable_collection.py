"""
STAC Catalog Builder - One Collection Per Variable Strategy

This builder creates a separate STAC collection for each variable type (e.g., ssh, sst, MLD1).

ADVANTAGES:
-----------
1. Simpler querying: Loading all files of a specific variable is straightforward
   - No need to filter items by variable name
   - Direct access: cat['ssh-collection'] gets all SSH data

2. Better metadata organization: Each collection can have variable-specific metadata
   - Units, standard_name, and CF parameters specific to that variable
   - Clearer description of what each collection contains

3. Clearer temporal extents: Each variable may have different time coverage
   - Some variables might be output more/less frequently
   - Temporal extent accurately reflects availability per variable

4. Easier discovery: Users can browse by variable type directly
   - Catalog structure mirrors scientific workflow (analyze by variable)
   - Intuitive for domain scientists

5. Performance: Smaller collections load faster in catalog browsers
   - Reduced memory footprint when working with specific variables
   - Faster item iteration within a collection

6. Parallelization: Different variables can be processed independently
   - Better for distributed workflows
   - Each collection can be updated separately

DISADVANTAGES:
--------------
1. More complex catalog structure: More collections to manage
   - More JSON files to maintain
   - Catalog root has many child collections

2. Harder cross-variable queries: If you need multiple variables together
   - Must query multiple collections
   - More complex code for multi-variable analysis

3. Redundant spatial metadata: All collections share the same mesh/spatial extent
   - Duplicated bbox and geometry information
   - Larger overall catalog size

4. Collection proliferation: Many variables = many collections
   - Can become unwieldy with 50+ variables
   - Harder to get overview of entire dataset

5. Temporal alignment complexity: Comparing temporal coverage across variables
   - Need to check multiple collections
   - No single place to see overall temporal extent

WHEN TO USE THIS STRATEGY:
--------------------------
- Variables are typically analyzed independently
- Time series analysis is variable-specific
- Users know which variable they need before querying
- Dataset has moderate number of variables (< 50)
- Performance is important for large catalogs

WHEN TO USE SINGLE COLLECTION:
------------------------------
- Frequent multi-variable analysis
- Small number of total items (< 1000)
- Variables always have same temporal coverage
- Users need to discover what variables exist
- Simpler catalog structure is preferred
"""

import json
from collections import defaultdict
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


def extract_variable_name(filename):
    """Extract variable name from FESOM filename.

    FESOM files follow pattern: {variable}.fesom.{date}.{suffix}
    Example: ssh.fesom.185001.01 -> ssh

    Args:
        filename: Name of the NetCDF file (with or without extension)

    Returns:
        str: Variable name extracted from filename
    """
    stem = Path(filename).stem
    parts = stem.split(".")
    if len(parts) > 0:
        return parts[0]
    return "unknown"


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


def initialize_catalog(catalog_id):
    """Initialize STAC Catalog structure.

    Creates a catalog that will contain multiple collections (one per variable).

    Args:
        catalog_id: Identifier for the STAC Catalog.

    Returns:
        pystac.Catalog: Empty catalog ready to have collections added
    """
    catalog = pystac.Catalog(
        id=catalog_id,
        description="STAC catalog for FESOM mesh-based NetCDF output (organized by variable)",
    )

    return catalog


def create_collection_for_variable(variable_name, bbox):
    """Create a STAC Collection for a specific variable.

    Args:
        variable_name: Name of the variable (e.g., 'ssh', 'sst')
        bbox: Bounding box for spatial extent

    Returns:
        pystac.Collection: Collection initialized with spatial extent,
                          temporal extent will be updated later
    """
    collection = pystac.Collection(
        id=f"{variable_name}-collection",
        description=f"FESOM unstructured mesh output for variable: {variable_name}",
        extent=pystac.Extent(
            spatial=pystac.SpatialExtent([bbox]),
            temporal=pystac.TemporalExtent([[None, None]]),
        ),
        license="proprietary",
    )

    return collection


def build_fesom_stac(
    data_dir,
    output_dir="stac",
    catalog_id="fesom-catalog",
):
    """Build a STAC catalog from FESOM NetCDF output files with one collection per variable.

    Processes all .nc files in the specified directory and creates a complete
    STAC catalog with:
    - One catalog containing multiple collections (one per variable type)
    - One STAC Item per NetCDF file with CF extension
    - Spatial extents from FESOM mesh geometry
    - Temporal extents from file timestamps (per variable)
    - CF parameter metadata extracted from variable attributes

    The catalog is saved as a self-contained structure with relative links.

    Args:
        data_dir: Directory containing FESOM NetCDF files (*.nc).
        output_dir: Directory where STAC catalog will be written. Default: "stac".
        catalog_id: Identifier for the STAC Catalog. Default: "fesom-catalog".

    Returns:
        None. Writes catalog structure to output_dir and prints confirmation.

    Example:
        >>> build_fesom_stac(
        ...     data_dir="/path/to/fesom/output",
        ...     output_dir="my_stac_catalog"
        ... )
        Processing ssh.fesom.185001.01.nc -> ssh-collection
        Processing ssh.fesom.185002.01.nc -> ssh-collection
        Processing sst.fesom.185001.01.nc -> sst-collection
        ...
        Created 15 collections for 15 variables
        FESOM STAC catalog written to: /full/path/to/my_stac_catalog
    """
    data_dir = Path(data_dir)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    catalog = initialize_catalog(catalog_id)

    # Get default bbox for collections
    try:
        bbox, _ = fesom2_geometry()
    except Exception:
        bbox = FALLBACK_BBOX

    # Group files by variable name
    variable_files = defaultdict(list)

    for nc_file in sorted(data_dir.glob("*.nc")):
        variable_name = extract_variable_name(nc_file.name)
        variable_files[variable_name].append(nc_file)

    print(f"Found {len(variable_files)} unique variables")

    # Create a collection for each variable
    collections = {}
    variable_datetimes = defaultdict(list)

    for variable_name, files in variable_files.items():
        print(f"\nProcessing variable: {variable_name} ({len(files)} files)")

        # Create collection for this variable
        collection = create_collection_for_variable(variable_name, bbox)
        collections[variable_name] = collection

        # Process each file for this variable
        for nc_file in files:
            print(f"  - {nc_file.name}")

            ds = xr.open_dataset(nc_file, decode_times=True)
            bbox_item, geometry = get_mesh_geometry(ds)

            item, dt = create_stac_item(nc_file, ds, bbox_item, geometry)

            try:
                item.validate()
            except Exception:
                pass

            collection.add_item(item)
            variable_datetimes[variable_name].append(dt)
            ds.close()

        # Update temporal extent for this collection
        if variable_datetimes[variable_name]:
            collection.extent.temporal.intervals = [
                [
                    min(variable_datetimes[variable_name]),
                    max(variable_datetimes[variable_name]),
                ]
            ]

        # Add collection to catalog
        catalog.add_child(collection)

    # Add titles to catalog links
    for link in catalog.links:
        if link.rel == "child" and not link.title:
            link.title = link.target.id if link.target else "FESOM Collection"

    # Add titles to collection links
    for collection in collections.values():
        for link in collection.links:
            if link.rel == "root" and not link.title:
                link.title = catalog_id
            elif link.rel == "parent" and not link.title:
                link.title = catalog_id

        # Add titles to item links
        for item in collection.get_all_items():
            for link in item.links:
                if link.rel == "root" and not link.title:
                    link.title = catalog_id
                elif link.rel == "collection" and not link.title:
                    link.title = collection.id
                elif link.rel == "parent" and not link.title:
                    link.title = collection.id

    # Save catalog
    catalog.normalize_and_save(
        root_href=str(output_dir),
        catalog_type=pystac.CatalogType.SELF_CONTAINED,
    )

    # Add titles to any remaining links without titles (created during normalize_and_save)
    for link in catalog.links:
        if link.rel == "root" and not link.title:
            link.title = catalog_id

    for collection in collections.values():
        for link in collection.links:
            if link.rel == "parent" and not link.title:
                link.title = catalog_id

        for item in collection.get_all_items():
            for link in item.links:
                if link.rel == "parent" and not link.title:
                    link.title = collection.id

    # Save again to persist the title changes
    catalog.save(catalog_type=pystac.CatalogType.SELF_CONTAINED)

    print(f"\n{'='*60}")
    print(f"Created {len(collections)} collections for {len(variable_files)} variables")
    print(f"FESOM STAC catalog written to: {output_dir.resolve()}")
    print(f"{'='*60}")
