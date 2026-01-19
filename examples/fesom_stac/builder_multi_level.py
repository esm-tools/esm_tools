"""
STAC Catalog Builder - Multi-Level Hierarchical Strategy

This builder creates a hierarchical STAC catalog structure:
Root Catalog → Experiment Catalogs → Variable Collections → Items

STRUCTURE:
----------
root-catalog/
├── experiment-1/
│   ├── ssh-collection/
│   │   └── items...
│   ├── sst-collection/
│   │   └── items...
│   └── ...
├── experiment-2/
│   ├── ssh-collection/
│   │   └── items...
│   └── ...
└── ...

ADVANTAGES:
-----------
1. Experiment-level organization: Natural grouping by simulation/experiment
   - Easy to browse different experiments
   - Clear separation of different model runs
   - Supports multiple experiments in one catalog

2. Variable-level organization within experiments: Best of both worlds
   - Direct access to specific variables within an experiment
   - No filtering needed once you know the experiment
   - Intuitive for FESOM workflows

3. Scalability: Handles large numbers of experiments and variables
   - Each experiment is independent
   - Can add new experiments without affecting existing ones
   - Parallel processing of different experiments

4. Metadata at multiple levels: Rich contextual information
   - Root catalog: Overall dataset description
   - Experiment catalog: Experiment-specific metadata (configuration, dates)
   - Collection: Variable-specific metadata
   - Item: File-specific metadata

5. Flexible querying: Multiple access patterns
   - Browse all experiments
   - Access specific experiment directly
   - Query specific variable across experiments
   - Compare same variable across experiments

6. Better for collaboration: Team-friendly structure
   - Different team members work on different experiments
   - Easy to share specific experiments
   - Clear ownership and organization

DISADVANTAGES:
--------------
1. More complex structure: Three-level hierarchy
   - More directories and JSON files
   - Steeper learning curve for new users
   - More complex navigation

2. Deeper nesting: Longer paths to items
   - More clicks/steps to reach data
   - Longer file paths
   - More catalog objects to load

3. Cross-experiment queries: Harder to compare across experiments
   - Must query multiple experiment catalogs
   - More complex code for multi-experiment analysis
   - No single view of all data

4. Metadata duplication: Spatial extent repeated at multiple levels
   - Same mesh info in each experiment
   - Larger overall catalog size
   - More maintenance overhead

5. Initial setup complexity: More planning required
   - Need to decide experiment naming/organization
   - More configuration options
   - Harder to restructure later

WHEN TO USE THIS STRATEGY:
--------------------------
- You have multiple experiments/simulations to organize
- Experiments are logically separate (different configs, time periods, etc.)
- Users typically work with one experiment at a time
- You need experiment-level metadata (configuration, description)
- Team members focus on specific experiments
- You want to scale to many experiments over time
- Clear organizational structure is important

WHEN NOT TO USE:
----------------
- Single experiment or very few experiments
- Frequent cross-experiment comparisons needed
- Users are not familiar with hierarchical structures
- Simpler flat structure is preferred
- All data should be treated as one unified dataset
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


def create_root_catalog(catalog_id, description=None):
    """Create the root STAC Catalog.

    Args:
        catalog_id: Identifier for the root catalog
        description: Optional description for the catalog

    Returns:
        pystac.Catalog: Root catalog
    """
    if description is None:
        description = "Multi-level STAC catalog for FESOM experiments"

    catalog = pystac.Catalog(
        id=catalog_id,
        description=description,
    )

    return catalog


def create_experiment_catalog(experiment_id, description=None, extra_fields=None):
    """Create a STAC Catalog for a specific experiment.

    Args:
        experiment_id: Identifier for the experiment
        description: Optional description for the experiment
        extra_fields: Optional dict of additional metadata

    Returns:
        pystac.Catalog: Experiment catalog
    """
    if description is None:
        description = f"FESOM experiment: {experiment_id}"

    catalog = pystac.Catalog(
        id=experiment_id,
        description=description,
    )

    if extra_fields:
        catalog.extra_fields.update(extra_fields)

    return catalog


def create_collection_for_variable(variable_name, bbox, variable_metadata=None):
    """Create a STAC Collection for a specific variable.

    Args:
        variable_name: Name of the variable (e.g., 'ssh', 'sst')
        bbox: Bounding box for spatial extent
        variable_metadata: Optional dict with variable metadata (long_name, units, description)

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

    # Add variable metadata to collection properties for UI visibility
    if variable_metadata:
        collection.extra_fields["properties"] = {
            "variable": variable_name,
            **variable_metadata,
        }

    return collection


def build_multi_level_catalog(
    experiments,
    output_dir="multi_level_catalog",
    root_catalog_id="fesom-root-catalog",
    root_description=None,
):
    """Build a multi-level STAC catalog from FESOM NetCDF output files.

    Creates a hierarchical structure:
    Root Catalog → Experiment Catalogs → Variable Collections → Items

    Args:
        experiments: Dict mapping experiment_id to dict with:
            - 'data_dir': Path to NetCDF files for this experiment
            - 'description': Optional description of the experiment
            - 'metadata': Optional dict of experiment-specific metadata
        output_dir: Directory where STAC catalog will be written
        root_catalog_id: Identifier for the root catalog
        root_description: Optional description for the root catalog

    Returns:
        None. Writes catalog structure to output_dir and prints confirmation.

    Example:
        >>> experiments = {
        ...     'awiesm-basic-001': {
        ...         'data_dir': '/path/to/basic-001/fesom',
        ...         'description': 'AWIESM basic experiment 001',
        ...         'metadata': {'resolution': 'CORE2', 'years': '1850-1852'}
        ...     },
        ...     'awiesm-basic-002': {
        ...         'data_dir': '/path/to/basic-002/fesom',
        ...         'description': 'AWIESM basic experiment 002'
        ...     }
        ... }
        >>> build_multi_level_catalog(experiments)
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Create root catalog
    root_catalog = create_root_catalog(root_catalog_id, root_description)

    # Get default bbox for collections
    try:
        bbox, _ = fesom2_geometry()
    except Exception:
        bbox = FALLBACK_BBOX

    print(f"Building multi-level catalog with {len(experiments)} experiments")
    print("=" * 70)

    # Process each experiment
    for exp_id, exp_config in experiments.items():
        print(f"\nExperiment: {exp_id}")
        print("-" * 70)

        data_dir = Path(exp_config["data_dir"])
        exp_description = exp_config.get("description")
        exp_metadata = exp_config.get("metadata", {})

        # Create experiment catalog
        exp_catalog = create_experiment_catalog(exp_id, exp_description, exp_metadata)

        # Group files by variable name
        variable_files = defaultdict(list)

        for nc_file in sorted(data_dir.glob("*.nc")):
            variable_name = extract_variable_name(nc_file.name)
            variable_files[variable_name].append(nc_file)

        print(f"  Found {len(variable_files)} unique variables")

        # Create collections and items for each variable
        variable_datetimes = defaultdict(list)

        for variable_name, files in variable_files.items():
            print(f"    Variable: {variable_name} ({len(files)} files)")

            # Extract variable metadata from the first file
            variable_metadata = None
            if files:
                first_file = files[0]
                ds_meta = xr.open_dataset(first_file, decode_times=True)

                # Extract metadata from the variable
                for var_name, da in ds_meta.data_vars.items():
                    if (
                        var_name == variable_name
                        or var_name.lower() == variable_name.lower()
                    ):
                        variable_metadata = {}

                        # Get long_name or description
                        long_name = da.attrs.get("long_name") or da.attrs.get(
                            "description"
                        )
                        if long_name:
                            variable_metadata["long_name"] = long_name

                        # Get units
                        units = da.attrs.get("units")
                        if units:
                            variable_metadata["units"] = units

                        # Get description if different from long_name
                        description = da.attrs.get("description")
                        if description and description != long_name:
                            variable_metadata["description"] = description

                        # Add CF parameters
                        cf_params = extract_cf_parameters(ds_meta)
                        if cf_params:
                            variable_metadata["cf:parameter"] = cf_params

                        break

                ds_meta.close()

            # Create collection for this variable with metadata
            collection = create_collection_for_variable(
                variable_name, bbox, variable_metadata
            )

            # Process each file for this variable
            for nc_file in files:
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

            # Add collection to experiment catalog
            exp_catalog.add_child(collection)

        # Add experiment catalog to root catalog
        root_catalog.add_child(exp_catalog)

    print("\n" + "=" * 70)
    print("Adding titles to links...")

    # Add titles to root catalog links
    for link in root_catalog.links:
        if link.rel == "child" and not link.title:
            link.title = link.target.id if link.target else "Experiment"

    # Add titles to experiment catalog links
    for exp_catalog in root_catalog.get_children():
        for link in exp_catalog.links:
            if link.rel == "root" and not link.title:
                link.title = root_catalog_id
            elif link.rel == "parent" and not link.title:
                link.title = root_catalog_id
            elif link.rel == "child" and not link.title:
                link.title = link.target.id if link.target else "Collection"

        # Add titles to collection links
        for collection in exp_catalog.get_children():
            for link in collection.links:
                if link.rel == "root" and not link.title:
                    link.title = root_catalog_id
                elif link.rel == "parent" and not link.title:
                    link.title = exp_catalog.id

            # Add titles to item links
            for item in collection.get_all_items():
                for link in item.links:
                    if link.rel == "root" and not link.title:
                        link.title = root_catalog_id
                    elif link.rel == "collection" and not link.title:
                        link.title = collection.id
                    elif link.rel == "parent" and not link.title:
                        link.title = collection.id

    # Save catalog
    print("Saving catalog structure...")
    root_catalog.normalize_and_save(
        root_href=str(output_dir),
        catalog_type=pystac.CatalogType.SELF_CONTAINED,
    )

    # Add titles to any remaining links without titles (created during normalize_and_save)
    for link in root_catalog.links:
        if link.rel == "root" and not link.title:
            link.title = root_catalog_id

    for exp_catalog in root_catalog.get_children():
        for link in exp_catalog.links:
            if link.rel == "parent" and not link.title:
                link.title = root_catalog_id

        for collection in exp_catalog.get_children():
            for link in collection.links:
                if link.rel == "parent" and not link.title:
                    link.title = exp_catalog.id

            for item in collection.get_all_items():
                for link in item.links:
                    if link.rel == "parent" and not link.title:
                        link.title = collection.id

    # Save again to persist the title changes
    root_catalog.save(catalog_type=pystac.CatalogType.SELF_CONTAINED)

    print("\n" + "=" * 70)
    print(f"Multi-level FESOM STAC catalog written to: {output_dir.resolve()}")
    print(f"Structure: Root → {len(experiments)} Experiments → Collections → Items")
    print("=" * 70)
