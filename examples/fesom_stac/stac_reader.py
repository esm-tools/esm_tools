"""
STAC Catalog Reader Utility

Human-friendly functions to load ESM-Tools STAC catalogs using encoded xarray metadata.

Example usage:
    import intake
    from stac_reader import load_collection

    cat = intake.open_stac_catalog("catalog.json")
    fesom_ds = load_collection(cat["basic-001"]["fesom"])
"""

import xarray as xr
from loguru import logger


def _get_xarray_params(asset):
    """
    Extract xarray engine and kwargs from STAC asset metadata.

    Args:
        asset: pystac.Asset object

    Returns:
        dict with keys: engine, open_kwargs, storage_options
    """
    extra_fields = asset.extra_fields or {}
    return {
        "engine": extra_fields.get("xarray:engine", "netcdf4"),
        "open_kwargs": extra_fields.get("xarray:open_kwargs", {}),
        "storage_options": extra_fields.get("xarray:storage_options", {}),
    }


def load_asset(stac_item, asset_name="data"):
    """
    Load a single STAC item's asset using its encoded xarray metadata.

    Handles both NetCDF and GRIB files, with special support for GRIB
    multi-message files.

    Args:
        stac_item: intake_stac.StacItem or pystac.Item
        asset_name: Name of asset to load (default: "data")

    Returns:
        xarray.Dataset
    """
    # Handle both intake-stac wrapper and raw pystac items
    if hasattr(stac_item, "_stac_obj"):
        # intake-stac wrapper
        stac_obj = stac_item._stac_obj
    else:
        # Raw pystac item
        stac_obj = stac_item

    asset = stac_obj.assets[asset_name]
    href = asset.href

    # Extract xarray metadata
    params = _get_xarray_params(asset)
    engine = params["engine"]
    open_kwargs = params["open_kwargs"]
    storage_options = params["storage_options"]

    logger.debug(f"Loading {href} with engine={engine}")

    # Handle GRIB multi-message files
    if storage_options.get("multi_message"):
        try:
            import cfgrib
        except ImportError:
            logger.error("cfgrib is required for GRIB files but not installed")
            raise

        logger.debug("Using cfgrib.open_datasets for multi-message GRIB")
        datasets = cfgrib.open_datasets(href, **open_kwargs)

        if storage_options.get("merge_datasets"):
            logger.debug(f"Merging {len(datasets)} message groups")
            ds = xr.merge(datasets)
        else:
            logger.warning(f"Using first of {len(datasets)} message groups")
            ds = datasets[0]
    else:
        # Standard xarray opening (NetCDF or single-message GRIB)
        ds = xr.open_dataset(href, engine=engine, **open_kwargs)

    return ds


def load_collection(stac_collection, asset_name="data", concat_dim="time", chunks=None):
    """
    Load all items in a STAC collection and concatenate into single dataset.

    Args:
        stac_collection: intake_stac.StacCollection
        asset_name: Name of asset to load from each item (default: "data")
        concat_dim: Dimension to concatenate along (default: "time")
        chunks: Optional dict for dask chunking (default: None = in-memory)

    Returns:
        xarray.Dataset with all items concatenated
    """
    logger.info(f"Loading collection with {len(list(stac_collection))} items")

    datasets = []
    errors = []

    for item_id in stac_collection:
        try:
            logger.debug(f"Loading item: {item_id}")
            item = stac_collection[item_id]
            ds = load_asset(item, asset_name=asset_name)

            # Apply chunking if requested
            if chunks is not None:
                ds = ds.chunk(chunks)

            datasets.append(ds)
        except Exception as e:
            logger.warning(f"Failed to load {item_id}: {e}")
            errors.append((item_id, e))

    if not datasets:
        raise ValueError(f"No items could be loaded. Errors: {errors}")

    if errors:
        logger.warning(f"Loaded {len(datasets)}/{len(list(stac_collection))} items successfully")

    # Concatenate all datasets
    logger.info(f"Concatenating {len(datasets)} datasets along '{concat_dim}'")
    combined = xr.concat(datasets, dim=concat_dim)

    return combined
