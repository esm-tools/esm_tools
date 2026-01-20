"""
cat[experiment][component]

Uses ESM-Tools finished config as source of truth
"""

import re
from datetime import datetime
from pathlib import Path

import click
import numpy as np
import pystac
from pystac.layout import TemplateLayoutStrategy
import xarray as xr
import yaml
from joblib import Parallel, delayed
from loguru import logger
from shapely.geometry import box, mapping


def _parse_codes_file(codes_path):
    """
    Parse ECHAM .codes file to extract variable names.

    Args:
        codes_path: Path to .codes file

    Returns:
        List of variable names (ECHAM naming convention)
    """
    variables = []
    with open(codes_path) as f:
        for line in f:
            parts = line.split()
            if len(parts) >= 3:
                short_name = parts[2]
                variables.append(short_name)
    return variables


def _get_asset_metadata(file_path):
    """
    Return media_type, roles, and xarray metadata for a file.

    Args:
        file_path: Path to the data file

    Returns:
        dict with media_type, roles, and extra_fields for xarray
    """
    suffix = file_path.suffix.lower()

    if suffix == '.nc':
        return {
            "media_type": "application/x-netcdf",
            "roles": ["data"],
            "extra_fields": {
                "xarray:engine": "netcdf4",
                "xarray:open_kwargs": {}
            }
        }
    else:  # GRIB files (often have no extension)
        return {
            "media_type": "application/x-grib",
            "roles": ["data"],
            "extra_fields": {
                "xarray:engine": "cfgrib",
                "xarray:open_kwargs": {
                    "backend_kwargs": {"indexpath": ""}
                },
                "xarray:storage_options": {
                    "multi_message": True,
                    "merge_datasets": True
                }
            }
        }


def _get_time_axis_from_dataset(ds):
    dt = None
    if "time" in ds:
        try:
            dt = np.datetime64(ds["time"].values[0]).astype("datetime64[ms]").item()
        except ValueError:
            logger.warning(
                f"Could not convert {ds['time'].values[0]} to datetime64[ms]"
            )
            logger.warning("Fallback to None")
            dt = None
    return dt


def _define_time_kwargs(ds, initial_date, final_date) -> dict:
    dt = _get_time_axis_from_dataset(ds)

    # Extract start and end times from dataset, handling both scalar and array times
    try:
        if "time" in ds and ds["time"].ndim == 0:
            # Scalar time coordinate (single timestep)
            time_val = np.datetime64(ds["time"].values).astype("datetime64[ms]").item()
            initial_date = time_val
            final_date = time_val
        elif "time" in ds:
            # Array time coordinate (multiple timesteps)
            initial_date = np.datetime64(ds["time"].values[0]).astype("datetime64[ms]").item()
            final_date = np.datetime64(ds["time"].values[-1]).astype("datetime64[ms]").item()
    except (ValueError, IndexError):
        # Keep the fallback dates from config
        pass

    return {"datetime": dt, "start_datetime": initial_date, "end_datetime": final_date}


def _process_file(file_path, component, initial_date, final_date):
    """
    Process a single data file and create a STAC item.

    Args:
        file_path: Path to data file
        component: Component name (e.g., "fesom", "echam")
        initial_date: Experiment initial date (fallback)
        final_date: Experiment final date (fallback)

    Returns:
        pystac.Item or None if file should be skipped
    """
    if not file_path.exists():
        logger.debug(f"File in config doesn't exist: {file_path}")
        return None

    # Skip empty files
    if file_path.stat().st_size == 0:
        logger.warning(f"Skipping empty file: {file_path}")
        return None

    logger.info(f"Processing {file_path}")

    # Get asset metadata (media_type, xarray engine, etc.)
    asset_metadata = _get_asset_metadata(file_path)

    # Check for .codes file (GRIB code tables with ECHAM variable names)
    # For ECHAM: codes files don't have date suffix, e.g.:
    #   Data: basic-001_185002.01_echam_18500201-18500228
    #   Codes: basic-001_185002.01_echam.codes
    # Strip date suffix pattern _YYYYMMDD-YYYYMMDD if present
    codes_base_name = re.sub(r'_\d{8}-\d{8}$', '', file_path.name)
    codes_file = file_path.parent / f"{codes_base_name}.codes"

    # Extract variables and temporal metadata
    if codes_file.exists() and asset_metadata["media_type"] == "application/x-grib":
        # Parse .codes file for ECHAM variable names (more reliable for GRIB)
        logger.info(f"Using .codes file for variable names: {codes_file}")
        variables = _parse_codes_file(codes_file)

        # Still need to open for temporal metadata, but handle cfgrib complexity
        try:
            import cfgrib
            datasets = cfgrib.open_datasets(
                file_path,
                backend_kwargs={"indexpath": ""}
            )
            # Use first dataset for time info
            ds = datasets[0] if datasets else xr.Dataset()
            item_time_kwargs = _define_time_kwargs(ds, initial_date, final_date)
        except Exception as e:
            logger.warning(f"Could not extract time from {file_path}: {e}")
            # Fallback to config dates
            item_time_kwargs = {
                "datetime": None,
                "start_datetime": initial_date,
                "end_datetime": final_date
            }
    else:
        # NetCDF or GRIB without .codes - open with xarray
        open_kwargs = {}
        if asset_metadata["extra_fields"]["xarray:engine"] == "cfgrib":
            open_kwargs["engine"] = "cfgrib"
            open_kwargs["backend_kwargs"] = {"indexpath": ""}

        try:
            with xr.open_dataset(file_path, **open_kwargs) as ds:
                variables = list(ds.data_vars)
                item_time_kwargs = _define_time_kwargs(ds, initial_date, final_date)
        except Exception as e:
            logger.error(f"Failed to open {file_path} with xarray: {e}")
            logger.warning(f"Skipping {file_path}")
            return None

    item = pystac.Item(
        id=file_path.stem,
        geometry=mapping(box(-180, -90, 180, 90)),
        bbox=[-180, -90, 180, 90],
        properties={"variables": variables, "component": component},
        **item_time_kwargs,
    )

    # Add primary data asset with xarray metadata
    item.add_asset(
        "data",
        pystac.Asset(
            href=str(file_path.resolve()),
            media_type=asset_metadata["media_type"],
            roles=asset_metadata["roles"],
            extra_fields=asset_metadata["extra_fields"],
        ),
    )

    # Add .codes auxiliary file if it exists (GRIB code tables with ECHAM names)
    if codes_file.exists():
        item.add_asset(
            "codes",
            pystac.Asset(
                href=str(codes_file.resolve()),
                media_type="text/plain",
                roles=["metadata"],
                title="ECHAM GRIB code table",
            ),
        )

    return item


def build_catalog(config_path, output_dir="catalog", n_jobs=-1):
    """
    Build catalog from ESM-Tools finished config

    Args:
        config_path: Path to *_finished_config.yaml file
        output_dir: Where to write the STAC catalog
        n_jobs: Number of parallel jobs (-1 = all cores, 1 = sequential)
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(exist_ok=True)

    # Load the config - this is our source of truth
    with open(config_path) as f:
        config = yaml.safe_load(f)

    # Extract experiment info
    expid = config["general"]["expid"]
    base_dir = Path(config["general"]["base_dir"])
    models = config["general"]["models"]
    initial_date = datetime.fromisoformat(config["general"]["initial_date"])
    final_date = datetime.fromisoformat(config["general"]["final_date"])

    # Load existing catalog or create new one
    catalog_file = output_dir / "catalog.json"
    if catalog_file.exists():
        logger.info(f"Loading existing catalog from {catalog_file}")
        root = pystac.Catalog.from_file(str(catalog_file))
    else:
        logger.info("Creating new catalog")
        root = pystac.Catalog(id="esm-tools-plus", description="ESM-Tools+ Demo Catalog")

    # Create experiment catalog with metadata from config
    exp_cat = pystac.Catalog(
        id=expid,
        description=f"Experiment {expid}",
        # [TODO]: Should this be something like meta-data?
        extra_fields={
            "model": config["general"]["model"],
            "version": config["general"]["version"],
            "resolution": config["general"]["resolution"],
            "scenario": config["general"]["scenario"],
            "beep": "boop",
        },
    )

    # Process each component
    for component in models:
        if component not in config:
            continue

        comp_config = config[component]

        # Use outdata_targets from config (already expanded from patterns)
        outdata_targets = comp_config.get("outdata_targets", {})

        # Check if we should use config or fall back to directory scan
        use_directory_scan = False

        if not outdata_targets:
            # No outdata_targets, must scan directory
            use_directory_scan = True
        else:
            # Check if any files from config actually exist
            existing_count = sum(1 for v in outdata_targets.values() if Path(v).exists())
            if existing_count == 0:
                logger.warning(
                    f"{component}: outdata_targets lists {len(outdata_targets)} files but none exist, "
                    f"falling back to directory scan"
                )
                use_directory_scan = True

        if use_directory_scan:
            experiment_outdata_dir = comp_config.get("experiment_outdata_dir")
            if not experiment_outdata_dir:
                logger.warning(f"Skipping {component}: no outdata_targets or experiment_outdata_dir")
                continue

            outdata_dir = Path(experiment_outdata_dir)
            if not outdata_dir.exists():
                logger.warning(f"Skipping {component}: {outdata_dir} does not exist")
                continue

            # Scan for all data files (both NetCDF and GRIB)
            all_files = sorted(outdata_dir.glob("*"))
            # Filter out .codes files, symlinks, and directories
            data_files = {
                f.stem: f for f in all_files
                if f.is_file() and not f.is_symlink() and not f.name.endswith('.codes')
            }
            logger.info(f"{component}: processing {len(data_files)} files (from directory scan)")
        else:
            # Use explicit file paths from config
            data_files = {k: Path(v) for k, v in outdata_targets.items()}
            logger.info(f"{component}: processing {len(data_files)} files (from outdata_targets)")

        # Create collection with component metadata
        collection = pystac.Collection(
            id=component,
            description=f"{component} output",
            extent=pystac.Extent(
                spatial=pystac.SpatialExtent([[-180, -90, 180, 90]]),
                temporal=pystac.TemporalExtent([[initial_date, final_date]]),
            ),
            extra_fields={
                "component_type": comp_config.get("type"),
                "version": comp_config.get("version"),
                "resolution": comp_config.get("resolution"),
            },
        )

        # Process files in parallel
        logger.info(f"Processing {len(data_files)} files with {n_jobs} jobs")
        file_paths = [path for _, path in sorted(data_files.items())]

        items = Parallel(n_jobs=n_jobs)(
            delayed(_process_file)(file_path, component, initial_date, final_date)
            for file_path in file_paths
        )

        # Filter out None (skipped files) and add to collection
        for item in items:
            if item is not None:
                collection.add_item(item)

        exp_cat.add_child(collection)

    # Check if experiment already exists and remove it
    try:
        existing_exp = root.get_child(expid, recursive=False)
        if existing_exp:
            logger.info(f"Replacing existing experiment catalog: {expid}")
            root.remove_child(expid)
    except KeyError:
        # Experiment doesn't exist yet, that's fine
        logger.info(f"Adding new experiment catalog: {expid}")

    root.add_child(exp_cat)

    # Use flat layout strategy to avoid nested directories
    layout_strategy = TemplateLayoutStrategy(
        catalog_template="${id}/catalog.json",
        collection_template="${id}/collection.json",
        item_template="${id}.json"
    )
    root.normalize_hrefs(str(output_dir), strategy=layout_strategy)
    root.save(catalog_type=pystac.CatalogType.SELF_CONTAINED)
    logger.success(f"\nCatalog saved to {output_dir.resolve()}")

    # ESM-Tools pattern: always return config
    return config


@click.group
def cli():
    pass


@cli.command
@click.argument("cfg", type=click.Path(exists=True))
@click.option("--n-jobs", "-j", default=-1, type=int, help="Number of parallel jobs (-1 = all cores)")
def generate(cfg, n_jobs):
    click.echo(f"Generate catalog (using {n_jobs} jobs)")
    build_catalog(
        config_path=cfg,
        output_dir="stac_catalog",
        n_jobs=n_jobs,
    )


if __name__ == "__main__":
    cli()
