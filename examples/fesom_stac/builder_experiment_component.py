"""
cat[experiment][component]

Uses ESM-Tools finished config as source of truth
"""

from datetime import datetime
from pathlib import Path

import click
import numpy as np
import pystac
from pystac.layout import TemplateLayoutStrategy
import xarray as xr
import yaml
from loguru import logger
from shapely.geometry import box, mapping


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
    try:
        initial_date = (
            np.datetime64(ds["time"].values[0]).astype("datetime64[ms]").item()
        )
    except ValueError:
        initial_date = initial_date
    try:
        final_date = (
            np.datetime64(ds["time"].values[-1]).astype("datetime64[ms]").item()
        )
    except ValueError:
        final_date = final_date

    return {"datetime": dt, "start_datetime": initial_date, "end_datetime": final_date}


def build_catalog(config_path, output_dir="catalog"):
    """
    Build catalog from ESM-Tools finished config

    Args:
        config_path: Path to *_finished_config.yaml file
        output_dir: Where to write the STAC catalog
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

        if not outdata_targets:
            # Some components (like fesom) don't populate outdata_targets
            # Fall back to experiment_outdata_dir
            experiment_outdata_dir = comp_config.get("experiment_outdata_dir")
            if not experiment_outdata_dir:
                logger.warning(f"Skipping {component}: no outdata_targets or experiment_outdata_dir")
                continue

            outdata_dir = Path(experiment_outdata_dir)
            if not outdata_dir.exists():
                logger.warning(f"Skipping {component}: {outdata_dir} does not exist")
                continue

            # Get all NetCDF files from directory
            nc_files = sorted(outdata_dir.glob("*.nc"))
            data_files = {f.stem: f for f in nc_files}
            logger.info(f"{component}: processing {len(data_files)} files (from directory glob)")
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

        # Process each data file
        for file_key, file_path in sorted(data_files.items()):
            if not file_path.exists():
                logger.debug(f"File in config doesn't exist: {file_path}")
                continue

            logger.info(f"Processing {file_path}")

            # Get asset metadata (media_type, xarray engine, etc.)
            asset_metadata = _get_asset_metadata(file_path)

            # Open dataset to extract metadata
            # Use the appropriate engine based on file type
            open_kwargs = {}
            if asset_metadata["extra_fields"]["xarray:engine"] == "cfgrib":
                open_kwargs["engine"] = "cfgrib"
                open_kwargs["backend_kwargs"] = {"indexpath": ""}

            with xr.open_dataset(file_path, **open_kwargs) as ds:
                variables = list(ds.data_vars)
                item_time_kwargs = _define_time_kwargs(ds, initial_date, final_date)

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

                # Check for .codes auxiliary file (GRIB code tables)
                codes_file = file_path.parent / f"{file_path.name}.codes"
                if codes_file.exists():
                    item.add_asset(
                        "codes",
                        pystac.Asset(
                            href=str(codes_file.resolve()),
                            media_type="text/plain",
                            roles=["metadata"],
                            title="GRIB code table",
                        ),
                    )

                collection.add_item(item)

        exp_cat.add_child(collection)

    root.add_child(exp_cat)

    # Use flat layout strategy to avoid nested directories
    layout_strategy = TemplateLayoutStrategy(
        item_template="${collection}/${id}.json"
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
def generate(cfg):
    click.echo("Generate catalog")
    build_catalog(
        # config_path="basic-001_finished_config.yaml_18500101-18500131",
        config_path=cfg,
        output_dir="stac_catalog",
    )


if __name__ == "__main__":
    cli()
