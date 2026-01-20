"""
cat[experiment][component]

Uses ESM-Tools finished config as source of truth
"""

from pathlib import Path

import numpy as np
import pystac
import xarray as xr
import yaml
from shapely.geometry import box, mapping


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

        # Build path to output data
        data_dir = base_dir / expid / "outdata" / component

        if not data_dir.exists():
            print(f"Skipping {component}: {data_dir} does not exist")
            continue

        # Create collection with component metadata
        collection = pystac.Collection(
            id=component,
            description=f"{component} output",
            extent=pystac.Extent(
                spatial=pystac.SpatialExtent([[-180, -90, 180, 90]]),
                temporal=pystac.TemporalExtent([[None, None]]),
            ),
            extra_fields={
                "component_type": comp_config.get("type"),
                "version": comp_config.get("version"),
                "resolution": comp_config.get("resolution"),
            },
        )

        # Add items
        nc_files = list(data_dir.glob("*.nc"))
        print(f"{component}: processing {len(nc_files)} files")

        for nc_file in sorted(nc_files):
            with xr.open_dataset(nc_file) as ds:

                variables = list(ds.data_vars)

                dt = None
                if "time" in ds:
                    dt = (
                        np.datetime64(ds["time"].values[0])
                        .astype("datetime64[ms]")
                        .item()
                    )

                item = pystac.Item(
                    id=nc_file.stem,
                    geometry=mapping(box(-180, -90, 180, 90)),
                    bbox=[-180, -90, 180, 90],
                    datetime=dt,
                    properties={"variables": variables, "component": component},
                )

                # [TODO]: Clarify if each "item" can have more than one asset??
                item.add_asset(
                    "data",
                    # [TODO] Figure out if we can have "file specific" things here? Date/variable??
                    pystac.Asset(
                        href=str(nc_file.resolve()),
                        media_type="application/x-netcdf",
                    ),
                )

                collection.add_item(item)

        exp_cat.add_child(collection)

    root.add_child(exp_cat)

    root.normalize_and_save(str(output_dir), pystac.CatalogType.SELF_CONTAINED)
    print(f"\nCatalog saved to {output_dir.resolve()}")

    # ESM-Tools pattern: always return config
    return config


if __name__ == "__main__":
    build_catalog(
        config_path="basic-001_finished_config.yaml_18500101-18500131",
        output_dir="stac_catalog",
    )
