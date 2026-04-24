"""Create and update STAC Collection objects."""

from datetime import datetime
from pathlib import Path

from loguru import logger


def make_collection(ctx, experiment_path: Path | str | None = None) -> dict:
    """Return a STAC Collection dict for the given CollectionContext.

    The collection is a minimal skeleton; its temporal and spatial extent
    are updated incrementally at insert time by update_collection_extent().

    Args:
        ctx: CollectionContext with experiment_id, component, collection_id.
        experiment_path: Optional path to experiment root for namelist scanning.
            If provided, namelists from config/{component}/ will be included.

    Returns:
        STAC Collection dict.
    """
    collection = {
        "type": "Collection",
        "id": ctx.collection_id,          # == ctx.experiment_id (Option A)
        "stac_version": "1.0.0",
        "stac_extensions": [],
        "title": ctx.experiment_id,
        "description": f"All model output for experiment {ctx.experiment_id}",
        "license": "proprietary",
        "extent": {
            "spatial": {"bbox": [[-180.0, -90.0, 180.0, 90.0]]},
            "temporal": {"interval": [[None, None]]},
        },
        "links": [
            {"rel": "parent", "href": f"#{ctx.experiment_id}", "type": "application/json"},
        ],
        # Custom fields for collection-level search
        "experiment": ctx.experiment_id,
        # components: list of model component names present in this experiment.
        # Populated with the first component at creation; subsequent components
        # are appended by CatalogDB.add_component_to_collection() as they are scanned.
        "components": [ctx.component],
    }

    # Scan namelists if experiment path is provided
    if experiment_path:
        collection = _add_namelists(collection, experiment_path, ctx.component)

    return collection


def _add_namelists(collection: dict, experiment_path: Path | str, component: str) -> dict:
    """Scan and add namelist data to collection."""
    try:
        from esm_catalog.scan.namelist import (
            extract_fesom_mesh_info,
            scan_namelist_directory,
        )
        from esm_catalog.stac.extensions.namelist import (
            add_namelist_extension,
            get_namelist_config_path,
        )

        experiment_path = Path(experiment_path)
        config_path = get_namelist_config_path(experiment_path, component)

        if config_path:
            namelists = scan_namelist_directory(config_path, component)
            if namelists:
                collection = add_namelist_extension(collection, namelists)
                logger.debug(
                    "Added namelists from {} to collection {}",
                    config_path,
                    collection["id"],
                )

            # Extract FESOM-specific mesh info
            if component.lower() in ("fesom", "fesom2"):
                fesom_info = extract_fesom_mesh_info(config_path)
                if fesom_info:
                    # Add FESOM mesh properties directly to collection
                    collection.update(fesom_info)
                    logger.debug(
                        "Added FESOM mesh info to collection {}",
                        collection["id"],
                    )
    except ImportError:
        # f90nml not installed - skip namelist scanning
        logger.debug("f90nml not available, skipping namelist scanning")
    except Exception as e:
        logger.warning("Failed to scan namelists for {}: {}", component, e)

    return collection


def update_collection_extent(collection: dict, item: dict) -> dict:
    """Expand collection spatial and temporal extent to include *item*.

    Modifies *collection* in-place and returns it.
    """
    # Spatial extent
    item_bbox = item.get("bbox")
    if item_bbox and len(item_bbox) == 4:
        current_bboxes = collection["extent"]["spatial"]["bbox"]
        if current_bboxes == [[-180.0, -90.0, 180.0, 90.0]]:
            # Still at global placeholder; replace with item's bbox
            collection["extent"]["spatial"]["bbox"] = [item_bbox]
        else:
            merged = _merge_bbox(current_bboxes[0], item_bbox)
            collection["extent"]["spatial"]["bbox"] = [merged]

    # Temporal extent
    props = item.get("properties", {})
    item_dt = props.get("datetime") or props.get("start_datetime")
    item_dt_end = props.get("end_datetime") or item_dt

    if item_dt:
        interval = collection["extent"]["temporal"]["interval"][0]
        start = interval[0]
        end = interval[1]

        item_dt_parsed = _parse_iso(item_dt)
        item_end_parsed = _parse_iso(item_dt_end)

        if start is None:
            start = item_dt
        else:
            start_parsed = _parse_iso(start)
            if item_dt_parsed and start_parsed and item_dt_parsed < start_parsed:
                start = item_dt

        if end is None:
            end = item_dt_end
        else:
            end_parsed = _parse_iso(end)
            if item_end_parsed and end_parsed and item_end_parsed > end_parsed:
                end = item_dt_end

        collection["extent"]["temporal"]["interval"] = [[start, end]]

    return collection


def _merge_bbox(a: list, b: list) -> list:
    """Return the bounding box that contains both a and b."""
    return [
        min(a[0], b[0]),
        min(a[1], b[1]),
        max(a[2], b[2]),
        max(a[3], b[3]),
    ]


def _parse_iso(s: str | None) -> datetime | None:
    if not s:
        return None
    try:
        return datetime.fromisoformat(s.rstrip("Z"))
    except (ValueError, AttributeError):
        return None
