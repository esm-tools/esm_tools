"""Create and update STAC Collection objects."""

from __future__ import annotations

from datetime import datetime

from esm_catalog.stac.extensions.namelist import add_namelist_extension


def make_collection(ctx, namelists: dict | None = None,
                    fesom_info: dict | None = None) -> dict:
    """Return a STAC Collection dict for the given CollectionContext.

    Args:
        ctx: CollectionContext (experiment_id, component, collection_id).
        namelists: Optional pre-scanned namelists ({filename -> {group -> {k: v}}}).
            Scanned by the scan layer and passed in; this function never scans.
        fesom_info: Optional pre-extracted FESOM mesh fields to merge in.

    Returns:
        STAC Collection dict.
    """
    collection = {
        "type": "Collection",
        "id": ctx.collection_id,
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
            {"rel": "parent", "href": f"#{ctx.experiment_id}",
             "type": "application/json"},
        ],
        "experiment": ctx.experiment_id,
        "components": [ctx.component],
    }

    if namelists:
        collection = add_namelist_extension(collection, namelists)
    if fesom_info:
        collection.update(fesom_info)

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
