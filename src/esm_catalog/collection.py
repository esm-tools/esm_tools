"""STAC Collection for ESM-Tools experiments."""

from __future__ import annotations

from pystac import Collection, Extent, Item, Link, SpatialExtent, TemporalExtent


def make_collection(ctx) -> Collection:
    """Construct a pystac Collection for an experiment component.

    Args:
        ctx: CollectionContext with experiment_id, component, collection_id.

    Returns:
        A pystac.Collection with default global extent.
    """
    col = Collection(
        id=ctx.collection_id,
        description=f"All model output for experiment {ctx.experiment_id}",
        extent=Extent(
            spatial=SpatialExtent(bboxes=[[-180.0, -90.0, 180.0, 90.0]]),
            temporal=TemporalExtent(intervals=[[None, None]]),
        ),
        title=ctx.experiment_id,
        license="proprietary",
        extra_fields={"experiment": ctx.experiment_id, "components": [ctx.component]},
    )
    col.add_link(Link("parent", f"#{ctx.experiment_id}", media_type="application/json"))
    return col


def update_extent(collection: Collection, item: Item) -> None:
    """Expand spatial and temporal extent of collection to include item."""
    if item.bbox and len(item.bbox) == 4:
        current = collection.extent.spatial.bboxes
        if current == [[-180.0, -90.0, 180.0, 90.0]]:
            collection.extent.spatial.bboxes = [list(item.bbox)]
        else:
            collection.extent.spatial.bboxes = [_merge_bbox(current[0], item.bbox)]

    item_dt = item.datetime or item.common_metadata.start_datetime
    item_dt_end = item.common_metadata.end_datetime or item_dt

    if item_dt:
        interval = collection.extent.temporal.intervals[0]
        start, end = interval[0], interval[1]

        if start is None or item_dt < start:
            start = item_dt
        if end is None or (item_dt_end and item_dt_end > end):
            end = item_dt_end

        collection.extent.temporal.intervals = [[start, end]]


def _merge_bbox(a: list, b: list) -> list:
    return [min(a[0], b[0]), min(a[1], b[1]), max(a[2], b[2]), max(a[3], b[3])]
