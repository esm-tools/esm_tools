"""STAC Collection for ESM-Tools experiments."""

from __future__ import annotations

from pystac import Collection, Extent, Item, SpatialExtent, TemporalExtent

from esm_catalog.models import ExperimentMetadata
from esm_catalog.namelist import add_namelist_collection_extension
from esm_catalog.paleo import add_paleo_collection_extension
from esm_catalog.types import BBox


def make_collection(exp_metadata: ExperimentMetadata) -> Collection:
    """Construct a pystac Collection for an experiment.

    A Collection *is* the experiment — all of its components — so it carries
    every component's namelists, not one.

    Parameters
    ----------
    exp_metadata : ExperimentMetadata
        Experiment identity and pre-scanned config (experiment_id, namelists,
        paleo config).

    Returns
    -------
    pystac.Collection
        A Collection with default global extent. Its parent (the root catalog)
        is set by the catalog-assembly layer via ``add_child``, not here.
    """
    collection = Collection(
        id=exp_metadata.experiment_id,
        description=exp_metadata.description,
        extent=Extent(
            spatial=SpatialExtent(bboxes=[[-180.0, -90.0, 180.0, 90.0]]),
            temporal=TemporalExtent(intervals=[[None, None]]),
        ),
        title=exp_metadata.experiment_id,
        license=exp_metadata.data_license or "proprietary",
        extra_fields={"components": sorted(exp_metadata.namelists_by_component)},
    )
    all_namelists = {
        filename: namelist
        for component_files in exp_metadata.namelists_by_component.values()
        for filename, namelist in component_files.items()
    }
    add_namelist_collection_extension(collection, all_namelists)
    add_paleo_collection_extension(collection, exp_metadata.paleo_config)
    # [NOTE] (LLM Claude-Opus 4.8): decode-companion assets shared by all items —
    # GRIB code/parameter tables, the FESOM/ICON mesh, remap weights — belong
    # here as Collection-level assets (one per experiment), not on each item.
    return collection


def update_extent(collection: Collection, item: Item) -> None:
    """Expand the spatial and temporal extent of *collection* to include *item*.

    Parameters
    ----------
    collection : pystac.Collection
        The collection whose extent is widened in place.
    item : pystac.Item
        The item whose bbox and datetime the extent must cover.
    """
    if item.bbox and len(item.bbox) == 4:
        current = collection.extent.spatial.bboxes
        if current == [[-180.0, -90.0, 180.0, 90.0]]:
            collection.extent.spatial.bboxes = [list(item.bbox)]
        else:
            collection.extent.spatial.bboxes = [_merge_bbox(current[0], item.bbox)]

    item_dt = item.datetime or item.common_metadata.start_datetime
    item_dt_end = item.common_metadata.end_datetime or item_dt

    if item_dt:
        start, end = collection.extent.temporal.intervals[0]

        if start is None or item_dt < start:
            start = item_dt
        if end is None or (item_dt_end and item_dt_end > end):
            end = item_dt_end

        collection.extent.temporal.intervals = [[start, end]]


def _merge_bbox(first: BBox, second: BBox) -> BBox:
    """Return the smallest bbox ([west, south, east, north]) containing both inputs.

    Parameters
    ----------
    first, second : BBox
        The two bounding boxes to union.

    Returns
    -------
    BBox
        The bounding box that encloses both.
    """
    return [
        min(first[0], second[0]),
        min(first[1], second[1]),
        max(first[2], second[2]),
        max(first[3], second[3]),
    ]
