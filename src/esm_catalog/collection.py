"""STAC Collection for ESM-Tools experiments."""

from __future__ import annotations

from pystac import Collection, Extent, Item, SpatialExtent, TemporalExtent

from esm_catalog.contacts import add_contacts_collection_extension
from esm_catalog.models import ExperimentMetadata
from esm_catalog.namelist import add_namelist_collection_extension
from esm_catalog.paleo import add_paleo_collection_extension
from esm_catalog.types import BBox

# STAC-valid "coverage unknown" default; update_extent swaps it out on the first
# real item bbox (tracked by a flag, since a global item equals this by value).
DEFAULT_BBOX: BBox = [-180.0, -90.0, 180.0, 90.0]


def make_collection(exp_metadata: ExperimentMetadata) -> Collection:
    """Build the STAC Collection for an experiment.

    A Collection is the whole experiment: all of its components.

    Parameters
    ----------
    exp_metadata : ExperimentMetadata
        Experiment identity and pre-scanned config (experiment_id, components,
        contacts, namelists, paleo config).

    Returns
    -------
    pystac.Collection
        A Collection with a default global extent and no parent — the
        experiment is the catalog root. Its ``id`` is unique (see
        ``ExperimentMetadata.collection_id``); its ``title`` is the human
        experiment name.
    """
    collection = Collection(
        id=exp_metadata.collection_id,
        # STAC requires a string description; fall back to the name when None.
        description=exp_metadata.description or exp_metadata.experiment_id,
        extent=Extent(
            spatial=SpatialExtent(bboxes=[DEFAULT_BBOX]),
            temporal=TemporalExtent(intervals=[[None, None]]),
        ),
        title=exp_metadata.experiment_id,
        license=exp_metadata.data_license or "proprietary",
        extra_fields={"components": sorted(exp_metadata.components)},
    )
    add_contacts_collection_extension(collection, exp_metadata.contacts)
    add_namelist_collection_extension(collection, exp_metadata.namelists_by_component)
    add_paleo_collection_extension(collection, exp_metadata.paleo_config)
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
        if getattr(collection, "_bbox_from_items", False):
            collection.extent.spatial.bboxes = [_merge_bbox(current[0], item.bbox)]
        else:
            # First real bbox replaces the placeholder; flag it (runtime-only,
            # unserialized) so a global item is not read as the unset default.
            collection.extent.spatial.bboxes = [list(item.bbox)]
            collection._bbox_from_items = True

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
