"""Patch stac_geoparquet's StacJsonBatch.from_dicts to avoid a wasteful deepcopy.

``StacJsonBatch.from_dicts`` (stac_geoparquet.arrow._batch) deep-copies the
*whole* item dict per item -- every property, asset, and link -- purely to
swap ``geometry``/``proj:geometry`` to WKB bytes without mutating the
caller's original. For an experiment with a large flattened namelist
(thousands of ``nml__`` properties per item, identical across the run),
that full deepcopy dominates the whole writing phase: measured 105s of a
~108s ``write_shard`` call scanning 32,200 items on a real AWI-CM2 run
(``ncalls`` showed 62 million recursive `copy.deepcopy` calls -- ~1,925 per
item).

A shallow copy of the item dict, re-copying only the specific sub-dicts
that actually get a ``proj:geometry`` key mutated (and only when that key
is present), is behaviourally identical -- verified against the original
(same WKB output, same properties, original input dict left untouched) --
and ~94x faster in a synthetic reproduction (1.88ms/item -> 0.02ms/item at
~4,500 properties/item).

Filed nowhere upstream yet; this is a local patch, applied once at import
time, not a fork -- everything else in ``StacJsonBatch``/``stac_geoparquet``
is untouched. Guarded so a stac_geoparquet upgrade that changes
``from_dicts``'s shape fails loudly (ImportError) rather than silently
patching over a function that no longer matches what we tested against.
"""

from __future__ import annotations

import inspect
from typing import Any, Iterable, Optional, Union

import pyarrow as pa
import pystac
import shapely
from stac_geoparquet.arrow._batch import StacJsonBatch

_EXPECTED_SOURCE_MARKERS = ("wkb_item = deepcopy(item)", "wkb_items.append(wkb_item)")


def _patched_from_dicts(
    cls,
    items: Iterable[Union[pystac.Item, dict[str, Any]]],
    *,
    schema: Optional[pa.Schema] = None,
):
    wkb_items = []
    for item in items:
        if isinstance(item, pystac.Item):
            item = item.to_dict(transform_hrefs=False)

        # Shallow copy: only the keys we're about to mutate get their own
        # copy. `properties`/`assets` are reused by reference when they
        # carry no `proj:geometry` to swap -- that's the whole fix, since
        # `properties` is what's huge (thousands of nml__ entries).
        wkb_item = dict(item)
        wkb_item["geometry"] = shapely.to_wkb(
            shapely.geometry.shape(wkb_item["geometry"]), flavor="iso"
        )

        if "proj:geometry" in wkb_item["properties"]:
            wkb_item["properties"] = dict(wkb_item["properties"])
            wkb_item["properties"]["proj:geometry"] = shapely.to_wkb(
                shapely.geometry.shape(wkb_item["properties"]["proj:geometry"]),
                flavor="iso",
            )

        if any("proj:geometry" in v for v in wkb_item["assets"].values()):
            wkb_item["assets"] = {k: dict(v) for k, v in wkb_item["assets"].items()}
            for asset_value in wkb_item["assets"].values():
                if "proj:geometry" in asset_value:
                    asset_value["proj:geometry"] = shapely.to_wkb(
                        shapely.geometry.shape(asset_value["proj:geometry"]),
                        flavor="iso",
                    )

        wkb_items.append(wkb_item)

    if schema is not None:
        array = pa.array(wkb_items, type=pa.struct(schema))
    else:
        array = pa.array(wkb_items)

    return cls(pa.RecordBatch.from_struct_array(array))


def apply() -> None:
    """Replace ``StacJsonBatch.from_dicts`` with the shallow-copy version.

    Idempotent -- calling twice just reassigns the same classmethod. Verifies
    the original still looks like what this patch was written against before
    touching it, so a stac_geoparquet upgrade that reshapes ``from_dicts``
    raises instead of silently applying a stale patch.
    """
    original_source = inspect.getsource(StacJsonBatch.__dict__["from_dicts"].__func__)
    missing = [m for m in _EXPECTED_SOURCE_MARKERS if m not in original_source]
    if missing:
        raise ImportError(
            "stac_geoparquet.arrow._batch.StacJsonBatch.from_dicts no longer "
            f"matches what this patch expects (missing: {missing!r}) -- "
            "re-verify the deepcopy-avoidance fix still applies before "
            "patching over a changed upstream implementation."
        )
    StacJsonBatch.from_dicts = classmethod(_patched_from_dicts)


apply()
