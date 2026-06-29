"""STAC Collection object for ESM-Tools experiments."""

from __future__ import annotations

from datetime import datetime


class ESMCollection(dict):
    """A STAC Collection dict for a single ESM experiment component.

    Behaves as a plain dict (JSON-serialisable) but carries update_extent
    as an instance method so callers don't need to pass the collection around.
    """

    def __init__(self, ctx):
        super().__init__({
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
                {"rel": "parent", "href": f"#{ctx.experiment_id}", "type": "application/json"},
            ],
            "experiment": ctx.experiment_id,
            "components": [ctx.component],
        })

    def update_extent(self, item: dict) -> None:
        """Expand spatial and temporal extent to include *item*."""
        item_bbox = item.get("bbox")
        if item_bbox and len(item_bbox) == 4:
            current = self["extent"]["spatial"]["bbox"]
            if current == [[-180.0, -90.0, 180.0, 90.0]]:
                self["extent"]["spatial"]["bbox"] = [item_bbox]
            else:
                self["extent"]["spatial"]["bbox"] = [_merge_bbox(current[0], item_bbox)]

        props = item.get("properties", {})
        item_dt = props.get("datetime") or props.get("start_datetime")
        item_dt_end = props.get("end_datetime") or item_dt

        if item_dt:
            interval = self["extent"]["temporal"]["interval"][0]
            start, end = interval[0], interval[1]

            if start is None:
                start = item_dt
            else:
                if _before(_parse_iso(item_dt), _parse_iso(start)):
                    start = item_dt

            if end is None:
                end = item_dt_end
            else:
                if _before(_parse_iso(end), _parse_iso(item_dt_end)):
                    end = item_dt_end

            self["extent"]["temporal"]["interval"] = [[start, end]]


def _merge_bbox(a: list, b: list) -> list:
    return [min(a[0], b[0]), min(a[1], b[1]), max(a[2], b[2]), max(a[3], b[3])]


def _parse_iso(s: str | None) -> datetime | None:
    if not s:
        return None
    try:
        return datetime.fromisoformat(s.removesuffix("Z"))
    except (ValueError, AttributeError):
        return None


def _before(a: datetime | None, b: datetime | None) -> bool:
    return a is not None and b is not None and a < b
