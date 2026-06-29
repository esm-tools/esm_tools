"""Build a STAC Item dict from scan metadata and collection context."""

from __future__ import annotations

import hashlib
from datetime import timezone
from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from upath import UPath


class ESMItem(dict):
    """A STAC Item dict for a single ESM output file.

    Behaves as a plain dict (JSON-serialisable).
    """

    def __init__(self, path: "Path | UPath | str", metadata: dict, ctx):
        """Build the STAC Item from a file path, scan metadata, and collection context.

        Args:
            path:     Path to the source file (local Path, UPath, or URI string).
            metadata: Dict returned by scan_netcdf() or scan_grib().
            ctx:      CollectionContext with experiment_id, component, collection_id.
        """
        self.path = self._resolve_path(path)
        self.metadata = metadata
        self.ctx = ctx

        super().__init__({
            "type": "Feature",
            "stac_version": "1.0.0",
            "stac_extensions": [],
            "id": self._make_id(),
            "geometry": metadata.get("geometry"),
            "bbox": metadata.get("bbox"),
            "properties": self._build_properties(),
            "assets": self._build_assets(),
            "links": [
                {"rel": "collection", "href": f"#{ctx.collection_id}", "type": "application/json"}
            ],
            "collection": ctx.collection_id,
        })

    def _resolve_path(self, path: "Path | UPath | str") -> "Path | UPath":
        """Normalise *path* to a Path or UPath; parse URI strings via parse_uri."""
        if isinstance(path, str):
            from esm_catalog.uri import parse_uri
            return parse_uri(path)
        return path

    def _make_id(self) -> str:
        """Return a stable unique item ID: {variable}.{component}.{datetime}.{hash}."""
        variable = self.metadata.get("variable", "unknown")
        dt_str = self.metadata.get("datetime_str", "000000")
        path_hash = hashlib.md5(str(self.path).encode()).hexdigest()[:6]
        return f"{variable}.{self.ctx.component}.{dt_str}.{path_hash}"

    def _build_properties(self) -> dict:
        """Assemble the STAC item properties dict from metadata and context."""
        _, _, item_datetime, start_datetime, end_datetime = self._build_datetime()

        properties: dict = {
            "datetime": item_datetime,
            "variable": self.metadata.get("variable", "unknown"),
            "experiment": self.ctx.experiment_id,
            "component": self.ctx.component,
            "format": self.metadata.get("format", "unknown"),
        }
        if start_datetime:
            properties["start_datetime"] = start_datetime
        if end_datetime:
            properties["end_datetime"] = end_datetime
        if self.metadata.get("output_frequency"):
            properties["output_frequency"] = self.metadata["output_frequency"]

        all_var_names = [
            v["name"] for v in self.metadata.get("variables", [])
            if v.get("name") and v["name"] != "unknown"
        ]
        if len(all_var_names) > 1:
            properties["variables"] = all_var_names

        return properties

    def _build_datetime(self) -> tuple:
        """Parse and normalise datetime fields from metadata.

        Returns:
            (dt_start, dt_end, item_datetime, start_datetime, end_datetime)
            where item_datetime is set for single-time files and
            start_datetime/end_datetime for multi-time files.
        """
        dt_start = self.metadata.get("datetime_start")
        dt_end = self.metadata.get("datetime_end")

        if dt_start and dt_start.tzinfo is None:
            dt_start = dt_start.replace(tzinfo=timezone.utc)
        if dt_end and dt_end.tzinfo is None:
            dt_end = dt_end.replace(tzinfo=timezone.utc)

        if dt_start == dt_end or dt_end is None:
            return dt_start, dt_end, dt_start.isoformat() if dt_start else None, None, None
        return (
            dt_start, dt_end, None,
            dt_start.isoformat() if dt_start else None,
            dt_end.isoformat() if dt_end else None,
        )

    def _build_assets(self) -> dict:
        """Build the STAC assets dict with a single 'data' asset for the source file."""
        fmt = self.metadata.get("format", "")
        media_type = "application/x-grib2" if fmt == "grib" else "application/x-netcdf"
        return {
            "data": {
                "href": self._to_href(),
                "type": media_type,
                "title": PurePosixPath(str(self.path)).name,
                "roles": ["data"],
            }
        }

    def _to_href(self) -> str:
        """Convert self.path to a STAC-compatible href (file:// or protocol URI)."""
        if hasattr(self.path, "protocol") and self.path.protocol and self.path.protocol != "file":
            from esm_catalog.uri import to_uri
            return to_uri(self.path)
        return f"file://{Path(self.path).resolve()}"
