"""Build a STAC Item dict from scan metadata and collection context."""

from __future__ import annotations

import hashlib
from datetime import timezone
from pathlib import Path, PurePosixPath
from typing import Union

from pystac import Asset, Item, Link
from upath import UPath

from esm_catalog.datacube import add_datacube_extension
from esm_catalog.paleo import add_paleo_data
from esm_catalog.registry import EXTENSION_URLS


def make_item(
    path: Union[Path, UPath, str],
    metadata: dict,
    ctx,
) -> dict:
    """Construct a STAC Item dict.

    Args:
        path:     Path to the source file (local Path, UPath, or URI string).
        metadata: Dict returned by scan_netcdf() or scan_grib().
        ctx:      CollectionContext with experiment_id, component, collection_id.

    Returns:
        pystac.item.Item object representing the STAC Item
    """
    if isinstance(path, str):
        path = UPath(path if "://" in path else Path(path).resolve())

    dt_start, dt_end, item_datetime, _, _ = _build_datetime(metadata)
    id = _make_id(
        metadata.get("variable", "unknown"),
        ctx.component,
        metadata.get("datetime_str", "000000"),
        path,
    )

    item = Item(
        id=id,
        geometry=metadata.get("geometry"),
        bbox=metadata.get("bbox"),
        datetime=item_datetime,
        properties=_build_properties(metadata, ctx),
        start_datetime=dt_start,
        end_datetime=dt_end,
        assets=_build_assets(path, metadata),
        collection=ctx.collection_id,
    )

    item.add_link(
        Link(
            rel="collection",
            target=f"#{ctx.collection_id}",
            media_type="application/json",
        )
    )

    add_contacts(item, ctx)
    add_datacube_extension(item, metadata)
    add_paleo_data(item, ctx.paleo_config)

    return item


def _make_id(
    variable: str, component: str, dt_str: str, path: Union[Path, UPath]
) -> str:
    """Return a stable unique item ID: {variable}.{component}.{datetime}.{hash}."""
    path_hash = hashlib.md5(str(path).encode()).hexdigest()[:6]
    return f"{variable}.{component}.{dt_str}.{path_hash}"


def _build_properties(metadata: dict, ctx) -> dict:
    """Assemble the STAC item properties dict from metadata and context."""

    properties: dict = {
        "variable": metadata.get("variable", "unknown"),
        "experiment": ctx.experiment_id,
        "component": ctx.component,
        "format": metadata.get("format", "unknown"),
    }
    if metadata.get("output_frequency"):
        properties["output_frequency"] = metadata["output_frequency"]

    all_var_names = [
        v["name"]
        for v in metadata.get("variables", [])
        if v.get("name") and v["name"] != "unknown"
    ]
    if len(all_var_names) > 1:
        properties["variables"] = all_var_names

    return properties


def _contact_to_stac(contact) -> dict:
    """Convert a Contact dataclass to STAC contacts extension format."""
    entry: dict = {"name": contact.name, "roles": list(contact.roles)}
    if contact.orcid:
        orcid = contact.orcid
        if not orcid.startswith("https://orcid.org/"):
            orcid = f"https://orcid.org/{orcid}"
        entry["identifier"] = orcid
    if contact.institution:
        entry["organization"] = contact.institution
    return entry


def add_contacts(item, ctx) -> None:
    """Inject contacts extension URL and properties into *item*."""
    if not ctx.contacts:
        return

    item.properties["contacts"] = [_contact_to_stac(c) for c in ctx.contacts]
    url = EXTENSION_URLS["contacts"]
    if url not in item.stac_extensions:
        item.stac_extensions.append(url)


def _build_datetime(metadata: dict) -> tuple:
    """Parse and normalise datetime fields from metadata.

    Returns:
        (dt_start, dt_end, item_datetime, start_datetime, end_datetime)
        where item_datetime is set for single-time files and
        start_datetime/end_datetime for multi-time files.
    """
    dt_start = metadata.get("datetime_start")
    dt_end = metadata.get("datetime_end")

    if dt_start and dt_start.tzinfo is None:
        dt_start = dt_start.replace(tzinfo=timezone.utc)
    if dt_end and dt_end.tzinfo is None:
        dt_end = dt_end.replace(tzinfo=timezone.utc)

    if dt_start == dt_end or dt_end is None:
        return dt_start, dt_end, dt_start if dt_start else None, None, None
    return (
        dt_start,
        dt_end,
        None,
        dt_start.isoformat() if dt_start else None,
        dt_end.isoformat() if dt_end else None,
    )


def _build_assets(path: Union[Path, UPath], metadata: dict) -> dict:
    """Build the STAC assets dict with a single 'data' asset for the source file."""
    fmt = metadata.get("format", "")
    media_type = "application/x-grib2" if fmt == "grib" else "application/x-netcdf"
    return {
        "data": Asset(
            href=_to_href(path),
            media_type=media_type,
            title=PurePosixPath(str(path)).name,
            roles=["data"],
        )
    }


def _to_href(path: Union[Path, UPath]) -> str:
    """Convert path to a STAC-compatible href (file:// or protocol URI)."""
    if hasattr(path, "protocol") and path.protocol and path.protocol != "file":
        protocol = path.protocol
        # UPath 0.3.x omits the host from str(path) for host-based protocols (ssh, sftp).
        # For bucket-based protocols (s3, gcs), str() is correct and storage_options has no host.
        host = getattr(path, "storage_options", {}).get("host", "")
        if host:
            return f"{protocol}://{host}{path.path}"
        uri = str(path)
        if uri.startswith(f"{protocol}:///"):
            raise ValueError(
                f"Cannot construct a valid URI for {path!r}: "
                f"'{protocol}' path has no host in storage_options."
            )
        return uri
    return path.as_uri()
