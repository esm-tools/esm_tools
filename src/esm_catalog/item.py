"""Build a STAC Item from file metadata and experiment metadata."""

from __future__ import annotations

import hashlib
from datetime import datetime, timezone
from pathlib import Path, PurePosixPath

from pystac import Asset, Item, Link
from upath import UPath

from esm_catalog.contacts import add_contacts_item_extension
from esm_catalog.datacube import add_datacube_item_extension
from esm_catalog.models import ExperimentMetadata
from esm_catalog.namelist import add_namelist_item_extension
from esm_catalog.paleo import add_paleo_item_extension
from esm_catalog.types import FileMetadata, Href

ItemId = str
"""A STAC Item id, e.g. 'tas.echam.20000101.a1b2c3'."""


def make_item(
    path: Path | UPath | str,
    file_metadata: FileMetadata,
    exp_metadata: ExperimentMetadata,
) -> Item:
    """Construct a STAC Item for a single output file.

    Parameters
    ----------
    path : Path or UPath or str
        Path to the source file (local Path, UPath, or URI string).
    file_metadata : FileMetadata
        The file's scanned metadata (scan_netcdf/scan_grib output).
    exp_metadata : ExperimentMetadata
        Experiment identity and pre-scanned config (experiment_id, namelists,
        paleo config, contacts).

    Returns
    -------
    pystac.Item
        A STAC Item for the file, with the datacube, contacts, namelist, and
        paleo extensions applied where the metadata warrants them.
    """
    if isinstance(path, str):
        path = UPath(path if "://" in path else Path(path).resolve())

    dt_start, dt_end, item_datetime = _build_datetime(file_metadata)
    item_id = _build_id(
        file_metadata.variable or "unknown",
        file_metadata.component or "unknown",
        file_metadata.datetime_str or "000000",
        path,
    )

    item = Item(
        id=item_id,
        geometry=file_metadata.geometry,
        bbox=file_metadata.bbox,
        datetime=item_datetime,
        properties=_build_properties(file_metadata, exp_metadata),
        start_datetime=dt_start,
        end_datetime=dt_end,
        assets={"data": _build_data_asset(path, file_metadata)},
        collection=exp_metadata.experiment_id,
    )

    item.add_link(
        Link(
            rel="collection",
            target=f"#{exp_metadata.experiment_id}",
            media_type="application/json",
        )
    )

    add_contacts_item_extension(item, exp_metadata.contacts)
    add_datacube_item_extension(item, file_metadata)
    add_namelist_item_extension(item, exp_metadata.namelists_by_component)
    add_paleo_item_extension(item, exp_metadata.paleo_config)

    return item


def _build_id(
    variable: str, component: str, datetime_str: str, path: Path | UPath
) -> ItemId:
    """Build a stable unique item id of the form {variable}.{component}.{datetime_str}.{hash}.

    Parameters
    ----------
    variable : str
        The primary data variable name.
    component : str
        The model component that produced the file.
    datetime_str : str
        The file's nominal timestamp, already formatted for the id.
    path : Path or UPath
        The source file path, hashed to disambiguate otherwise-identical ids.

    Returns
    -------
    ItemId
        The composed item id.
    """
    path_hash = hashlib.md5(str(path).encode()).hexdigest()[:6]
    return f"{variable}.{component}.{datetime_str}.{path_hash}"


def _build_properties(
    file_metadata: FileMetadata, exp_metadata: ExperimentMetadata
) -> dict:
    """Assemble the STAC item properties from file metadata and experiment.

    Parameters
    ----------
    file_metadata : FileMetadata
        The file's scanned metadata.
    exp_metadata : ExperimentMetadata
        The owning experiment, source of the ``experiment`` property.

    Returns
    -------
    dict
        The STAC item properties. A ``variables`` list is added only when the
        file holds more than one named data variable.
    """

    properties: dict = {
        "variable": file_metadata.variable or "unknown",
        "experiment": exp_metadata.experiment_id,
        "component": file_metadata.component or "unknown",
        "format": file_metadata.format or "unknown",
    }
    if file_metadata.output_frequency:
        properties["output_frequency"] = file_metadata.output_frequency

    variable_names = [
        variable.name
        for variable in file_metadata.variables
        if variable.name and variable.name != "unknown"
    ]
    if len(variable_names) > 1:
        properties["variables"] = variable_names

    return properties


def _build_datetime(
    file_metadata: FileMetadata,
) -> tuple[datetime | None, datetime | None, datetime | None]:
    """Parse and normalise the datetime fields from file metadata.

    Naive datetimes are assumed UTC and made timezone-aware.

    Parameters
    ----------
    file_metadata : FileMetadata
        The file's scanned metadata.

    Returns
    -------
    tuple of (datetime or None, datetime or None, datetime or None)
        ``(dt_start, dt_end, item_datetime)``. ``item_datetime`` is set only for
        a single-time file (start == end, or no end), otherwise None.
    """
    dt_start = file_metadata.datetime_start
    dt_end = file_metadata.datetime_end
    if dt_start and dt_start.tzinfo is None:
        dt_start = dt_start.replace(tzinfo=timezone.utc)
    if dt_end and dt_end.tzinfo is None:
        dt_end = dt_end.replace(tzinfo=timezone.utc)

    single = dt_end is None or dt_start == dt_end
    return dt_start, dt_end, (dt_start if single else None)


# [NOTE] (LLM Claude-Opus 4.8): further per-file assets (thumbnail, metadata
# sidecar, kerchunk/zarr index) join the item at the make_item assembly site as
# extra keys alongside "data". Decode-companions shared across all items — GRIB
# code/parameter tables, the unstructured mesh/grid, remap weights — are
# one-per-experiment; put those on the Collection (see make_collection), not
# here, so they are not duplicated across every item.
def _build_data_asset(path: Path | UPath, file_metadata: FileMetadata) -> Asset:
    """Build the single ``data`` asset for the source file.

    Parameters
    ----------
    path : Path or UPath
        The source file path, used for the asset href and title.
    file_metadata : FileMetadata
        The file's scanned metadata; its ``format`` selects the media type.

    Returns
    -------
    pystac.Asset
        The file's data asset.
    """
    file_format = file_metadata.format or ""
    media_type = (
        "application/x-grib2" if file_format == "grib" else "application/x-netcdf"
    )
    return Asset(
        href=_to_href(path),
        media_type=media_type,
        title=PurePosixPath(str(path)).name,
        roles=["data"],
    )


def _to_href(path: Path | UPath) -> Href:
    """Convert a path to a STAC-compatible href (file:// or protocol URI).

    Parameters
    ----------
    path : Path or UPath
        The source file path.

    Returns
    -------
    Href
        A ``file://`` URI for local paths, or a protocol URI (with host where
        the storage options provide one) for remote paths.

    Raises
    ------
    ValueError
        If a host-based remote path carries no host in its storage options, so
        no valid URI can be constructed.
    """
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
    # Local path: as_uri() rejects relative paths, so resolve to absolute first.
    if not path.is_absolute():
        path = Path(path).resolve()
    return path.as_uri()
