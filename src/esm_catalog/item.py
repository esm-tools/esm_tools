"""Build a STAC Item from file metadata and experiment metadata."""

from __future__ import annotations

import hashlib
from datetime import datetime, timezone
from pathlib import Path, PurePosixPath
from typing import Optional

from pystac import Asset, Item, Link
from upath import UPath

from esm_catalog.datacube import add_datacube_item_extension
from esm_catalog.models import ExperimentMetadata
from esm_catalog.namelist import add_namelist_item_extension
from esm_catalog.paleo import add_paleo_item_extension
from esm_catalog.types import FileMetadata, Href

ItemId = str
"""A STAC Item id, e.g. 'tas.echam.20000101.a1b2c3'."""

FX_FREQUENCY = "fx"
"""The ``properties.frequency`` value marking a time-invariant item (CMIP 'fx');
the scan layer routes these to the mutable fx shard."""


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
        The file's scanned metadata (a Reader's output, e.g. NetCdfReader).
    exp_metadata : ExperimentMetadata
        Experiment identity and pre-scanned config (experiment_id, namelists,
        paleo config). Contacts are set on the Collection.

    Returns
    -------
    pystac.Item
        A STAC Item for the file, with the datacube, namelist, and paleo
        extensions applied where the metadata warrants them. A time-invariant
        (fx) file, one with no per-file datetime, is placed across the
        experiment's run span and marked ``frequency == "fx"``.

    Raises
    ------
    ValueError
        If the file has no per-file datetime and the experiment has no run span
        (``run_start``/``run_end``) to place it across.
    """
    if isinstance(path, str):
        path = UPath(path if "://" in path else Path(path).resolve())

    dt_start, dt_end, item_datetime, is_fx = _build_datetime(
        file_metadata, exp_metadata
    )
    # A time-varying file stamps its id from its own datetime; a time-invariant
    # (fx) file has none, so it borrows the experiment run span's start
    # (dt_start, set to run_start by _build_datetime) -- the stamp is never
    # guessed from the filename.
    id_stamp = file_metadata.get("datetime_str") or dt_start.strftime("%Y%m")
    item_id = _build_id(
        file_metadata.get("variable", "unknown"),
        file_metadata.get("component", "unknown"),
        id_stamp,
        path,
    )

    properties = _build_properties(file_metadata, exp_metadata)
    if is_fx:
        properties["frequency"] = FX_FREQUENCY

    item = Item(
        id=item_id,
        geometry=file_metadata.get("geometry"),
        bbox=file_metadata.get("bbox"),
        datetime=item_datetime,
        properties=properties,
        start_datetime=dt_start,
        end_datetime=dt_end,
        assets={"data": _build_data_asset(path, file_metadata)},
        collection=exp_metadata.collection_id,
    )

    item.add_link(
        Link(
            rel="collection",
            target=f"#{exp_metadata.collection_id}",
            media_type="application/json",
        )
    )

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
        "variable": file_metadata.get("variable", "unknown"),
        "experiment": exp_metadata.experiment_id,
        "component": file_metadata.get("component", "unknown"),
        "format": file_metadata.get("format", "unknown"),
    }
    if file_metadata.get("frequency"):
        properties["frequency"] = file_metadata["frequency"]

    variable_names = [
        variable["name"]
        for variable in file_metadata.get("variables", [])
        if variable.get("name") and variable["name"] != "unknown"
    ]
    if len(variable_names) > 1:
        properties["variables"] = variable_names

    return properties


def _build_datetime(
    file_metadata: FileMetadata,
    exp_metadata: ExperimentMetadata,
) -> tuple[Optional[datetime], Optional[datetime], Optional[datetime], bool]:
    """Parse and normalise the datetime fields for the item.

    Naive datetimes are assumed UTC and made timezone-aware.

    A time-varying file carries a per-file ``datetime_start``; it keeps that
    time (single instant, or an interval when ``datetime_end`` differs). A
    time-invariant (fx) file has no ``datetime_start``: STAC forbids a truly
    timeless Item, so it is placed across the experiment's run span
    (``run_start``/``run_end``) with a null instant, and flagged as fx.

    Parameters
    ----------
    file_metadata : FileMetadata
        The file's scanned metadata.
    exp_metadata : ExperimentMetadata
        The owning experiment; its ``run_start``/``run_end`` provide the span
        for a time-invariant file.

    Returns
    -------
    tuple of (datetime or None, datetime or None, datetime or None, bool)
        ``(dt_start, dt_end, item_datetime, is_fx)``. For a time-varying file,
        ``item_datetime`` is set only for a single instant (start == end, or no
        end) and ``is_fx`` is False. For an fx file, ``dt_start``/``dt_end`` are
        the run span, ``item_datetime`` is None, and ``is_fx`` is True.

    Raises
    ------
    ValueError
        If the file has no per-file datetime and the experiment has no run span.
    """
    dt_start = _as_utc(file_metadata.get("datetime_start"))
    dt_end = _as_utc(file_metadata.get("datetime_end"))

    if dt_start is None:
        run_start = _as_utc(exp_metadata.run_start)
        run_end = _as_utc(exp_metadata.run_end)
        if run_start is None or run_end is None:
            raise ValueError(
                "cannot place a time-invariant item: experiment has no "
                "run_start/run_end"
            )
        return run_start, run_end, None, True

    single = dt_end is None or dt_start == dt_end
    return dt_start, dt_end, (dt_start if single else None), False


def _as_utc(value: Optional[datetime]) -> Optional[datetime]:
    """Return *value* as a UTC-aware datetime, assuming UTC when it is naive."""
    if value is not None and value.tzinfo is None:
        return value.replace(tzinfo=timezone.utc)
    return value


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
    file_format = file_metadata.get("format", "")
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
