"""LPJ-GUESS's own plain-text table format: format claim + reader.

LPJ-GUESS writes one whitespace-delimited ``.out`` file per output variable,
one row per gridcell (and, for the un-suffixed and ``_yearly`` files, per
year too) -- confirmed live against a real AWI-ESM3 run's
``outdata/lpj_guess/<segment>/`` directory:

- ``<var>_yearly.out``: ``Lon Lat Year <single value column, e.g. Total>``.
- ``<var>_monthly.out``: ``Lon Lat Year Jan Feb ... Dec`` -- twelve monthly
  values per row, still one row per gridcell-year (not one row per month).
- bare ``<var>.out`` (no suffix): ``Lon Lat Year <breakdown columns>``, e.g.
  per-PFT (``cmass.out``) or per-flux-component (``cflux.out``) -- confirmed
  these are annual too (one row per gridcell-year), despite some carrying
  misleadingly month-like names (``amon.out`` is annual, not monthly).

``.out`` is a suffix plenty of unrelated tools also use, so this claims it
only when *component* is ``lpj_guess`` (see
:func:`esm_catalog.scan.readers.format_plugins.FormatSpec.claim_by_suffix`'s
own docstring for the mechanism) -- never unconditionally.
"""

from __future__ import annotations

import io
from datetime import datetime, timezone
from typing import Optional

import pandas as pd
from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.readers.format_plugins import hookimpl as format_hookimpl
from esm_catalog.scan.readers.netcdf.coords import _bbox_to_polygon
from esm_catalog.scan.readers.plugins import hookimpl as reader_hookimpl
from esm_catalog.types import FileMetadata, ScannedVariable

__all__ = ["LPJGuessReader"]

_NON_VARIABLE_COLUMNS = {"Lon", "Lat", "Year"}


def _variable_and_frequency(stem: str) -> tuple[str, str]:
    """The variable name and frequency implied by a filename stem.

    ``cLand_yearly`` -> (``cLand``, ``yr``); ``cLand_monthly`` -> (``cLand``,
    ``mon``); a bare stem (``cmass``, ``cflux``, ...) -> (stem, ``yr``) --
    confirmed live that every un-suffixed ``.out`` file in a real run is
    annual, one row per gridcell-year, regardless of name.
    """
    if stem.endswith("_yearly"):
        return stem[: -len("_yearly")], "yr"
    if stem.endswith("_monthly"):
        return stem[: -len("_monthly")], "mon"
    return stem, "yr"


class LPJGuessReader:
    """Reads :class:`~esm_catalog.types.FileMetadata` from an LPJ-GUESS ``.out`` table.

    Attributes
    ----------
    supports_remote : bool
        ``True`` -- plain text over ``UPath.read_text()`` works the same
        local or remote.
    """

    supports_remote = True

    def read(self, path: UPath) -> FileMetadata:
        """Parse *path* and extract its scan metadata.

        The whole file is read into memory first rather than handed to
        pandas as a live stream -- same reasoning as ``sourcing.py``'s
        ``_load_yaml`` fix: a live remote stream pays one round trip per
        internal read, a single ``read_bytes()`` pays one for the lot.
        """
        text = path.read_bytes().decode("utf-8", errors="replace")
        frame = pd.read_csv(io.StringIO(text), sep=r"\s+")

        variable, frequency = _variable_and_frequency(path.stem)
        value_columns = [c for c in frame.columns if c not in _NON_VARIABLE_COLUMNS]
        variables = [ScannedVariable(name=str(c)) for c in value_columns]

        years = frame["Year"]
        start = datetime(int(years.min()), 1, 1, tzinfo=timezone.utc)
        end = datetime(int(years.max()), 12, 31, 23, 59, 59, tzinfo=timezone.utc)

        lon, lat = frame["Lon"], frame["Lat"]
        bbox = [float(lon.min()), float(lat.min()), float(lon.max()), float(lat.max())]

        return FileMetadata(
            variable=variable,
            variables=variables,
            format=str(FileFormat.lpjguess_table),
            bbox=bbox,
            geometry=_bbox_to_polygon(bbox),
            datetime_start=start,
            datetime_end=end,
            datetime_str=start.strftime("%Y%m"),
            frequency=frequency,
        )


@format_hookimpl
def claim_by_suffix(suffix: str, component: Optional[str]) -> Optional[FileFormat]:
    if suffix == ".out" and component == "lpj_guess":
        return FileFormat.lpjguess_table
    return None


@reader_hookimpl
def get_reader(file_format: FileFormat) -> Optional[LPJGuessReader]:
    return LPJGuessReader() if file_format == FileFormat.lpjguess_table else None
