"""The GRIB reader for the scan layer.

A GRIB file is opened with :mod:`cfgrib`, which presents it as one or more
xarray Datasets -- one per *hypercube* (a distinct ``(gridType, typeOfLevel)``
combination), because a single GRIB file routinely mixes surface and
pressure-level fields on different grids. Each hypercube is a plain CF dataset,
so the NetCDF reader's extraction submodules (spatial extent, cube dimensions,
time range, frequency, per-variable metadata) are reused verbatim -- and because
a GRIB grid carries real latitude/longitude, the spatial extent is genuine, not
the whole-Earth fallback.

This module is the *basic*, model-agnostic reader -- always correct, but pays
cfgrib's per-open reindexing cost. A model module can register two different
kinds of model-specific help, via :mod:`.plugins`:

- ``try_model_specific_read`` (see :mod:`.echam`) -- an alternative, faster
  read for files it recognises (e.g. straight from eccodes headers), tried
  before cfgrib; returning ``None`` falls through to the generic path.
- an *enricher* (:func:`register_enricher`) -- post-processes the metadata
  cfgrib/the fast path already produced (e.g. ECHAM's GRIB1 encoding, where
  every field is stored under ``paramId=0`` and collapses to a single
  ``unknown`` variable, which no fast path claims).
"""

from __future__ import annotations

import warnings
from typing import Callable

import xarray as xr
from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.reader import UnsupportedContentError
from esm_catalog.scan.readers.grib.plugins import get_grib_plugin_manager
from esm_catalog.scan.readers.plugins import hookimpl
from esm_catalog.scan.readers.netcdf.coords import _extract_bbox
from esm_catalog.scan.readers.netcdf.dimensions import _extract_dimensions
from esm_catalog.scan.readers.netcdf.frequency import _infer_frequency
from esm_catalog.scan.readers.netcdf.timeaxis import _extract_time_range
from esm_catalog.scan.readers.netcdf.variables import _extract_variables
from esm_catalog.types import FileMetadata

__all__ = ["GRIBReader", "register_enricher"]

#: An enricher post-processes a GRIB file's metadata in place, given the opened
#: hypercube datasets. It returns the (possibly replaced) metadata.
GribEnricher = Callable[[UPath, FileMetadata, list], FileMetadata]

_ENRICHERS: list[GribEnricher] = []


def register_enricher(enricher: GribEnricher) -> None:
    """Register a model-specific *enricher* to run after the basic extraction."""
    _ENRICHERS.append(enricher)


class GRIBReader:
    """Reads :class:`~esm_catalog.types.FileMetadata` from a GRIB file.

    Attributes
    ----------
    supports_remote : bool
        ``False`` -- cfgrib/eccodes read through the local filesystem only; a
        remote path is fetched (cached) by the caller before scanning.
    """

    supports_remote = False

    def read(self, path: UPath) -> FileMetadata:
        """Read *path*'s scan metadata: a model-specific fast path if one
        claims the file, else cfgrib.

        Raises
        ------
        UnsupportedContentError
            If cfgrib is unavailable, or the file yields no readable hypercube.
        """
        fast = get_grib_plugin_manager().hook.try_model_specific_read(
            path=path, file_format=FileFormat.grib
        )
        if fast is not None:
            return fast
        return self._read_generic(path)

    def _read_generic(self, path: UPath) -> FileMetadata:
        """Open *path* with cfgrib and extract its scan metadata."""
        datasets = _open_hypercubes(path)
        if not datasets:
            raise UnsupportedContentError(f"{path}: no readable GRIB hypercube")
        try:
            # Suppression has to span metadata extraction too, not just the open
            # in _open_hypercubes -- SerializationWarning fires again whenever a
            # lazily-decoded time coordinate is materialized (see netcdf/__init__
            # for the same reasoning; this reuses its extractors).
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", category=xr.SerializationWarning)
                metadata = _basic_metadata(datasets)
                for enricher in _ENRICHERS:
                    metadata = enricher(path, metadata, datasets)
            return metadata
        finally:
            for dataset in datasets:
                dataset.close()


def _open_hypercubes(path: UPath) -> list:
    """Open *path* as a list of per-hypercube xarray Datasets via cfgrib."""
    try:
        import cfgrib
    except ImportError as exc:  # cfgrib is an optional extra
        raise UnsupportedContentError(
            f"{path}: GRIB support needs the 'cfgrib' extra ({exc})"
        ) from exc
    # cfgrib is a C/eccodes wrapper that opens a local file by name (which is why
    # this reader is local-only); ``.path`` is the UPath's filesystem path.
    # indexpath="" keeps cfgrib from writing a .idx sidecar next to the data;
    # errors="ignore" drops messages cfgrib cannot decode rather than aborting.
    #
    # Dates outside datetime64[ns] range (routine for paleoclimate calendars)
    # do not raise -- cfgrib's own xarray_plugin falls back to cftime objects
    # and warns instead, same as the NetCDF reader's open path (see
    # netcdf/open.py). Already handled downstream (timeaxis.py accepts cftime
    # objects same as datetime64), so suppressed here too rather than left to
    # print on every such file.
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=xr.SerializationWarning)
        return cfgrib.open_datasets(
            path.path, backend_kwargs={"indexpath": "", "errors": "ignore"}
        )


def _basic_metadata(datasets: list) -> FileMetadata:
    """Build model-agnostic metadata from the opened hypercubes.

    Variables and cube dimensions are collected across every hypercube; the
    spatial extent, time range and frequency come from the first (all hypercubes
    of one file share a grid and time axis).
    """
    variables = []
    dimensions: dict = {}
    for dataset in datasets:
        variables.extend(_extract_variables(dataset))
        dimensions.update(_extract_dimensions(dataset))

    representative = datasets[0]
    bbox, geometry = _extract_bbox(representative)
    start, end = _extract_time_range(representative)
    frequency = _infer_frequency(representative)

    metadata: FileMetadata = {
        "variables": variables,
        "variable": variables[0]["name"] if variables else "unknown",
        "dimensions": dimensions,
        "bbox": bbox,
        "geometry": geometry,
        "format": "grib",
    }
    if start is not None and end is not None:
        metadata["datetime_start"] = start
        metadata["datetime_end"] = end
        metadata["datetime_str"] = start.strftime("%Y%m")
    if frequency is not None:
        metadata["frequency"] = frequency
    return metadata


_READER = GRIBReader()


@hookimpl
def get_reader(file_format: FileFormat):
    return _READER if file_format == FileFormat.grib else None


# Import model enrichers for their registration side effect. Kept last so
# register_enricher and the reader are defined first.
from esm_catalog.scan.readers.grib import echam as _echam  # noqa: E402,F401
