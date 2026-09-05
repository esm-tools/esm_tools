"""The GRIB reader for the scan layer.

A GRIB file is opened with :mod:`cfgrib`, which presents it as one or more
xarray Datasets -- one per *hypercube* (a distinct ``(gridType, typeOfLevel)``
combination), because a single GRIB file routinely mixes surface and
pressure-level fields on different grids. Each hypercube is a plain CF dataset,
so the NetCDF reader's extraction submodules (spatial extent, cube dimensions,
time range, frequency, per-variable metadata) are reused verbatim -- and because
a GRIB grid carries real latitude/longitude, the spatial extent is genuine, not
the whole-Earth fallback.

This module is the *basic*, model-agnostic reader. Model-specific quirks that
cfgrib alone cannot resolve -- most importantly ECHAM's GRIB1 encoding, where
every field is stored under ``paramId=0`` and collapses to a single ``unknown``
variable -- are handled by pluggable *enrichers* (see :mod:`.echam`), which
post-process the metadata. An enricher registers itself with
:func:`register_enricher`; the reader applies each in turn.
"""

from __future__ import annotations

from typing import Callable

from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.reader import UnsupportedContentError, register
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
        """Open *path* with cfgrib and extract its scan metadata.

        Raises
        ------
        UnsupportedContentError
            If cfgrib is unavailable, or the file yields no readable hypercube.
        """
        datasets = _open_hypercubes(path)
        if not datasets:
            raise UnsupportedContentError(f"{path}: no readable GRIB hypercube")
        try:
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


register(FileFormat.grib, GRIBReader())

# Import model enrichers for their registration side effect. Kept last so
# register_enricher and the reader are defined first.
from esm_catalog.scan.readers.grib import echam as _echam  # noqa: E402,F401
