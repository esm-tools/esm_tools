"""The NetCDF reader for the scan layer.

Opens a NetCDF file with xarray and extracts the
:class:`~esm_catalog.types.FileMetadata` a STAC Item is built from -- variables,
cube dimensions, spatial extent, temporal range, and an inferred frequency.
Everything is read from the file: coordinates are found by their CF attributes
(via :mod:`cf_xarray`), never by hard-coded names, and nothing is guessed from
the filename. The component name, checksum, and the run span an ``fx`` file is
stamped with are the sourcing layer's job.

The absence of a time coordinate is meaningful: a time-invariant file yields no
temporal range and no frequency, and that omission is the ``fx`` signal the
downstream layer keys on -- it also means the item's datetime stamp cannot come
from the file and must be supplied from the experiment run span.

The implementation is split by concern into submodules (``open``, ``coords``,
``timeaxis``, ``dimensions``, ``variables``, ``frequency``, ``encoding``). This
module is the public face: the :class:`NetCDFReader` and its registration.
``_drop_surrogates`` is re-exported for the encoding test.
"""

from __future__ import annotations

import cf_xarray  # noqa: F401 - registers the .cf accessor on Dataset/DataArray
from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.reader import register
from esm_catalog.types import FileMetadata

from .coords import _extract_bbox
from .dimensions import _extract_dimensions
from .encoding import _drop_surrogates
from .frequency import _infer_frequency
from .open import _open_dataset
from .timeaxis import _extract_time_range
from .variables import _extract_variables

__all__ = ["NetCDFReader"]


class NetCDFReader:
    """Reads :class:`~esm_catalog.types.FileMetadata` from a NetCDF file.

    Attributes
    ----------
    supports_remote : bool
        ``True`` -- remote paths are read through ``h5netcdf`` over fsspec.
    """

    supports_remote = True

    def read(self, path: UPath) -> FileMetadata:
        """Open *path* with xarray and extract its scan metadata.

        Parameters
        ----------
        path : UPath
            The NetCDF file to read, local or remote.

        Returns
        -------
        FileMetadata
            Every key the file supports. ``datetime_start``/``datetime_end``,
            ``frequency`` and ``datetime_str`` are omitted for a time-invariant
            file -- their absence marks it as ``fx``.
        """
        with _open_dataset(path) as opened:
            # Stamp CF axis/coordinate attributes onto coordinates the model left
            # bare (regex over common names) so the extractors below find lat, lon,
            # level and time whether or not the file carried CF attributes -- no
            # fixed name list, which never stays complete.
            dataset = opened.cf.guess_coord_axis()
            variables = _extract_variables(dataset)
            dimensions = _extract_dimensions(dataset)
            bbox, geometry = _extract_bbox(dataset)
            start, end = _extract_time_range(dataset)
            frequency = _infer_frequency(dataset)
            primary = next(iter(dataset.data_vars), "unknown")

        metadata: FileMetadata = {
            "variable": primary,
            "variables": variables,
            "dimensions": dimensions,
            "bbox": bbox,
            "geometry": geometry,
            "format": "netcdf",
        }
        if start is not None:
            metadata["datetime_start"] = start
            metadata["datetime_str"] = start.strftime("%Y%m")
        if end is not None:
            metadata["datetime_end"] = end
        if frequency is not None:
            metadata["frequency"] = frequency
        return _drop_surrogates(metadata)


register(FileFormat.netcdf, NetCDFReader())
