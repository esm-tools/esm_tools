"""Pluggable format -> reader dispatch.

Mirrors :mod:`esm_catalog.plugins` (the Item/Collection contract) and
:mod:`esm_catalog.scan.readers.grib.plugins` (the GRIB model-specific
fast-path contract) -- the same pluggy mechanism, a third, independent hook,
so the scan core dispatches through it instead of a hand-rolled
``dict[FileFormat, Reader]`` registry. A reader module opts in by
implementing ``get_reader``; the core never imports a concrete reader.

netcdf and grib are esm_catalog's own, always registered directly. A
separately-installed package (e.g. a community Zarr reader) contributes one
by shipping a module that implements ``get_reader`` and declaring it under
the ``esm_catalog.readers`` entry-point group in its own package metadata,
e.g. in ``pyproject.toml``::

    [project.entry-points."esm_catalog.readers"]
    zarr = "esm_catalog_zarr:reader_module"

See :func:`get_reader_plugin_manager`.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

import pluggy

if TYPE_CHECKING:
    from esm_catalog.scan.format import FileFormat
    from esm_catalog.scan.reader import Reader

hookspec = pluggy.HookspecMarker("esm_catalog.readers")
hookimpl = pluggy.HookimplMarker("esm_catalog.readers")


class ReaderSpec:
    """One hook call per registered reader module, first match wins."""

    @hookspec(firstresult=True)
    def get_reader(file_format: "FileFormat") -> "Optional[Reader]":
        """Return this module's Reader for *file_format*, or None.

        ``None`` means "not my format" -- :func:`esm_catalog.scan.reader.reader_for`
        raises ``LookupError`` when every registered reader returns ``None``.
        """


def _build_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog.readers")
    pm.add_hookspecs(ReaderSpec)
    from esm_catalog.scan.readers import grib, netcdf

    pm.register(netcdf)
    pm.register(grib)
    pm.load_setuptools_entrypoints("esm_catalog.readers")
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_reader_plugin_manager() -> pluggy.PluginManager:
    """The process-wide reader plugin manager, built once and reused."""
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm
