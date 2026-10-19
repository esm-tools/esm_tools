"""Pluggable model-specific fast path for the GRIB reader.

``GRIBReader.read`` always works (cfgrib, model-agnostic), but is slow —
``cfgrib.open_datasets`` rebuilds its index on every open. A model module
(e.g. :mod:`.echam`) can register a faster, model-specific read for the files
it recognises; ``GRIBReader`` doesn't know which models have one.

A third-party package contributes one by shipping a module that implements
``try_model_specific_read`` and declaring it under the ``esm_catalog.fastpath.grib``
entry-point group in its own package metadata, e.g. in ``pyproject.toml``::

    [project.entry-points."esm_catalog.fastpath.grib"]
    my_model = "my_package.grib_fastpath"

See :func:`get_grib_plugin_manager`.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

import pluggy

if TYPE_CHECKING:
    from upath import UPath

    from esm_catalog.scan.format import FileFormat
    from esm_catalog.types import FileMetadata

hookspec = pluggy.HookspecMarker("esm_catalog.fastpath.grib")
hookimpl = pluggy.HookimplMarker("esm_catalog.fastpath.grib")


class GribFastPathSpec:
    """One hook call, first non-``None`` result wins."""

    @hookspec(firstresult=True)
    def try_model_specific_read(
        path: "UPath", file_format: "FileFormat"
    ) -> "Optional[FileMetadata]":
        """Return metadata read via a model-specific path, or None.

        Returning ``None`` means "not my file" (or "my fast path failed") —
        the generic cfgrib reader is always the fallback, so a
        model-specific implementation should never raise; catch its own
        errors and return ``None`` instead.
        """


def _build_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog.fastpath.grib")
    pm.add_hookspecs(GribFastPathSpec)
    # echam is discovered via this entry-point group, declared in this
    # package's own setup.py for now -- the same mechanism a
    # separately-installed package would use (see esm-tools/esm_tools#1614).
    pm.load_setuptools_entrypoints("esm_catalog.fastpath.grib")
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_grib_plugin_manager() -> pluggy.PluginManager:
    """The process-wide GRIB fast-path plugin manager, built once and reused."""
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm
