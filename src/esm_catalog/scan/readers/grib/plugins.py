"""Pluggable model-specific fast path for the GRIB reader.

``GRIBReader.read`` always works (cfgrib, model-agnostic), but is slow —
``cfgrib.open_datasets`` rebuilds its index on every open. A model module
(e.g. :mod:`.echam`) can register a faster, model-specific read for the files
it recognises; ``GRIBReader`` doesn't know which models have one.

A third-party package contributes one by shipping a module that implements
``try_model_specific_read`` and declaring it under the ``esm_catalog.grib``
entry-point group in its own package metadata, e.g. in ``pyproject.toml``::

    [project.entry-points."esm_catalog.grib"]
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

hookspec = pluggy.HookspecMarker("esm_catalog.grib")
hookimpl = pluggy.HookimplMarker("esm_catalog.grib")


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
    pm = pluggy.PluginManager("esm_catalog.grib")
    pm.add_hookspecs(GribFastPathSpec)
    from esm_catalog.scan.readers.grib import echam

    pm.register(echam)
    # Built-ins are registered directly above (guaranteed, no install-metadata
    # round-trip needed); this discovers anything a separately-installed
    # package declared under the group in its own entry_points.
    pm.load_setuptools_entrypoints("esm_catalog.grib")
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_grib_plugin_manager() -> pluggy.PluginManager:
    """The process-wide GRIB fast-path plugin manager, built once and reused."""
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm
