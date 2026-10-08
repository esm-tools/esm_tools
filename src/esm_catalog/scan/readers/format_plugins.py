"""Pluggable file-format detection.

Mirrors :mod:`esm_catalog.scan.readers.plugins` (the format -> Reader
contract) and :mod:`esm_catalog.scan.readers.grib.plugins` (the GRIB
model-specific fast-path contract) -- the same pluggy mechanism, a fourth,
independent hook, so :func:`esm_catalog.scan.format.detect` never hard-codes a
format's suffixes or magic bytes. A reader module opts in by implementing
``claim_by_suffix`` and/or ``claim_by_magic``; ``format.py`` never imports a
concrete reader.

netcdf and grib are esm_catalog's own, always registered directly. A
separately-installed package contributes a format by shipping a module that
implements ``claim_by_suffix``/``claim_by_magic`` and declaring it under the
``esm_catalog.formats`` entry-point group in its own package metadata, e.g.
in ``pyproject.toml``::

    [project.entry-points."esm_catalog.formats"]
    zarr = "esm_catalog_zarr:format_module"

See :func:`get_format_plugin_manager`.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

import pluggy

if TYPE_CHECKING:
    from esm_catalog.scan.format import FileFormat

hookspec = pluggy.HookspecMarker("esm_catalog.formats")
hookimpl = pluggy.HookimplMarker("esm_catalog.formats")


class FormatSpec:
    """One hook call per registered reader module, first match wins."""

    @hookspec(firstresult=True)
    def claim_by_suffix(suffix: str) -> "Optional[FileFormat]":
        """Return this module's :class:`FileFormat` for *suffix*, or None.

        *suffix* is the lowercased path suffix (e.g. ``.nc``). ``None`` means
        "not my extension" -- :func:`esm_catalog.scan.format.detect` falls
        through to :func:`claim_by_magic` when every hook returns ``None``.
        """

    @hookspec(firstresult=True)
    def claim_by_magic(head: bytes) -> "Optional[FileFormat]":
        """Return this module's :class:`FileFormat` for *head*, or None.

        *head* is the file's first few bytes. ``None`` means "not my
        signature" -- :func:`esm_catalog.scan.format.detect` raises
        ``UnknownFormatError`` when every hook returns ``None``.
        """


def _build_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog.formats")
    pm.add_hookspecs(FormatSpec)
    from esm_catalog.scan.readers import grib, netcdf

    pm.register(netcdf)
    pm.register(grib)
    pm.load_setuptools_entrypoints("esm_catalog.formats")
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_format_plugin_manager() -> pluggy.PluginManager:
    """The process-wide format-detection plugin manager, built once and reused."""
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm
