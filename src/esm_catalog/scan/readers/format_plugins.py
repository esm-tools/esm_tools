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
    def claim_by_suffix(
        suffix: str, component: "Optional[str]"
    ) -> "Optional[FileFormat]":
        """Return this module's :class:`FileFormat` for *suffix*, or None.

        *suffix* is the lowercased path suffix (e.g. ``.nc``). ``None`` means
        "not my extension" -- :func:`esm_catalog.scan.format.detect` falls
        through to :func:`claim_by_magic` when every hook returns ``None``.

        *component* is the owning component (e.g. ``'lpj_guess'``) when
        known, ``None`` otherwise -- lets a claimer scope a generic-looking
        suffix to one model's own convention (a bare ``.out``, which many
        unrelated tools also use) instead of claiming it unconditionally for
        every file sharing that suffix. A claimer not interested in it
        simply doesn't declare the parameter in its own signature; pluggy
        only passes the subset of a hookspec's parameters each hookimpl
        actually asks for by name, so existing claimers need no change.

        Deliberately has no default here: pluggy's ``_multicall`` only
        forwards a hookimpl's *no-default* parameters (its ``argnames``) --
        a defaulted parameter is never forwarded at all, the hookimpl's own
        default silently wins regardless of what the caller passed. A
        hookimpl that wants the real value must declare this parameter
        without a default too; that's only legal if the spec itself has no
        default (pluggy requires every no-default hookimpl parameter to
        appear in the spec's own no-default set).
        """

    @hookspec(firstresult=True)
    def claim_by_magic(
        head: bytes, component: "Optional[str]"
    ) -> "Optional[FileFormat]":
        """Return this module's :class:`FileFormat` for *head*, or None.

        *head* is the file's first few bytes. ``None`` means "not my
        signature" -- :func:`esm_catalog.scan.format.detect` raises
        ``UnknownFormatError`` when every hook returns ``None``.

        *component* -- see :func:`claim_by_suffix`'s docstring; same
        meaning, same opt-in-by-declaring-the-parameter mechanism, same
        reason it has no default.
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
