"""Path-facet extraction: a file's stream + start datetime from its path alone.

Confirmed live: once schema (variables/dims/geometry/format) is frozen per
``(component, stream)`` after the first file (see
:func:`esm_catalog.scan.ingest.scan_experiment`), every later file of that
stream only needs its own datetime -- and a model's own on-disk naming
convention (not the fuzzy, wildcard-bearing ``outdata_sources`` glob pattern
declared in ``configs/components/*.yaml``, which locates files but does not
precisely describe their structure) usually already encodes it. Extracting it
from the path avoids opening the file at all.

*stream* is also recovered here, not just datetime -- confirmed live: a real
experiment's declared ``outdata_targets`` paths can all be stale (pointing at
files never produced), meaning the actual files are found only through the
undeclared filesystem walk, which carries no stream identity at all until
after a real read. Without the path supplying it, every walked file would
need a full read just to learn which stream it belongs to, defeating the
schema-freeze this module exists to enable.

Mirrors :mod:`esm_catalog.scan.readers.grib.plugins` -- the same pluggy
shape, a fourth independent hook. A module opts in by implementing
``extract_path_facets``; the scan core never imports a concrete provider. A
separately-installed package contributes one by shipping a module that
implements it and declaring it under the ``esm_catalog.path_facets``
entry-point group, e.g. in ``pyproject.toml``::

    [project.entry-points."esm_catalog.path_facets"]
    my_model = "my_package.path_facets"

See :func:`get_path_facet_plugin_manager`.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

import pluggy

if TYPE_CHECKING:
    from datetime import datetime

    from upath import UPath

hookspec = pluggy.HookspecMarker("esm_catalog.path_facets")
hookimpl = pluggy.HookimplMarker("esm_catalog.path_facets")


class PathFacetSpec:
    """One hook call, first non-``None`` result wins."""

    @hookspec(firstresult=True)
    def extract_path_facets(
        path: "UPath", component: str, stream: Optional[str]
    ) -> "Optional[tuple[str, datetime]]":
        """Return ``(stream, start)`` recovered from *path* alone, or ``None``.

        *stream* is the caller's own best guess, already known for a
        declared/tidy-log file -- ``None`` for a walked, undeclared one. A
        provider must recover the stream itself from the path when it is
        ``None`` (returning it as part of the result); when it is already
        given, a provider whose own naming convention disagrees with it
        should return ``None`` rather than override a stream identity it did
        not establish. ``None`` overall means "not my naming convention" --
        the caller falls back to actually opening the file. An
        implementation should never raise; catch its own parsing errors and
        return ``None`` instead, the same contract as the GRIB fast path's
        ``try_model_specific_read``.
        """


def _build_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog.path_facets")
    pm.add_hookspecs(PathFacetSpec)
    from esm_catalog.scan.path_facets import templates

    pm.register(templates)
    pm.load_setuptools_entrypoints("esm_catalog.path_facets")
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_path_facet_plugin_manager() -> pluggy.PluginManager:
    """The process-wide path-facet plugin manager, built once and reused."""
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm
