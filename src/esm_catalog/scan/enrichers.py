"""Pluggable per-file metadata enrichment, shared across readers.

A reader's basic extraction (cfgrib/xarray) is model-agnostic; a model module
can post-process the result for its own quirks by registering an enricher
here -- e.g. ECHAM's GRIB1 encoding, where every field is stored under
``paramId=0`` and cfgrib collapses them to a single ``unknown`` variable (see
:mod:`esm_catalog.scan.readers.grib.echam`). The same mechanism works for a
NetCDF-writing model with its own quirks (a hypothetical FESOM enricher, say)
-- this module isn't GRIB-specific, which is why it lives here rather than
under ``scan/readers/grib/``.

Enrichers form a pipeline, not independent fan-out: each mutates *metadata*
in place (it's a plain mutable ``dict`` at runtime even though it's typed as
a ``TypedDict``), so pluggy's ordinary multicall -- which calls every
registered hookimpl in turn, passing the *same* ``metadata`` object each
time, not a copy -- threads the shared mutable state through the chain
correctly on its own. There is no need to walk hook implementations by hand;
the hook's return value is ignored, only the in-place mutation matters.

A separately-installed package contributes an enricher by shipping a module
that implements ``enrich_metadata`` and declaring it under the
``esm_catalog.enrichers`` entry-point group in its own package metadata::

    [project.entry-points."esm_catalog.enrichers"]
    my_model = "my_package.enrichment"

See :func:`get_enricher_plugin_manager`.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

import pluggy

if TYPE_CHECKING:
    from upath import UPath

    from esm_catalog.scan.format import FileFormat
    from esm_catalog.types import FileMetadata

hookspec = pluggy.HookspecMarker("esm_catalog.enrichers")
hookimpl = pluggy.HookimplMarker("esm_catalog.enrichers")


class EnricherSpec:
    """One hook call per registered enricher, each mutating *metadata* in place."""

    @hookspec
    def enrich_metadata(
        path: "UPath",
        file_format: "FileFormat",
        metadata: "FileMetadata",
        datasets: list,
    ) -> None:
        """Post-process *metadata* in place for *path*'s already-extracted data.

        Every enricher runs, regardless of *file_format* -- one not
        interested in this format checks *file_format* first and returns
        immediately without touching *metadata*. The return value is
        ignored; only mutating *metadata* has an effect.
        """


def _build_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog.enrichers")
    pm.add_hookspecs(EnricherSpec)
    from esm_catalog.scan.readers.grib import echam

    pm.register(echam)
    pm.load_setuptools_entrypoints("esm_catalog.enrichers")
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_enricher_plugin_manager() -> pluggy.PluginManager:
    """The process-wide enricher plugin manager, built once and reused."""
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm


def run_enrichers(
    path: "UPath", file_format: "FileFormat", metadata: "FileMetadata", datasets: list
) -> "FileMetadata":
    """Run every registered enricher against *metadata* in place, then return it."""
    get_enricher_plugin_manager().hook.enrich_metadata(
        path=path, file_format=file_format, metadata=metadata, datasets=datasets
    )
    return metadata
