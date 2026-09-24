"""esm-catalog-game -- a ranger-style terminal game for cataloging an ESM-Tools
experiment, built directly on esm_catalog's own scan-sourcing layer.

The game never reimplements esm_catalog's discovery logic: :mod:`.catalog_data`
calls :func:`esm_catalog.scan.sourcing.source_files` and
:func:`esm_catalog.scan.sourcing.source_experiment` to get the real, authoritative
list of files an ``esm-catalog scan`` would catalogue, and the player's job is to
find and correctly classify each one by navigating the real experiment tree.
"""

from __future__ import annotations

__version__ = "0.1.0"
