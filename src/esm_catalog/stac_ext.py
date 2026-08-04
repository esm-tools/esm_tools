"""Shared helpers for the ESM-Tools STAC extensions.

Cross-cutting behavior every extension module needs: registering the extension
URL on a STAC object, loading an extension's local schema, and validating an
instance against it. Kept separate from registry.py so that module stays pure
URL data with no runtime dependencies.
"""

from __future__ import annotations

import json
from functools import lru_cache
from typing import TYPE_CHECKING

import esm_tools
from jsonschema.validators import validator_for

from esm_catalog.registry import EXTENSION_URLS

if TYPE_CHECKING:
    import pystac


def register_extension(obj: "pystac.STACObject", url: str) -> None:
    """Append *url* to ``obj.stac_extensions`` once (idempotent)."""
    if url not in obj.stac_extensions:
        obj.stac_extensions.append(url)


@lru_cache(maxsize=None)
def load_schema(name: str) -> dict:
    """Load an ESM-Tools extension's JSON schema by registry *name* (memoized).

    The local config path mirrors the hosted URL's ``/stac-extensions/...`` tail,
    so it resolves install-aware via esm_tools. Raises for extensions whose
    schema is hosted remotely (no local copy), e.g. the upstream stac-extensions.
    """
    url = EXTENSION_URLS[name]
    marker = "/stac-extensions/"
    idx = url.find(marker)
    if idx == -1:
        raise ValueError(f"No local schema for '{name}': {url!r} is hosted remotely.")
    rel = url[idx + 1 :]  # 'stac-extensions/<name>/<version>/schema.json'
    with open(esm_tools.get_config_filepath(rel)) as fh:
        return json.load(fh)


def validate(instance: dict, name: str) -> None:
    """Validate a STAC object's ``.to_dict()`` against extension *name*'s schema.

    The compiled validator is cached per extension, so schema compilation
    happens once — the per-object validation itself always runs.
    """
    _validator(name).validate(instance)


@lru_cache(maxsize=None)
def _validator(name: str):
    """A jsonschema validator compiled once for extension *name*'s schema."""
    schema = load_schema(name)
    cls = validator_for(schema)
    cls.check_schema(schema)
    return cls(schema)
