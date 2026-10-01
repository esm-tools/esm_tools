"""Shared helpers for the ESM-Tools STAC extensions.

Cross-cutting behavior every extension module needs: registering the extension
URL on a STAC object, loading an extension's local schema, and validating an
instance against it. Kept separate from registry.py so that module stays pure
URL data with no runtime dependencies.
"""

from __future__ import annotations

import json
from functools import lru_cache

import pystac
from jsonschema.validators import validator_for

import esm_tools
from esm_catalog.registry import EXTENSION_URLS, Extension
from esm_catalog.types import ExtensionUrl


def register_extension(obj: pystac.STACObject, url: ExtensionUrl) -> None:
    """Append *url* to ``obj.stac_extensions`` once (idempotent)."""
    if url not in obj.stac_extensions:
        obj.stac_extensions.append(url)


def apply_extension(
    obj: pystac.STACObject, name: Extension, *, validate: bool = True
) -> None:
    """Register extension *name* on *obj*, then (by default) validate the result.

    Register-before-validate is the required order — the schema's
    ``stac_extensions`` ``contains`` check needs the URL present. Callers write
    their fields onto *obj* first, then call this.

    Parameters
    ----------
    obj : pystac.STACObject
        The Item or Collection to register the extension on and validate.
    name : Extension
        The registry name of the extension to apply.
    validate : bool, optional
        Whether to validate *obj* against the extension schema. Pass False for
        extensions whose schema is hosted remotely (no local copy to check
        against). Defaults to True.
    """
    register_extension(obj, EXTENSION_URLS[name])
    if validate:
        _validator(name).validate(obj.to_dict())


@lru_cache(maxsize=None)
def load_schema(name: Extension) -> dict:
    """Load an ESM-Tools extension's JSON schema by registry *name* (memoized).

    The local config path mirrors the hosted URL's ``/stac-extensions/...`` tail,
    so it resolves install-aware via esm_tools.

    Parameters
    ----------
    name : Extension
        The registry name of the extension whose schema to load.

    Returns
    -------
    dict
        The parsed JSON schema.

    Raises
    ------
    ValueError
        For extensions whose schema is hosted remotely (no local copy), e.g. the
        upstream stac-extensions.
    """
    url = EXTENSION_URLS[name]
    marker = "/stac-extensions/"
    marker_index = url.find(marker)
    if marker_index == -1:
        raise ValueError(f"No local schema for '{name}': {url!r} is hosted remotely.")
    relative_path = url[
        marker_index + 1 :
    ]  # 'stac-extensions/<name>/<version>/schema.json'
    with open(esm_tools.get_config_filepath(relative_path)) as schema_file:
        return json.load(schema_file)


@lru_cache(maxsize=None)
def _validator(name: Extension):
    """A jsonschema validator compiled once for extension *name*'s schema."""
    schema = load_schema(name)
    cls = validator_for(schema)
    cls.check_schema(schema)
    return cls(schema)
