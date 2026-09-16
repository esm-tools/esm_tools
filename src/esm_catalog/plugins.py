"""Pluggable extension contract: how datacube/namelist/paleo/contacts attach
themselves to an Item or Collection.

``item.py``/``collection.py`` build the base STAC object, then fire
``apply_to_item``/``apply_to_collection`` and let whichever extensions are
registered do their own work -- neither file hardcodes the list of known
extensions. An extension opts in by implementing the hook(s) that match which
``add_*_extension`` function(s) it actually has (see each extension module's
own ``apply_to_item``/``apply_to_collection``, e.g. ``esm_catalog.contacts``
implements only ``apply_to_collection``, ``esm_catalog.datacube`` only
``apply_to_item``).
"""

from __future__ import annotations

from typing import Optional

import pluggy

hookspec = pluggy.HookspecMarker("esm_catalog")
hookimpl = pluggy.HookimplMarker("esm_catalog")


class ItemContractSpec:
    """One hook call per registered extension, each mutating *item* in place."""

    @hookspec
    def apply_to_item(item, file_metadata, exp_metadata, hints: dict) -> None:
        """Apply this extension to *item*.

        Parameters
        ----------
        item : pystac.Item
            The item to annotate in place.
        file_metadata : FileMetadata
            The file's scanned metadata.
        exp_metadata : ExperimentMetadata
            The owning experiment.
        hints : dict
            Values a bulk caller (e.g. the scan loop) has already computed
            once for the whole run -- e.g. ``namelist_props``, ``paleo_props``,
            ``validate`` -- so an extension that gets them from *hints* is not
            forced to recompute per item. An implementation not interested in
            any of these keys ignores the dict entirely.
        """


class CollectionContractSpec:
    """One hook call per registered extension, each mutating *collection* in place."""

    @hookspec
    def apply_to_collection(collection, exp_metadata, hints: dict) -> None:
        """Apply this extension to *collection*.

        Parameters mirror :func:`ItemContractSpec.apply_to_item`, at the
        Collection level.
        """


def _build_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog")
    pm.add_hookspecs(ItemContractSpec)
    pm.add_hookspecs(CollectionContractSpec)
    from esm_catalog import contacts, datacube, namelist, paleo

    pm.register(datacube)
    pm.register(namelist)
    pm.register(paleo)
    pm.register(contacts)
    return pm


_pm: Optional[pluggy.PluginManager] = None


def get_plugin_manager() -> pluggy.PluginManager:
    """The process-wide plugin manager, built once and reused.

    Building a :class:`pluggy.PluginManager` walks every registered plugin's
    hookimpls; a scan builds thousands of items, so this is cached rather
    than rebuilt per item/collection.
    """
    global _pm
    if _pm is None:
        _pm = _build_plugin_manager()
    return _pm
