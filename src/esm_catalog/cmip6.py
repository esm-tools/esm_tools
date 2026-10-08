"""CMIP6 STAC extension: real DRS facets, config-declared, never fabricated.

Populates the ``cmip6:*`` namespace from the official upstream schema
(``stac-extensions.github.io/cmip6``) -- the same extension real CMIP6 STAC
catalogs use in production (e.g. CEDA's ``api.stac.ceda.ac.uk/collections/cmip6``).

Deliberately scoped to the DRS facets that describe *the experiment as a
whole* and can be honestly declared by a runscript author -- ``activity_id``,
``institution_id``, ``source_id``, ``experiment_id``, ``mip_era``,
``variant_label``, ``sub_experiment_id``, ``source_type``,
``further_info_url``. Left out on purpose:

- ``variable_id``, ``table_id``, ``frequency``, ``cf_standard_name``,
  ``variable_units``, ``nominal_resolution``, ``grid_label`` -- these describe
  one *output file*, not the experiment, and need either scan-time derivation
  (frequency is already in ``FileMetadata``) or a raw-name -> CMOR-standard-name
  mapping this extension does not attempt (that belongs to a PyCMOR/CMOR-style
  enricher, see ``scan/enrichers.py``, not here).
- ``version``, ``retracted``, ``data_specs_version``, ``citation_url`` -- these
  are ESGF publication-time facts (assigned when data is actually submitted),
  not properties of a scanned experiment.

The values come from the ``general.metadata.cmip6`` config section -- an
``extra`` key alongside ``general.metadata``'s existing Description/Authors/
License/Institute fields (see ``scan/sourcing.py:MetadataBlock``), e.g.::

    general:
      metadata:
        cmip6:
          activity_id: "PMIP"
          experiment_id: "piControl"
          mip_era: "CMIP6"          # the literal DRS round string, a schema
                                     # `const` -- not esgvoc's lowercase CV
                                     # term id ("cmip6") for the same round
          institution_id: "AWI"
          source_id: "AWI-CM-1-1-MR"  # must be an actually-registered CMIP6
                                       # source_id -- a new, unregistered
                                       # model (e.g. AWI-CM3) has none yet

No-op (nothing declared, nothing added) for the vast majority of runs that
are not CMIP submissions -- this extension never invents a facet value, and
does not itself check a declared value against the real controlled
vocabulary (several fields -- activity_id, institution_id, source_id,
experiment_id -- are closed enums upstream; an invalid value will fail
schema validation downstream). See :mod:`esm_catalog.esgvoc_validate` for an
explicit, separate CV-conformance check.
"""

from __future__ import annotations

from typing import Optional

import pystac
from pydantic import BaseModel, ConfigDict

from esm_catalog.plugins import hookimpl
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension

Cmip6FacetValue = str
"""A single CMIP6 DRS facet value, e.g. 'piControl', 'AWI', 'CMIP6' -- free-text
here; see :mod:`esm_catalog.esgvoc_validate` to check a value against the real
controlled vocabulary."""


class Cmip6Config(BaseModel):
    """The ``general.metadata.cmip6`` config section — every field optional.

    Declaring any field marks this experiment (or part of it) as CMIP-aligned;
    declaring none is the default and this extension is a complete no-op.
    """

    model_config = ConfigDict(extra="allow")

    activity_id: Optional[Cmip6FacetValue] = None
    institution_id: Optional[Cmip6FacetValue] = None
    source_id: Optional[Cmip6FacetValue] = None
    experiment_id: Optional[Cmip6FacetValue] = None
    mip_era: Optional[Cmip6FacetValue] = None
    sub_experiment_id: Optional[Cmip6FacetValue] = None
    variant_label: Optional[Cmip6FacetValue] = None
    source_type: Optional[Cmip6FacetValue] = None
    further_info_url: Optional[Cmip6FacetValue] = None


#: The config keys map 1:1 onto the cmip6: fields — derived from Cmip6Config so
#: the two never drift.
_KEYS = tuple(Cmip6Config.model_fields)


def cmip6_props(cmip6_config: Optional[Cmip6Config]) -> dict:
    """Return the ``cmip6:*`` fields set by *cmip6_config*.

    Parameters
    ----------
    cmip6_config : Cmip6Config or None
        The ``general.metadata.cmip6`` config section, or None.

    Returns
    -------
    dict
        The ``cmip6:*`` properties; empty when the config is None or declares
        no facets (i.e. not a CMIP-aligned run).
    """
    if cmip6_config is None:
        return {}
    cmip6_config = Cmip6Config.model_validate(cmip6_config)
    fields_set = cmip6_config.model_dump(exclude_none=True)
    return {f"cmip6:{key}": fields_set[key] for key in _KEYS if key in fields_set}


def add_cmip6_item_extension(
    item: pystac.Item, cmip6_config: Optional[Cmip6Config] = None
) -> None:
    """Set ``cmip6:*`` DRS facets on *item* from *cmip6_config*.

    No-op when *cmip6_config* declares no facets.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    cmip6_config : Cmip6Config or None, optional
        The ``general.metadata.cmip6`` config section.
    """
    props = cmip6_props(cmip6_config)
    if not props:
        return
    item.properties.update(props)
    # Remote schema (upstream stac-extensions) — nothing local to validate against.
    apply_extension(item, Extension.cmip6, validate=False)


def add_cmip6_collection_extension(
    collection: pystac.Collection, cmip6_config: Optional[Cmip6Config] = None
) -> None:
    """Summarize the ``cmip6:*`` DRS facets on *collection* from *cmip6_config*.

    No-op when *cmip6_config* declares no facets.

    Parameters
    ----------
    collection : pystac.Collection
        The collection to annotate in place.
    cmip6_config : Cmip6Config or None, optional
        The ``general.metadata.cmip6`` config section.
    """
    props = cmip6_props(cmip6_config)
    if not props:
        return
    for key, value in props.items():
        collection.summaries.add(key, [value])
    apply_extension(collection, Extension.cmip6, validate=False)


@hookimpl
def apply_to_item(item, file_metadata, exp_metadata, hints) -> None:
    add_cmip6_item_extension(item, exp_metadata.cmip6_config)


@hookimpl
def apply_to_collection(collection, exp_metadata, hints) -> None:
    add_cmip6_collection_extension(collection, exp_metadata.cmip6_config)
