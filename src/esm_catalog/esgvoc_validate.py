"""esgvoc-backed CV validation for cmip6.py's declared DRS facets.

Separate from :mod:`esm_catalog.cmip6` (the STAC extension itself) on
purpose: checking a facet against a real controlled vocabulary needs either
network access or a locally-installed esgvoc CV database, and is a
publish-time correctness question, not a scan-time one -- scanning a
32k-file experiment must never depend on reaching a CV service, the same
reasoning that already keeps ``version``/``retracted`` out of ``cmip6.py``
(see its module docstring). Call this deliberately (e.g. before publishing a
CMIP-aligned experiment), never from the scan pipeline.

Not hard-coded to CMIP6: the esgvoc *project* validated against is derived
from the config's own ``mip_era`` (see :func:`validate_cmip6_config`), so a
``mip_era: "CMIP7"`` config validates against esgvoc's ``cmip7`` CV
automatically -- e.g. AWI-ESM3-veg's real, currently-registered CMIP7
``source_id`` (``AWI-ESM3-4-2-veg-HR``) validates the same way CMIP6's does.

Requires the optional ``esgvoc`` package (the ``catalog-esgvoc`` extra) and a
locally installed CV (``esgvoc use <project>@latest``) -- imported lazily, so
esm_catalog itself never depends on it.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from esm_catalog.cmip6 import Cmip6Config

#: Cmip6Config field -> esgvoc cmip6 collection id. Only fields backed by a
#: real, closed CV collection are checked here -- variant_label (a pattern
#: like 'r1i1p1f1', not an enum), source_type, and further_info_url (a URL,
#: not a CV term) are declared facets cmip6.py still populates, just not
#: ones this validator has a CV collection to check them against.
_VALIDATABLE_FIELDS: dict[str, str] = {
    "activity_id": "activity_id",
    "institution_id": "institution_id",
    "source_id": "source_id",
    "experiment_id": "experiment_id",
    "mip_era": "mip_era",
    "sub_experiment_id": "sub_experiment_id",
}


@dataclass(frozen=True)
class Cmip6ValidationIssue:
    """One declared facet whose value is not a registered CMIP6 CV term."""

    field: str
    value: str
    collection: str


#: esgvoc project id to validate against when *cmip6_config* declares no
#: ``mip_era`` at all -- the overwhelming majority of declared configs today.
_DEFAULT_PROJECT = "cmip6"


def validate_cmip6_config(
    cmip6_config: Optional[Cmip6Config],
) -> list[Cmip6ValidationIssue]:
    """Check every declared facet in *cmip6_config* against the live esgvoc CV.

    The esgvoc *project* to validate against is derived from
    ``cmip6_config.mip_era`` (lowercased -- esgvoc's project ids are
    lowercase, e.g. ``"cmip6"``/``"cmip7"``, while the DRS/STAC value is the
    literal ``"CMIP6"``/``"CMIP7"``), not hard-coded to CMIP6 -- a
    ``mip_era: "CMIP7"`` config validates against esgvoc's ``cmip7`` project
    automatically. Falls back to :data:`_DEFAULT_PROJECT` when ``mip_era``
    is not declared.

    Parameters
    ----------
    cmip6_config : Cmip6Config or None
        The ``general.metadata.cmip6`` config section, or None.

    Returns
    -------
    list of Cmip6ValidationIssue
        One entry per facet whose declared value is not a registered term in
        the relevant esgvoc project (e.g. an unregistered model name). Empty
        when every declared facet is valid -- including when *cmip6_config*
        is None or declares nothing at all.

    Raises
    ------
    ImportError
        If the optional ``esgvoc`` package is not installed.
    """
    if cmip6_config is None:
        return []
    try:
        import esgvoc.api as esgvoc_api
    except ImportError as exc:
        raise ImportError(
            "esgvoc is required for CV validation: pip install "
            "esm_tools[catalog-esgvoc], then `esgvoc use <project>@latest` "
            "to install the relevant CV database (e.g. cmip6, cmip7)."
        ) from exc

    from esgvoc.core.exceptions import EsgvocNotFoundError

    project = cmip6_config.mip_era.lower() if cmip6_config.mip_era else _DEFAULT_PROJECT
    fields_set = cmip6_config.model_dump(exclude_none=True)

    issues: list[Cmip6ValidationIssue] = []
    for field, collection in _VALIDATABLE_FIELDS.items():
        if field not in fields_set:
            continue
        value = fields_set[field]
        try:
            valid = esgvoc_api.valid_term_in_collection(value, project, collection)
        except EsgvocNotFoundError:
            # CMIP6's collection names don't all carry over to other projects
            # -- e.g. CMIP7's model-identity collection is "source", not
            # "source_id". A missing collection means "can't check this
            # field under this project", not "the value is invalid"; skip
            # rather than either crash or falsely flag a value never checked.
            continue
        if not valid:
            issues.append(
                Cmip6ValidationIssue(field=field, value=value, collection=collection)
            )
    return issues
