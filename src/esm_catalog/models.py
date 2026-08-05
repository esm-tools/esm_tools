"""Pydantic models for experiment identity and PI/author contacts."""

from __future__ import annotations

from pathlib import Path
from typing import Optional, TypedDict, cast

from pydantic import BaseModel, ConfigDict, Field, SkipValidation, field_serializer

from esm_catalog.namelist import NamelistsByComponent
from esm_catalog.paleo import PaleoConfig
from esm_catalog.types import ExperimentId, License

Orcid = str
"""An ORCID identifier, e.g. '0000-0001-2345-6789' or its full URL."""

Institution = str
"""An affiliated institution name."""


class StacContact(TypedDict, total=False):
    """A Contact serialized to the STAC contacts extension entry shape."""

    name: str
    roles: list[str]
    identifier: str
    organization: str


class Contact(BaseModel):
    """A single PI or author contact for an experiment.

    Attributes
    ----------
    name : str or None
        Full name.
    orcid : Orcid or None
        ORCID identifier, e.g. "0000-0001-2345-6789".
    institution : Institution or None
        Affiliated institution.
    roles : list of str
        STAC contact roles (defaults to ["principal_investigator"]).
    """

    model_config = ConfigDict(frozen=True)

    name: Optional[str] = None
    orcid: Optional[Orcid] = Field(default=None, serialization_alias="identifier")
    institution: Optional[Institution] = Field(
        default=None, serialization_alias="organization"
    )
    roles: list[str] = ["principal_investigator"]

    @field_serializer("orcid")
    def _full_orcid_url(self, orcid: Optional[Orcid]) -> Optional[Orcid]:
        """Serialize an ORCID as its full ``https://orcid.org/`` URL (stored raw)."""
        if orcid and not orcid.startswith("https://orcid.org/"):
            return f"https://orcid.org/{orcid}"
        return orcid

    def to_stac(self) -> StacContact:
        """Serialize to the STAC contacts entry (aliased fields, empties dropped)."""
        return cast(StacContact, self.model_dump(by_alias=True, exclude_none=True))


class ExperimentMetadata(BaseModel):
    """Experiment identity + pre-scanned config for building its Collection and Items.

    A STAC Collection is a whole experiment (all components), so this carries no
    single component — the per-file component lives on FileMetadata instead.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True)

    experiment_id: ExperimentId
    description: Optional[str] = None
    data_license: Optional[License] = None
    experiment_path: Optional[Path] = None
    # Populated by the scan layer, keyed by component. Empty by default, in
    # which case the namelist extension is a no-op. Values are f90nml.Namelist
    # objects, so validation is skipped rather than coerced.
    namelists_by_component: SkipValidation[NamelistsByComponent] = {}
    paleo_config: Optional[PaleoConfig] = None
    contacts: list[Contact] = []
