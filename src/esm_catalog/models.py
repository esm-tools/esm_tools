from __future__ import annotations

from pathlib import Path
from typing import ClassVar, TypedDict, cast

from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    SkipValidation,
    field_serializer,
    model_validator,
)

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
        Full name (production-required).
    orcid : Orcid or None
        ORCID identifier, e.g. "0000-0001-2345-6789" (optional).
    institution : Institution or None
        Affiliated institution (optional in dev, required in production).
    roles : list of str
        STAC contact roles (defaults to ["principal_investigator"]).
    """

    model_config = ConfigDict(frozen=True)

    PRODUCTION_REQUIRED: ClassVar[list[str]] = ["name", "institution"]

    name: str | None = None
    orcid: Orcid | None = Field(default=None, serialization_alias="identifier")
    institution: Institution | None = Field(
        default=None, serialization_alias="organization"
    )
    roles: list[str] = ["principal_investigator"]

    @field_serializer("orcid")
    def _full_orcid_url(self, orcid: Orcid | None) -> Orcid | None:
        """Serialize an ORCID as its full ``https://orcid.org/`` URL (stored raw)."""
        if orcid and not orcid.startswith("https://orcid.org/"):
            return f"https://orcid.org/{orcid}"
        return orcid

    def to_stac(self) -> StacContact:
        """Serialize to the STAC contacts entry (aliased fields, empties dropped)."""
        return cast(StacContact, self.model_dump(by_alias=True, exclude_none=True))

    @classmethod
    def from_dict(cls, entry: dict) -> Contact:
        """Build a Contact from a dict (keys: name, orcid, institution, roles)."""
        return cls.model_validate(entry)

    def validate_production_req(self) -> None:
        """Check that the production-required fields are present.

        Raises
        ------
        ValueError
            If any production-required field (name, institution) is missing.
        """
        missing = [
            field for field in self.PRODUCTION_REQUIRED if not getattr(self, field)
        ]
        if missing:
            raise ValueError(
                f"Contact '{self.name}' is missing required fields: {', '.join(missing)}"
            )


class ExperimentMetadata(BaseModel):
    """Experiment identity + pre-scanned config for building its Collection and Items.

    A STAC Collection is a whole experiment (all components), so this carries no
    single component — the per-file component lives on FileMetadata instead.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True)

    PRODUCTION_REQUIRED: ClassVar[list[str]] = ["description", "data_license"]

    experiment_id: ExperimentId
    production: bool = False
    description: str | None = None
    data_license: License | None = None
    experiment_path: Path | None = None
    # Populated by the scan layer, keyed by component. Empty by default, in
    # which case the namelist extension is a no-op. Values are f90nml.Namelist
    # objects, so validation is skipped rather than coerced.
    namelists_by_component: SkipValidation[NamelistsByComponent] = {}
    paleo_config: PaleoConfig | None = None
    contacts: list[Contact] = []

    @model_validator(mode="after")
    def _check_production(self) -> ExperimentMetadata:
        """Require the metadata fields and at least one valid contact in production.

        Returns
        -------
        ExperimentMetadata
            The validated model (unchanged).

        Raises
        ------
        ValueError
            If, with ``production`` set, a required field is empty or no valid
            contact is present.
        """
        if not self.production:
            return self
        missing = [
            field
            for field in self.PRODUCTION_REQUIRED
            if getattr(self, field) in (None, "")
        ]
        if missing:
            raise ValueError(
                f"Production context requires non-empty fields: {', '.join(missing)}"
            )
        if not self.contacts:
            raise ValueError("Production context requires at least one contact")
        for contact in self.contacts:
            contact.validate_production_req()
        return self
