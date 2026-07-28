from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import ClassVar


@dataclass(frozen=True)
class Contact:
    """A single PI or author contact for an experiment.

    Attributes:
        name:        Full name (required).
        orcid:       ORCID identifier, e.g. "0000-0001-2345-6789" (optional).
        institution: Affiliated institution (optional in dev, required in production).
        roles:       STAC contact roles (defaults to ["principal_investigator"]).
    """

    PRODUCTION_REQUIRED: ClassVar[list[str]] = ["name", "institution"]

    name: str
    orcid: str | None = None
    institution: str | None = None
    roles: list = field(default_factory=lambda: ["principal_investigator"])

    @classmethod
    def from_dict(cls, entry: dict) -> Contact:
        """Build a Contact from a dict entry.

        Expected keys: name, orcid, institution, roles.
        """
        return cls(
            name=entry.get("name"),
            orcid=entry.get("orcid"),
            institution=entry.get("institution"),
            roles=entry.get("roles", ["principal_investigator"]),
        )

    def validate_production_req(self) -> None:
        """Raise ValueError if production-required fields are missing."""
        missing = [f for f in self.PRODUCTION_REQUIRED if not getattr(self, f)]
        if missing:
            raise ValueError(
                f"Contact '{self.name}' is missing required fields: {', '.join(missing)}"
            )


@dataclass
class CollectionContext:
    """Identity + pre-scanned context for building a collection's items."""

    PRODUCTION_REQUIRED: ClassVar[list[str]] = [
        "description",
        "data_license",
    ]

    experiment_id: str
    component: str
    collection_id: str
    production: bool = False
    description: str | None = None
    data_license: str | None = None
    experiment_path: Path | None = None
    namelists_by_component: dict = field(default_factory=dict)
    contacts: list[Contact] = field(default_factory=list)

    def __post_init__(self):
        """Validate production-required fields immediately after construction."""
        if self.production:
            self._check_production_fields()

    def _check_production_fields(self):
        """Raise ValueError if any PRODUCTION_REQUIRED field is empty or None."""
        missing = [
            f for f in self.PRODUCTION_REQUIRED if getattr(self, f) in (None, "")
        ]
        if missing:
            raise ValueError(
                f"Production context requires non-empty fields: {', '.join(missing)}"
            )
        if not self.contacts:
            raise ValueError("Production context requires at least one contact")
        for contact in self.contacts:
            contact.validate_production_req()
