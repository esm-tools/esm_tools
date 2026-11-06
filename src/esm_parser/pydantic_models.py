"""
Pydantic support for configuration values carrying provenance.

pydantic validates by the real type of the input, so the ``*WithProvenance``
wrappers that cannot subclass their target type (``BoolWithProvenance``,
``NoneWithProvenance``) are rejected for plain ``bool``/``Optional[...]``
fields. Models built from ``config`` values should subclass
``ProvenanceFreeModel`` instead of ``pydantic.BaseModel`` and annotate their
fields with plain types. Kept out of ``provenance.py`` so that importing
``esm_parser`` does not import pydantic.
"""

from pydantic import BaseModel, model_validator

from .provenance import strip_provenance


class ProvenanceFreeModel(BaseModel):
    """
    ``pydantic.BaseModel`` that accepts values with provenance by replacing them
    with their plain values before validation. The provenance is not kept in the
    model.
    """

    @model_validator(mode="before")
    @classmethod
    def _strip_provenance(cls, data):
        return strip_provenance(data)
