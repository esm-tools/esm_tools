from typing import List, Optional

from pydantic import BaseModel, ConfigDict

from esm_parser.provenance import (
    BoolWithProvenance,
    DictWithProvenance,
    NoneWithProvenance,
)
from esm_parser.pydantic_models import ProvenanceFreeModel


class Frozen(BaseModel):
    model_config = ConfigDict(frozen=True)

    name: str


class Model(ProvenanceFreeModel):
    flag: bool = False
    queue: Optional[str] = "default"
    extra: Optional[dict] = None
    items: List[Frozen] = []


def test_bool_with_provenance_becomes_bool():
    model = Model(flag=BoolWithProvenance(True, None))
    assert type(model.flag) is bool
    assert model.flag is True


def test_none_with_provenance_becomes_none():
    model = Model(queue=NoneWithProvenance(None, None))
    assert model.queue is None


def test_nested_dict_with_provenance_becomes_plain_dict():
    extra = DictWithProvenance({"a": True, "b": None}, {})
    model = Model(extra=extra)
    assert type(model.extra) is dict
    assert model.extra == {"a": True, "b": None}
    assert type(model.extra["a"]) is bool


def test_nested_frozen_model_passes_through():
    item = Frozen(name="compute")
    model = Model(items=[item])
    assert model.items[0] is item


def test_model_validate_dict_input():
    model = Model.model_validate({"flag": BoolWithProvenance(True, None)})
    assert model.flag is True


def test_string_bools_use_pydantic_parsing():
    assert Model(flag="false").flag is False
