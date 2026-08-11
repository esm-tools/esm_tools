"""Tests for make_collection and update_extent."""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from pystac import Collection, Item

from esm_catalog.collection import make_collection, update_extent
from esm_catalog.models import Contact
from esm_catalog.registry import EXTENSION_URLS

from .helpers import make_exp_metadata


@pytest.fixture
def collection():
    """A Collection built from a default experiment (overrides the bare-object
    conftest fixture; these tests exercise the built collection, not an empty one)."""
    return make_collection(make_exp_metadata())


def _item(dt, bbox=None, dt_end=None):
    return Item(
        id="test",
        geometry=None,
        bbox=bbox,
        datetime=dt if dt_end is None or dt == dt_end else None,
        properties={},
        start_datetime=dt if dt_end is not None and dt != dt_end else None,
        end_datetime=dt_end if dt_end is not None and dt != dt_end else None,
    )


def test_collection_is_pystac(collection):
    assert isinstance(collection, Collection)


def test_collection_minimal_skeleton(collection):
    # id is unique (name + path hash); the human name is the title.
    assert collection.id.startswith("exp-alpha-")
    assert collection.title == "exp-alpha"
    # components come from the experiment's `components` field; none set → empty.
    assert collection.extra_fields["components"] == []


def test_collection_lists_all_experiment_components():
    # A Collection is the whole experiment: it lists every component (from the
    # components field, incl. baked-in submodels), independent of namelists.
    exp_metadata = make_exp_metadata(
        components=["echam", "fesom"],
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"delta_time": 450}}},
            "fesom": {"namelist.fesom": {"clockinit": {"yearnew": 1850}}},
        },
    )
    collection = make_collection(exp_metadata)
    assert collection.extra_fields["components"] == ["echam", "fesom"]
    params = collection.extra_fields["nml:parameters"]
    assert params["echam:namelist.echam:runctl:delta_time"] == 450
    assert params["fesom:namelist.fesom:clockinit:yearnew"] == 1850


def test_update_extent_expands_temporal(collection):
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt, bbox=[0, 0, 1, 1]))
    assert collection.extent.temporal.intervals[0][0] == dt


def test_update_extent_expands_to_wider_range(collection):
    dt1 = datetime(2000, 6, 1, tzinfo=timezone.utc)
    dt2 = datetime(1990, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt1, bbox=[0, 0, 1, 1]))
    update_extent(collection, _item(dt2, bbox=[0, 0, 1, 1]))
    assert collection.extent.temporal.intervals[0][0] == dt2
    assert collection.extent.temporal.intervals[0][1] == dt1


# --- collection description ---


def test_collection_description_falls_back_to_experiment_id():
    # STAC requires a string description; a None experiment description must not
    # produce "description": null. Fall back to the experiment id.
    collection = make_collection(make_exp_metadata(description=None))
    assert collection.description == "exp-alpha"
    assert collection.to_dict()["description"] == "exp-alpha"


def test_collection_description_used_when_present():
    collection = make_collection(make_exp_metadata(description="a control run"))
    assert collection.description == "a control run"


# --- collection license ---


def test_collection_license_defaults_to_proprietary():
    assert make_collection(make_exp_metadata()).license == "proprietary"


def test_collection_license_from_exp_metadata():
    exp_metadata = make_exp_metadata(
        experiment_id="exp-alpha",
        data_license="CC-BY-4.0",
    )
    assert make_collection(exp_metadata).license == "CC-BY-4.0"


# --- spatial extent ---


def test_spatial_extent_replaces_default_with_first_bbox(collection):
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt, bbox=[0, 0, 10, 10]))
    assert collection.extent.spatial.bboxes == [[0, 0, 10, 10]]


def test_spatial_extent_merges_second_bbox(collection):
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt, bbox=[0, 0, 10, 10]))
    update_extent(collection, _item(dt, bbox=[-20, 5, 5, 30]))
    assert collection.extent.spatial.bboxes == [[-20, 0, 10, 30]]


def test_item_without_bbox_leaves_spatial_untouched(collection):
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt))
    assert collection.extent.spatial.bboxes == [[-180.0, -90.0, 180.0, 90.0]]


def test_spatial_extent_global_first_item_not_shrunk_by_regional(collection):
    # A global first item equals the default by value but must not read as
    # 'unset': a later regional item widens (stays global), never replaces.
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt, bbox=[-180, -90, 180, 90]))
    update_extent(collection, _item(dt, bbox=[0, 0, 10, 10]))
    assert collection.extent.spatial.bboxes == [[-180, -90, 180, 90]]


def test_update_extent_ignores_non_four_length_bbox(collection):
    # update_extent only widens for a 4-element (2D) bbox; a 3D/6-element bbox
    # is left untouched rather than mis-merged against the 2D default.
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt, bbox=[0, 0, 0, 10, 10, 10]))
    assert collection.extent.spatial.bboxes == [[-180.0, -90.0, 180.0, 90.0]]


# --- temporal extent ---


def test_temporal_extent_not_shrunk_by_inner_item(collection):
    dt_lo = datetime(1990, 1, 1, tzinfo=timezone.utc)
    dt_hi = datetime(2010, 1, 1, tzinfo=timezone.utc)
    dt_mid = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(collection, _item(dt_lo, bbox=[0, 0, 1, 1], dt_end=dt_hi))
    update_extent(collection, _item(dt_mid, bbox=[0, 0, 1, 1]))
    assert collection.extent.temporal.intervals[0] == [dt_lo, dt_hi]


# --- contacts ---


def test_collection_no_contacts_no_extension(collection):
    # Default experiment has no contacts, so the collection carries none and does
    # not register the extension.
    assert "contacts" not in collection.extra_fields
    assert EXTENSION_URLS["contacts"] not in collection.stac_extensions


def test_collection_wires_contacts_and_registers_extension():
    # make_collection places the serialized contacts on the collection and
    # registers the extension. Contact.to_stac() serialization is tested in
    # test_models.py; here we only check the wiring.
    contacts = [
        Contact(name="Jane Doe", institution="AWI"),
        Contact(name="John Smith", institution="DKRZ"),
    ]
    collection = make_collection(make_exp_metadata(contacts=contacts))
    assert collection.extra_fields["contacts"] == [c.to_stac() for c in contacts]
    assert EXTENSION_URLS["contacts"] in collection.stac_extensions
