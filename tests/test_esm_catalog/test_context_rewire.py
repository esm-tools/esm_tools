"""CollectionContext regains collection_title for the scan layer."""

from __future__ import annotations

from esm_catalog.context import CollectionContext


def test_context_has_collection_title_default():
    ctx = CollectionContext(experiment_id="e", component="c", collection_id="e")
    assert ctx.collection_title == ""


def test_context_collection_title_settable():
    ctx = CollectionContext(
        experiment_id="e", component="c", collection_id="e", collection_title="My Exp"
    )
    assert ctx.collection_title == "My Exp"
