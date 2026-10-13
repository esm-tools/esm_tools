"""Tests for esm_catalog.scan.workspace."""

from __future__ import annotations

from upath import UPath

from esm_catalog.scan.workspace import WorkspaceState, load_state, save_state


def test_load_state_none_when_never_scanned(tmp_path):
    assert load_state(UPath(tmp_path) / "catalog") is None


def test_load_state_none_for_missing_remote_backend_too():
    # A single read_text() + FileNotFoundError, not exists()-then-read_text()
    # -- confirm the exception path also fires on a non-local fsspec backend.
    assert load_state(UPath("memory://workspace/never-scanned/catalog")) is None


def test_load_state_round_trips_saved_state(tmp_path):
    catalog = UPath(tmp_path) / "catalog"
    state = WorkspaceState(experiment_id="exp-alpha")
    save_state(catalog, state)

    loaded = load_state(catalog)

    assert loaded == state
