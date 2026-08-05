"""Shared fixtures for the esm_catalog STAC tests.

The builders themselves live in helpers.py (importable plain functions so they
compose inline in make_item(...) calls); these fixtures are thin wrappers for
tests that prefer fixture injection.
"""

from __future__ import annotations

import pytest

from .helpers import bare_collection as _bare_collection
from .helpers import bare_item as _bare_item
from .helpers import make_ctx as _make_ctx


@pytest.fixture
def bare_collection():
    return _bare_collection()


@pytest.fixture
def bare_item():
    return _bare_item()


@pytest.fixture
def make_ctx():
    return _make_ctx


@pytest.fixture
def temp_nc(tmp_path):
    """A throwaway .nc file on disk; make_item only needs the path to exist."""
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    return f
