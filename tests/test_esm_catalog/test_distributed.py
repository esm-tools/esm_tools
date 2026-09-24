"""Unit tests for esm_catalog.distributed.render_scripts, at the level below
the CLI (worker_mode is a click.Choice at the CLI layer, so an unknown mode
can only be exercised by calling render_scripts directly).
"""

from __future__ import annotations

import pytest

from esm_catalog.distributed import DEFAULT_VARS_TEMPLATE, render_scripts, render_vars_template

_BASE = {
    "job_prefix": "test",
    "scratch_dir": "/tmp/scratch",
    "image_tag": "v0.1.12",
    "exp_root": "/tmp/exp",
    "catalog_dir": "/tmp/exp/catalog",
}


def test_unknown_worker_mode_raises_value_error(tmp_path):
    with pytest.raises(ValueError, match="unknown worker_mode"):
        render_scripts({**_BASE, "worker_mode": "bogus"}, tmp_path)


def test_render_vars_template_no_overrides_matches_default_byte_for_byte():
    # A stricter guard than test_cli.py's substring checks: catches any drift
    # between DEFAULT_VARS_TEMPLATE and render_vars_template()'s no-override
    # path (they must stay byte-identical -- see render_vars_template's
    # docstring) that a substring assertion would silently miss.
    assert render_vars_template() == DEFAULT_VARS_TEMPLATE
