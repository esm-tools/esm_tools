"""The queryables.json sidecar: bare Item properties only.

Namelist params are deliberately excluded -- confirmed live, registering one
per parameter per component flooded the generic stock stac-browser filter
dropdown with thousands of entries, drowning out "variable". The dedicated
namelist widget doesn't need this registration at all (it reads parameter
names from each Collection's own nml:parameters field).
"""

from __future__ import annotations

import json
from pathlib import Path

from esm_catalog.models import ExperimentMetadata
from esm_catalog.scan.ingest import _write_queryables


def _experiment_metadata(**kwargs) -> ExperimentMetadata:
    return ExperimentMetadata(
        experiment_id="exp1", experiment_path=Path("/tmp/exp1"), **kwargs
    )


def test_variable_is_always_registered_even_with_no_namelists(tmp_path):
    _write_queryables(tmp_path, _experiment_metadata())

    properties = json.loads((tmp_path / "queryables.json").read_text())["properties"]
    assert properties["variable"] == {"type": "string"}


def test_namelist_params_are_not_registered_as_generic_queryables(tmp_path):
    namelists_by_component = {
        "echam": {"namelist.echam": {"radctl": {"io3": 1}}},
    }
    _write_queryables(
        tmp_path, _experiment_metadata(namelists_by_component=namelists_by_component)
    )

    properties = json.loads((tmp_path / "queryables.json").read_text())["properties"]
    assert properties == {"variable": {"type": "string"}}
    assert "nml__echam__namelist_echam__radctl__io3" not in properties
