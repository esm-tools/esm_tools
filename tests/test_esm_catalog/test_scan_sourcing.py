"""Tests for the config-sourcing layer (esm_catalog.scan.sourcing)."""

from __future__ import annotations

from datetime import datetime

import pytest
from ruamel.yaml import YAML
from upath import UPath

from esm_catalog.scan.sourcing import (
    SourcingError,
    TidyOutdataEntry,
    _namelists_by_component,
    _parse_datestamp,
    _tidy_log_outdata,
    _walk_outdata,
    output_files,
    source_experiment,
)

_EXPID = "exp-alpha"


def _write_finished_config(
    exp_root: UPath,
    *,
    suffix: str = "",
    start_date: str = "2000-01-01",
    end_date: str = "2000-12-31",
    outdata_targets: dict | None = None,
) -> UPath:
    """Write a minimal synthetic finished_config under ``<exp_root>/config``."""
    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)

    targets = outdata_targets or {
        "echam_nc": str(exp_root / "outdata" / "echam" / f"{_EXPID}_echam.nc"),
    }
    # Materialize each declared target: output_files now trusts only config
    # targets that resolve to a real file (existence is checked, contents are not).
    for target in targets.values():
        tpath = UPath(target)
        tpath.parent.mkdir(parents=True, exist_ok=True)
        tpath.write_bytes(b"")
    doc = {
        "general": {
            "expid": _EXPID,
            "start_date": start_date,
            "end_date": end_date,
            "metadata": {
                "Description": "A synthetic test experiment.",
                "Authors": ["Ada Lovelace", "Grace Hopper"],
                "Institute": "AWI",
                "License": "CC-BY-4.0",
            },
        },
        "echam": {
            "outdata_targets": targets,
        },
    }

    name = f"{_EXPID}_finished_config.yaml{suffix}"
    path = config_dir / name
    with path.open("w") as stream:
        YAML(typ="safe").dump(doc, stream)
    return path


def test_source_experiment_reads_identity_and_metadata(tmp_path):
    exp_root = UPath(tmp_path)
    _write_finished_config(exp_root)

    metadata = source_experiment(exp_root)

    assert metadata.experiment_id == _EXPID
    assert metadata.description == "A synthetic test experiment."
    assert metadata.data_license == "CC-BY-4.0"
    assert metadata.run_start == datetime(2000, 1, 1)
    assert metadata.run_end == datetime(2000, 12, 31)
    assert [contact.name for contact in metadata.contacts] == [
        "Ada Lovelace",
        "Grace Hopper",
    ]
    assert all(contact.institution == "AWI" for contact in metadata.contacts)
    assert metadata.namelists_by_component == {}


def test_output_files_from_outdata_targets(tmp_path):
    exp_root = UPath(tmp_path)
    _write_finished_config(exp_root)

    files = output_files(exp_root)

    assert len(files) == 1
    (only,) = files
    assert only.component == "echam"
    assert only.path.name == f"{_EXPID}_echam.nc"
    assert only.md5 is None


def test_output_files_excludes_restart(tmp_path):
    exp_root = UPath(tmp_path)
    _write_finished_config(
        exp_root,
        outdata_targets={
            "echam_nc": str(exp_root / "outdata" / f"{_EXPID}_echam.nc"),
            "echam_restart": str(exp_root / "restart" / f"{_EXPID}_echam_restart.nc"),
        },
    )

    files = output_files(exp_root)

    assert [f.path.name for f in files] == [f"{_EXPID}_echam.nc"]


def test_output_files_pulls_md5_from_tidy_log(tmp_path):
    exp_root = UPath(tmp_path)
    target = str(exp_root / "outdata" / f"{_EXPID}_echam.nc")
    _write_finished_config(exp_root, outdata_targets={"echam_nc": target})

    log_dir = exp_root / "log"
    log_dir.mkdir(parents=True, exist_ok=True)
    tidy = {
        "echam": {
            "files": {
                "outdata": {
                    f"{_EXPID}_echam.nc": {
                        "destination": target,
                        "checksum": "d41d8cd98f00b204e9800998ecf8427e",
                    }
                }
            }
        }
    }
    tidy_path = log_dir / f"{_EXPID}_echam_file_operations_tidy_20000101-20001231.yaml"
    with tidy_path.open("w") as stream:
        YAML(typ="safe").dump(tidy, stream)

    (only,) = output_files(exp_root)
    assert only.md5 == "d41d8cd98f00b204e9800998ecf8427e"


def test_run_span_is_union_over_segments(tmp_path):
    exp_root = UPath(tmp_path)
    _write_finished_config(
        exp_root,
        suffix="_20000101-20001231",
        start_date="2000-01-01",
        end_date="2000-12-31",
    )
    _write_finished_config(
        exp_root,
        suffix="_20010101-20011231",
        start_date="2001-01-01",
        end_date="2001-12-31",
    )

    metadata = source_experiment(exp_root)

    assert metadata.run_start == datetime(2000, 1, 1)
    assert metadata.run_end == datetime(2001, 12, 31)


def test_run_span_from_filename_suffix(tmp_path):
    exp_root = UPath(tmp_path)
    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {"general": {"expid": _EXPID}, "echam": {"outdata_targets": {}}}
    path = config_dir / f"{_EXPID}_finished_config.yaml_20000101-20001231"
    with path.open("w") as stream:
        YAML(typ="safe").dump(doc, stream)

    metadata = source_experiment(exp_root)

    assert metadata.run_start == datetime(2000, 1, 1)
    assert metadata.run_end == datetime(2000, 12, 31)


def test_component_metadata_does_not_leak_into_experiment(tmp_path):
    # A component's own metadata describes the model, not the experiment. Only
    # general.metadata feeds the experiment's description/license/contacts.
    exp_root = UPath(tmp_path)
    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {
        "general": {
            "expid": _EXPID,
            "start_date": "2000-01-01",
            "end_date": "2000-12-31",
            "metadata": {
                "Description": "The experiment.",
                "Authors": ["Ada Lovelace"],
                "Institute": "AWI",
                "License": "CC-BY-4.0",
            },
        },
        "echam": {
            "outdata_targets": {},
            "metadata": {
                "Description": "The ECHAM atmosphere model, major version 6",
                "Authors": ["Bjorn Stevens"],
                "Institute": "MPI-Met",
                "License": "MPI-M-SLA",
            },
        },
    }
    path = config_dir / f"{_EXPID}_finished_config.yaml_20000101-20001231"
    with path.open("w") as stream:
        YAML(typ="safe").dump(doc, stream)

    metadata = source_experiment(exp_root)

    assert metadata.description == "The experiment."
    assert metadata.data_license == "CC-BY-4.0"
    assert [contact.name for contact in metadata.contacts] == ["Ada Lovelace"]


def test_no_general_metadata_leaves_fields_empty(tmp_path):
    # No general.metadata means no experiment description/license/contacts; there
    # is no fallback to component metadata (production validation flags this).
    exp_root = UPath(tmp_path)
    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {
        "general": {"expid": _EXPID, "start_date": "2000-01-01", "end_date": "2000-12-31"},
        "echam": {
            "outdata_targets": {},
            "metadata": {"Description": "model desc", "Authors": ["Dev"]},
        },
    }
    path = config_dir / f"{_EXPID}_finished_config.yaml_20000101-20001231"
    with path.open("w") as stream:
        YAML(typ="safe").dump(doc, stream)

    metadata = source_experiment(exp_root)

    assert metadata.description is None
    assert metadata.data_license is None
    assert metadata.contacts == []


def test_missing_config_raises_sourcing_error(tmp_path):
    exp_root = UPath(tmp_path)
    (exp_root / "config").mkdir(parents=True, exist_ok=True)

    with pytest.raises(SourcingError, match="no file matching"):
        source_experiment(exp_root)


def test_parse_datestamp_requires_both_bounds():
    """A datestamp is only a window when *both* ends parse; one bad end -> None."""
    assert _parse_datestamp("20000101-20001231") == (
        datetime(2000, 1, 1),
        datetime(2000, 12, 31),
    )
    assert _parse_datestamp("20000101-garbage") is None
    assert _parse_datestamp("garbage-20001231") is None


def test_tidy_log_outdata_yields_component_dest_md5_and_skips_malformed():
    """Each outdata entry yields (component, destination, md5).

    Malformed shapes (a non-dict block, non-dict ``outdata``, a non-dict entry)
    all precede valid entries, so a loop that ``break``\\s instead of
    ``continue``\\s is caught. A file with a destination but no checksum is still
    catalogued (md5 ``None`` -- incrementality falls back to a stat fingerprint);
    an entry with no destination names no file and is skipped.
    """
    doc = {
        "_not_a_dict": "junk",
        "log": {"files": {"outdata": "not-a-dict"}},
        "echam": {
            "files": {
                "outdata": {
                    "bad_entry": "not-a-dict",
                    "no_checksum": {"destination": "/d/no_checksum"},
                    "no_destination": {"checksum": "cafe"},
                    "good": {"destination": "/d/good", "checksum": "abc123"},
                }
            }
        },
    }
    assert list(_tidy_log_outdata(doc)) == [
        TidyOutdataEntry("echam", "/d/no_checksum", None),
        TidyOutdataEntry("echam", "/d/good", "abc123"),
    ]


def test_namelists_by_component(tmp_path):
    config = UPath(tmp_path) / "config"
    (config / "echam").mkdir(parents=True)
    (config / "fesom").mkdir(parents=True)
    (config / "oasis3mct").mkdir(parents=True)  # no namelist.* -> excluded

    (config / "echam" / "namelist.echam").write_text(
        "&radctl\n  co2vmr = 284.3e-6\n/\n"
    )
    # a per-segment copy must be skipped (the base file is canonical)
    (config / "echam" / "namelist.echam_18500101-18500131").write_text(
        "&radctl\n  co2vmr = 999.0\n/\n"
    )
    (config / "fesom" / "namelist.oce").write_text("&oce_dyn\n  c_d = 0.0025\n/\n")

    result = _namelists_by_component(UPath(tmp_path))

    assert set(result) == {"echam", "fesom"}  # oasis3mct dropped (no namelists)
    assert set(result["echam"]) == {"namelist.echam"}  # stamped copy skipped
    assert result["echam"]["namelist.echam"]["radctl"]["co2vmr"] == 284.3e-6
    assert result["fesom"]["namelist.oce"]["oce_dyn"]["c_d"] == 0.0025


def test_walk_outdata_component_from_boundary(tmp_path):
    outdata = UPath(tmp_path) / "outdata"
    (outdata / "fesom").mkdir(parents=True)
    (outdata / "echam").mkdir(parents=True)
    (outdata / "fesom" / "MLD1.fesom.1850.nc").write_text("x")
    (outdata / "echam" / "tas.nc").write_text("x")

    def components(root: UPath) -> dict[str, str]:
        return {cp.path.name: cp.component for cp in _walk_outdata(root, None)}

    # Absolute exp_root: component is the outdata subdir.
    assert components(UPath(tmp_path)) == {
        "MLD1.fesom.1850.nc": "fesom",
        "tas.nc": "echam",
    }
    # Relative exp_root must give the same result. Regression: a len(base) slice
    # stripped an absolute walk-root prefix and mislabelled the component (a
    # '/albedo/...' root under 'outdata' produced 'work').
    import os

    cwd = os.getcwd()
    try:
        os.chdir(tmp_path)
        assert components(UPath(".")) == {
            "MLD1.fesom.1850.nc": "fesom",
            "tas.nc": "echam",
        }
    finally:
        os.chdir(cwd)
