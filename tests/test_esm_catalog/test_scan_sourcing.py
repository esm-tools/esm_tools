"""Tests for the config-sourcing layer (esm_catalog.scan.sourcing)."""

from __future__ import annotations

import json
import os
from datetime import datetime

import pytest
from ruamel.yaml import YAML
from upath import UPath

from esm_catalog.scan import sourcing
from esm_catalog.scan.sourcing import (
    SourcingError,
    TidyOutdataEntry,
    _load_run_cfgs,
    _namelists_by_component,
    _parse_datestamp,
    _tidy_log_outdata,
    _walk_outdata,
    _walk_restart,
    output_files,
    restart_files,
    source_experiment,
    source_files,
)

_EXPID = "exp-alpha"


def _write_finished_config(
    exp_root: UPath,
    *,
    suffix: str = "",
    start_date: str = "2000-01-01",
    end_date: str = "2000-12-31",
    outdata_targets: dict | None = None,
    restart_out_targets: dict | None = None,
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
    if restart_out_targets is not None:
        for source in restart_out_targets.values():
            spath = UPath(source)
            spath.parent.mkdir(parents=True, exist_ok=True)
            spath.write_bytes(b"")
        doc["fesom"] = {"restart_out_targets": restart_out_targets}

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


def test_output_files_dedupes_before_exists_check(tmp_path, monkeypatch):
    # Two segments declaring the same destination (the default helper target
    # is static across suffixes) must not pay an exists() round trip twice --
    # dedup has to happen before the check, not just on the final result list.
    exp_root = UPath(tmp_path)
    _write_finished_config(exp_root, suffix="_20000101-20001231")
    _write_finished_config(exp_root, suffix="_20010101-20011231")

    calls = []
    path_cls = type(exp_root)  # local paths dispatch to a pathlib subclass
    real_exists = path_cls.exists

    def counting_exists(self):
        calls.append(str(self))
        return real_exists(self)

    monkeypatch.setattr(path_cls, "exists", counting_exists)

    files = output_files(exp_root)

    assert len(files) == 1
    target_path = files[0].path
    assert calls.count(str(target_path)) == 1


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


def test_restart_files_reads_restart_out_targets(tmp_path):
    exp_root = UPath(tmp_path)
    restart_dir = exp_root / "restart" / "fesom"
    _write_finished_config(
        exp_root,
        restart_out_targets={
            "oce_restart": str(restart_dir / "fesom.2000.oce.nc"),
            "ice_restart": str(restart_dir / "fesom.2000.ice.nc"),
        },
    )

    files = restart_files(exp_root)

    assert len(files) == 2
    assert {f.category for f in files} == {"oce_restart", "ice_restart"}
    # One Item per component, not per restart category -- both share a stream.
    assert {f.stream for f in files} == {"restart"}
    assert {f.role for f in files} == {"restart"}
    assert {f.component for f in files} == {"fesom"}


def test_restart_files_skips_undeclared_or_missing(tmp_path):
    exp_root = UPath(tmp_path)
    # No restart_out_targets declared at all -- no finished_config.fesom block.
    _write_finished_config(exp_root)
    assert restart_files(exp_root) == []


def test_restart_files_skips_a_directory_target_without_crashing(tmp_path):
    # FESOM/OIFS restarts are real directories in production (e.g.
    # fesom.<year>.oce.restart/, one NetCDF per field). A declared
    # restart_out_targets entry resolving to one must not become an
    # OutputFile -- detect() raises IsADirectoryError on a directory, which
    # previously escaped the worker and aborted the whole scan.
    exp_root = UPath(tmp_path)
    restart_dir_target = exp_root / "restart" / "fesom" / "fesom.2000.oce.restart"
    _write_finished_config(
        exp_root, restart_out_targets={"oce_restart": str(restart_dir_target)}
    )
    restart_dir_target.unlink()
    restart_dir_target.mkdir()
    (restart_dir_target / "temp.nc").write_bytes(b"")

    files = restart_files(exp_root)

    assert all(f.path != restart_dir_target for f in files)


def test_restart_files_walks_when_component_declares_no_targets(tmp_path):
    # Confirmed against a real production experiment: fesom manages restarts
    # via wildcard matching and declares no restart_out_targets at all, so
    # its tidied files are only findable by walking restart/<component>/.
    exp_root = UPath(tmp_path)
    _write_finished_config(exp_root)  # no fesom block at all
    restart_dir = exp_root / "restart" / "fesom"
    (restart_dir).mkdir(parents=True)
    (restart_dir / "fesom.2000.oce.restart").write_bytes(b"")
    (restart_dir / "fesom.2000.ice.restart").write_bytes(b"")

    files = restart_files(exp_root)

    assert len(files) == 2
    assert {f.component for f in files} == {"fesom"}
    assert {f.stream for f in files} == {"restart"}
    assert {f.category for f in files} == {"fesom.oce", "fesom.ice"}


def test_source_files_combines_outdata_and_restart(tmp_path):
    exp_root = UPath(tmp_path)
    restart_dir = exp_root / "restart" / "fesom"
    _write_finished_config(
        exp_root,
        restart_out_targets={"oce_restart": str(restart_dir / "fesom.2000.oce.nc")},
    )

    files = source_files(exp_root)

    assert len(files) == 2  # the default echam outdata target + one restart file
    assert {f.role for f in files} == {"data", "restart"}


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


def _write_tidy_log(exp_root, suffix, component, filename, destination, checksum):
    log_dir = exp_root / "log"
    log_dir.mkdir(parents=True, exist_ok=True)
    tidy = {
        component: {
            "files": {
                "outdata": {
                    filename: {"destination": destination, "checksum": checksum}
                }
            }
        }
    }
    tidy_path = log_dir / f"{_EXPID}_echam_file_operations_tidy_{suffix}.yaml"
    with tidy_path.open("w") as stream:
        YAML(typ="safe").dump(tidy, stream)


def test_output_files_reports_progress_per_tidy_log(tmp_path):
    # Each tidy log is a full YAML parse, same cost shape as a finished_config
    # -- without this callback, a run with many segments goes silent for as
    # long as parsing all of them takes (confirmed live: a frozen progress
    # display, mistaken for a hang).
    exp_root = UPath(tmp_path)
    _write_finished_config(exp_root)
    target1 = str(exp_root / "outdata" / "a.nc")
    target2 = str(exp_root / "outdata" / "b.nc")
    for target in (target1, target2):
        UPath(target).parent.mkdir(parents=True, exist_ok=True)
        UPath(target).write_bytes(b"")
    _write_tidy_log(exp_root, "20000101-20001231", "echam", "a.nc", target1, "a" * 32)
    _write_tidy_log(exp_root, "20010101-20011231", "echam", "b.nc", target2, "b" * 32)

    calls = []
    output_files(exp_root, on_tidy_log=lambda i, n: calls.append((i, n)))

    assert calls == [(1, 2), (2, 2)]


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


def _write_general(exp_root, general):
    """Write a minimal finished_config with the given ``general`` section."""
    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {
        "general": {
            "expid": _EXPID,
            "start_date": "2000-01-01",
            "end_date": "2000-12-31",
            **general,
        },
    }
    path = config_dir / f"{_EXPID}_finished_config.yaml_20000101-20001231"
    with path.open("w") as stream:
        YAML(typ="safe").dump(doc, stream)


def test_author_roles_are_read_from_config(tmp_path):
    # Each Authors entry may name its own role(s); one without roles is an author.
    exp_root = UPath(tmp_path)
    _write_general(
        exp_root,
        {
            "user": "a123456",
            "metadata": {
                "Institute": "Example Institute",
                "Authors": [
                    {"name": "Ada Lovelace"},
                    {"name": "Grace Hopper", "roles": ["principalInvestigator"]},
                    {"name": "Alan Turing", "roles": "pointOfContact"},
                ],
            },
        },
    )

    metadata = source_experiment(exp_root)

    assert [(c.name, c.roles) for c in metadata.contacts] == [
        ("Ada Lovelace", ["author"]),
        ("Grace Hopper", ["principalInvestigator"]),
        ("Alan Turing", ["pointOfContact"]),
    ]
    # Given Authors win: the user id is not added on top of them.
    assert "a123456" not in [c.name for c in metadata.contacts]


def test_plain_author_names_default_to_author_role(tmp_path):
    exp_root = UPath(tmp_path)
    _write_general(exp_root, {"metadata": {"Authors": ["Ada Lovelace"]}})

    metadata = source_experiment(exp_root)

    assert [(c.name, c.roles) for c in metadata.contacts] == [
        ("Ada Lovelace", ["author"])
    ]


def test_user_id_is_fallback_contact_without_authors(tmp_path, monkeypatch):
    # No Authors given: the user who ran the experiment becomes its author,
    # so the run can still be traced back to a person. Here the scanning
    # machine does not know the account, so only the id is shown.
    monkeypatch.setattr(sourcing, "_full_name_of", lambda user: None)
    exp_root = UPath(tmp_path)
    _write_general(exp_root, {"user": "a123456"})

    metadata = source_experiment(exp_root)

    assert [(c.name, c.roles) for c in metadata.contacts] == [
        ("a123456", ["author"])
    ]


def test_fallback_contact_shows_full_name_when_known(tmp_path, monkeypatch):
    # On the machine where the account exists, its full name is added.
    monkeypatch.setattr(
        sourcing, "_full_name_of", lambda user: "Ada Lovelace"
    )
    exp_root = UPath(tmp_path)
    _write_general(exp_root, {"user": "a123456"})

    metadata = source_experiment(exp_root)

    assert [(c.name, c.roles) for c in metadata.contacts] == [
        ("Ada Lovelace (a123456)", ["author"])
    ]


@pytest.mark.parametrize(
    "comment, expected",
    [
        ("Ada Lovelace", "Ada Lovelace"),
        ("Ada Lovelace,Room 1,0471", "Ada Lovelace"),
        ("", None),
    ],
)
def test_full_name_of_reads_account_comment(monkeypatch, comment, expected):
    pwd = pytest.importorskip("pwd")

    class Entry:
        pw_gecos = comment

    monkeypatch.setattr(pwd, "getpwnam", lambda user: Entry())
    assert sourcing._full_name_of("a123456") == expected


def test_full_name_of_unknown_account_is_none(monkeypatch):
    pwd = pytest.importorskip("pwd")

    def unknown(user):
        raise KeyError(user)

    monkeypatch.setattr(pwd, "getpwnam", unknown)
    assert sourcing._full_name_of("nobody-here") is None


def _write_contacts_file(catalog, text):
    """Write a catalog_contacts.yaml into *catalog* (an experiment's catalog dir)."""
    catalog.mkdir(parents=True, exist_ok=True)
    (catalog / "catalog_contacts.yaml").write_text(text)


def test_contacts_file_wins_over_authors_and_user(tmp_path, monkeypatch):
    monkeypatch.setattr(sourcing, "_full_name_of", lambda user: None)
    exp_root = UPath(tmp_path)
    _write_general(
        exp_root,
        {"user": "a123456", "metadata": {"Authors": ["Someone From Runscript"]}},
    )
    _write_contacts_file(
        exp_root / "catalog",
        """
institution: Example Institute
contacts:
    - name: Ada Lovelace
      emails: [ada@example.org, ada@home.example]
    - name: Grace Hopper
      roles: [principalInvestigator]
      institution: Example University
      orcid: 0000-0000-0000-0000
      email: grace@example.org
""",
    )

    metadata = source_experiment(exp_root)

    assert [
        (c.name, c.roles, c.institution, c.orcid, c.emails)
        for c in metadata.contacts
    ] == [
        (
            "Ada Lovelace",
            ["author"],
            "Example Institute",
            None,
            ["ada@example.org", "ada@home.example"],
        ),
        (
            "Grace Hopper",
            ["principalInvestigator"],
            "Example University",
            "0000-0000-0000-0000",
            ["grace@example.org"],  # a single 'email:' is read too
        ),
    ]


@pytest.mark.parametrize(
    "text",
    ["", "contacts: []", "just a sentence", "contacts: [unclosed"],
    ids=["empty", "no-contacts", "not-a-mapping", "broken-yaml"],
)
def test_unusable_contacts_file_falls_back(tmp_path, monkeypatch, text):
    # A missing, empty or broken file never stops the scan.
    monkeypatch.setattr(sourcing, "_full_name_of", lambda user: None)
    exp_root = UPath(tmp_path)
    _write_general(exp_root, {"user": "a123456"})
    _write_contacts_file(exp_root / "catalog", text)

    metadata = source_experiment(exp_root)

    assert [(c.name, c.roles) for c in metadata.contacts] == [
        ("a123456", ["author"])
    ]


def test_contacts_file_is_read_from_a_separate_catalog_dir(tmp_path, monkeypatch):
    # With --catalog-dir elsewhere (e.g. scanning someone else's run), the
    # contacts file lives next to that catalog, not in the experiment.
    monkeypatch.setattr(sourcing, "_full_name_of", lambda user: None)
    exp_root = UPath(tmp_path / "experiment")
    catalog = UPath(tmp_path / "my_catalogs" / "run01")
    _write_general(exp_root, {"user": "b654321"})
    _write_contacts_file(catalog, "contacts:\n    - name: Alan Turing\n")

    with_catalog = source_experiment(exp_root, catalog=catalog)
    without_catalog = source_experiment(exp_root)

    assert [c.name for c in with_catalog.contacts] == ["Alan Turing"]
    assert [c.name for c in without_catalog.contacts] == ["b654321"]


def test_load_run_cfgs_reports_progress_per_file(tmp_path):
    # The sequential (non-distributed) path is the common case; without this
    # callback a slow remote load of many finished_configs is silent for its
    # whole duration (confirmed live: one static spinner, no movement).
    exp_root = UPath(tmp_path)
    _write_finished_config(exp_root, suffix="_20000101-20001231")
    _write_finished_config(exp_root, suffix="_20010101-20011231")
    _write_finished_config(exp_root, suffix="_20020101-20021231")

    calls = []
    run_cfgs = _load_run_cfgs(exp_root, on_config=lambda i, n: calls.append((i, n)))

    assert len(run_cfgs) == 3
    assert calls == [(1, 3), (2, 3), (3, 3)]


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
        TidyOutdataEntry("echam", "no_checksum", "/d/no_checksum", None),
        TidyOutdataEntry("echam", "good", "/d/good", "abc123"),
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
    cwd = os.getcwd()
    try:
        os.chdir(tmp_path)
        assert components(UPath(".")) == {
            "MLD1.fesom.1850.nc": "fesom",
            "tas.nc": "echam",
        }
    finally:
        os.chdir(cwd)


def test_walk_outdata_local_root_never_stats_an_entry(tmp_path, monkeypatch):
    # fsspec's generic walk() stat()s every entry just to populate size/mtime
    # this walk never uses -- on a Lustre-backed root each of those is its
    # own MDS round trip. os.scandir()'s is_dir()/is_file() must settle the
    # file/dir question from readdir()'s own d_type instead, with no syscall
    # at all -- confirmed here by making a real stat() call fail the test.
    outdata = UPath(tmp_path) / "outdata" / "echam"
    outdata.mkdir(parents=True)
    (outdata / "tas.nc").write_text("x")

    def exploding_stat(*args, **kwargs):
        raise AssertionError("must not stat() while walking")

    monkeypatch.setattr(os, "stat", exploding_stat)

    (only,) = list(_walk_outdata(UPath(tmp_path)))
    assert only.component == "echam"


def test_walk_restart_local_root_never_stats_an_entry(tmp_path, monkeypatch):
    restart = UPath(tmp_path) / "restart" / "fesom"
    restart.mkdir(parents=True)
    (restart / "fesom.oce.restart.nc").write_text("x")

    def exploding_stat(*args, **kwargs):
        raise AssertionError("must not stat() while walking")

    monkeypatch.setattr(os, "stat", exploding_stat)

    (only,) = list(_walk_restart(UPath(tmp_path)))
    assert only.component == "fesom"


# --------------------------------------------------------------------------- #
# Collection id: a random stamp, made once per catalog folder.
# --------------------------------------------------------------------------- #


def test_collection_stamp_is_made_once_and_reused(tmp_path, monkeypatch):
    exp_root = UPath(tmp_path / "run01")
    _write_general(exp_root, {})

    first = source_experiment(exp_root).collection_id
    monkeypatch.chdir(tmp_path / "run01")
    from_inside = source_experiment(UPath(".")).collection_id

    stamp = first.rsplit("-", 1)[1]
    assert len(stamp) == 8 and int(stamp, 16) >= 0  # 8 hex digits
    assert from_inside == first
    saved = json.loads((exp_root / "catalog" / "esm-catalog.json").read_text())
    assert saved["collection_stamp"] == stamp


def test_same_name_in_the_same_place_gets_a_new_id_after_a_move(tmp_path):
    exp_root = UPath(tmp_path / "run01")
    _write_general(exp_root, {})
    old_id = source_experiment(exp_root).collection_id

    (tmp_path / "run01").rename(tmp_path / "moved")  # old experiment moved away...
    moved_id = source_experiment(UPath(tmp_path / "moved")).collection_id
    _write_general(exp_root, {})  # ...and a new one with the same name in its place
    new_id = source_experiment(exp_root).collection_id

    assert moved_id == old_id  # the moved experiment keeps its id
    assert new_id != old_id  # the new one does not overwrite it


def test_existing_catalog_keeps_its_collection_id(tmp_path):
    exp_root = UPath(tmp_path / "run01")
    _write_general(exp_root, {})
    catalog = exp_root / "catalog"
    catalog.mkdir()
    (catalog / "collection.json").write_text(json.dumps({"id": f"{_EXPID}-c7b37639"}))

    assert source_experiment(exp_root).collection_id == f"{_EXPID}-c7b37639"
