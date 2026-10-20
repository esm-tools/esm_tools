"""VCS/provenance extension: read the frozen snapshot, or reconstruct live."""

from __future__ import annotations

import subprocess
from pathlib import Path

import yaml
from upath import UPath

from esm_catalog.models import ExperimentMetadata
from esm_catalog.vcs import (
    _ssh_host,
    _vcs_by_component_once,
    add_vcs_item_extension,
    read_vcs_info_initial,
    vcs_by_component,
)


def _experiment_metadata(exp_root: Path, **kwargs) -> ExperimentMetadata:
    return ExperimentMetadata(
        experiment_id="exp1", experiment_path=exp_root, **kwargs
    )


def _git_repo(path: Path, remote: str | None = None) -> None:
    path.mkdir(parents=True, exist_ok=True)
    subprocess.run(["git", "init", "-q"], cwd=path, check=True)
    subprocess.run(["git", "config", "user.email", "t@example.com"], cwd=path, check=True)
    subprocess.run(["git", "config", "user.name", "T"], cwd=path, check=True)
    (path / "f.txt").write_text("hello\n")
    subprocess.run(["git", "add", "f.txt"], cwd=path, check=True)
    subprocess.run(["git", "commit", "-q", "-m", "init"], cwd=path, check=True)
    if remote:
        subprocess.run(["git", "remote", "add", "origin", remote], cwd=path, check=True)


def test_reads_frozen_snapshot_when_present(tmp_path):
    log_dir = tmp_path / "log"
    log_dir.mkdir()
    frozen = {"echam": {"path": "/x", "hash": "abc123", "branch_name": "release"}}
    (log_dir / "exp1_vcs_info_initial.yaml").write_text(yaml.dump(frozen))

    em = _experiment_metadata(tmp_path)
    assert read_vcs_info_initial(em) == frozen
    assert vcs_by_component(em) == frozen


def test_no_snapshot_file_returns_none(tmp_path):
    em = _experiment_metadata(tmp_path)
    assert read_vcs_info_initial(em) is None


def test_reconstructs_live_from_model_dir_when_no_snapshot(tmp_path):
    model_dir = tmp_path / "models" / "echam"
    _git_repo(model_dir, remote="https://example.com/echam.git")

    em = _experiment_metadata(tmp_path, model_dirs={"echam": str(model_dir)})
    software = vcs_by_component(em)

    assert software["echam"]["reconstructed"] is True
    assert software["echam"]["remote"] == "https://example.com/echam.git"
    assert software["echam"]["branch_name"] in ("main", "master")
    assert software["echam"]["hash"]
    assert "note" in software["echam"]


def test_reconstruction_flags_non_git_directory(tmp_path):
    model_dir = tmp_path / "models" / "xios"
    model_dir.mkdir(parents=True)

    em = _experiment_metadata(tmp_path, model_dirs={"xios": str(model_dir)})
    software = vcs_by_component(em)

    assert software["xios"]["reconstructed"] is True
    assert software["xios"]["note"] == "not a git-controlled checkout"


def test_add_vcs_item_extension_sets_fields(tmp_path, item):
    model_dir = tmp_path / "models" / "echam"
    _git_repo(model_dir, remote="https://example.com/echam.git")
    em = _experiment_metadata(tmp_path, model_dirs={"echam": str(model_dir)})

    add_vcs_item_extension(item, "echam", em)

    assert item.properties["vcs:type"] == "git"
    assert item.properties["vcs:revision"]
    assert item.properties["vcs:branch"] in ("main", "master")
    assert item.properties["remote"] == "https://example.com/echam.git"
    assert item.properties["reconstructed"] is True
    assert "note" not in item.properties  # the dict key is "note"; the Item field is "reconstructed_note"
    assert "after the fact" in item.properties["reconstructed_note"]
    assert any("vcs" in ext for ext in item.stac_extensions)


def test_add_vcs_item_extension_noop_without_model_dirs(item, tmp_path):
    em = _experiment_metadata(tmp_path)
    add_vcs_item_extension(item, "echam", em)
    assert "vcs:type" not in item.properties


def test_add_vcs_item_extension_noop_for_unknown_component(tmp_path, item):
    model_dir = tmp_path / "models" / "echam"
    _git_repo(model_dir)
    em = _experiment_metadata(tmp_path, model_dirs={"echam": str(model_dir)})

    add_vcs_item_extension(item, "fesom", em)  # not in model_dirs

    assert "vcs:type" not in item.properties


# --- missing-hash/missing-component logging: confirmed live that every real --
# production collection had zero vcs:revision stamping with nothing in the
# logs saying why; these pin that the reason is now surfaced, not silent.


def test_warns_once_per_component_with_no_hash(tmp_path, monkeypatch):
    import esm_catalog.vcs as vcs_mod

    warnings = []
    monkeypatch.setattr(
        vcs_mod.logger, "warning", lambda *a, **k: warnings.append((a, k))
    )
    model_dir = tmp_path / "models" / "xios"
    model_dir.mkdir(parents=True)  # not a git checkout -> no hash
    em = _experiment_metadata(tmp_path, model_dirs={"xios": str(model_dir)})

    _vcs_by_component_once(em)
    _vcs_by_component_once(em)  # cached -- must not warn a second time

    assert len(warnings) == 1
    args, _kwargs = warnings[0]
    assert args[1] == "xios"
    assert "not a git-controlled checkout" in args[3]


def test_warns_once_per_missing_component(tmp_path, item, monkeypatch):
    import esm_catalog.vcs as vcs_mod

    warnings = []
    monkeypatch.setattr(
        vcs_mod.logger, "warning", lambda *a, **k: warnings.append((a, k))
    )
    model_dir = tmp_path / "models" / "echam"
    _git_repo(model_dir)
    em = _experiment_metadata(tmp_path, model_dirs={"echam": str(model_dir)})

    add_vcs_item_extension(item, "fesom", em)
    add_vcs_item_extension(item, "fesom", em)  # second item, same component

    assert len(warnings) == 1
    args, _kwargs = warnings[0]
    assert args[1] == "fesom"
    assert "not present" in args[0]


# --- remote (sftp) scans: model_dir reconstruction must run on the remote host ---


def test_ssh_host_none_for_local_scan(tmp_path):
    em = _experiment_metadata(tmp_path)
    assert _ssh_host(em) is None


def test_ssh_host_from_sftp_experiment_path():
    em = ExperimentMetadata(
        experiment_id="exp1",
        experiment_path=UPath("sftp://albedo0.dmawi.de/work/exp1"),
    )
    assert _ssh_host(em) == "albedo0.dmawi.de"


def test_reconstruction_runs_git_over_ssh_for_remote_scan(monkeypatch):
    calls = []

    def fake_run(argv, **kwargs):
        calls.append(argv)
        out = "true" if argv[:2] == ["ssh", "albedo0.dmawi.de"] else ""
        return subprocess.CompletedProcess(argv, 0, stdout=out, stderr="")

    monkeypatch.setattr(subprocess, "run", fake_run)

    em = ExperimentMetadata(
        experiment_id="exp1",
        experiment_path=UPath("sftp://albedo0.dmawi.de/work/exp1"),
        model_dirs={"echam": "/work/models/echam"},
    )
    software = vcs_by_component(em)

    assert software["echam"]["reconstructed"] is True
    assert all(argv[0] == "ssh" and argv[1] == "albedo0.dmawi.de" for argv in calls)
    # the git invocation itself is one already-quoted string, not split argv
    assert all(len(argv) == 3 for argv in calls)
    assert "git -C /work/models/echam" in calls[0][2]


def test_reconstruction_remote_unreachable_note_distinct_from_non_git(monkeypatch):
    def fake_run(argv, **kwargs):
        return subprocess.CompletedProcess(argv, 255, stdout="", stderr="ssh: connect failed")

    monkeypatch.setattr(subprocess, "run", fake_run)

    em = ExperimentMetadata(
        experiment_id="exp1",
        experiment_path=UPath("sftp://albedo0.dmawi.de/work/exp1"),
        model_dirs={"echam": "/work/models/echam"},
    )
    software = vcs_by_component(em)

    assert software["echam"]["note"] == "unreachable over ssh"
