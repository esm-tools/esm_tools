"""VCS/provenance extension: read the frozen snapshot, or reconstruct live."""

from __future__ import annotations

import subprocess
from pathlib import Path

import yaml

from esm_catalog.models import ExperimentMetadata
from esm_catalog.vcs import (
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
