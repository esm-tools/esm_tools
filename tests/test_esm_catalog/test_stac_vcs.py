"""VCS/provenance extension: read the frozen snapshot, or reconstruct live."""

from __future__ import annotations

import subprocess
from pathlib import Path

import yaml

from esm_catalog.models import ExperimentMetadata
from esm_catalog.vcs import (
    add_vcs_collection_extension,
    read_vcs_info_initial,
    vcs_processing_software,
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
    assert vcs_processing_software(em) == frozen


def test_no_snapshot_file_returns_none(tmp_path):
    em = _experiment_metadata(tmp_path)
    assert read_vcs_info_initial(em) is None


def test_reconstructs_live_from_model_dir_when_no_snapshot(tmp_path):
    model_dir = tmp_path / "models" / "echam"
    _git_repo(model_dir, remote="https://example.com/echam.git")

    em = _experiment_metadata(tmp_path, model_dirs={"echam": str(model_dir)})
    software = vcs_processing_software(em)

    assert software["echam"]["reconstructed"] is True
    assert software["echam"]["remote"] == "https://example.com/echam.git"
    assert software["echam"]["branch_name"] in ("main", "master")
    assert software["echam"]["hash"]
    assert "note" in software["echam"]


def test_reconstruction_flags_non_git_directory(tmp_path):
    model_dir = tmp_path / "models" / "xios"
    model_dir.mkdir(parents=True)

    em = _experiment_metadata(tmp_path, model_dirs={"xios": str(model_dir)})
    software = vcs_processing_software(em)

    assert software["xios"]["reconstructed"] is True
    assert software["xios"]["note"] == "not a git-controlled checkout"


def test_add_vcs_collection_extension_sets_fields(tmp_path, collection):
    model_dir = tmp_path / "models" / "echam"
    _git_repo(model_dir)
    em = _experiment_metadata(tmp_path, model_dirs={"echam": str(model_dir)})

    add_vcs_collection_extension(collection, em)

    assert "echam" in collection.extra_fields["processing:software"]
    assert "reconstructed" in collection.extra_fields["processing:lineage"].lower() or (
        "after the fact" in collection.extra_fields["processing:lineage"]
    )


def test_add_vcs_collection_extension_noop_without_model_dirs(collection, tmp_path):
    em = _experiment_metadata(tmp_path)
    add_vcs_collection_extension(collection, em)
    assert "processing:software" not in collection.extra_fields
