import subprocess
from pathlib import Path

from esm_runscripts import prepcompute


def _config(experiment_src_dir, model="echam", shared_dir=None, **general_extra):
    config = {
        "general": {"experiment_src_dir": str(experiment_src_dir), **general_extra}
    }
    config[model] = {"model_dir": str(shared_dir)} if shared_dir is not None else {}
    return config


def test_first_run_creates_snapshot(tmp_path, git_repo):
    shared_dir = git_repo("shared_model_checkout")
    config = _config(tmp_path / "exp" / "src", shared_dir=shared_dir)

    result = prepcompute.snapshot_model_source(config, "echam", "6.3")
    assert result is not None
    snapshot_dir = Path(result)

    assert snapshot_dir.is_dir()
    assert (snapshot_dir / "source.f90").is_file()
    assert (snapshot_dir / ".git").is_dir()


def test_no_shared_source_returns_none(tmp_path):
    config = _config(tmp_path / "exp" / "src", model="xios")
    assert prepcompute.snapshot_model_source(config, "xios", "1.0") is None


def test_reuse_on_subsequent_segments_no_recopy(tmp_path, git_repo):
    shared_dir = git_repo("shared_model_checkout")
    config = _config(tmp_path / "exp" / "src", shared_dir=shared_dir)

    result = prepcompute.snapshot_model_source(config, "echam", "6.3")
    assert result is not None
    snapshot_dir = Path(result)

    # Simulate "version drift" in the shared/central checkout between
    # segments: a new commit lands there after the snapshot was made.
    (shared_dir / "new_file.f90").write_text("! added after snapshot\n")
    subprocess.run(["git", "-C", str(shared_dir), "add", "new_file.f90"], check=True)
    subprocess.run(
        ["git", "-C", str(shared_dir), "commit", "-q", "-m", "drift"], check=True
    )

    # Second segment: snapshot already exists, must not be touched.
    result_2 = prepcompute.snapshot_model_source(config, "echam", "6.3")
    assert result_2 is not None
    assert Path(result_2) == snapshot_dir
    assert not (
        snapshot_dir / "new_file.f90"
    ).is_file(), "Snapshot was re-copied even though it already existed"


def test_race_pre_existing_snapshot_is_not_overwritten(tmp_path, git_repo):
    shared_dir = git_repo("shared_model_checkout")
    config = _config(tmp_path / "exp" / "src", shared_dir=shared_dir)

    # Simulate another process having already won the race.
    snapshot_dir = tmp_path / "exp" / "src" / "echam-6.3"
    snapshot_dir.mkdir(parents=True)
    (snapshot_dir / "marker.txt").write_text("already here")

    raw_result = prepcompute.snapshot_model_source(config, "echam", "6.3")
    assert raw_result is not None
    result = Path(raw_result)

    assert result == snapshot_dir
    assert (snapshot_dir / "marker.txt").is_file()
    assert not (snapshot_dir / "source.f90").is_file()


def test_repoints_model_dir_at_snapshot(tmp_path, git_repo):
    shared_dir = git_repo("shared_model_checkout")
    experiment_src_dir = tmp_path / "exp" / "src"
    config = _config(
        experiment_src_dir, shared_dir=shared_dir, models=["echam"]
    )
    config["echam"]["version"] = "6.3"

    config = prepcompute.snapshot_model_sources(config)
    expected_snapshot = experiment_src_dir / "echam-6.3"

    assert Path(config["echam"]["model_dir"]) == expected_snapshot
    assert expected_snapshot.is_dir()


def test_models_without_version_are_skipped(tmp_path):
    config = _config(tmp_path / "exp" / "src", model="xios", models=["xios"])
    result = prepcompute.snapshot_model_sources(config)
    assert "model_dir" not in result["xios"]
