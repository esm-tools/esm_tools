import subprocess
from typing import NamedTuple

import pytest
import yaml

from esm_runscripts import prepare


class VcsFixture(NamedTuple):
    config: dict
    snapshot_dir: object
    log_dir: object
    run1_log_dir: object
    run2_log_dir: object


@pytest.fixture
def vcs_fixture(tmp_path, git_repo):
    snapshot_dir = git_repo("exp/src/echam-6.3")
    git_repo("esm_tools_repo")

    log_dir = tmp_path / "exp" / "log"
    run1_log_dir = log_dir / "run_1"
    run2_log_dir = log_dir / "run_2"
    run1_log_dir.mkdir(parents=True)
    run2_log_dir.mkdir(parents=True)

    esm_configs_dir = tmp_path / "esm_tools_repo" / "configs"
    esm_configs_dir.mkdir(parents=True)

    config = {
        "general": {
            "expid": "test",
            "models": ["echam"],
            "thisrun_log_dir": str(run1_log_dir),
            "experiment_log_dir": str(log_dir),
            "esm_configs_dir": str(esm_configs_dir),
            "run_number": 1,
        },
        "echam": {"model_dir": str(snapshot_dir)},
    }
    return VcsFixture(config, snapshot_dir, log_dir, run1_log_dir, run2_log_dir)


def _next_segment(config, run_number, log_dir):
    config = dict(config)
    config["general"] = dict(config["general"])
    config["general"]["run_number"] = run_number
    config["general"]["thisrun_log_dir"] = str(log_dir)
    return config


def _run_segment(config, run_number, log_dir):
    config = prepare.add_vcs_info(_next_segment(config, run_number, log_dir))
    return prepare.check_vcs_info_against_last_run(config)


def test_clean_snapshot_is_recorded(vcs_fixture):
    config = prepare.add_vcs_info(dict(vcs_fixture.config))

    exp_vcs_info_file = vcs_fixture.run1_log_dir / "test_vcs_info.yaml"
    initial_vcs_info_file = vcs_fixture.log_dir / "test_vcs_info_initial.yaml"

    assert exp_vcs_info_file.is_file()
    assert initial_vcs_info_file.is_file()

    info = yaml.safe_load(exp_vcs_info_file.read_text())
    assert "echam" in info
    assert info["echam"]["diffs"] == ""

    assert config["general"]["vcs_info"] == info


def test_dirty_snapshot_is_recorded_not_failed(vcs_fixture):
    # Legitimate local modification at compile time: should be recorded,
    # not treated as an error.
    with (vcs_fixture.snapshot_dir / "source.f90").open("a") as f:
        f.write("! local tweak\n")

    config = prepare.add_vcs_info(dict(vcs_fixture.config))
    assert config["general"]["vcs_info"]["echam"]["diffs"] != ""


def test_initial_file_not_overwritten_on_later_runs(vcs_fixture):
    prepare.add_vcs_info(dict(vcs_fixture.config))
    initial_vcs_info_file = vcs_fixture.log_dir / "test_vcs_info_initial.yaml"
    initial_info_first_write = initial_vcs_info_file.read_text()

    # New segment, new commit lands in the snapshot (should not happen in
    # practice since the snapshot is frozen, but verifies we don't clobber
    # the initial reference file).
    with (vcs_fixture.snapshot_dir / "source.f90").open("a") as f:
        f.write("! drift\n")

    prepare.add_vcs_info(_next_segment(vcs_fixture.config, 2, vcs_fixture.run2_log_dir))

    assert initial_vcs_info_file.read_text() == initial_info_first_write


def test_first_run_skips_check(vcs_fixture):
    # Should not raise even though no initial file existed before this call.
    result = _run_segment(vcs_fixture.config, 1, vcs_fixture.run1_log_dir)
    assert "vcs_info" in result["general"]


def test_no_false_positive_when_shared_dir_drifts(vcs_fixture, git_repo):
    _run_segment(vcs_fixture.config, 1, vcs_fixture.run1_log_dir)

    # Unrelated activity on what *used to* be the shared model_dir. The
    # snapshot used by segment 2 is untouched, so this must not matter.
    unrelated_dir = git_repo("unrelated_shared_checkout")
    (unrelated_dir / "extra.f90").write_text("! someone else's commit\n")
    subprocess.run(["git", "-C", str(unrelated_dir), "add", "extra.f90"], check=True)
    subprocess.run(
        ["git", "-C", str(unrelated_dir), "commit", "-q", "-m", "unrelated"],
        check=True,
    )

    # Should not raise SystemExit.
    _run_segment(vcs_fixture.config, 2, vcs_fixture.run2_log_dir)


def test_fails_loudly_when_snapshot_itself_changed(vcs_fixture):
    _run_segment(vcs_fixture.config, 1, vcs_fixture.run1_log_dir)

    # Something genuinely wrong: the frozen snapshot itself was touched
    # between segments.
    snapshot_dir = vcs_fixture.snapshot_dir
    with (snapshot_dir / "source.f90").open("a") as f:
        f.write("! should not happen\n")
    subprocess.run(["git", "-C", str(snapshot_dir), "add", "source.f90"], check=True)
    subprocess.run(
        ["git", "-C", str(snapshot_dir), "commit", "-q", "-m", "tampered"],
        check=True,
    )

    with pytest.raises(SystemExit):
        _run_segment(vcs_fixture.config, 2, vcs_fixture.run2_log_dir)


def test_allow_vcs_differences_bypasses_check(vcs_fixture):
    _run_segment(vcs_fixture.config, 1, vcs_fixture.run1_log_dir)

    snapshot_dir = vcs_fixture.snapshot_dir
    with (snapshot_dir / "source.f90").open("a") as f:
        f.write("! should not happen\n")
    subprocess.run(["git", "-C", str(snapshot_dir), "add", "source.f90"], check=True)
    subprocess.run(
        ["git", "-C", str(snapshot_dir), "commit", "-q", "-m", "tampered"],
        check=True,
    )

    next_config = _next_segment(vcs_fixture.config, 2, vcs_fixture.run2_log_dir)
    next_config["general"]["allow_vcs_differences"] = True
    next_config = prepare.add_vcs_info(next_config)

    # Should not raise.
    prepare.check_vcs_info_against_last_run(next_config)
