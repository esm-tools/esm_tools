import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml

from esm_runscripts import prepare


def _init_git_repo(path: Path) -> None:
    subprocess.run(["git", "init", "-q", str(path)], check=True)
    subprocess.run(
        ["git", "-C", str(path), "config", "user.email", "test@example.com"],
        check=True,
    )
    subprocess.run(["git", "-C", str(path), "config", "user.name", "Test"], check=True)
    subprocess.run(
        ["git", "-C", str(path), "config", "commit.gpgsign", "false"], check=True
    )
    (path / "source.f90").write_text("program test\nend program test\n")
    subprocess.run(["git", "-C", str(path), "add", "source.f90"], check=True)
    subprocess.run(
        ["git", "-C", str(path), "commit", "-q", "-m", "initial commit"], check=True
    )


class VcsInfoTestBase(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.tmpdir_path = Path(self.tmpdir.name)

        self.snapshot_dir = self.tmpdir_path / "exp" / "src" / "echam-6.3"
        self.snapshot_dir.mkdir(parents=True)
        _init_git_repo(self.snapshot_dir)

        self.log_dir = self.tmpdir_path / "exp" / "log"
        self.run1_log_dir = self.log_dir / "run_1"
        self.run2_log_dir = self.log_dir / "run_2"
        self.run1_log_dir.mkdir(parents=True)
        self.run2_log_dir.mkdir(parents=True)

        self.esm_configs_dir = self.tmpdir_path / "esm_tools_repo" / "configs"
        self.esm_configs_dir.mkdir(parents=True)
        _init_git_repo(self.tmpdir_path / "esm_tools_repo")

        self.base_config = {
            "general": {
                "expid": "test",
                "models": ["echam"],
                "thisrun_log_dir": str(self.run1_log_dir),
                "experiment_log_dir": str(self.log_dir),
                "esm_configs_dir": str(self.esm_configs_dir),
                "run_number": 1,
            },
            "echam": {"model_dir": str(self.snapshot_dir)},
        }


class TestAddVcsInfo(VcsInfoTestBase):
    def test_clean_snapshot_is_recorded(self):
        config = prepare.add_vcs_info(dict(self.base_config))

        exp_vcs_info_file = self.run1_log_dir / "test_vcs_info.yaml"
        initial_vcs_info_file = self.log_dir / "test_vcs_info_initial.yaml"

        self.assertTrue(exp_vcs_info_file.is_file())
        self.assertTrue(initial_vcs_info_file.is_file())

        info = yaml.safe_load(exp_vcs_info_file.read_text())
        self.assertIn("echam", info)
        self.assertEqual(info["echam"]["diffs"], "")

        self.assertIn("vcs_info", config["general"])
        self.assertEqual(config["general"]["vcs_info"], info)

    def test_dirty_snapshot_is_recorded_not_failed(self):
        # Legitimate local modification at compile time: should be recorded,
        # not treated as an error.
        with (self.snapshot_dir / "source.f90").open("a") as f:
            f.write("! local tweak\n")

        config = prepare.add_vcs_info(dict(self.base_config))
        info = config["general"]["vcs_info"]
        self.assertNotEqual(info["echam"]["diffs"], "")

    def test_initial_file_not_overwritten_on_later_runs(self):
        config1 = dict(self.base_config)
        prepare.add_vcs_info(config1)
        initial_vcs_info_file = self.log_dir / "test_vcs_info_initial.yaml"
        initial_info_first_write = initial_vcs_info_file.read_text()

        # New segment, new commit lands in the snapshot (should not happen in
        # practice since the snapshot is frozen, but verifies we don't clobber
        # the initial reference file).
        with (self.snapshot_dir / "source.f90").open("a") as f:
            f.write("! drift\n")

        config2 = dict(self.base_config)
        config2["general"] = dict(config2["general"])
        config2["general"]["thisrun_log_dir"] = str(self.run2_log_dir)
        config2["general"]["run_number"] = 2
        prepare.add_vcs_info(config2)

        initial_info_second_write = initial_vcs_info_file.read_text()

        self.assertEqual(initial_info_first_write, initial_info_second_write)


class TestCheckVcsInfoAgainstLastRun(VcsInfoTestBase):
    def _run_segment(self, run_number, log_dir):
        config = dict(self.base_config)
        config["general"] = dict(config["general"])
        config["general"]["run_number"] = run_number
        config["general"]["thisrun_log_dir"] = str(log_dir)
        config = prepare.add_vcs_info(config)
        return prepare.check_vcs_info_against_last_run(config)

    def test_first_run_skips_check(self):
        # Should not raise even though no initial file existed before this call.
        config = self._run_segment(1, self.run1_log_dir)
        self.assertIn("vcs_info", config["general"])

    def test_no_false_positive_when_shared_dir_drifts(self):
        self._run_segment(1, self.run1_log_dir)

        # Unrelated activity on what *used to* be the shared model_dir. The
        # snapshot used by segment 2 is untouched, so this must not matter.
        unrelated_dir = self.tmpdir_path / "unrelated_shared_checkout"
        unrelated_dir.mkdir()
        _init_git_repo(unrelated_dir)
        (unrelated_dir / "extra.f90").write_text("! someone else's commit\n")
        subprocess.run(
            ["git", "-C", str(unrelated_dir), "add", "extra.f90"], check=True
        )
        subprocess.run(
            ["git", "-C", str(unrelated_dir), "commit", "-q", "-m", "unrelated"],
            check=True,
        )

        # Should not raise SystemExit.
        self._run_segment(2, self.run2_log_dir)

    def test_fails_loudly_when_snapshot_itself_changed(self):
        self._run_segment(1, self.run1_log_dir)

        # Something genuinely wrong: the frozen snapshot itself was touched
        # between segments.
        with (self.snapshot_dir / "source.f90").open("a") as f:
            f.write("! should not happen\n")
        subprocess.run(
            ["git", "-C", str(self.snapshot_dir), "add", "source.f90"], check=True
        )
        subprocess.run(
            ["git", "-C", str(self.snapshot_dir), "commit", "-q", "-m", "tampered"],
            check=True,
        )

        with self.assertRaises(SystemExit):
            self._run_segment(2, self.run2_log_dir)

    def test_allow_vcs_differences_bypasses_check(self):
        self._run_segment(1, self.run1_log_dir)

        with (self.snapshot_dir / "source.f90").open("a") as f:
            f.write("! should not happen\n")
        subprocess.run(
            ["git", "-C", str(self.snapshot_dir), "add", "source.f90"], check=True
        )
        subprocess.run(
            ["git", "-C", str(self.snapshot_dir), "commit", "-q", "-m", "tampered"],
            check=True,
        )

        config = dict(self.base_config)
        config["general"] = dict(config["general"])
        config["general"]["run_number"] = 2
        config["general"]["thisrun_log_dir"] = str(self.run2_log_dir)
        config["general"]["allow_vcs_differences"] = True
        config = prepare.add_vcs_info(config)
        # Should not raise.
        prepare.check_vcs_info_against_last_run(config)


if __name__ == "__main__":
    unittest.main()
