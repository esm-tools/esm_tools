import os
import subprocess
import tempfile
import unittest

from esm_runscripts import prepcompute


def _init_git_repo(path):
    subprocess.run(["git", "init", "-q", path], check=True)
    subprocess.run(
        ["git", "-C", path, "config", "user.email", "test@example.com"], check=True
    )
    subprocess.run(["git", "-C", path, "config", "user.name", "Test"], check=True)
    subprocess.run(["git", "-C", path, "config", "commit.gpgsign", "false"], check=True)
    with open(os.path.join(path, "source.f90"), "w") as f:
        f.write("program test\nend program test\n")
    subprocess.run(["git", "-C", path, "add", "source.f90"], check=True)
    subprocess.run(
        ["git", "-C", path, "commit", "-q", "-m", "initial commit"], check=True
    )


class TestSnapshotModelSource(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.shared_dir = os.path.join(self.tmpdir.name, "shared_model_checkout")
        os.makedirs(self.shared_dir)
        _init_git_repo(self.shared_dir)
        self.experiment_src_dir = os.path.join(self.tmpdir.name, "exp", "src")
        self.config = {
            "general": {"experiment_src_dir": self.experiment_src_dir},
            "echam": {"model_dir": self.shared_dir},
        }

    def test_first_run_creates_snapshot(self):
        snapshot_dir = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        self.assertTrue(os.path.isdir(snapshot_dir))
        self.assertTrue(os.path.isfile(os.path.join(snapshot_dir, "source.f90")))
        self.assertTrue(os.path.isdir(os.path.join(snapshot_dir, ".git")))

    def test_no_shared_source_returns_none(self):
        config = {
            "general": {"experiment_src_dir": self.experiment_src_dir},
            "xios": {},
        }
        self.assertIsNone(prepcompute.snapshot_model_source(config, "xios", "1.0"))

    def test_reuse_on_subsequent_segments_no_recopy(self):
        snapshot_dir = prepcompute.snapshot_model_source(self.config, "echam", "6.3")

        # Simulate "version drift" in the shared/central checkout between
        # segments: a new commit lands there after the snapshot was made.
        with open(os.path.join(self.shared_dir, "new_file.f90"), "w") as f:
            f.write("! added after snapshot\n")
        subprocess.run(["git", "-C", self.shared_dir, "add", "new_file.f90"], check=True)
        subprocess.run(
            ["git", "-C", self.shared_dir, "commit", "-q", "-m", "drift"], check=True
        )

        # Second segment: snapshot already exists, must not be touched.
        snapshot_dir_2 = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        self.assertEqual(snapshot_dir, snapshot_dir_2)
        self.assertFalse(
            os.path.isfile(os.path.join(snapshot_dir, "new_file.f90")),
            "Snapshot was re-copied even though it already existed",
        )

    def test_race_pre_existing_snapshot_is_not_overwritten(self):
        # Simulate another process having already won the race.
        snapshot_dir = os.path.join(self.experiment_src_dir, "echam-6.3")
        os.makedirs(snapshot_dir)
        with open(os.path.join(snapshot_dir, "marker.txt"), "w") as f:
            f.write("already here")

        result = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        self.assertEqual(result, snapshot_dir)
        self.assertTrue(os.path.isfile(os.path.join(snapshot_dir, "marker.txt")))
        self.assertFalse(os.path.isfile(os.path.join(snapshot_dir, "source.f90")))


class TestSnapshotModelSources(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.shared_dir = os.path.join(self.tmpdir.name, "shared_model_checkout")
        os.makedirs(self.shared_dir)
        _init_git_repo(self.shared_dir)
        self.experiment_src_dir = os.path.join(self.tmpdir.name, "exp", "src")
        self.config = {
            "general": {
                "experiment_src_dir": self.experiment_src_dir,
                "models": ["echam"],
            },
            "echam": {"model_dir": self.shared_dir, "version": "6.3"},
        }

    def test_repoints_model_dir_at_snapshot(self):
        config = prepcompute.snapshot_model_sources(self.config)
        expected_snapshot = os.path.join(self.experiment_src_dir, "echam-6.3")
        self.assertEqual(config["echam"]["model_dir"], expected_snapshot)
        self.assertTrue(os.path.isdir(expected_snapshot))

    def test_models_without_version_are_skipped(self):
        config = {
            "general": {
                "experiment_src_dir": self.experiment_src_dir,
                "models": ["xios"],
            },
            "xios": {},
        }
        result = prepcompute.snapshot_model_sources(config)
        self.assertNotIn("model_dir", result["xios"])


if __name__ == "__main__":
    unittest.main()
