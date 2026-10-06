import subprocess
import tempfile
import unittest
from pathlib import Path

from esm_runscripts import prepcompute


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


class TestSnapshotModelSource(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.tmpdir_path = Path(self.tmpdir.name)
        self.shared_dir = self.tmpdir_path / "shared_model_checkout"
        self.shared_dir.mkdir()
        _init_git_repo(self.shared_dir)
        self.experiment_src_dir = self.tmpdir_path / "exp" / "src"
        self.config = {
            "general": {"experiment_src_dir": str(self.experiment_src_dir)},
            "echam": {"model_dir": str(self.shared_dir)},
        }

    def test_first_run_creates_snapshot(self):
        result = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        assert result is not None
        snapshot_dir = Path(result)
        self.assertTrue(snapshot_dir.is_dir())
        self.assertTrue((snapshot_dir / "source.f90").is_file())
        self.assertTrue((snapshot_dir / ".git").is_dir())

    def test_no_shared_source_returns_none(self):
        config = {
            "general": {"experiment_src_dir": str(self.experiment_src_dir)},
            "xios": {},
        }
        self.assertIsNone(prepcompute.snapshot_model_source(config, "xios", "1.0"))

    def test_reuse_on_subsequent_segments_no_recopy(self):
        result = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        assert result is not None
        snapshot_dir = Path(result)

        # Simulate "version drift" in the shared/central checkout between
        # segments: a new commit lands there after the snapshot was made.
        (self.shared_dir / "new_file.f90").write_text("! added after snapshot\n")
        subprocess.run(
            ["git", "-C", str(self.shared_dir), "add", "new_file.f90"], check=True
        )
        subprocess.run(
            ["git", "-C", str(self.shared_dir), "commit", "-q", "-m", "drift"],
            check=True,
        )

        # Second segment: snapshot already exists, must not be touched.
        result_2 = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        assert result_2 is not None
        snapshot_dir_2 = Path(result_2)
        self.assertEqual(snapshot_dir, snapshot_dir_2)
        self.assertFalse(
            (snapshot_dir / "new_file.f90").is_file(),
            "Snapshot was re-copied even though it already existed",
        )

    def test_race_pre_existing_snapshot_is_not_overwritten(self):
        # Simulate another process having already won the race.
        snapshot_dir = self.experiment_src_dir / "echam-6.3"
        snapshot_dir.mkdir(parents=True)
        (snapshot_dir / "marker.txt").write_text("already here")

        raw_result = prepcompute.snapshot_model_source(self.config, "echam", "6.3")
        assert raw_result is not None
        result = Path(raw_result)
        self.assertEqual(result, snapshot_dir)
        self.assertTrue((snapshot_dir / "marker.txt").is_file())
        self.assertFalse((snapshot_dir / "source.f90").is_file())


class TestSnapshotModelSources(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.tmpdir_path = Path(self.tmpdir.name)
        self.shared_dir = self.tmpdir_path / "shared_model_checkout"
        self.shared_dir.mkdir()
        _init_git_repo(self.shared_dir)
        self.experiment_src_dir = self.tmpdir_path / "exp" / "src"
        self.config = {
            "general": {
                "experiment_src_dir": str(self.experiment_src_dir),
                "models": ["echam"],
            },
            "echam": {"model_dir": str(self.shared_dir), "version": "6.3"},
        }

    def test_repoints_model_dir_at_snapshot(self):
        config = prepcompute.snapshot_model_sources(self.config)
        expected_snapshot = self.experiment_src_dir / "echam-6.3"
        self.assertEqual(Path(config["echam"]["model_dir"]), expected_snapshot)
        self.assertTrue(expected_snapshot.is_dir())

    def test_models_without_version_are_skipped(self):
        config = {
            "general": {
                "experiment_src_dir": str(self.experiment_src_dir),
                "models": ["xios"],
            },
            "xios": {},
        }
        result = prepcompute.snapshot_model_sources(config)
        self.assertNotIn("model_dir", result["xios"])


if __name__ == "__main__":
    unittest.main()
