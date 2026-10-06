import subprocess
from pathlib import Path

import pytest


def _init_git_repo(path: Path) -> None:
    """Init a git repo at path with one committed source.f90, for VCS-capture tests."""
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


@pytest.fixture
def git_repo(tmp_path):
    """Factory fixture: ``git_repo("name")`` -> an initialized git repo under tmp_path."""

    def make(name="repo"):
        path = tmp_path / name
        path.mkdir(parents=True, exist_ok=True)
        _init_git_repo(path)
        return path

    return make
