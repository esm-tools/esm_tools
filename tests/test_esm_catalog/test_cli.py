"""CLI scaffold tests: every command is registered and shows help, and the
not-yet-implemented stubs return an error. Per-command behavior is tested as
each command is built.
"""

from __future__ import annotations

import pytest
from click.testing import CliRunner

from esm_catalog.cli import main


@pytest.fixture
def runner():
    return CliRunner()


def test_help_lists_all_commands(runner):
    result = runner.invoke(main, ["--help"])
    assert result.exit_code == 0
    for command in ("auth", "init", "scan", "push", "add", "rm", "edit"):
        assert command in result.output


def test_auth_subcommands_registered(runner):
    result = runner.invoke(main, ["auth", "--help"])
    assert result.exit_code == 0
    assert "login" in result.output
    assert "logout" in result.output


@pytest.mark.parametrize(
    "argv",
    [
        ["auth", "login", "https://stac.awi.de"],
        ["auth", "logout"],
        ["init", "/tmp/exp"],
        ["push"],
        ["add", "/tmp/f.nc"],
        ["rm", "/tmp/f.nc"],
        ["edit", "collection"],
    ],
)
def test_stub_commands_report_not_implemented(runner, argv):
    result = runner.invoke(main, argv)
    assert result.exit_code != 0
    assert "not implemented" in result.output


def test_scan_missing_run_reports_clean_error(runner):
    """A directory with no finished_config yields a one-line error, no traceback."""
    result = runner.invoke(main, ["scan", "--exp-root", "/nonexistent/xyz"])
    assert result.exit_code != 0
    assert "scan requires a completed ESM-Tools run" in result.output
    assert "Traceback" not in result.output
    # The SourcingError was translated to a clean ClickException, not propagated.
    assert not isinstance(result.exception, Exception) or isinstance(
        result.exception, SystemExit
    )
