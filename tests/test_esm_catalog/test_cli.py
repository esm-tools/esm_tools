"""CLI scaffold tests: every command is registered and shows help, and the
not-yet-implemented stubs return an error. Per-command behavior is tested as
each command is built.
"""

from __future__ import annotations

import pytest
from click.testing import CliRunner

from esm_catalog.cli import main


def test_help_lists_all_commands():
    result = CliRunner().invoke(main, ["--help"])
    assert result.exit_code == 0
    for command in ("auth", "init", "scan", "push", "add", "rm", "edit"):
        assert command in result.output


def test_auth_subcommands_registered():
    result = CliRunner().invoke(main, ["auth", "--help"])
    assert result.exit_code == 0
    assert "login" in result.output
    assert "logout" in result.output


@pytest.mark.parametrize(
    "argv",
    [
        ["auth", "login", "https://stac.awi.de"],
        ["auth", "logout"],
        ["init", "/tmp/exp"],
        ["scan"],
        ["push"],
        ["add", "/tmp/f.nc"],
        ["rm", "/tmp/f.nc"],
        ["edit", "collection"],
    ],
)
def test_stub_commands_report_not_implemented(argv):
    result = CliRunner().invoke(main, argv)
    assert result.exit_code != 0
    assert "not implemented" in result.output
