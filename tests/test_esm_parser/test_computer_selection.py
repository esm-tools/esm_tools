"""
Tests for selecting the machine configuration explicitly (``--computer`` flag of
``esm_master``, ``esm_runscripts`` and ``esm_tests``) instead of detecting it from
the hostname.
"""

import os

import pytest

from esm_parser.esm_parser import determine_computer_yaml_from_hostname


def test_explicit_computer_skips_hostname_detection(monkeypatch):
    """An explicit computer name returns that machine's yaml, whatever the hostname."""
    monkeypatch.setattr("socket.gethostname", lambda: "albedo0")
    monkeypatch.setattr("socket.getfqdn", lambda: "albedo0")

    computer_file = determine_computer_yaml_from_hostname("levante")

    assert computer_file.endswith("/machines/levante.yaml")
    assert os.path.isfile(computer_file)


def test_no_computer_falls_back_to_hostname(monkeypatch):
    """Without a computer name the hostname detection is used, as before."""
    monkeypatch.setattr("socket.gethostname", lambda: "levante0")
    monkeypatch.setattr("socket.getfqdn", lambda: "levante0")

    assert determine_computer_yaml_from_hostname().endswith("/machines/levante.yaml")
    assert determine_computer_yaml_from_hostname(None).endswith(
        "/machines/levante.yaml"
    )


def test_unknown_computer_errors():
    """A computer without a ``configs/machines/<name>.yaml`` stops with an error."""
    with pytest.raises(SystemExit):
        determine_computer_yaml_from_hostname("not_a_machine")
