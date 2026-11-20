"""Reviewing contacts at 'esm-catalog server put', before anything is sent."""

from __future__ import annotations

import json

import pytest
from click.testing import CliRunner
from ruamel.yaml import YAML

from esm_catalog import auth, contact_prompts
from esm_catalog.cli import main

FALLBACK = {"name": "Ada Lovelace (a123456)", "roles": ["author"]}


@pytest.fixture
def catalog(tmp_path, monkeypatch):
    """A catalog dir with one collection, a server set, and a person 'at the terminal'."""
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "config"))
    monkeypatch.setenv("ESM_CATALOG_SERVER_URL", "https://stac.example.org")
    monkeypatch.setattr(contact_prompts, "can_ask", lambda: True)

    def no_network(settings):
        raise auth.AuthError("test: the server must not be contacted")

    monkeypatch.setattr(auth, "get_bearer_token", no_network)

    directory = tmp_path / "catalog"
    directory.mkdir()
    collection = {
        "type": "Collection",
        "id": "run01-abc",
        "stac_version": "1.0.0",
        "description": "run01",
        "license": "proprietary",
        "links": [],
        "extent": {},
        "contacts": [FALLBACK],
    }
    (directory / "collection.json").write_text(json.dumps(collection))
    return directory


def _put(catalog, answers, *extra):
    """Run 'server put' on *catalog*, typing *answers* (one per question)."""
    return CliRunner().invoke(
        main, ["server", "put", str(catalog), *extra], input="\n".join(answers) + "\n"
    )


def _collection_contacts(catalog):
    return json.loads((catalog / "collection.json").read_text()).get("contacts")


def test_enter_keeps_contacts_and_no_says_nothing_is_sent(catalog):
    result = _put(catalog, ["", "n"])

    assert result.exit_code == 0, result.output
    assert "About to publish run01-abc to https://stac.example.org" in result.output
    assert "1. Ada Lovelace (a123456) - author" in result.output
    assert "Not published: nothing was sent to the server." in result.output
    assert not (catalog / "catalog_contacts.yaml").exists()
    assert _collection_contacts(catalog) == [FALLBACK]


def test_add_details_and_a_second_contact(catalog):
    answers = [
        "y",  # add or change contacts
        "Ada Lovelace",  # name (replaces the prefilled one)
        "",  # role: keep author
        "Example Institute",  # institution
        "ada@example.org, ada@home.example",  # two emails
        "",  # ORCID: skip
        "y",  # add another contact
        "Grace Hopper",
        "principalInvestigator",
        "",  # institution: skip
        "",  # email: skip
        "",  # ORCID: skip
        "",  # add another: no
        "",  # change anything: no
        "n",  # publish now: no
    ]
    result = _put(catalog, answers)

    assert result.exit_code == 0, result.output
    assert "Not published" in result.output
    saved = YAML(typ="safe").load((catalog / "catalog_contacts.yaml").read_text())
    assert saved == {
        "contacts": [
            {
                "name": "Ada Lovelace",
                "roles": ["author"],
                "institution": "Example Institute",
                "emails": ["ada@example.org", "ada@home.example"],
            },
            {"name": "Grace Hopper", "roles": ["principalInvestigator"]},
        ]
    }
    # The local collection carries them too, so this upload would include them.
    assert _collection_contacts(catalog) == [
        {
            "name": "Ada Lovelace",
            "organization": "Example Institute",
            "roles": ["author"],
            "emails": [{"value": "ada@example.org"}, {"value": "ada@home.example"}],
        },
        {"name": "Grace Hopper", "roles": ["principalInvestigator"]},
    ]


def test_existing_values_are_kept_by_enter_and_removed_by_dash(catalog):
    _put(catalog, ["y", "", "", "Example Institute", "old@example.org", "", "", "", "n"])
    # Second round: keep the institution, remove the email.
    result = _put(catalog, ["y", "", "", "", "-", "", "", "", "n"])

    assert result.exit_code == 0, result.output
    assert "Ada Lovelace (a123456) - author, Example Institute, old@example.org" in result.output
    assert _collection_contacts(catalog) == [
        {"name": "Ada Lovelace (a123456)", "organization": "Example Institute", "roles": ["author"]}
    ]


def test_removing_a_contact(catalog):
    result = _put(catalog, ["y", "-", "", "", "n"])  # "-" as the name removes it

    assert result.exit_code == 0, result.output
    assert "(none)" in result.output
    assert _collection_contacts(catalog) is None


def test_yes_to_publish_goes_on_to_upload(catalog):
    # Past the questions, put proceeds to log in -- stopped here by the fixture.
    result = _put(catalog, ["", "y"])

    assert "test: the server must not be contacted" in result.output
    assert "Not published" not in result.output


@pytest.mark.parametrize(
    "extra, can_ask",
    [(("--no-questions",), True), ((), False)],
    ids=["--no-questions", "nobody-at-terminal"],
)
def test_questions_are_skipped(catalog, monkeypatch, extra, can_ask):
    monkeypatch.setattr(contact_prompts, "can_ask", lambda: can_ask)

    result = _put(catalog, [], *extra)

    assert "About to publish" not in result.output
    assert "Publish now?" not in result.output
    assert "test: the server must not be contacted" in result.output


def test_missing_server_is_reported_before_any_question(catalog, monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_SERVER_URL")

    result = _put(catalog, ["", "y"])

    assert result.exit_code != 0
    assert "No server configured" in result.output
    assert "About to publish" not in result.output


def test_typo_is_fixed_by_typing_over_it_in_a_second_round(catalog):
    answers = [
        "y",  # add or change
        "", "",  # name, role
        "Exmaple Institute",  # typo
        "", "",  # email, ORCID
        "",  # add another: no
        "y",  # change anything: yes -> second round
        "", "",  # name, role
        "Example Institute",  # typed over the typo, no '-' needed
        "", "",  # email, ORCID
        "",  # add another: no
        "",  # change anything: no
        "n",  # publish now: no
    ]
    result = _put(catalog, answers)

    assert result.exit_code == 0, result.output
    assert 'Press Enter to continue, type a new one to replace it, or "-" to remove it.' in result.output
    assert "  Institution [Exmaple Institute]:" in result.output  # short question
    assert _collection_contacts(catalog) == [
        {
            "name": "Ada Lovelace (a123456)",
            "organization": "Example Institute",
            "roles": ["author"],
        }
    ]
