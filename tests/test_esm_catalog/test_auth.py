"""Unit tests for the offline-testable parts of the auth/config/xdg layer.

Network and interactive flows (discover, exchange_code, the login prompt) are
not exercised here — only pure logic: XDG resolution, PKCE derivation, the token
cache (roundtrip + 0600 perms), expiry, and config precedence.
"""

import base64
import hashlib
import stat

import pytest

from esm_catalog import auth, xdg
from esm_catalog.auth import TokenSet
from esm_catalog.config import Settings


# --------------------------------------------------------------------------- #
# XDG.
# --------------------------------------------------------------------------- #


def test_xdg_honours_env(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "cfg"))
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path / "state"))
    assert xdg.config_file() == tmp_path / "cfg" / "esm-catalog" / "config.yaml"
    assert xdg.token_file() == tmp_path / "state" / "esm-catalog" / "token.json"


def test_xdg_falls_back_to_home(monkeypatch):
    monkeypatch.delenv("XDG_CONFIG_HOME", raising=False)
    monkeypatch.delenv("XDG_STATE_HOME", raising=False)
    assert xdg.config_dir().name == "esm-catalog"
    assert xdg.config_dir().parent.name == ".config"
    assert xdg.state_dir().parent.name == "state"  # ~/.local/state


def test_xdg_ignores_relative_env(monkeypatch):
    # The spec says relative paths must be ignored.
    monkeypatch.setenv("XDG_CONFIG_HOME", "relative/path")
    assert xdg.config_home().is_absolute()


# --------------------------------------------------------------------------- #
# PKCE.
# --------------------------------------------------------------------------- #


def test_pkce_challenge_is_s256_of_verifier():
    verifier, challenge = auth.pkce_pair()
    expected = (
        base64.urlsafe_b64encode(hashlib.sha256(verifier.encode()).digest())
        .decode()
        .rstrip("=")
    )
    assert challenge == expected
    assert "=" not in challenge and "=" not in verifier


def test_pkce_is_unique_each_call():
    assert auth.pkce_pair()[0] != auth.pkce_pair()[0]


# --------------------------------------------------------------------------- #
# TokenSet expiry.
# --------------------------------------------------------------------------- #


def test_token_expiry():
    assert TokenSet(access_token="x").is_expired()  # no expires_at -> expired
    assert not TokenSet(access_token="x", expires_at=1000).is_expired(now=100)
    assert TokenSet(access_token="x", expires_at=1000).is_expired(now=1000)
    # within skew window
    assert TokenSet(access_token="x", expires_at=1000).is_expired(now=980, skew=30)


# --------------------------------------------------------------------------- #
# Token cache.
# --------------------------------------------------------------------------- #


def test_token_cache_roundtrip_and_perms(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    assert auth.load_token() is None  # absent

    auth.save_token(TokenSet(access_token="abc", refresh_token="r", expires_at=42))
    loaded = auth.load_token()
    assert loaded is not None
    assert loaded.access_token == "abc" and loaded.refresh_token == "r"

    mode = stat.S_IMODE(xdg.token_file().stat().st_mode)
    assert mode == 0o600  # secret material not world-readable


def test_load_token_tolerates_garbage(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    path = xdg.token_file()
    path.parent.mkdir(parents=True)
    path.write_text("{ not json")
    assert auth.load_token() is None


def test_clear_token(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    assert auth.clear_token() is False  # nothing yet
    auth.save_token(TokenSet(access_token="x"))
    assert auth.clear_token() is True
    assert auth.load_token() is None


# --------------------------------------------------------------------------- #
# Config precedence + api_url.
# --------------------------------------------------------------------------- #


def test_config_env_beats_yaml(monkeypatch, tmp_path):
    cfg = tmp_path / "esm-catalog"
    cfg.mkdir(parents=True)
    (cfg / "config.yaml").write_text(
        "server_url: https://from-yaml\nclient_id: yaml-client\n"
    )
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path))
    monkeypatch.setenv("ESM_CATALOG_SERVER_URL", "https://from-env")

    # yaml_file path is bound at class-definition time; point the source at ours.
    from pydantic_settings import YamlConfigSettingsSource

    class LocalSettings(Settings):
        @classmethod
        def settings_customise_sources(cls, settings_cls, init_settings, env_settings, dotenv_settings, file_secret_settings):
            return (
                init_settings,
                env_settings,
                YamlConfigSettingsSource(settings_cls, yaml_file=str(cfg / "config.yaml")),
            )

    s = LocalSettings()
    assert s.server_url == "https://from-env"  # env wins
    assert s.client_id == "yaml-client"  # yaml fills the gap
    assert s.api_url == "https://from-env/api"


def test_api_url_requires_server(monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_SERVER_URL", raising=False)
    s = Settings(server_url=None)
    with pytest.raises(ValueError):
        _ = s.api_url


def test_secret_not_leaked_in_repr(monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_CLIENT_SECRET", "supersecret")
    s = Settings()
    assert "supersecret" not in repr(s)
    assert s.client_secret.get_secret_value() == "supersecret"
