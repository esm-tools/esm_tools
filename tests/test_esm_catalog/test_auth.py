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
    # platformdirs honours XDG_*_HOME on every platform when set; the fallback
    # and relative-path handling are platformdirs' own contract, not ours.
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "cfg"))
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path / "state"))
    assert xdg.config_file() == tmp_path / "cfg" / "esm-catalog" / "config.yaml"
    assert (
        xdg.token_file("https://stac.example.org")
        == tmp_path / "state" / "esm-catalog" / "tokens" / "stac.example.org.json"
    )


def test_token_file_sanitises_unparsable_server_url(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    # No scheme -> urlsplit finds no netloc; falls back to the raw string, still
    # sanitised to a safe filename rather than raising.
    path = xdg.token_file("not a url at all")
    assert path.parent == tmp_path / "esm-catalog" / "tokens"
    assert path.name == "not_a_url_at_all.json"


# --------------------------------------------------------------------------- #
# PKCE.
# --------------------------------------------------------------------------- #


def test_pkce_challenge_is_s256_of_verifier():
    verifier, challenge = auth.generate_pkce_pair()
    expected = (
        base64.urlsafe_b64encode(hashlib.sha256(verifier.encode()).digest())
        .decode()
        .rstrip("=")
    )
    assert challenge == expected
    assert "=" not in challenge and "=" not in verifier


def test_pkce_is_unique_each_call():
    assert auth.generate_pkce_pair()[0] != auth.generate_pkce_pair()[0]


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


_SERVER = "https://stac.example.org"


def test_token_cache_roundtrip_and_perms(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    assert auth.load_token(_SERVER) is None  # absent

    auth.save_token(
        TokenSet(access_token="abc", refresh_token="r", expires_at=42), _SERVER
    )
    loaded = auth.load_token(_SERVER)
    assert loaded is not None
    assert loaded.access_token == "abc" and loaded.refresh_token == "r"

    mode = stat.S_IMODE(xdg.token_file(_SERVER).stat().st_mode)
    assert mode == 0o600  # secret material not world-readable


def test_token_cache_is_scoped_per_server(monkeypatch, tmp_path):
    # The bug this closes: logging into a second server must not clobber the
    # first's cached token, and each server's token must resolve independently.
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    server_a, server_b = "https://stac-a.example.org", "https://stac-b.example.org"

    auth.save_token(TokenSet(access_token="token-a"), server_a)
    auth.save_token(TokenSet(access_token="token-b"), server_b)

    assert auth.load_token(server_a).access_token == "token-a"
    assert auth.load_token(server_b).access_token == "token-b"
    assert auth.clear_token(server_a) is True
    assert auth.load_token(server_a) is None
    assert auth.load_token(server_b).access_token == "token-b"  # untouched


def test_load_token_tolerates_garbage(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    path = xdg.token_file(_SERVER)
    path.parent.mkdir(parents=True)
    path.write_text("{ not json")
    assert auth.load_token(_SERVER) is None


def test_get_bearer_token_requires_a_configured_server(monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_SERVER_URL", raising=False)
    with pytest.raises(auth.AuthError, match="no server configured"):
        auth.get_bearer_token(Settings(server_url=None))


def test_get_bearer_token_names_the_server_when_not_logged_in(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    with pytest.raises(auth.AuthError, match=_SERVER):
        auth.get_bearer_token(Settings(server_url=_SERVER))


def test_clear_token(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    assert auth.clear_token(_SERVER) is False  # nothing yet
    auth.save_token(TokenSet(access_token="x"), _SERVER)
    assert auth.clear_token(_SERVER) is True
    assert auth.load_token(_SERVER) is None


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


def test_verify_tls_from_env(monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_VERIFY_TLS", "false")
    assert Settings().verify_tls is False
    monkeypatch.setenv("ESM_CATALOG_VERIFY_TLS", "true")
    assert Settings().verify_tls is True


def test_secret_not_leaked_in_repr(monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_CLIENT_SECRET", "supersecret")
    s = Settings()
    assert "supersecret" not in repr(s)
    assert s.client_secret.get_secret_value() == "supersecret"
