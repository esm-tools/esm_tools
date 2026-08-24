"""OIDC authentication for the catalog client.

The ``glab auth login`` pattern against an OIDC provider (Helmholtz AAI): open a
browser, log in, copy the returned code back into the terminal. The provider
here advertises only the authorization-code grant, so this uses
authorization-code + PKCE with an out-of-band (copy-paste) redirect — which,
unlike a localhost-loopback flow, also works from a headless HPC login node
where no browser is available on the machine running the CLI.

The resulting token is cached at ``$XDG_STATE_HOME/esm-catalog/token.json``
(mode ``0600``). ``bearer()`` returns a valid access token, silently refreshing
via the refresh token when the cached one has expired.
"""

from __future__ import annotations

import base64
import hashlib
import os
import secrets
import time
from typing import Optional

import httpx
from loguru import logger
from pydantic import BaseModel

from esm_catalog.config import Settings
from esm_catalog.xdg import token_file

# Refresh a little before real expiry so a long push does not die mid-flight.
EXPIRY_SKEW_SECONDS = 30


class OIDCMetadata(BaseModel):
    """The subset of the OIDC discovery document the client uses."""

    authorization_endpoint: str
    token_endpoint: str
    issuer: Optional[str] = None


class TokenSet(BaseModel):
    """A cached OAuth token, plus the absolute time it expires."""

    access_token: str
    refresh_token: Optional[str] = None
    token_type: str = "Bearer"
    scope: Optional[str] = None
    #: Absolute epoch seconds at which ``access_token`` expires, if known.
    expires_at: Optional[float] = None

    def is_expired(self, now: Optional[float] = None, skew: int = EXPIRY_SKEW_SECONDS) -> bool:
        """True if the access token is (within *skew* of) expiry, or unknown."""
        if self.expires_at is None:
            return True
        now = time.time() if now is None else now
        return now >= (self.expires_at - skew)


class AuthError(RuntimeError):
    """Authentication failed (bad exchange, refresh failure, or no session)."""


# --------------------------------------------------------------------------- #
# PKCE.
# --------------------------------------------------------------------------- #


def _b64url(raw: bytes) -> str:
    """Base64url without padding (RFC 7636)."""
    return base64.urlsafe_b64encode(raw).decode("ascii").rstrip("=")


def pkce_pair() -> tuple[str, str]:
    """Return ``(code_verifier, code_challenge)`` for PKCE S256."""
    verifier = _b64url(secrets.token_bytes(64))
    challenge = _b64url(hashlib.sha256(verifier.encode("ascii")).digest())
    return verifier, challenge


# --------------------------------------------------------------------------- #
# Token cache (0600 file).
# --------------------------------------------------------------------------- #


def load_token() -> Optional[TokenSet]:
    """Load the cached token, or ``None`` if absent or unreadable."""
    path = token_file()
    if not path.exists():
        return None
    try:
        return TokenSet.model_validate_json(path.read_text())
    except (ValueError, OSError):
        logger.warning("Ignoring unreadable token cache at {}", path)
        return None


def save_token(token: TokenSet) -> None:
    """Write *token* to the cache, creating it ``0600`` before any secret lands."""
    path = token_file()
    path.parent.mkdir(parents=True, exist_ok=True)
    fd = os.open(str(path), os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as handle:
        handle.write(token.model_dump_json(indent=2))
    os.chmod(path, 0o600)


def clear_token() -> bool:
    """Delete the cached token; return whether a file was removed."""
    path = token_file()
    if path.exists():
        path.unlink()
        return True
    return False


# --------------------------------------------------------------------------- #
# OIDC calls.
# --------------------------------------------------------------------------- #


def discover(settings: Settings) -> OIDCMetadata:
    """Fetch and parse the OIDC discovery document."""
    resp = httpx.get(settings.oidc_discovery_url, timeout=15, verify=settings.verify_tls)
    resp.raise_for_status()
    return OIDCMetadata.model_validate(resp.json())


def build_login_url(
    meta: OIDCMetadata, settings: Settings, challenge: str, state: str
) -> str:
    """Build the authorization-endpoint URL to open in a browser."""
    params = {
        "response_type": "code",
        "client_id": settings.client_id,
        "redirect_uri": settings.redirect_uri,
        "scope": settings.scopes,
        "state": state,
        "code_challenge": challenge,
        "code_challenge_method": "S256",
    }
    return str(httpx.URL(meta.authorization_endpoint, params=params))


def _token_request(meta: OIDCMetadata, settings: Settings, data: dict) -> TokenSet:
    """POST to the token endpoint with client-secret-basic auth; return a TokenSet."""
    resp = httpx.post(
        meta.token_endpoint,
        data=data,
        auth=(settings.client_id, settings.client_secret.get_secret_value()),
        timeout=15,
        verify=settings.verify_tls,
    )
    if resp.status_code != 200:
        raise AuthError(f"token endpoint returned HTTP {resp.status_code}: {resp.text}")
    payload = resp.json()
    token = TokenSet.model_validate(payload)
    if "expires_in" in payload:
        token.expires_at = time.time() + float(payload["expires_in"])
    return token


def exchange_code(
    meta: OIDCMetadata, settings: Settings, code: str, verifier: str
) -> TokenSet:
    """Exchange an authorization *code* (+ PKCE verifier) for a token."""
    return _token_request(
        meta,
        settings,
        {
            "grant_type": "authorization_code",
            "code": code,
            "redirect_uri": settings.redirect_uri,
            "code_verifier": verifier,
        },
    )


def refresh(meta: OIDCMetadata, settings: Settings, token: TokenSet) -> TokenSet:
    """Mint a fresh access token from *token*'s refresh token."""
    if not token.refresh_token:
        raise AuthError("no refresh token cached; run 'esm-catalog auth login'")
    new = _token_request(
        meta,
        settings,
        {"grant_type": "refresh_token", "refresh_token": token.refresh_token},
    )
    # AAI may not rotate the refresh token; keep the old one if none came back.
    if new.refresh_token is None:
        new.refresh_token = token.refresh_token
    return new


def bearer(settings: Settings) -> str:
    """Return a valid access token, refreshing (and re-caching) if expired.

    Raises :class:`AuthError` with an actionable message if there is no session
    or the refresh fails — the caller should surface it as a CLI error.
    """
    token = load_token()
    if token is None:
        raise AuthError("not logged in; run 'esm-catalog auth login <server>'")
    if token.is_expired():
        logger.debug("Access token expired; refreshing.")
        token = refresh(discover(settings), settings, token)
        save_token(token)
    return token.access_token
