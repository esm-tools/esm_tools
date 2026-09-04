"""OIDC authentication for the catalog client.

The ``glab auth login`` pattern against an OIDC provider (Helmholtz AAI): open a
browser, log in, copy the returned code back into the terminal. The provider
here advertises only the authorization-code grant, so this uses
authorization-code + PKCE with an out-of-band (copy-paste) redirect — which,
unlike a localhost-loopback flow, also works from a headless HPC login node
where no browser is available on the machine running the CLI.

The resulting token is cached per server at
``$XDG_STATE_HOME/esm-catalog/tokens/<host>.json`` (mode ``0600``) — a STAC
server and its identity provider are separate systems (``oidc_discovery_url``/
``client_id`` are configured independently, not derived from the server URL),
so the server URL only *labels* which server a token belongs to; it never
selects the identity provider. ``get_bearer_token()`` returns a valid access
token for ``settings.server_url``, refreshing via the refresh token when the
cached one has expired.
"""

from __future__ import annotations

import base64
import hashlib
import os
import secrets
import time
from typing import NewType, Optional, TypedDict

import httpx
from loguru import logger
from pydantic import BaseModel

from esm_catalog.config import Scope, Settings, TokenType, Url
from esm_catalog.xdg import token_file

# Refresh a little before real expiry so a long push does not die mid-flight.
EXPIRY_SKEW_SECONDS = 30

# Opaque OAuth credential strings. NewTypes (not the transparent aliases in
# types.py) so the checker rejects passing one where another is meant: an access
# token is not a refresh token is not an authorization code is not a PKCE
# verifier/challenge, even though all are strings on the wire.
AccessToken = NewType("AccessToken", str)
RefreshToken = NewType("RefreshToken", str)
AuthCode = NewType("AuthCode", str)
CodeVerifier = NewType("CodeVerifier", str)
CodeChallenge = NewType("CodeChallenge", str)


class TokenResponse(TypedDict, total=False):
    """The raw token-endpoint JSON — carries a *relative* ``expires_in``.

    Distinct from :class:`TokenSet`, which stores an *absolute* ``expires_at``;
    :meth:`TokenSet.from_response` performs that transform.
    """

    access_token: AccessToken
    refresh_token: RefreshToken
    token_type: TokenType
    scope: Scope
    expires_in: float


class OIDCMetadata(BaseModel):
    """The subset of the OIDC discovery document the client uses."""

    authorization_endpoint: Url
    token_endpoint: Url
    issuer: Optional[Url] = None


class TokenSet(BaseModel):
    """A cached OAuth token, plus the absolute time it expires."""

    access_token: AccessToken
    refresh_token: Optional[RefreshToken] = None
    token_type: TokenType = "Bearer"
    scope: Optional[Scope] = None
    #: Absolute epoch seconds at which ``access_token`` expires, if known.
    expires_at: Optional[float] = None

    @classmethod
    def from_response(cls, payload: TokenResponse) -> "TokenSet":
        """Build from a token-endpoint payload, stamping absolute expiry.

        ``payload`` is a plain ``dict`` (``resp.json()``), so ``"expires_in" in
        payload`` is the correct membership test — the relative lifetime becomes
        an absolute ``expires_at``.
        """
        token = cls.model_validate(payload)
        if "expires_in" in payload:
            token.expires_at = time.time() + float(payload["expires_in"])
        return token

    def is_expired(
        self, now: Optional[float] = None, skew: int = EXPIRY_SKEW_SECONDS
    ) -> bool:
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


def generate_pkce_pair() -> tuple[CodeVerifier, CodeChallenge]:
    """Return ``(code_verifier, code_challenge)`` for PKCE S256."""
    verifier = _b64url(secrets.token_bytes(64))
    challenge = _b64url(hashlib.sha256(verifier.encode("ascii")).digest())
    return CodeVerifier(verifier), CodeChallenge(challenge)


# --------------------------------------------------------------------------- #
# Token cache (0600 file).
# --------------------------------------------------------------------------- #


def load_token(server_url: str) -> Optional[TokenSet]:
    """Load the token cached for *server_url*, or ``None`` if absent or unreadable."""
    path = token_file(server_url)
    if not path.exists():
        return None
    try:
        return TokenSet.model_validate_json(path.read_text())
    except (ValueError, OSError):
        logger.warning("Ignoring unreadable token cache at {}", path)
        return None


def save_token(token: TokenSet, server_url: str) -> None:
    """Cache *token* for *server_url*, creating it ``0600`` before any secret lands."""
    path = token_file(server_url)
    path.parent.mkdir(parents=True, exist_ok=True)
    # O_CREAT's mode applies only when the file is created (and is umask-masked);
    # on a re-login the existing file keeps its old perms. The explicit chmod
    # pins 0600 regardless of umask or a pre-existing, possibly-widened file.
    fd = os.open(str(path), os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as handle:
        handle.write(token.model_dump_json(indent=2))
    os.chmod(path, 0o600)


def clear_token(server_url: str) -> bool:
    """Delete the token cached for *server_url*; return whether a file was removed."""
    path = token_file(server_url)
    if path.exists():
        path.unlink()
        return True
    return False


# --------------------------------------------------------------------------- #
# OIDC calls.
# --------------------------------------------------------------------------- #


def fetch_oidc_metadata(settings: Settings) -> OIDCMetadata:
    """Fetch and parse the OIDC discovery document."""
    resp = httpx.get(
        settings.oidc_discovery_url, timeout=15, verify=settings.verify_tls
    )
    resp.raise_for_status()
    return OIDCMetadata.model_validate(resp.json())


def build_login_url(
    meta: OIDCMetadata, settings: Settings, challenge: CodeChallenge
) -> Url:
    """Build the authorization-endpoint URL to open in a browser.

    No ``state`` is sent: the out-of-band copy-paste flow has no automated
    callback to verify the echo against, so a ``state`` nonce would be security
    theatre. It returns (with verification) if a loopback-callback login is added.
    """
    params = {
        "response_type": "code",
        "client_id": settings.client_id,
        "redirect_uri": settings.redirect_uri,
        "scope": settings.scopes,
        "code_challenge": challenge,
        "code_challenge_method": "S256",
    }
    return str(httpx.URL(meta.authorization_endpoint, params=params))


def _request_token(
    meta: OIDCMetadata, settings: Settings, data: dict[str, str]
) -> TokenSet:
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
    return TokenSet.from_response(resp.json())


def exchange_code_for_token(
    meta: OIDCMetadata, settings: Settings, code: AuthCode, verifier: CodeVerifier
) -> TokenSet:
    """Exchange an authorization *code* (+ PKCE verifier) for a token."""
    return _request_token(
        meta,
        settings,
        {
            "grant_type": "authorization_code",
            "code": code,
            "redirect_uri": settings.redirect_uri,
            "code_verifier": verifier,
        },
    )


def refresh_access_token(meta: OIDCMetadata, settings: Settings, token: TokenSet) -> TokenSet:
    """Mint a fresh access token from *token*'s refresh token."""
    if not token.refresh_token:
        raise AuthError("no refresh token cached; run 'esm-catalog auth login'")
    new = _request_token(
        meta,
        settings,
        {"grant_type": "refresh_token", "refresh_token": token.refresh_token},
    )
    # AAI may not rotate the refresh token; keep the old one if none came back.
    if new.refresh_token is None:
        new.refresh_token = token.refresh_token
    return new


def get_bearer_token(settings: Settings) -> AccessToken:
    """Return a valid access token, refreshing (and re-caching) if expired.

    The ``get_`` verb is deliberate: this does real work — reads the token
    cache, may refresh over the network, re-caches, and can raise
    :class:`AuthError` (no session, or refresh failed), which the caller should
    surface as a CLI error.
    """
    if not settings.server_url:
        raise AuthError(
            "no server configured; pass --server, set ESM_CATALOG_SERVER_URL, "
            "or add server_url to the config file"
        )
    token = load_token(settings.server_url)
    if token is None:
        raise AuthError(
            f"not logged in to {settings.server_url}; "
            f"run 'esm-catalog auth login {settings.server_url}'"
        )
    if token.is_expired():
        logger.debug("Access token expired; refreshing.")
        token = refresh_access_token(fetch_oidc_metadata(settings), settings, token)
        save_token(token, settings.server_url)
    return token.access_token
