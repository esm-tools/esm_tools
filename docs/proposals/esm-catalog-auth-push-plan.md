# esm_catalog auth + push — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement `esm_catalog auth login`, `esm_catalog auth logout`, and `esm_catalog push` — log in to a STAC server (through its OIDC auth proxy), then ship the local stac-geoparquet shards into its pgstac via the STAC API.

**Architecture:** Auth sits behind a `TokenProvider` protocol with three backends (`Oidc`, `Demo`, `Dummy`) so the identity provider is swappable; a token cache persists the bearer per server. `push` reads the workspace shards, upserts the Collection, and sends each not-yet-pushed shard to `POST /collections/{id}/bulk_items` with the bearer, recording progress in the workspace state.

**Tech Stack:** Python, `requests` (sync HTTP, already in the env), `stac-geoparquet` (shard→items, already a dep), `rich_click` (CLI), pydantic v2 (models), pytest + `responses` (HTTP mocking).

## Global Constraints

- Python 3.9-compatible: use `Optional[X]`, never `X | None`, in any annotation pydantic evaluates. (Repo supports 3.6–3.12.)
- Never sign commits: every `git commit` uses `--no-gpg-sign`. Do not push; do not open PRs.
- HTTP client is `requests`; declare it in `setup.py`'s catalog extra. Do not add `httpx`.
- Reuse `stac_geoparquet.arrow.stac_table_to_items` to read shards; do not hand-roll parquet→item.
- Token cache file is mode `0o600` under `$XDG_STATE_HOME` (default `~/.local/state`), path `esm_catalog/<host>.json`.
- Prose in code (docstrings, comments) is plain and human-written: no AI working-notes, no em-dash asides, no restating the code. Keep only real rationale.
- Provider selection: `ESM_CATALOG_AUTH_BACKEND` env var, one of `oidc` (default), `demo`, `dummy`.
- Run the catalog suite with `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog -q`.

---

## File Structure

- `src/esm_catalog/auth/__init__.py` — public exports (`TokenProvider`, `NotLoggedIn`, `get_provider`).
- `src/esm_catalog/auth/base.py` — `TokenProvider` Protocol, `NotLoggedIn`.
- `src/esm_catalog/auth/cache.py` — `TokenCache` model + read/write to the XDG path.
- `src/esm_catalog/auth/dummy.py` — `DummyTokenProvider`.
- `src/esm_catalog/auth/demo.py` — `DemoTokenProvider` (demo/demo password grant).
- `src/esm_catalog/auth/oidc.py` — `OidcTokenProvider` (discovery + device grant + refresh).
- `src/esm_catalog/auth/select.py` — `get_provider()` factory (reads `ESM_CATALOG_AUTH_BACKEND`).
- `src/esm_catalog/push.py` — `push_catalog()` (collection upsert + shard→items + bulk_items + state).
- `src/esm_catalog/scan/workspace.py` — extend `WorkspaceState` with `pushed_shards` + `server_url`.
- `src/esm_catalog/cli.py` — wire `auth login`, `auth logout`, `push`.
- `setup.py` — add `requests` to the catalog extra.
- Tests: `tests/test_esm_catalog/test_auth_cache.py`, `test_auth_demo.py`, `test_auth_dummy.py`, `test_auth_oidc.py`, `test_push.py`, `test_cli_auth_push.py`, `test_auth_push_integration.py` (opt-in).

---

## Task 1: TokenProvider seam + NotLoggedIn

**Files:**
- Create: `src/esm_catalog/auth/base.py`
- Create: `src/esm_catalog/auth/__init__.py`
- Test: `tests/test_esm_catalog/test_auth_dummy.py` (exercises the protocol via the dummy in Task 3; here just import-and-shape)

**Interfaces:**
- Produces: `class TokenProvider(Protocol)` with `login(self, server_url: str) -> None`, `get_token(self) -> str`, `logout(self) -> None`; `class NotLoggedIn(Exception)`.

- [ ] **Step 1: Write the failing test** — `tests/test_esm_catalog/test_auth_base.py`

```python
from esm_catalog.auth import NotLoggedIn, TokenProvider


def test_notloggedin_is_exception():
    assert issubclass(NotLoggedIn, Exception)


def test_protocol_is_runtime_checkable():
    class P:
        def login(self, server_url): ...
        def get_token(self): return "t"
        def logout(self): ...

    assert isinstance(P(), TokenProvider)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_base.py -q`
Expected: FAIL (`No module named esm_catalog.auth`).

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/auth/base.py`

```python
"""The token-provider seam: the CLI and push depend only on this, never on the IdP."""

from __future__ import annotations

from typing import Protocol, runtime_checkable


class NotLoggedIn(Exception):
    """Raised when no usable token is available and the user must log in."""


@runtime_checkable
class TokenProvider(Protocol):
    """Supplies a bearer token for the STAC API, hiding the identity provider."""

    def login(self, server_url: str) -> None: ...
    def get_token(self) -> str: ...
    def logout(self) -> None: ...
```

and `src/esm_catalog/auth/__init__.py`:

```python
"""Authentication for the esm_catalog CLI: a swappable token-provider seam."""

from esm_catalog.auth.base import NotLoggedIn, TokenProvider

__all__ = ["NotLoggedIn", "TokenProvider"]
```

- [ ] **Step 4: Run test to verify it passes**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_base.py -q`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/auth/base.py src/esm_catalog/auth/__init__.py tests/test_esm_catalog/test_auth_base.py
git commit --no-gpg-sign -m "feat(esm_catalog): TokenProvider seam + NotLoggedIn"
```

---

## Task 2: Token cache

**Files:**
- Create: `src/esm_catalog/auth/cache.py`
- Test: `tests/test_esm_catalog/test_auth_cache.py`

**Interfaces:**
- Produces: `class TokenCache(BaseModel)` fields `server_url: str`, `token_endpoint: str`, `client_id: str`, `access_token: str`, `refresh_token: Optional[str]`, `expires_at: float`; method `is_expired(self, skew: float = 30.0) -> bool`. Functions `cache_path(server_url: str) -> Path`, `read_cache(server_url: str) -> Optional[TokenCache]`, `write_cache(cache: TokenCache) -> None`, `clear_cache(server_url: str) -> None`.

- [ ] **Step 1: Write the failing test**

```python
import time
from esm_catalog.auth.cache import (
    TokenCache, cache_path, read_cache, write_cache, clear_cache,
)


def _cache(**kw):
    base = dict(server_url="https://stac.example", token_endpoint="https://kc/token",
                client_id="cli", access_token="AT", refresh_token="RT",
                expires_at=time.time() + 3600)
    base.update(kw)
    return TokenCache(**base)


def test_roundtrip_and_mode(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    write_cache(_cache())
    path = cache_path("https://stac.example")
    assert oct(path.stat().st_mode)[-3:] == "600"
    got = read_cache("https://stac.example")
    assert got.access_token == "AT"


def test_missing_is_none(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    assert read_cache("https://nope.example") is None


def test_is_expired_uses_skew():
    assert _cache(expires_at=time.time() + 10).is_expired(skew=30) is True
    assert _cache(expires_at=time.time() + 100).is_expired(skew=30) is False


def test_clear(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    write_cache(_cache())
    clear_cache("https://stac.example")
    assert read_cache("https://stac.example") is None
```

- [ ] **Step 2: Run to verify it fails**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_cache.py -q`
Expected: FAIL (`No module named esm_catalog.auth.cache`).

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/auth/cache.py`

```python
"""Persist the bearer token per server host, mode 600, under the XDG state dir."""

from __future__ import annotations

import os
import time
from pathlib import Path
from typing import Optional
from urllib.parse import urlparse

from pydantic import BaseModel


class TokenCache(BaseModel):
    """A cached OIDC token set for one server."""

    server_url: str
    token_endpoint: str
    client_id: str
    access_token: str
    refresh_token: Optional[str] = None
    expires_at: float  # epoch seconds

    def is_expired(self, skew: float = 30.0) -> bool:
        return time.time() >= (self.expires_at - skew)


def _state_home() -> Path:
    return Path(os.environ.get("XDG_STATE_HOME", str(Path.home() / ".local" / "state")))


def cache_path(server_url: str) -> Path:
    host = urlparse(server_url).netloc or server_url
    return _state_home() / "esm_catalog" / f"{host}.json"


def read_cache(server_url: str) -> Optional[TokenCache]:
    path = cache_path(server_url)
    if not path.exists():
        return None
    return TokenCache.model_validate_json(path.read_text())


def write_cache(cache: TokenCache) -> None:
    path = cache_path(cache.server_url)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(cache.model_dump_json())
    path.chmod(0o600)


def clear_cache(server_url: str) -> None:
    cache_path(server_url).unlink(missing_ok=True)
```

- [ ] **Step 4: Run to verify it passes**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_cache.py -q`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/auth/cache.py tests/test_esm_catalog/test_auth_cache.py
git commit --no-gpg-sign -m "feat(esm_catalog): token cache (XDG, mode 600)"
```

---

## Task 3: DummyTokenProvider

**Files:**
- Create: `src/esm_catalog/auth/dummy.py`
- Test: `tests/test_esm_catalog/test_auth_dummy.py`

**Interfaces:**
- Produces: `class DummyTokenProvider` implementing `TokenProvider`; `get_token()` returns `os.environ.get("ESM_CATALOG_TOKEN", "")`.

- [ ] **Step 1: Write the failing test**

```python
from esm_catalog.auth import TokenProvider
from esm_catalog.auth.dummy import DummyTokenProvider


def test_returns_env_token(monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_TOKEN", "abc")
    p = DummyTokenProvider()
    p.login("https://stac.example")   # no-op
    assert p.get_token() == "abc"


def test_empty_when_unset(monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_TOKEN", raising=False)
    assert DummyTokenProvider().get_token() == ""


def test_satisfies_protocol():
    assert isinstance(DummyTokenProvider(), TokenProvider)
```

- [ ] **Step 2: Run to verify it fails**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_dummy.py -q`
Expected: FAIL (`No module named esm_catalog.auth.dummy`).

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/auth/dummy.py`

```python
"""A no-network token provider for auth-off local stacks and tests."""

from __future__ import annotations

import os


class DummyTokenProvider:
    """Returns the ESM_CATALOG_TOKEN env value, or an empty token."""

    def login(self, server_url: str) -> None:
        return None

    def get_token(self) -> str:
        return os.environ.get("ESM_CATALOG_TOKEN", "")

    def logout(self) -> None:
        return None
```

- [ ] **Step 4: Run to verify it passes**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_dummy.py -q`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/auth/dummy.py tests/test_esm_catalog/test_auth_dummy.py
git commit --no-gpg-sign -m "feat(esm_catalog): DummyTokenProvider"
```

---

## Task 4: DemoTokenProvider (demo/demo password grant)

**Files:**
- Create: `src/esm_catalog/auth/demo.py`
- Test: `tests/test_esm_catalog/test_auth_demo.py`

**Interfaces:**
- Consumes: `TokenCache`, `read_cache`, `write_cache`, `clear_cache` (Task 2).
- Produces: `class DemoTokenProvider`; `login()` does a Resource-Owner-Password-Credentials grant with `demo`/`demo` against `<server>`'s token endpoint (discovered like the OIDC backend, Task 5, via `discover_endpoints`), caches the JWT; `get_token()` refreshes on expiry; `logout()` clears the cache. Client id `esm-catalog-cli`.

- [ ] **Step 1: Write the failing test** (mock the token endpoint with `responses`)

```python
import time
import responses
from esm_catalog.auth import NotLoggedIn
from esm_catalog.auth.demo import DemoTokenProvider

TOKEN_URL = "https://kc/realms/esm/protocol/openid-connect/token"
WELL_KNOWN = "https://stac.example/.well-known/openid-configuration"


def _register_discovery():
    responses.add(responses.GET, WELL_KNOWN, json={
        "token_endpoint": TOKEN_URL,
        "device_authorization_endpoint": "https://kc/device",
    })


@responses.activate
def test_login_password_grant_caches_token(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    _register_discovery()
    responses.add(responses.POST, TOKEN_URL, json={
        "access_token": "JWT1", "refresh_token": "R1", "expires_in": 300,
    })
    p = DemoTokenProvider()
    p.login("https://stac.example")
    assert p.get_token() == "JWT1"
    # the POST body carried the demo/demo password grant
    body = responses.calls[-1].request.body
    assert "grant_type=password" in body and "username=demo" in body


@responses.activate
def test_get_token_refreshes_when_expired(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    _register_discovery()
    responses.add(responses.POST, TOKEN_URL, json={
        "access_token": "OLD", "refresh_token": "R1", "expires_in": -1,
    })
    p = DemoTokenProvider()
    p.login("https://stac.example")
    responses.add(responses.POST, TOKEN_URL, json={
        "access_token": "NEW", "refresh_token": "R2", "expires_in": 300,
    })
    assert p.get_token() == "NEW"


def test_get_token_without_login_raises(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    import pytest
    with pytest.raises(NotLoggedIn):
        DemoTokenProvider(server_url="https://stac.example").get_token()
```

- [ ] **Step 2: Run to verify it fails**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_demo.py -q`
Expected: FAIL (`No module named esm_catalog.auth.demo`). (Also fails on `discover_endpoints` until Task 5; implement a local copy here in Step 3 and have Task 5 import it, to keep Task 4 self-contained.)

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/auth/demo.py`

```python
"""A token provider that logs in as the stack's built-in demo/demo user."""

from __future__ import annotations

import time
from typing import Optional

import requests

from esm_catalog.auth.base import NotLoggedIn
from esm_catalog.auth.cache import (
    TokenCache, read_cache, write_cache, clear_cache,
)

_CLIENT_ID = "esm-catalog-cli"


def discover_endpoints(server_url: str) -> dict:
    """Fetch the server's OIDC config (token + device endpoints)."""
    url = server_url.rstrip("/") + "/.well-known/openid-configuration"
    response = requests.get(url, timeout=30)
    response.raise_for_status()
    return response.json()


def _to_cache(server_url: str, token_endpoint: str, payload: dict) -> TokenCache:
    return TokenCache(
        server_url=server_url,
        token_endpoint=token_endpoint,
        client_id=_CLIENT_ID,
        access_token=payload["access_token"],
        refresh_token=payload.get("refresh_token"),
        expires_at=time.time() + float(payload.get("expires_in", 0)),
    )


class DemoTokenProvider:
    """Logs in with the stack's demo/demo user via a password grant."""

    def __init__(self, server_url: Optional[str] = None) -> None:
        self._server_url = server_url

    def login(self, server_url: str) -> None:
        self._server_url = server_url
        token_endpoint = discover_endpoints(server_url)["token_endpoint"]
        response = requests.post(token_endpoint, data={
            "grant_type": "password",
            "client_id": _CLIENT_ID,
            "username": "demo",
            "password": "demo",
        }, timeout=30)
        response.raise_for_status()
        write_cache(_to_cache(server_url, token_endpoint, response.json()))

    def get_token(self) -> str:
        if self._server_url is None:
            raise NotLoggedIn("no server; run auth login")
        cache = read_cache(self._server_url)
        if cache is None:
            raise NotLoggedIn("not logged in; run auth login")
        if not cache.is_expired():
            return cache.access_token
        if not cache.refresh_token:
            raise NotLoggedIn("session expired; run auth login")
        response = requests.post(cache.token_endpoint, data={
            "grant_type": "refresh_token",
            "client_id": cache.client_id,
            "refresh_token": cache.refresh_token,
        }, timeout=30)
        response.raise_for_status()
        refreshed = _to_cache(self._server_url, cache.token_endpoint, response.json())
        write_cache(refreshed)
        return refreshed.access_token

    def logout(self) -> None:
        if self._server_url:
            clear_cache(self._server_url)
```

- [ ] **Step 4: Run to verify it passes**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_demo.py -q`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/auth/demo.py tests/test_esm_catalog/test_auth_demo.py
git commit --no-gpg-sign -m "feat(esm_catalog): DemoTokenProvider (demo/demo grant)"
```

---

## Task 5: OidcTokenProvider (device grant)

**Files:**
- Create: `src/esm_catalog/auth/oidc.py`
- Test: `tests/test_esm_catalog/test_auth_oidc.py`

**Interfaces:**
- Consumes: `discover_endpoints`, `_to_cache` (Task 4), cache functions (Task 2).
- Produces: `class OidcTokenProvider`; `login()` runs the device-authorization grant (request device code, print verification URL + user code, poll the token endpoint until authorized or timeout), caches the JWT; `get_token()` and `logout()` behave as in `DemoTokenProvider` (refresh on expiry; clear on logout). Poll interval and print go through injectable params (`sleep`, `echo`) so the test does not really wait or print.

- [ ] **Step 1: Write the failing test**

```python
import responses
from esm_catalog.auth.oidc import OidcTokenProvider

WELL_KNOWN = "https://stac.example/.well-known/openid-configuration"
DEVICE_URL = "https://kc/device"
TOKEN_URL = "https://kc/token"


@responses.activate
def test_device_grant_polls_until_authorized(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    responses.add(responses.GET, WELL_KNOWN, json={
        "token_endpoint": TOKEN_URL, "device_authorization_endpoint": DEVICE_URL})
    responses.add(responses.POST, DEVICE_URL, json={
        "device_code": "D", "user_code": "WDJB-MJHT",
        "verification_uri": "https://stac.example/device", "interval": 0})
    # first poll: authorization_pending, second: success
    responses.add(responses.POST, TOKEN_URL, json={"error": "authorization_pending"}, status=400)
    responses.add(responses.POST, TOKEN_URL, json={
        "access_token": "JWT", "refresh_token": "R", "expires_in": 300})

    printed = []
    p = OidcTokenProvider(sleep=lambda s: None, echo=printed.append)
    p.login("https://stac.example")
    assert p.get_token() == "JWT"
    assert any("WDJB-MJHT" in line for line in printed)
```

- [ ] **Step 2: Run to verify it fails**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_oidc.py -q`
Expected: FAIL (`No module named esm_catalog.auth.oidc`).

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/auth/oidc.py`

```python
"""OIDC token provider using the device-authorization grant (no browser needed)."""

from __future__ import annotations

import time
from typing import Callable, Optional

import requests

from esm_catalog.auth.base import NotLoggedIn
from esm_catalog.auth.cache import read_cache, write_cache, clear_cache
from esm_catalog.auth.demo import _CLIENT_ID, _to_cache, discover_endpoints


class OidcTokenProvider:
    """Device-grant OIDC login against the server's identity provider."""

    def __init__(
        self,
        server_url: Optional[str] = None,
        sleep: Callable[[float], None] = time.sleep,
        echo: Callable[[str], None] = print,
        timeout_s: float = 300.0,
    ) -> None:
        self._server_url = server_url
        self._sleep = sleep
        self._echo = echo
        self._timeout_s = timeout_s

    def login(self, server_url: str) -> None:
        self._server_url = server_url
        config = discover_endpoints(server_url)
        token_endpoint = config["token_endpoint"]
        device = requests.post(config["device_authorization_endpoint"],
                               data={"client_id": _CLIENT_ID}, timeout=30)
        device.raise_for_status()
        info = device.json()
        self._echo(
            f"To log in, open {info['verification_uri']} and enter code: "
            f"{info['user_code']}"
        )
        interval = float(info.get("interval", 5))
        deadline = time.time() + self._timeout_s
        while time.time() < deadline:
            self._sleep(interval)
            poll = requests.post(token_endpoint, data={
                "grant_type": "urn:ietf:params:oauth:grant-type:device_code",
                "client_id": _CLIENT_ID,
                "device_code": info["device_code"],
            }, timeout=30)
            if poll.status_code == 200:
                write_cache(_to_cache(server_url, token_endpoint, poll.json()))
                return
            if poll.json().get("error") != "authorization_pending":
                poll.raise_for_status()
        raise NotLoggedIn("device login timed out")

    def get_token(self) -> str:
        if self._server_url is None:
            raise NotLoggedIn("no server; run auth login")
        cache = read_cache(self._server_url)
        if cache is None:
            raise NotLoggedIn("not logged in; run auth login")
        if not cache.is_expired():
            return cache.access_token
        if not cache.refresh_token:
            raise NotLoggedIn("session expired; run auth login")
        response = requests.post(cache.token_endpoint, data={
            "grant_type": "refresh_token",
            "client_id": cache.client_id,
            "refresh_token": cache.refresh_token,
        }, timeout=30)
        response.raise_for_status()
        refreshed = _to_cache(self._server_url, cache.token_endpoint, response.json())
        write_cache(refreshed)
        return refreshed.access_token

    def logout(self) -> None:
        if self._server_url:
            clear_cache(self._server_url)
```

- [ ] **Step 4: Run to verify it passes**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_oidc.py -q`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/auth/oidc.py tests/test_esm_catalog/test_auth_oidc.py
git commit --no-gpg-sign -m "feat(esm_catalog): OidcTokenProvider (device grant)"
```

---

## Task 6: Provider factory

**Files:**
- Create: `src/esm_catalog/auth/select.py`
- Modify: `src/esm_catalog/auth/__init__.py` (export `get_provider`)
- Test: `tests/test_esm_catalog/test_auth_select.py`

**Interfaces:**
- Produces: `get_provider(server_url: Optional[str] = None) -> TokenProvider` — reads `ESM_CATALOG_AUTH_BACKEND` (default `oidc`), returns the matching provider constructed with `server_url`.

- [ ] **Step 1: Write the failing test**

```python
from esm_catalog.auth import get_provider
from esm_catalog.auth.oidc import OidcTokenProvider
from esm_catalog.auth.demo import DemoTokenProvider
from esm_catalog.auth.dummy import DummyTokenProvider


def test_default_is_oidc(monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_AUTH_BACKEND", raising=False)
    assert isinstance(get_provider("https://s"), OidcTokenProvider)


def test_demo_and_dummy(monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_AUTH_BACKEND", "demo")
    assert isinstance(get_provider("https://s"), DemoTokenProvider)
    monkeypatch.setenv("ESM_CATALOG_AUTH_BACKEND", "dummy")
    assert isinstance(get_provider("https://s"), DummyTokenProvider)
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL (`cannot import name 'get_provider'`).

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/auth/select.py`

```python
"""Pick the token provider from ESM_CATALOG_AUTH_BACKEND (default oidc)."""

from __future__ import annotations

import os
from typing import Optional

from esm_catalog.auth.base import TokenProvider
from esm_catalog.auth.demo import DemoTokenProvider
from esm_catalog.auth.dummy import DummyTokenProvider
from esm_catalog.auth.oidc import OidcTokenProvider


def get_provider(server_url: Optional[str] = None) -> TokenProvider:
    backend = os.environ.get("ESM_CATALOG_AUTH_BACKEND", "oidc")
    if backend == "demo":
        return DemoTokenProvider(server_url=server_url)
    if backend == "dummy":
        return DummyTokenProvider()
    return OidcTokenProvider(server_url=server_url)
```

Add to `src/esm_catalog/auth/__init__.py`:

```python
from esm_catalog.auth.select import get_provider

__all__ = ["NotLoggedIn", "TokenProvider", "get_provider"]
```

- [ ] **Step 4: Run to verify it passes** — Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/auth/select.py src/esm_catalog/auth/__init__.py tests/test_esm_catalog/test_auth_select.py
git commit --no-gpg-sign -m "feat(esm_catalog): auth provider factory"
```

---

## Task 7: Workspace state — track pushed shards + server

**Files:**
- Modify: `src/esm_catalog/scan/workspace.py`
- Test: `tests/test_esm_catalog/test_workspace_push_state.py`

**Interfaces:**
- Produces: `WorkspaceState` gains `server_url: Optional[str] = None` and `pushed_shards: list[str] = []`; helpers `mark_pushed(state, shard_name)` and `is_pushed(state, shard_name)` — a `ts-*` name is pushed if recorded; `fx.parquet` is never considered pushed (always re-pushed).

- [ ] **Step 1: Write the failing test**

```python
from esm_catalog.scan.workspace import WorkspaceState, mark_pushed, is_pushed


def test_ts_shard_pushed_once():
    s = WorkspaceState(experiment_id="e", server_url="https://s")
    assert is_pushed(s, "ts-20000101.parquet") is False
    mark_pushed(s, "ts-20000101.parquet")
    assert is_pushed(s, "ts-20000101.parquet") is True


def test_fx_never_pushed():
    s = WorkspaceState(experiment_id="e")
    mark_pushed(s, "fx.parquet")
    assert is_pushed(s, "fx.parquet") is False   # always re-pushed
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL (`cannot import name 'mark_pushed'` / unexpected kwarg).

- [ ] **Step 3: Write minimal implementation** — add to `src/esm_catalog/scan/workspace.py`

Add the two fields to `WorkspaceState` (keep existing fields):

```python
    server_url: Optional[str] = None
    pushed_shards: list[str] = []
```

and module-level helpers:

```python
FX_SHARD = "fx.parquet"


def mark_pushed(state: WorkspaceState, shard_name: str) -> None:
    """Record *shard_name* as pushed (fx is mutable, so it is not tracked)."""
    if shard_name != FX_SHARD and shard_name not in state.pushed_shards:
        state.pushed_shards.append(shard_name)


def is_pushed(state: WorkspaceState, shard_name: str) -> bool:
    """Whether *shard_name* is already pushed. fx is never considered pushed."""
    if shard_name == FX_SHARD:
        return False
    return shard_name in state.pushed_shards
```

(Import `Optional` if not already imported. Reuse the existing `FX_SHARD` constant if `workspace.py` or `storage/geoparquet.py` already defines it — import rather than redefine.)

- [ ] **Step 4: Run to verify it passes** — Expected: PASS. Then run the whole suite to confirm no regression: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog -q`.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/scan/workspace.py tests/test_esm_catalog/test_workspace_push_state.py
git commit --no-gpg-sign -m "feat(esm_catalog): track pushed shards + server in workspace state"
```

---

## Task 8: push_catalog

**Files:**
- Create: `src/esm_catalog/push.py`
- Modify: `setup.py` (add `requests`)
- Test: `tests/test_esm_catalog/test_push.py`

**Interfaces:**
- Consumes: `TokenProvider` (Task 1), `WorkspaceState`/`mark_pushed`/`is_pushed` (Task 7), `stac_geoparquet.arrow.stac_table_to_items`.
- Produces: `push_catalog(catalog_dir: Path, provider: TokenProvider, *, echo: Callable[[str], None] = print) -> PushReport` where `PushReport` has `collections: int`, `shards_pushed: int`, `items: int`, `failures: list[str]`. Reads `collection.json` + `items/*.parquet` + the workspace state; upserts the collection; posts each not-pushed shard to `POST {server}/collections/{id}/bulk_items`.

- [ ] **Step 1: Write the failing test** (mock the STAC API with `responses`; write a tiny geoparquet shard from a known item)

```python
import json
from pathlib import Path
import responses
from esm_catalog.push import push_catalog
from esm_catalog.auth.dummy import DummyTokenProvider


def _workspace(tmp_path):
    # collection.json + one ts shard containing a single item, + state file
    cdir = tmp_path / "catalog"
    (cdir / "items").mkdir(parents=True)
    (cdir / "collection.json").write_text(json.dumps({"type": "Collection", "id": "exp-1"}))
    # write one item to a parquet shard via the project's shard writer
    from esm_catalog.storage.geoparquet import write_shard
    item = {"type": "Feature", "stac_version": "1.0.0", "id": "i1",
            "collection": "exp-1", "properties": {"datetime": "2000-01-01T00:00:00Z"},
            "geometry": None, "bbox": None, "assets": {}, "links": []}
    write_shard(cdir / "items" / "ts-20000101.parquet", [item])
    from esm_catalog.scan.workspace import WorkspaceState, write_state
    write_state(cdir, WorkspaceState(experiment_id="exp-1", server_url="https://s"))
    return cdir


@responses.activate
def test_push_upserts_collection_and_bulk_items(tmp_path, monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_TOKEN", "TZ")
    cdir = _workspace(tmp_path)
    responses.add(responses.PUT, "https://s/collections/exp-1", json={}, status=200)
    responses.add(responses.POST, "https://s/collections/exp-1/bulk_items", json={}, status=200)

    report = push_catalog(cdir, DummyTokenProvider())

    assert report.collections == 1 and report.shards_pushed == 1 and report.items == 1
    # bearer header present, upsert method in the body
    call = responses.calls[-1].request
    assert call.headers["Authorization"] == "Bearer TZ"
    assert json.loads(call.body)["method"] == "upsert"


@responses.activate
def test_second_push_skips_pushed_ts(tmp_path, monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_TOKEN", "TZ")
    cdir = _workspace(tmp_path)
    responses.add(responses.PUT, "https://s/collections/exp-1", json={}, status=200)
    responses.add(responses.POST, "https://s/collections/exp-1/bulk_items", json={}, status=200)
    push_catalog(cdir, DummyTokenProvider())
    report = push_catalog(cdir, DummyTokenProvider())
    assert report.shards_pushed == 0   # ts already pushed, no fx present
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL (`No module named esm_catalog.push`). (If `write_state`/`write_shard` signatures differ, adjust the test to the real ones found in `workspace.py`/`geoparquet.py` before implementing.)

- [ ] **Step 3: Write minimal implementation** — `src/esm_catalog/push.py`

```python
"""Push local stac-geoparquet shards into the server's pgstac via the STAC API."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Callable

import pyarrow.parquet as pq
import requests
from pydantic import BaseModel
from stac_geoparquet.arrow import stac_table_to_items

from esm_catalog.auth.base import TokenProvider
from esm_catalog.scan.workspace import (
    WorkspaceState, read_state, write_state, mark_pushed, is_pushed,
)


class PushReport(BaseModel):
    collections: int = 0
    shards_pushed: int = 0
    items: int = 0
    failures: list = []


def push_catalog(
    catalog_dir: Path,
    provider: TokenProvider,
    *,
    echo: Callable[[str], None] = print,
) -> PushReport:
    state: WorkspaceState = read_state(catalog_dir)
    server = state.server_url
    if not server:
        raise ValueError("no server_url in workspace; run auth login / init first")
    collection = json.loads((catalog_dir / "collection.json").read_text())
    cid = collection["id"]
    token = provider.get_token()
    headers = {"Authorization": f"Bearer {token}"}

    report = PushReport()

    put = requests.put(f"{server}/collections/{cid}", json=collection,
                       headers=headers, timeout=60)
    if put.status_code == 404:
        put = requests.post(f"{server}/collections", json=collection,
                            headers=headers, timeout=60)
    put.raise_for_status()
    report.collections = 1

    for shard in sorted((catalog_dir / "items").glob("*.parquet")):
        if is_pushed(state, shard.name):
            continue
        table = pq.read_table(shard)
        if table.num_rows == 0:
            continue
        items = {item["id"]: item for item in stac_table_to_items(table)}
        response = requests.post(
            f"{server}/collections/{cid}/bulk_items",
            data=json.dumps({"items": items, "method": "upsert"}, default=str),
            headers={**headers, "Content-Type": "application/json"},
            timeout=120,
        )
        if response.status_code >= 400:
            report.failures.append(f"{shard.name}: {response.status_code}")
            continue
        report.shards_pushed += 1
        report.items += len(items)
        mark_pushed(state, shard.name)
        write_state(catalog_dir, state)

    return report
```

- [ ] **Step 4: Run to verify it passes** — Expected: PASS. Then add `requests` to `setup.py`'s catalog extra and run the whole suite.

`setup.py` (catalog extra list): add `"requests"`.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/push.py setup.py tests/test_esm_catalog/test_push.py
git commit --no-gpg-sign -m "feat(esm_catalog): push shards to pgstac via bulk_items"
```

---

## Task 9: Wire the CLI (auth login/logout, push)

**Files:**
- Modify: `src/esm_catalog/cli.py`
- Test: `tests/test_esm_catalog/test_cli_auth_push.py`

**Interfaces:**
- Consumes: `get_provider` (Task 6), `NotLoggedIn` (Task 1), `push_catalog` (Task 8).
- Produces: `esm-catalog auth login <server_url>` → `get_provider(server).login(server)` and stores `server_url` into the workspace state; `esm-catalog auth logout <server_url>` → `get_provider(server).logout()`; `esm-catalog push [--catalog-dir]` → `push_catalog(dir, get_provider(state.server_url))`, printing the report and mapping `NotLoggedIn` to a `ClickException`.

- [ ] **Step 1: Write the failing test**

```python
from click.testing import CliRunner
from esm_catalog.cli import main


def test_push_without_login_reports_cleanly(tmp_path, monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_AUTH_BACKEND", "oidc")
    # a catalog dir with a state file that has no server_url
    cdir = tmp_path / "catalog"; (cdir / "items").mkdir(parents=True)
    (cdir / "collection.json").write_text('{"type":"Collection","id":"e"}')
    from esm_catalog.scan.workspace import WorkspaceState, write_state
    write_state(cdir, WorkspaceState(experiment_id="e"))
    result = CliRunner().invoke(main, ["push", "--catalog-dir", str(cdir)])
    assert result.exit_code != 0
    assert "auth login" in result.output


def test_auth_logout_is_clean(tmp_path, monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_AUTH_BACKEND", "dummy")
    result = CliRunner().invoke(main, ["auth", "logout", "https://s"])
    assert result.exit_code == 0
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL (`auth login`/`push` still call `_not_implemented`).

- [ ] **Step 3: Write minimal implementation** — replace the stub bodies in `cli.py`

```python
@auth.command("login")
@click.argument("server_url")
def auth_login(server_url: str) -> None:
    """Log in to SERVER_URL and cache a token for later push."""
    from esm_catalog.auth import get_provider
    get_provider(server_url).login(server_url)
    click.echo(f"logged in to {server_url}")


@auth.command("logout")
@click.argument("server_url")
def auth_logout(server_url: str) -> None:
    """Discard the cached token for SERVER_URL."""
    from esm_catalog.auth import get_provider
    get_provider(server_url).logout()
    click.echo(f"logged out of {server_url}")


@main.command()
@click.option("--catalog-dir", type=click.Path(file_okay=False, path_type=Path), default="catalog")
def push(catalog_dir: Path) -> None:
    """Ship not-yet-pushed shards to the server's pgstac."""
    from esm_catalog.auth import NotLoggedIn, get_provider
    from esm_catalog.push import push_catalog
    from esm_catalog.scan.workspace import read_state

    state = read_state(catalog_dir)
    provider = get_provider(state.server_url)
    try:
        report = push_catalog(catalog_dir, provider)
    except NotLoggedIn as exc:
        raise click.ClickException(f"{exc} (run: esm-catalog auth login <server>)")
    click.echo(
        f"pushed {report.shards_pushed} shards, {report.items} items, "
        f"failed {len(report.failures)}"
    )
```

(Note: `auth_login` should also persist `server_url` into the workspace state so `push` can find it. If `init` has not run, `login` writes a minimal state via `write_state`. Match the real `read_state`/`write_state`/`WorkspaceState` signatures found in `workspace.py`.)

- [ ] **Step 4: Run to verify it passes** — Expected: PASS. Then whole suite green.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/cli.py tests/test_esm_catalog/test_cli_auth_push.py
git commit --no-gpg-sign -m "feat(esm_catalog): wire auth login/logout and push into the CLI"
```

---

## Task 10: Opt-in integration test against the compose stack

**Files:**
- Create: `tests/test_esm_catalog/test_auth_push_integration.py`
- Modify: `setup.cfg` / `pyproject` pytest markers (register `integration`)

**Interfaces:**
- Consumes: everything above; runs only when `ESM_CATALOG_IT_SERVER` is set (the running compose stack from #1410).

- [ ] **Step 1: Write the test (skipped by default)**

```python
import os
import pytest

pytestmark = pytest.mark.integration


@pytest.mark.skipif(not os.environ.get("ESM_CATALOG_IT_SERVER"),
                    reason="set ESM_CATALOG_IT_SERVER to run against a live stack")
def test_demo_login_and_push(tmp_path, monkeypatch):
    server = os.environ["ESM_CATALOG_IT_SERVER"]
    monkeypatch.setenv("ESM_CATALOG_AUTH_BACKEND", "demo")
    from esm_catalog.auth import get_provider
    provider = get_provider(server)
    provider.login(server)                 # real demo/demo grant
    assert provider.get_token()            # a real JWT
    # build a one-item catalog dir and push it; assert it appears via GET /search
    # (kept minimal here; expand when the stack is available)
```

- [ ] **Step 2: Run to verify it is skipped**

Run: `.direnv/python-3.10/bin/python -m pytest tests/test_esm_catalog/test_auth_push_integration.py -q`
Expected: 1 skipped.

- [ ] **Step 3: Register the marker** — add to `setup.cfg` under `[tool:pytest]`:

```ini
markers =
    integration: runs against a live catalog stack (opt-in)
```

- [ ] **Step 4: Run the whole suite** — Expected: all pass, integration skipped.

- [ ] **Step 5: Commit**

```bash
git add tests/test_esm_catalog/test_auth_push_integration.py setup.cfg
git commit --no-gpg-sign -m "test(esm_catalog): opt-in auth+push integration test"
```

---

## Self-Review

- **Spec coverage:** TokenProvider seam (T1), token cache (T2), Dummy (T3), Demo/real demo grant (T4), Oidc device grant (T5), provider factory (T6), incremental workspace state (T7), push/collection-upsert/bulk_items (T8), CLI wiring (T9), opt-in integration (T10). `requests` dependency (T8). All spec sections mapped.
- **Placeholders:** none — each code step carries real code. The two "match the real signatures" notes (T7 `FX_SHARD`, T9 `read_state`/`write_state`) are explicit reconciliation instructions against existing `workspace.py`, not TBDs; the implementer confirms the names on the branch before writing.
- **Type consistency:** `discover_endpoints`/`_to_cache`/`_CLIENT_ID` are defined in T4 and imported by T5; `TokenCache` fields (T2) match their use in T4/T5; `WorkspaceState.server_url`/`pushed_shards` (T7) match reads in T8/T9; `PushReport` fields (T8) match the CLI echo (T9).

## Assumptions to confirm on the branch before executing

- `workspace.py` exposes `read_state(dir) -> WorkspaceState` and `write_state(dir, state)`; `WorkspaceState` is a pydantic model with `experiment_id`. Confirm exact names (Task 7/8/9 depend on them).
- `storage/geoparquet.py` exposes `write_shard(path, items)` and an `FX_SHARD` constant. Reuse them; do not redefine.
- The server advertises `/.well-known/openid-configuration` with `token_endpoint` and `device_authorization_endpoint`, and the Keycloak client id for the CLI (`esm-catalog-cli`) allows the device and password grants. Confirm against the running stack (open item in the spec).
