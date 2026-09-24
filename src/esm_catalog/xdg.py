"""Base directories for esm-catalog, via platformdirs.

platformdirs honours ``XDG_CONFIG_HOME``/``XDG_STATE_HOME`` when they are set
(and the OS-native location otherwise), so the config file and token cache land
where the platform expects without us hand-rolling the spec.
"""

from __future__ import annotations

import re
from pathlib import Path
from urllib.parse import urlsplit

from platformdirs import user_config_path, user_state_path

APP_NAME = "esm-catalog"

_UNSAFE_HOST_CHARS = re.compile(r"[^A-Za-z0-9.-]")


def config_dir() -> Path:
    """The app's config directory (``$XDG_CONFIG_HOME/esm-catalog`` on Linux)."""
    return user_config_path(APP_NAME)


def state_dir() -> Path:
    """The app's state directory (``$XDG_STATE_HOME/esm-catalog`` on Linux)."""
    return user_state_path(APP_NAME)


def config_file() -> Path:
    """The config file, ``<config-dir>/config.yaml``."""
    return config_dir() / "config.yaml"


def token_file(server_url: str) -> Path:
    """The cached token for *server_url* (``<state-dir>/tokens/<host>.json``).

    Keyed per server (not a single global file): a STAC server and its identity
    provider are separate systems, so logging into a second server must not
    silently overwrite the first's token. Every character but the hostname's own
    ``[A-Za-z0-9.-]`` is stripped, so an unparsable *server_url* still yields a
    stable, filesystem-safe (if ugly) name rather than raising.
    """
    host = urlsplit(server_url).netloc or server_url
    slug = _UNSAFE_HOST_CHARS.sub("_", host) or "unknown"
    return state_dir() / "tokens" / f"{slug}.json"
