"""XDG base-directory resolution for esm-catalog.

Faithful to the `XDG Base Directory spec
<https://specifications.freedesktop.org/basedir-spec/latest/>`_: honour the
``XDG_*_HOME`` environment variables when set, else fall back to the spec's
defaults under ``$HOME``. Used for the config file and the token cache.
"""

from __future__ import annotations

import os
from pathlib import Path

APP_NAME = "esm-catalog"


def _xdg_home(var: str, default: Path) -> Path:
    """Return ``$var`` as a path if set and absolute, else *default*."""
    value = os.environ.get(var)
    if value and os.path.isabs(value):
        return Path(value)
    return default


def config_home() -> Path:
    """``$XDG_CONFIG_HOME`` or ``~/.config``."""
    return _xdg_home("XDG_CONFIG_HOME", Path.home() / ".config")


def state_home() -> Path:
    """``$XDG_STATE_HOME`` or ``~/.local/state``."""
    return _xdg_home("XDG_STATE_HOME", Path.home() / ".local" / "state")


def config_dir() -> Path:
    """The app's config directory, ``<config-home>/esm-catalog``."""
    return config_home() / APP_NAME


def state_dir() -> Path:
    """The app's state directory, ``<state-home>/esm-catalog``."""
    return state_home() / APP_NAME


def config_file() -> Path:
    """The config file, ``<config-dir>/config.yaml``."""
    return config_dir() / "config.yaml"


def token_file() -> Path:
    """The cached token, ``<state-dir>/token.json`` (holds secret material)."""
    return state_dir() / "token.json"
