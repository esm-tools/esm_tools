"""Base directories for esm-catalog, via platformdirs.

platformdirs honours ``XDG_CONFIG_HOME``/``XDG_STATE_HOME`` when they are set
(and the OS-native location otherwise), so the config file and token cache land
where the platform expects without us hand-rolling the spec.
"""

from __future__ import annotations

from pathlib import Path

from platformdirs import user_config_path, user_state_path

APP_NAME = "esm-catalog"


def config_dir() -> Path:
    """The app's config directory (``$XDG_CONFIG_HOME/esm-catalog`` on Linux)."""
    return user_config_path(APP_NAME)


def state_dir() -> Path:
    """The app's state directory (``$XDG_STATE_HOME/esm-catalog`` on Linux)."""
    return user_state_path(APP_NAME)


def config_file() -> Path:
    """The config file, ``<config-dir>/config.yaml``."""
    return config_dir() / "config.yaml"


def token_file() -> Path:
    """The cached token, ``<state-dir>/token.json`` (holds secret material)."""
    return state_dir() / "token.json"
