"""Client configuration for esm-catalog (server + OIDC), via pydantic-settings.

Resolution order (highest precedence first):

1. explicit keyword arguments (e.g. ``--server`` on the CLI),
2. environment variables prefixed ``ESM_CATALOG_`` (e.g. ``ESM_CATALOG_CLIENT_SECRET``),
3. the YAML config file at ``$XDG_CONFIG_HOME/esm-catalog/config.yaml``.

Only ``server`` and OIDC client credentials are needed to push. A minimal
``config.yaml`` looks like::

    server_url: https://stac-dev.dmawi.de
    oidc_discovery_url: https://login-dev.helmholtz.de/oauth2/.well-known/openid-configuration
    client_id: esm-catalog-dev
    client_secret: "…"          # or set ESM_CATALOG_CLIENT_SECRET
    redirect_uri: https://stac-dev.dmawi.de
"""

from __future__ import annotations

from typing import Optional, Type

from pydantic import SecretStr
from pydantic_settings import (
    BaseSettings,
    PydanticBaseSettingsSource,
    SettingsConfigDict,
    YamlConfigSettingsSource,
)

from esm_catalog.xdg import config_file

DEFAULT_SCOPES = "openid eduperson_entitlement offline_access"


class Settings(BaseSettings):
    """Everything the client needs to authenticate and reach the catalog."""

    model_config = SettingsConfigDict(
        env_prefix="ESM_CATALOG_",
        yaml_file=str(config_file()),
        extra="ignore",
    )

    #: STAC API root, e.g. ``https://stac-dev.dmawi.de``. The API lives under
    #: ``<server_url>/api`` (see :attr:`api_url`).
    server_url: Optional[str] = None

    #: OIDC discovery document URL (the identity provider, e.g. Helmholtz AAI).
    oidc_discovery_url: str = (
        "https://login-dev.helmholtz.de/oauth2/.well-known/openid-configuration"
    )

    #: OAuth client id registered with the IdP for this catalog.
    client_id: str = "esm-catalog-dev"

    #: OAuth client secret. Prefer ``ESM_CATALOG_CLIENT_SECRET`` over the file.
    client_secret: SecretStr = SecretStr("")

    #: Redirect URI registered with the IdP; the login code lands here.
    redirect_uri: str = "https://stac-dev.dmawi.de"

    #: Space-separated OAuth scopes; ``offline_access`` yields a refresh token.
    scopes: str = DEFAULT_SCOPES

    #: Verify the server's TLS certificate. Disable only for dev self-signed.
    verify_tls: bool = True

    @property
    def api_url(self) -> str:
        """The STAC API base (``<server_url>/api``), without a trailing slash."""
        if not self.server_url:
            raise ValueError(
                "No server configured. Pass --server, set ESM_CATALOG_SERVER_URL, "
                "or add server_url to the config file."
            )
        return self.server_url.rstrip("/") + "/api"

    @classmethod
    def settings_customise_sources(
        cls,
        settings_cls: Type[BaseSettings],
        init_settings: PydanticBaseSettingsSource,
        env_settings: PydanticBaseSettingsSource,
        dotenv_settings: PydanticBaseSettingsSource,
        file_secret_settings: PydanticBaseSettingsSource,
    ) -> tuple[PydanticBaseSettingsSource, ...]:
        """init > env > YAML file (drops the unused dotenv/secret-file sources)."""
        return (
            init_settings,
            env_settings,
            YamlConfigSettingsSource(settings_cls),
        )
