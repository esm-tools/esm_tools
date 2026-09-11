"""curl-style ``--resolve HOST:PORT:IP`` DNS overrides for the STAC client.

Pre-seeds a hostname's DNS lookup with a caller-supplied IP, skipping the
actual query -- the same trick as curl's own ``--resolve`` flag. Everything
downstream (TLS SNI, certificate hostname check, the ``Host`` header) still
sees the original hostname; only the socket-level connect target changes.
That distinction matters: a straight URL-rewrite to the IP would also change
SNI and break certificate validation against a cert issued for the hostname.

Exists because DNS itself can be broken independently of the server being
reachable (a malformed CNAME record, a resolver that hasn't caught up yet) --
this lets a push proceed anyway once the operator knows the real IP.
"""

from __future__ import annotations

import ssl
from typing import Optional

import httpcore
import httpx
from httpx._config import create_ssl_context

#: One caller-supplied override: (host, port) -> IP.
ResolveMap = dict[tuple[str, int], str]


def parse_resolve(spec: str) -> tuple[str, int, str]:
    """Parse one ``--resolve`` value, curl's own ``HOST:PORT:IP`` syntax.

    Parameters
    ----------
    spec : str
        E.g. ``"stac-dev.dmawi.de:443:134.1.7.56"``.

    Returns
    -------
    tuple of (str, int, str)
        ``(host, port, ip)``.

    Raises
    ------
    ValueError
        If *spec* isn't exactly three colon-separated fields, or the port
        isn't a valid integer.
    """
    parts = spec.split(":")
    if len(parts) != 3:
        raise ValueError(f"--resolve expects HOST:PORT:IP, got {spec!r}")
    host, port_str, ip = parts
    try:
        port = int(port_str)
    except ValueError:
        raise ValueError(f"--resolve port must be an integer, got {port_str!r} in {spec!r}") from None
    return host, port, ip


class _ResolvingNetworkBackend(httpcore.NetworkBackend):
    """Wraps a real backend, substituting the connect target per *overrides*.

    Only ``connect_tcp`` is overridden -- the (host, port) actually dialed
    changes; the caller (httpx/httpcore) still believes it's talking to the
    original host for every other purpose (SNI, the ``Host`` header).
    """

    def __init__(self, overrides: ResolveMap) -> None:
        self._overrides = overrides
        # NetworkBackend itself is the abstract interface (connect_tcp raises
        # NotImplementedError) -- SyncBackend is httpcore's real socket
        # implementation, the same one httpx uses when no override is given.
        self._backend = httpcore.SyncBackend()

    def connect_tcp(
        self,
        host: str,
        port: int,
        timeout: Optional[float] = None,
        local_address: Optional[str] = None,
        socket_options=None,
    ) -> httpcore.NetworkStream:
        target = self._overrides.get((host, port), host)
        return self._backend.connect_tcp(
            target,
            port,
            timeout=timeout,
            local_address=local_address,
            socket_options=socket_options,
        )


class ResolvingTransport(httpx.HTTPTransport):
    """``httpx.HTTPTransport`` with DNS overrides -- see :func:`parse_resolve`.

    ``httpx.HTTPTransport.__init__`` builds its own ``httpcore.ConnectionPool``
    and doesn't expose httpcore's ``network_backend`` parameter, so this
    reimplements that one constructor call with it added; :meth:`handle_request`
    and everything else is inherited unchanged from ``httpx.HTTPTransport``,
    which only ever calls through ``self._pool``.
    """

    def __init__(
        self,
        overrides: ResolveMap,
        verify_tls: bool = True,
    ) -> None:
        ssl_context: ssl.SSLContext = create_ssl_context(verify=verify_tls)
        self._pool = httpcore.ConnectionPool(
            ssl_context=ssl_context,
            network_backend=_ResolvingNetworkBackend(overrides),
        )
