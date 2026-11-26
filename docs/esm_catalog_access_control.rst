esm_catalog: Logging In and Ownership
=====================================

Reading the catalogue needs no account. Writing to it — ``esm-catalog push``
— needs a login, and you can only write to experiments your group owns.

Logging in
----------

.. code-block:: bash

   esm-catalog auth login https://stac-dev.awi.de

   # on a login node with no browser: copy the printed URL to your laptop
   # on a workstation: let it open the browser for you
   esm-catalog auth login --open https://stac-dev.awi.de

You are sent to the Helmholtz AAI, log in with your institute account, and the
CLI receives a token. The token is cached at
``$XDG_STATE_HOME/esm-catalog/tokens/<host>.json`` (readable only by you) and
refreshed automatically, so one login per server lasts weeks. ``esm-catalog
auth logout`` throws it away.

To see whether you are logged in and for how long, look at the cache:

.. code-block:: bash

   $ python -c "import platformdirs; print(platformdirs.user_state_path('esm-catalog'))"
   /home/pgierz/.local/state/esm-catalog
   $ jq '{expires_at: (.expires_at | todate), scope}' \
       ~/.local/state/esm-catalog/tokens/stac-dev.awi.de.json
   {
     "expires_at": "2026-09-14T13:02:11Z",
     "scope": "openid eduperson_entitlement offline_access"
   }

The file is one JSON object per server, mode ``0600``; the refresh token in
it is what lets ``push`` renew the access token without another browser
trip.

The dev server now carries a real certificate (AWI's own, via HARICA/GEANT) —
``-k`` / ``--insecure`` is no longer needed.

.. TODO screencast: auth login on a login node, copying the URL to a browser

The login flow
--------------

.. mermaid::
   :align: center

   sequenceDiagram
       actor You
       participant CLI as esm-catalog auth login
       participant IDP as Helmholtz AAI
       participant TK as Token cache
       participant PX as Auth proxy
       participant API as STAC API

       You->>CLI: 1  run auth login
       CLI->>IDP: 2  login request
       IDP-->>You: 3  browser login
       IDP-->>CLI: 4  token
       CLI->>TK: 5  cache
       CLI->>PX: 6  push with token
       PX-->>IDP: 7  check groups
       PX->>API: 8  forward if your group owns the experiment

The CLI is a *public* OIDC client (``esm-catalog-dev``): there is no secret
in it, and login uses the authorization-code flow with PKCE, asking for the
``openid``, ``eduperson_entitlement`` and ``offline_access`` scopes — the
entitlement claim is where your group membership comes from. The server
address only labels which cache a token goes into; the identity provider is
configured separately (``oidc_discovery_url`` in the client config) and
defaults to the Helmholtz AAI development instance (``login-dev.helmholtz.de``).

Permissions by Action
----------------------

.. note::

   This table is the design intent, not what is enforced today.
   ``ENTITLEMENT_SCOPE_MAP`` (the proxy setting that would turn AAI group
   membership into write scopes) is still empty, so right now **any
   logged-in user can push, update, or delete any experiment** — there is no
   group check yet. Tracked in `esm_tools#1535
   <https://github.com/esm-tools/esm_tools/issues/1535>`_.

.. list-table::
   :header-rows: 1
   :widths: 40 60

   * - Action
     - Who
   * - Browse, search, read any experiment
     - anyone on the network
   * - Push a new experiment
     - any logged-in user; the experiment is stamped with your VO group
   * - Update an experiment (re-push, add run segments)
     - members of the owning group -- **not enforced yet, see the note above**
   * - Delete an experiment
     - members of the owning group, or the catalogue operator -- **not
       enforced yet, see the note above**
   * - Register new searchable fields (queryables)
     - the catalogue operator

Groups come from the Helmholtz AAI virtual organisation your account is in.
If ``push`` refuses with ``403``, you are logged in but not in the group that
owns that experiment — check with the person who first pushed it. A ``401``
means the token is missing or expired past its refresh window: run ``auth
login`` again.

.. code-block:: text

   $ esm-catalog push catalog/
   pushing pi-ctrl-001-5058f1af → https://stac-dev.awi.de
   403 Forbidden: collection 'pi-ctrl-001-5058f1af' is owned by group
   'urn:geant:helmholtz.de:group:awi-paleo' and you are not a member.
