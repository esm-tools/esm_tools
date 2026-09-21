esm_catalog: Scan, Push, Browse
===============================

The simulation catalogue is a STAC API with a small client around it. You
scan an experiment on the HPC side, push the result to the server, and from
then on anything that speaks STAC — the web browser, ``pystac-client``,
``xarray`` — can find your runs.

Quick start
-----------

Install ``esm_tools`` with the ``catalog`` extra, which adds ``pystac``, the
STAC extension schemas, and the ``esm-catalog`` CLI:

.. code-block:: bash

   pip install "esm-tools[catalog]"

``cd`` into a finished experiment and scan it. ``scan`` reads the experiment
directory (``finished_config.yaml``, namelists, ``outdata/`` -- the tree is
below) and writes a local catalog at ``<exp>/catalog/``. Nothing leaves this
machine yet -- no network access, no login required:

.. code-block:: bash

   cd /albedo/work/user/$USER/runs/pi-ctrl-001
   esm-catalog scan

Log in, once per server. The one positional argument is the server URL;
``auth login`` sends you to the Helmholtz AAI login page (opens a browser,
or prints a URL to copy on a headless login node) and caches the token it
gets back. One login lasts weeks -- see :doc:`esm_catalog_access_control`:

.. code-block:: bash

   esm-catalog auth login https://stac-dev.awi.de

Push. The one positional argument is the local catalog directory ``scan``
just wrote. ``push`` is idempotent -- rerunning it updates existing records,
never deletes -- so it's safe to run again after every ``scan``:

.. code-block:: bash

   esm-catalog push catalog/

Check what you have. ``status`` makes no server contact -- it reports the
local ``catalog/``'s contents and where ``push`` would send them, which is
worth checking before an actual push:

.. code-block:: bash

   esm-catalog status

Set the server once in ``$XDG_CONFIG_HOME/esm-catalog/config.yaml`` and drop
``--server`` afterwards:

.. code-block:: yaml

   server_url: https://stac-dev.awi.de

The commands above cover two of three structural concerns: the CLI running
locally (``scan``, ``status``), and writing to the server (``push``). The
third -- reading from the server -- needs no CLI at all: the browser,
``pystac-client``, ``xarray`` query the API directly.

Your side — turning an experiment into a local catalogue:

.. mermaid::
   :align: center

   flowchart TB
       EXP["Experiment directory<br/>(finished_config, namelists, outdata)"]
       CLI["esm-catalog CLI<br/>scan · push · status"]
       LOCAL["Local catalogue<br/>&lt;exp&gt;/catalog/items/*.parquet"]

       CLI -->|scan reads| EXP
       CLI -->|scan writes| LOCAL
       CLI -->|push reads| LOCAL

       classDef highlight fill:#e3eefc,stroke:#999
       class CLI highlight

Writing to the server — ``push`` needs a login (see
:doc:`esm_catalog_access_control`):

.. mermaid::
   :align: center

   flowchart TB
       CLI["esm-catalog CLI"]
       PROXY["Auth proxy<br/>(who may write what)"]
       API["STAC API<br/>(stac-fastapi-pgstac)"]
       PG[("PostgreSQL + pgSTAC")]

       CLI -->|push, logged in| PROXY
       PROXY -->|authorised writes| API
       API --> PG

       classDef highlight fill:#e3eefc,stroke:#999
       classDef proxy fill:#fde9d9,stroke:#999
       classDef muted fill:#eeeeee,stroke:#999
       class CLI,API highlight
       class PROXY proxy
       class PG muted

Reading from the server — no login needed:

.. mermaid::
   :align: center

   flowchart TB
       API["STAC API<br/>(stac-fastapi-pgstac)"]
       WEB["STAC-browser<br/>(web viewer)"]
       READ["Your scripts<br/>pystac-client · xarray · intake"]

       WEB -->|browse| API
       READ -->|search, CQL2 filter| API

       classDef highlight fill:#e3eefc,stroke:#999
       class API,WEB highlight
       style READ stroke-dasharray: 5 5

The diagram above is structural (what talks to what); it doesn't show
*order*. This is the same workflow as a timeline — scan, then push, then
(any time later, by anyone) browse:

.. mermaid::
   :align: center

   sequenceDiagram
       actor You
       participant CLI as esm-catalog CLI
       participant EXP as Experiment directory
       participant LOCAL as Local catalogue
       participant API as STAC API
       participant Reader as Browser / scripts

       You->>CLI: scan
       CLI->>EXP: read finished_config, namelists, outdata
       CLI->>LOCAL: write shards

       You->>CLI: push
       CLI->>LOCAL: read shards
       CLI->>API: push (authenticated)
       API-->>CLI: ok

       Note over Reader,API: any time later, by anyone
       Reader->>API: search / browse
       API-->>Reader: results

"Experiment directory" above is a real ESM-Tools experiment tree (see
:ref:`esm_runscripts:Experiment Directory Structure` for the full structure —
every subfolder, not just the two ``scan`` reads):

.. mermaid::
   :align: center

   flowchart TB
       EXPID["&lt;expid&gt;/"]
       CONFIG["config/"]
       OUTDATA["outdata/"]
       CONFIG_ECHAM["echam/"]
       CONFIG_FESOM["fesom/"]
       OUTDATA_ECHAM["echam/"]
       OUTDATA_FESOM["fesom/"]
       RUN["run_YYYYMMDD-YYYYMMDD/"]
       TOP[".top_of_exp_tree"]

       FC["`**&lt;expid&gt;_finished_config.yaml**
       general.expid
       general.metadata
       general.paleo`"]
       NML_ECHAM["`**namelist.echam**
       runctl
       radctl`"]
       NML_FESOM["`**namelist.fesom**
       paths
       timestep`"]
       FILE["`**PI_185001.01_echam, ...**
       variable: t, q, vo, ...
       cube:dimensions, cube:variables`"]

       EXPID -.- TOP
       EXPID --- CONFIG
       EXPID --- OUTDATA
       EXPID -.- RUN
       CONFIG --- FC
       CONFIG --- CONFIG_ECHAM
       CONFIG --- CONFIG_FESOM
       CONFIG_ECHAM --- NML_ECHAM
       CONFIG_FESOM --- NML_FESOM
       OUTDATA --- OUTDATA_ECHAM
       OUTDATA --- OUTDATA_FESOM
       OUTDATA_ECHAM --- FILE

       classDef read fill:#e3eefc,stroke:#999
       classDef context fill:#eeeeee,stroke:#999
       class CONFIG,OUTDATA,CONFIG_ECHAM,CONFIG_FESOM,OUTDATA_ECHAM,OUTDATA_FESOM read
       class RUN,TOP context

Blue folders are what ``scan`` actually walks; grey (dashed edge) is shown
only for context. ``scan`` reads ``config/<expid>_finished_config.yaml`` for experiment
metadata, ``config/<component>/namelist.*`` for namelists, and walks
``outdata/<component>/`` for output files (see :doc:`esm_catalog_metadata`
for exactly what comes from where). ``.top_of_exp_tree`` marks the root
(``esm_runscripts`` writes it once at setup) but ``scan`` does not read it —
``--exp-root`` is given explicitly, or defaults to the current directory.
``run_YYYYMMDD-YYYYMMDD/`` (one per run, dashed above) is per-run staging;
the tidy phase moves its files into the folders above before ``scan`` ever
sees them, so ``scan`` never reads inside a run folder directly.

.. TODO screencast: scan → push → open in browser, end to end (~2 min)

Components
----------

.. list-table::
   :header-rows: 1
   :widths: 22 46 32

   * - Component
     - Role
     - Where
   * - ``esm-catalog`` CLI
     - Turns an experiment directory into STAC records and sends them to the
       server. Daily-workflow commands: ``auth login``/``logout``, ``scan``,
       ``push``, ``status``. ``list-plugins`` is a contributor/debugging
       command that lists registered extension plugins (see
       :doc:`esm_catalog_architecture`).
     - Any node that can see the experiment; ``pip install
       esm-tools[catalog]``
   * - Local catalogue
     - What ``scan`` writes: ``collection.json`` plus GeoParquet shards under
       ``items/``. Plain GeoParquet — pyarrow, DuckDB, and polars can read
       it without a server.
     - ``<experiment>/catalog/`` (change with ``--catalog-dir``)
   * - STAC API
     - Serves Collections (experiments) and Items (files) and answers
       searches. The browser, ``pystac-client``, and ``xarray`` all query
       this directly.
     - ``https://stac-dev.awi.de``
   * - Auth proxy
     - Checks your login token on ``push`` and lets you write to the
       experiments your group owns. Reading needs no login.
     - in front of the API
   * - STAC-browser
     - Point-and-click view of the same API.
     - ``https://stac-dev.awi.de`` (see :doc:`esm_catalog_viewer`)
   * - Helmholtz AAI
     - Where ``auth login`` sends you. Your institute account works.
     - external

Scanning a large experiment
---------------------------

``scan`` opens every output file to read its header, so on a big run you want
parallelism. By default it uses a local process pool; ``--jobs`` sets the
width. On a compute node with a Dask scheduler running, ``--distributed``
spreads the work across it:

.. code-block:: bash

   esm-catalog scan --jobs 32
   esm-catalog scan --distributed --scheduler tcp://10.0.0.5:8786

Scans are incremental: a second ``scan`` only reads files that are new or
changed since the last one (tracked in ``catalog/esm-catalog.json``). That
also means a change to the experiment's metadata or namelists is not
re-stamped onto files already catalogued; delete ``catalog/esm-catalog.json``
to force a full rescan. ``--strict`` makes a scan fail if any file could not
be read; otherwise failures are listed in ``catalog/failures.json`` and the
rest of the scan completes normally.

A remote experiment works too — ``scan`` and ``status`` accept an
``sftp://`` root:

.. code-block:: bash

   esm-catalog scan --exp-root sftp://albedo1/albedo/work/user/pgierz/runs/pi-ctrl-001

Finding your run from Python
----------------------------

Once pushed, use `pystac-client <https://pystac-client.readthedocs.io/>`_.
Filters use CQL2; namelist parameters are available as ``nml__`` fields (see
:doc:`esm_catalog_metadata`).

.. code-block:: python

   from pystac_client import Client

   cat = Client.open("https://stac-dev.awi.de")

   # All experiments
   for coll in cat.get_collections():
       print(coll.id, coll.title)

   # Monthly surface temperature from one experiment
   search = cat.search(
       collections=["pi-ctrl-001-3f9a1c2e"],
       filter={"op": "and", "args": [
           {"op": "=", "args": [{"property": "variable"}, "temp2"]},
           {"op": "=", "args": [{"property": "component"}, "echam"]},
       ]},
   )
   hrefs = [item.assets["data"].href for item in search.items()]

   # ...and open them
   import xarray as xr
   ds = xr.open_mfdataset(hrefs, combine="by_coords")

Reading the local shards without a server
-----------------------------------------

The shards are stac-geoparquet: every Item property is a column, so pyarrow,
DuckDB and polars can query them with no server and no esm_catalog import.
The snippets below run against a small demo experiment built and scanned by
``docs/_demo/demo_experiment.py`` (``make doctest`` executes them).

.. testsetup:: catalog

   from demo_experiment import build_demo
   catalog = build_demo() / "catalog"

.. testcode:: catalog

   import pyarrow.parquet as pq

   shard = next((catalog / "items").glob("*_stac_1850*.parquet"))
   table = pq.read_table(shard)
   print(table.num_rows, "items")
   print(table.select(["variable", "component", "frequency"]).slice(0, 2).to_pandas())

.. testoutput:: catalog

   158 items
     variable component frequency
   0      var     echam       mon
   1      var     echam       mon

DuckDB does the same from the shell or Python. The time-invariant ``fx``
shard has a narrower schema (it is empty when an experiment has no fx
files), so read the directory with ``union_by_name``:

.. testcode:: catalog

   import duckdb

   print(duckdb.sql(f"""
       SELECT component, count(*) AS files
       FROM read_parquet('{catalog}/items/*.parquet', union_by_name = true)
       GROUP BY 1 ORDER BY 1
   """))

.. testoutput:: catalog
   :options: +NORMALIZE_WHITESPACE

   ┌───────────┬───────┐
   │ component │ files │
   │  varchar  │ int64 │
   ├───────────┼───────┤
   │ echam     │    96 │
   │ fesom     │     2 │
   │ jsbach    │    28 │
   │ oasis3mct │    32 │
   └───────────┴───────┘

The experiment record (``collection.json``)
---------------------------------------------

``collection.json`` is the experiment as the server will see it — plain STAC,
so ``json`` or ``pystac`` read it:

.. testcode:: catalog

   import json

   coll = json.loads((catalog / "collection.json").read_text())
   print(coll["id"], "|", coll["license"])
   print([c["name"] for c in coll["contacts"]])
   print(coll["extent"]["temporal"]["interval"][0])

.. testoutput:: catalog
   :options: +ELLIPSIS

   pi-ctrl-001-... | CC-BY-4.0
   ['Jane Modeller', 'Karl Klima']
   ['1850-01-16T00:00:00Z', '1851-02-16T00:00:00Z']

The trailing eight characters of the id are the hash of the experiment path;
they stay stable as long as the experiment does not move.

Date range and file count per component
----------------------------------------

.. testcode:: catalog

   print(duckdb.sql(f"""
       SELECT component,
              min(start_datetime)::date AS first,
              max(end_datetime)::date   AS last,
              count(*)                  AS files
       FROM read_parquet('{catalog}/items/*.parquet', union_by_name = true)
       GROUP BY 1 ORDER BY 1
   """))

.. testoutput:: catalog
   :options: +NORMALIZE_WHITESPACE

   ┌───────────┬────────────┬────────────┬───────┐
   │ component │   first    │    last    │ files │
   │  varchar  │    date    │    date    │ int64 │
   ├───────────┼────────────┼────────────┼───────┤
   │ echam     │ 1850-01-16 │ 1851-02-16 │    96 │
   │ fesom     │ 1850-01-16 │ 1851-01-16 │     2 │
   │ jsbach    │ 1850-01-16 │ 1851-02-16 │    28 │
   │ oasis3mct │ 1851-01-16 │ 1851-01-16 │    32 │
   └───────────┴────────────┴────────────┴───────┘

From the catalogue to xarray
----------------------------

The point of the exercise: pick files by what they are, not by where they
sit. Filter the shard, take the ``data`` asset hrefs, open them. Hrefs are
URLs — ``file://`` for a local scan, ``sftp://`` for a remote one — so
strip the scheme before handing them to a netCDF library:

.. testcode:: catalog

   import pyarrow.compute as pc
   import xarray as xr
   from urllib.parse import urlparse

   fesom = table.filter(pc.equal(table["component"], "fesom"))
   hrefs = [row["assets"]["data"]["href"] for row in fesom.to_pylist()]
   print(hrefs[0])

   paths = [urlparse(h).path for h in hrefs]
   ds = xr.open_mfdataset(paths, combine="by_coords")
   print(dict(ds.sizes))

.. testoutput:: catalog
   :options: +ELLIPSIS

   file:///.../pi-ctrl-001/outdata/fesom/pi-ctrl-001.fesom.1850.nc
   {'time': 2, 'lat': 4, 'lon': 5}

Against the server the same three steps are ``cat.search(...)``,
``item.assets["data"].href``, ``xr.open_mfdataset`` — see *Finding your run
from Python* above.

Real STAC objects, if you want them
-----------------------------------

The shards convert back into ``pystac`` objects with ``stac_geoparquet``,
which is what ``push`` does on the way to the server:

.. testcode:: catalog

   import pystac
   import stac_geoparquet.arrow as sga

   first = next(sga.stac_table_to_items(table.slice(0, 1)))
   item = pystac.Item.from_dict(first)
   print(item.id, "->", item.collection_id)
   print(item.properties["variable"], item.properties["component"], item.properties["frequency"])

   coll = pystac.Collection.from_file(str(catalog / "collection.json"))
   print(coll.extra_fields["nml:groups"])

.. testoutput:: catalog
   :options: +ELLIPSIS

   var.echam.185001... -> pi-ctrl-001-...
   var echam mon
   ['paths', 'radctl', 'runctl', 'timestep']
