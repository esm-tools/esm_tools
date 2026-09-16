esm_catalog: How the Pieces Fit
===============================

The simulation catalogue is a STAC API with a small client around it. You
scan an experiment on the HPC side, push the result to the server, and from
then on anything that speaks STAC — the web browser, ``pystac-client``,
``xarray`` — can find your runs.

.. graphviz::
   :align: center

   digraph services {
       rankdir=LR;
       compound=true;
       fontname="Helvetica"; fontsize=10;
       node [shape=box, style="rounded,filled", fillcolor="#f7f7f7",
             fontname="Helvetica", fontsize=10];
       edge [fontname="Helvetica", fontsize=9];

       subgraph cluster_hpc {
           label="HPC / workstation (your side)";
           style="rounded"; color="#999999";
           EXP   [label="Experiment directory\n(finished_config, namelists, outdata)"];
           CLI   [label="esm-catalog CLI\nscan · push · status · auth", fillcolor="#e3eefc"];
           LOCAL [label="Local catalogue\n<exp>/catalog/items/*.parquet"];
           TOKEN [label="Token cache\n$XDG_STATE_HOME/esm-catalog/tokens", shape=note];
       }

       subgraph cluster_srv {
           label="Catalogue server (stac-dev.dmawi.de)";
           style="rounded"; color="#999999";
           PROXY [label="Auth proxy\n(who may write what)", fillcolor="#fde9d9"];
           API   [label="STAC API\n(stac-fastapi-pgstac)", fillcolor="#e3eefc"];
           PG    [label="PostgreSQL + pgSTAC", shape=cylinder, fillcolor="#eeeeee"];
           WEB   [label="STAC-browser\n(web viewer)", fillcolor="#e3eefc"];
       }

       IDP   [label="Helmholtz AAI\n(login)", style="rounded,dashed"];
       READ  [label="Your scripts\npystac-client · xarray · intake", style="rounded,dashed"];

       EXP   -> CLI   [label="scan"];
       CLI   -> LOCAL [label="writes shards"];
       LOCAL -> CLI   [label="push reads"];
       CLI   -> IDP   [label="auth login", style=dashed];
       IDP   -> TOKEN [style=dashed];
       TOKEN -> CLI   [style=dashed];
       CLI   -> PROXY [label="push"];
       PROXY -> API   [label="authorised\nwrites"];
       API   -> PG;
       WEB   -> API   [label="browse"];
       READ  -> API   [label="search\n(CQL2 filter)"];
   }

.. TODO screencast: scan → push → open in browser, end to end (~2 min)

The pieces
----------

.. list-table::
   :header-rows: 1
   :widths: 22 46 32

   * - Piece
     - What it does for you
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
       searches. Everything downstream talks to this.
     - ``https://stac-dev.dmawi.de``
   * - Auth proxy
     - Checks your login token on ``push`` and lets you write to the
       experiments your group owns. Reading needs no login.
     - in front of the API
   * - STAC-browser
     - Point-and-click view of the same API.
     - ``https://stac-dev.dmawi.de`` (see :doc:`esm_catalog_viewer`)
   * - Helmholtz AAI
     - Where ``auth login`` sends you. Your institute account works.
     - external

Quick start
-----------

.. code-block:: bash

   # 1. Install the client (once)
   pip install "esm-tools[catalog]"

   # 2. Scan a finished experiment. Writes <exp>/catalog/.
   cd /albedo/work/user/$USER/runs/pi-ctrl-001
   esm-catalog scan

   # 3. Log in (once per server; the token is cached and refreshed)
   esm-catalog auth login https://stac-dev.dmawi.de

   # 4. Push. Idempotent: re-running updates, never deletes.
   esm-catalog push catalog/

   # 5. See what you have locally and where push would send it
   esm-catalog status

Set the server once in ``$XDG_CONFIG_HOME/esm-catalog/config.yaml`` and drop
``--server`` afterwards:

.. code-block:: yaml

   server_url: https://stac-dev.dmawi.de

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
rest goes through.

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

   cat = Client.open("https://stac-dev.dmawi.de")

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

The experiment record
---------------------

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

What each component covers
--------------------------

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
