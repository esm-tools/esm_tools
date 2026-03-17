ESM Catalog + Viz Platform: Architecture
========================================

Overview
--------

The ESM Catalog + Viz platform is a STAC-based (SpatioTemporal Asset
Catalog) system for browsing, searching, and visualizing climate model
output from ESM-Tools experiments. It provides a unified interface over
per-experiment DuckDB catalogs, exposing them through a standard STAC
API that any conforming browser or client can query.

The platform consists of three cooperating services: a STAC Catalog API
that federates multiple DuckDB catalog files into a single searchable
collection, a Visualization Server that generates both static previews
and live interactive plots from the referenced NetCDF and GRIB data, and
a Vue 3 browser frontend (STAC Browser) that presents the catalog to
users and embeds visualization panels inline. The services are deployed
independently and communicate over HTTP, which means the catalog and viz
servers can run on separate compute nodes or even different clusters.

Scientific datasets receive first-class treatment throughout. The viz
server automatically detects FESOM unstructured mesh data and routes it
to a specialized GeoViews/Datashader rendering pipeline. Time averaging
across multi-file collections uses xarray DataTree to organize
variables, and Dask distributed compute clusters can be registered at
runtime to offload heavy computations. Paleo climate simulations are
supported via a built-in preset registry (LGM, Mid-Holocene, Eemian, and
others) that maps human-readable time period names to STAC datetime
filters.

Architecture Diagram
--------------------

::

     Browser / STAC Browser (Vue 3 + Bootstrap Vue Next)
     ┌─────────────────────────────────────────────────┐
     │  SearchFilter   PersonalCollections  Compare UI  │
     │  DataPreview    CollectionComparison             │
     └──────────┬───────────────────┬──────────────────┘
                │  STAC HTTP        │  Viz HTTP
                │                   │
     ┌──────────▼──────┐   ┌────────▼───────────────────┐
     │  STAC Catalog   │   │  Visualization Server       │
     │  API            │   │  (FastAPI + Panel)          │
     │  (FastAPI +     │   │                             │
     │  stac-fastapi)  │   │  /preview/{id}.png  ──►  matplotlib/
     │                 │   │  /preview/{id}.json         │  Cartopy
     │  DuckDB         │   │  /preview/{id}/panel ──►  Panel/
     │  CatalogPool    │   │  /_panel (Bokeh WS)        │  HoloViews/
     │  CatalogRegistry│   │                             │  GeoViews/
     │  PersonalStore  │   │  /compute/clusters ──►   Dask
     │                 │   │                          (local/SLURM/
     │  *.duckdb files │   │  STAC item fetch ──►  Gateway)
     │  (per-experiment│   │  xarray.DataTree
     │   on shared FS) │   │  FESOM mesh files
     └─────────────────┘   └────────────────────────────┘

Components
----------

STAC Catalog API
~~~~~~~~~~~~~~~~

**Technology:** FastAPI, stac-fastapi, DuckDB

The catalog API is built on ``stac-fastapi`` with a custom
``DuckDBCatalogClient`` backend. Each ESM-Tools experiment produces a
``catalog.duckdb`` file that is registered with the running server via
the catalog management routes or loaded at startup via the
``ESM_CATALOG_DB`` environment variable. Multiple catalog files are
federated transparently behind a single STAC API endpoint.

Key internal components:

- ``CatalogRegistry`` – tracks the set of registered ``.duckdb`` paths,
  optionally persisting them to a JSON file so registrations survive
  server restarts.
- ``CatalogPool`` – maintains a pool of DuckDB connections, one per
  catalog file, reused across requests.
- ``CollectionCache`` / ``QueryablesCache`` – in-memory TTL caches
  (5-minute default) for collection lists and CQL2 queryable schemas,
  avoiding repeated full-table scans.
- ``PersonalCollectionStore`` – a separate DuckDB database
  (``personal.duckdb``) storing user-curated item collections with RBAC.

The API implements the STAC Filter Extension (CQL2-JSON) so that STAC
Browser’s “Additional Filtering” panel populates
variable/model/experiment dropdowns from the live catalog rather than
requiring free-text entry.

Visualization Server
~~~~~~~~~~~~~~~~~~~~

**Technology:** FastAPI, Panel, HoloViews, GeoViews, Datashader, xarray,
Cartopy, matplotlib

The viz server exposes REST endpoints for static previews and metadata,
and mounts a live Panel application at ``/_panel`` using
``panel.io.fastapi.add_application``. The Panel app is created per Bokeh
session (each browser tab gets its own app instance) by reading URL
query parameters from the Bokeh session context.

Three Panel app modes are available:

- **Item preview** – opens a single NetCDF/GRIB file, detects if it is
  FESOM unstructured data, and renders either a generic interactive plot
  or a GeoViews TriMesh rasterized with Datashader.
- **Collection preview** – fetches all items from a STAC collection,
  builds an ``xr.DataTree`` grouped by variable name (using
  ``open_mfdataset`` along the time dimension with Dask lazy loading),
  and presents controls for time averaging, anomaly computation, level
  selection, and colormap choice.
- **Comparison view** – loads DataTrees for two collections
  simultaneously and presents side-by-side panels plus a difference
  panel (B - A), with independent time range selectors per collection.

FESOM mesh coordinates are read from the standard ``nod2d.out`` and
``elem2d.out`` files whose path is embedded in STAC item properties
under ``nml:fesom:paths:meshpath``. Euler rotation parameters
(``nml:fesom:geometry:alphaeuler``, ``betaeuler``, ``gammaeuler``) are
applied when ``force_rotation`` is true. Mesh data is LRU-cached (up to
8 meshes) so repeated requests for the same experiment do not re-read
the mesh files from disk.

Compute-heavy operations (time averages, anomaly diffs across hundreds
of files) automatically use a registered Dask ``distributed.Client``
when one is present. The viz code calls ``da.compute()`` on all data
arrays; Dask routes work to the cluster automatically without any
explicit routing in the plotting code.

STAC Browser
~~~~~~~~~~~~

**Technology:** Vue 3, Bootstrap Vue Next, Leaflet/OpenLayers, Axios

The frontend is a customized STAC Browser with additional components
specific to this platform:

- ``DataPreview.vue`` – embeds the viz server’s static PNG previews and
  links to interactive Panel sessions. Detects dataset type to choose
  the right visualization path.
- ``PersonalCollections.vue`` – renders the user’s personal collection
  tree in the sidebar. Communicates with the personal collections REST
  API for CRUD operations.
- ``CollectionComparison.vue`` – provides UI for selecting two
  collections to compare and launches the comparison Panel application.
- ``SearchFilter.vue`` – adds climate-specific quick filters (variable,
  model, experiment, paleo period) on top of standard STAC
  spatial/temporal filtering.

API Reference
-------------

STAC Catalog API (default port 8000)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Standard STAC Endpoints
^^^^^^^^^^^^^^^^^^^^^^^

+--------+--------------------------------------------------+------------------------------+
| Method | Path                                             | Description                  |
+========+==================================================+==============================+
| GET    | ``/``                                            | Landing page with            |
|        |                                                  | conformance links            |
+--------+--------------------------------------------------+------------------------------+
| GET    | ``/conformance``                                 | OGC API conformance classes  |
+--------+--------------------------------------------------+------------------------------+
| GET    | ``/collections``                                 | List all registered          |
|        |                                                  | collections                  |
+--------+--------------------------------------------------+------------------------------+
| GET    | ``/collections/{collection_id}``                 | Collection metadata          |
+--------+--------------------------------------------------+------------------------------+
| GET    | ``/collections/{collection_id}/items``           | Paginated item list          |
+--------+--------------------------------------------------+------------------------------+
| GET    | ``/collections/{collection_id}/items/{item_id}`` | Single item                  |
+--------+--------------------------------------------------+------------------------------+
| GET    | ``/search``                                      | Item search (GET form)       |
+--------+--------------------------------------------------+------------------------------+
| POST   | ``/search``                                      | Item search with CQL2-JSON   |
|        |                                                  | filter body                  |
+--------+--------------------------------------------------+------------------------------+

Filter Extension
^^^^^^^^^^^^^^^^

+--------------------+---------------------------------------------+---------------------------------+
| Method             | Path                                        | Description                     |
+====================+=============================================+=================================+
| GET                | ``/queryables``                             | CQL2 queryable properties for   |
|                    |                                             | all catalogs                    |
+--------------------+---------------------------------------------+---------------------------------+
| GET                | ``/collections/{collection_id}/queryables`` | Queryables scoped to one        |
|                    |                                             | collection                      |
+--------------------+---------------------------------------------+---------------------------------+
| POST               | ``/format``                                 | OGC format-negotiation probe    |
|                    |                                             | (returns 200)                   |
+--------------------+---------------------------------------------+---------------------------------+

Catalog Management
^^^^^^^^^^^^^^^^^^

====== ==================== =======================================
Method Path                 Description
====== ==================== =======================================
POST   ``/catalogs``        Register a new ``.duckdb`` catalog file
GET    ``/catalogs``        List registered catalog paths
DELETE ``/catalogs/{path}`` Deregister a catalog
====== ==================== =======================================

Personal Collections
^^^^^^^^^^^^^^^^^^^^

+--------------------+----------------------------------------------------------+---------------------------------+
| Method             | Path                                                     | Description                     |
+====================+==========================================================+=================================+
| POST               | ``/users/{username}/collections``                        | Create a personal collection    |
+--------------------+----------------------------------------------------------+---------------------------------+
| GET                | ``/users/{username}/collections``                        | List visible collections        |
+--------------------+----------------------------------------------------------+---------------------------------+
| GET                | ``/users/{username}/collections/{id}``                   | Collection detail including     |
|                    |                                                          | item IDs                        |
+--------------------+----------------------------------------------------------+---------------------------------+
| PATCH              | ``/users/{username}/collections/{id}``                   | Update name / description /     |
|                    |                                                          | parent                          |
+--------------------+----------------------------------------------------------+---------------------------------+
| DELETE             | ``/users/{username}/collections/{id}``                   | Delete collection               |
+--------------------+----------------------------------------------------------+---------------------------------+
| POST               | ``/users/{username}/collections/{id}/items``             | Add catalog items               |
+--------------------+----------------------------------------------------------+---------------------------------+
| DELETE             | ``/users/{username}/collections/{id}/items/{item_id}``   | Remove an item                  |
+--------------------+----------------------------------------------------------+---------------------------------+
| POST               | ``/users/{username}/collections/{id}/shares``            | Grant access to another user    |
+--------------------+----------------------------------------------------------+---------------------------------+
| GET                | ``/users/{username}/collections/{id}/shares``            | List share grants               |
+--------------------+----------------------------------------------------------+---------------------------------+
| DELETE             | ``/users/{username}/collections/{id}/shares/{share_id}`` | Revoke a share                  |
+--------------------+----------------------------------------------------------+---------------------------------+
| GET                | ``/users/{username}/labels``                             | List user labels                |
+--------------------+----------------------------------------------------------+---------------------------------+
| POST               | ``/users/{username}/labels``                             | Create a label                  |
+--------------------+----------------------------------------------------------+---------------------------------+
| DELETE             | ``/users/{username}/labels/{label_id}``                  | Delete a label                  |
+--------------------+----------------------------------------------------------+---------------------------------+
| GET                | ``/users/{username}/tree``                               | Get collection tree (nested)    |
+--------------------+----------------------------------------------------------+---------------------------------+
| PATCH              | ``/users/{username}/tree``                               | Move / reorder a tree node      |
+--------------------+----------------------------------------------------------+---------------------------------+

Climate Science Extras
^^^^^^^^^^^^^^^^^^^^^^

====== ============================== ==================================
Method Path                           Description
====== ============================== ==================================
GET    ``/paleo-presets``             List all paleo time period presets
POST   ``/paleo-presets``             Add a user-defined preset
DELETE ``/paleo-presets/{preset_id}`` Delete a user-added preset
====== ============================== ==================================

System
^^^^^^

+--------------------+----------------+---------------------------------+
| Method             | Path           | Description                     |
+====================+================+=================================+
| GET                | ``/health``    | Liveness probe; returns catalog |
|                    |                | count and pool size             |
+--------------------+----------------+---------------------------------+
| GET                | ``/readiness`` | Kubernetes readiness probe;     |
|                    |                | checks catalog files are        |
|                    |                | accessible                      |
+--------------------+----------------+---------------------------------+

Visualization Server (default port 8001)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------+-----------------------------------------------+---------------------------------+
| Method             | Path                                          | Description                     |
+====================+===============================================+=================================+
| GET                | ``/``                                         | Root with endpoint index        |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/health``                                   | Liveness probe                  |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/preview/{item_id}.png``                    | Static PNG render; query        |
|                    |                                               | params: ``var``, ``stac_api``,  |
|                    |                                               | ``time``, ``level``, ``cmap``,  |
|                    |                                               | ``collection_id``               |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/preview/{item_id}.json``                   | Dataset metadata (variables,    |
|                    |                                               | dims, coordinate ranges); query |
|                    |                                               | params: ``stac_api``,           |
|                    |                                               | ``collection_id``               |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/preview/{item_id}/panel``                  | Redirect to interactive Panel   |
|                    |                                               | app for a single item           |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/preview/collection/{collection_id}/panel`` | Redirect to collection-level    |
|                    |                                               | Panel app                       |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/preview/compare/panel``                    | Redirect to comparison Panel    |
|                    |                                               | app; query params:              |
|                    |                                               | ``collection_a``,               |
|                    |                                               | ``collection_b``, ``stac_api``  |
+--------------------+-----------------------------------------------+---------------------------------+
| GET                | ``/_panel``                                   | Live Bokeh WebSocket endpoint   |
|                    |                                               | (Panel app host)                |
+--------------------+-----------------------------------------------+---------------------------------+

Compute Cluster Management (Viz Server)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+--------------------+----------------------------+---------------------------------+
| Method             | Path                       | Description                     |
+====================+============================+=================================+
| POST               | ``/compute/clusters``      | Create or register a Dask       |
|                    |                            | cluster                         |
+--------------------+----------------------------+---------------------------------+
| GET                | ``/compute/clusters``      | List active clusters            |
+--------------------+----------------------------+---------------------------------+
| GET                | ``/compute/clusters/{id}`` | Cluster status and worker count |
+--------------------+----------------------------+---------------------------------+
| PATCH              | ``/compute/clusters/{id}`` | Scale workers or enable         |
|                    |                            | adaptive scaling                |
+--------------------+----------------------------+---------------------------------+
| DELETE             | ``/compute/clusters/{id}`` | Disconnect / shut down cluster  |
+--------------------+----------------------------+---------------------------------+

Features
--------

Collection Search with Climate-Specific Quick Filters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

STAC Browser is extended with a ``SearchFilter`` component that exposes
climate-specific filter facets: variable name, model, experiment
identifier, and paleo time period. These filters map to CQL2 property
filters sent in the POST body to ``/search``. The ``/queryables`` and
``/collections/{id}/queryables`` endpoints return enum lists populated
from the live catalog data, so dropdown menus reflect the actual content
of the registered experiments rather than a hardcoded vocabulary.

Interactive Previews (Gridded and FESOM Unstructured Mesh)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``DataPreview`` component fetches metadata from
``/preview/{item_id}.json`` to determine what variables and time steps
are available, then embeds either a static PNG
(``/preview/{item_id}.png``) for fast first paint or an interactive
Panel session for exploration. FESOM datasets are detected automatically
by inspecting dimension names (``nod2``, ``nod3``, ``ncells``, etc.) and
variable names (``elem``, ``face_node_connectivity``, etc.). When FESOM
data is detected and a mesh path is available in the STAC item
properties, the viz server renders a GeoViews ``Points`` element
rasterized with Datashader at Robinson projection with automatic
coastlines. Cyclic elements (triangles spanning the antimeridian) are
removed before rendering to avoid horizontal stripe artifacts.

Collection-Level DataTree Visualization with Time Averaging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The collection Panel app (``/preview/collection/{collection_id}/panel``)
fetches all items from a collection (up to 200), groups them by the
``properties.variable`` field, and opens each variable group as a
multi-file dataset using ``xr.open_mfdataset`` concatenated along the
time dimension with Dask lazy loading. The resulting ``xr.DataTree`` is
cached in memory keyed by (collection_id, stac_api) to avoid rebuilding
on subsequent visits.

Users interact with:

- Variable selector – populated from DataTree node names
- Time range slider – for computing temporal means over an index range
- Anomaly mode – toggles separate reference period and comparison period
  sliders; the displayed field is comparison_mean - reference_mean
- Level / depth slider – disabled when the selected variable has no
  vertical dimension
- Colormap selector – with smart defaults per climate variable (SST and
  temperature use ``thermal``, salinity uses ``haline``, SSH uses
  ``balance``, sea ice uses ``ice``, precipitation uses ``rain``)

Cross-Experiment Comparison with Anomaly / Difference Views
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The comparison Panel app (``/preview/compare/panel``) loads DataTrees
for two collections simultaneously and presents three views in a tabbed
layout: Collection A mean, Collection B mean, and the difference (B -
A). Each collection has an independent time range slider. The difference
panel is only enabled when both collections are on the same grid type
(both gridded or both FESOM unstructured). Difference plots default to a
diverging colormap (``RdBu_r``); the main plots and the difference
colormap are independently selectable.

Personal Collections with RBAC and Tree Organization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Users can create personal collections by saving STAC item IDs from the
global catalog into named groups. Collections can be nested using a
parent/child relationship and organized in a sidebar tree with position
ordering. Access control follows a four-level role hierarchy:

============== ==================================================
Role           Permissions
============== ==================================================
``owner``      Full control: CRUD items, metadata, shares, delete
``maintainer`` CRUD items and update collection metadata
``developer``  Update collection metadata only
``viewer``     Read-only
============== ==================================================

Sharing is managed via the ``/users/{username}/collections/{id}/shares``
endpoints. The owner may grant any role except ``owner`` to other users.
Re-sharing to the same user updates the existing role grant. Collections
can also be tagged with colored labels for visual organization.

Dask Cluster Management for Distributed Compute
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The viz server exposes a ``/compute/clusters`` CRUD API for registering
Dask clusters. Four cluster types are supported:

+--------------+------------------------------------+--------------------+
| Type         | Required Package                   | Use Case           |
+==============+====================================+====================+
| ``local``    | ``distributed``                    | Development or     |
|              |                                    | small analysis on  |
|              |                                    | the viz server     |
|              |                                    | node               |
+--------------+------------------------------------+--------------------+
| ``slurm``    | ``dask-jobqueue``                  | HPC batch cluster; |
|              |                                    | each worker is a   |
|              |                                    | SLURM job          |
+--------------+------------------------------------+--------------------+
| ``gateway``  | ``dask-gateway``                   | Managed Dask       |
|              |                                    | Gateway clusters   |
+--------------+------------------------------------+--------------------+
| ``existing`` | ``distributed``                    | Connect to a       |
|              |                                    | pre-running        |
|              |                                    | scheduler via      |
|              |                                    | ``tcp://`` address |
+--------------+------------------------------------+--------------------+

Once a cluster is registered, all subsequent ``da.compute()`` calls in
the viz server automatically route to the most recently registered
cluster’s client. A heuristic (``n_files > 5`` or ``total_size > 1 GB``)
is provided for callers to decide when cluster use is warranted.

Paleo Time Period Presets
~~~~~~~~~~~~~~~~~~~~~~~~~

The catalog API ships with built-in paleo period presets stored in an
in-memory DuckDB table for the lifetime of the server process:

================= ========================== ============
ID                Name                       Age
================= ========================== ============
``lgm``           Last Glacial Maximum       21.0 ka
``mid_holocene``  Mid-Holocene               6.0 ka
``eemian``        Last Interglacial (Eemian) 125.0 ka
``lig``           Last Interglacial          130.0 ka
``mis3``          MIS 3                      50.0 ka
``pliocene``      Mid-Pliocene Warm Period   3.0 Ma
``miocene``       Late Miocene               10.0 Ma
``preindustrial`` Pre-Industrial             1850 CE
``historical``    Historical Period          1850-2014 CE
================= ========================== ============

Users can add custom presets via ``POST /paleo-presets``. Only
user-added presets can be deleted; built-in presets are protected.
Presets reset on server restart (they are in-memory only).

Deployment
----------

HPC Deployment with Apptainer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The recommended HPC deployment uses Apptainer (formerly Singularity)
containers so that the Python environment is portable across cluster
nodes without requiring per-user installations.

Typical layout on a shared filesystem:

::

   /work/awiesm/catalog/
     deploy/
       esm-catalog.sif        # Apptainer container image
       esm-viz.sif
       registry.json          # Persistent catalog registry (optional)
       personal.duckdb        # Personal collections database
       run_catalog.sh
       run_viz.sh
     experiments/
       awiesm-2.1-piControl/
         catalog.duckdb
       awiesm-2.1-historical/
         catalog.duckdb

Example ``run_catalog.sh``:

.. code:: bash

   #!/bin/bash
   #SBATCH --job-name=esm-catalog
   #SBATCH --partition=service
   #SBATCH --ntasks=1
   #SBATCH --cpus-per-task=4

   export ESM_CATALOG_DB="/work/awiesm/catalog/experiments/awiesm-2.1-piControl/catalog.duckdb:/work/awiesm/catalog/experiments/awiesm-2.1-historical/catalog.duckdb"
   export ESM_CATALOG_REGISTRY="/work/awiesm/catalog/deploy/registry.json"
   export ESM_PERSONAL_DB="/work/awiesm/catalog/deploy/personal.duckdb"

   apptainer run esm-catalog.sif \
       uvicorn esm_catalog.api.app:app \
       --host 0.0.0.0 --port 8000 --workers 4

Example ``run_viz.sh``:

.. code:: bash

   #!/bin/bash
   #SBATCH --job-name=esm-viz
   #SBATCH --partition=service
   #SBATCH --ntasks=1
   #SBATCH --cpus-per-task=8

   apptainer run esm-viz.sif \
       uvicorn esm_viz.app:app \
       --host 0.0.0.0 --port 8001 --workers 2

The STAC Browser is a static site. Build it once and serve with nginx or
any static file server. Configure the catalog API URL before building:

.. code:: javascript

   // stac-browser/config.js
   export default {
     STAC_API_URL: "https://catalog.cluster.example/",
     VIZ_SERVER_URL: "https://viz.cluster.example/",
   };

Dynamic Catalog Registration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After the catalog API is running you can add additional ``.duckdb``
catalogs at runtime without restarting the server:

.. code:: bash

   curl -X POST https://catalog.cluster.example/catalogs \
       -H "Content-Type: application/json" \
       -d '{"path": "/work/awiesm/new-experiment/catalog.duckdb"}'

Development
-----------

Running Locally
~~~~~~~~~~~~~~~

.. code:: bash

   # Terminal 1: Catalog API
   cd src/
   ESM_CATALOG_DB="./test-catalog.db" \
   uvicorn esm_catalog.api.app:app --reload --port 8000

   # Terminal 2: Viz Server
   uvicorn esm_viz.app:app --reload --port 8001

   # Terminal 3: STAC Browser
   cd stac-browser/
   npm install
   npm run dev   # Vite dev server, default port 5173

If ``ESM_CATALOG_DB`` is not set, the catalog server looks for
``catalog.duckdb`` in the current directory. If that file does not exist
either, the server starts with an empty catalog and returns empty
results until catalogs are registered via the API.

Running Tests
~~~~~~~~~~~~~

.. code:: bash

   # Backend unit and integration tests
   cd src/
   pytest esm_catalog/ esm_viz/ -v

   # End-to-end browser tests (Playwright)
   cd stac-browser/
   npx playwright install
   npx playwright test
   # Results are written to test-results/

Configuration
-------------

STAC Catalog API Environment Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------------+------------------------------------------+----------------------------+
| Variable                 | Default                                  | Description                |
+==========================+==========================================+============================+
| ``ESM_CATALOG_DB``       | ``catalog.duckdb`` in cwd                | Colon-separated list of    |
|                          |                                          | ``.duckdb`` catalog file   |
|                          |                                          | paths to load at startup   |
+--------------------------+------------------------------------------+----------------------------+
| ``ESM_CATALOG_REGISTRY`` | None (in-memory only)                    | Path to JSON file for      |
|                          |                                          | persisting dynamic catalog |
|                          |                                          | registrations across       |
|                          |                                          | restarts                   |
+--------------------------+------------------------------------------+----------------------------+
| ``ESM_PERSONAL_DB``      | ``/tmp/esm-personal-collections.duckdb`` | Path to personal           |
|                          |                                          | collections DuckDB         |
|                          |                                          | database                   |
+--------------------------+------------------------------------------+----------------------------+

The ``create_app()`` factory also accepts programmatic arguments:
``cors_origins`` (list of allowed origins, defaults to ``["*"]``),
``title``, ``description``, ``version``, and an ``authenticator``
object. The default authenticator is ``NoAuthenticator`` (open access).

.. _visualization-server-1:

Visualization Server
~~~~~~~~~~~~~~~~~~~~

The viz server has no required environment variables. Feature
availability depends on installed packages:

+------------------------+---------------------------------------------+
| Package                | Feature Enabled                             |
+========================+=============================================+
| ``geoviews`` +         | Interactive FESOM rasterized plots,         |
| ``datashader``         | Robinson projection                         |
+------------------------+---------------------------------------------+
| ``cartopy``            | Static FESOM plots with coastlines          |
+------------------------+---------------------------------------------+
| ``cmocean``            | Oceanographic colormaps (``thermal``,       |
|                        | ``haline``, ``balance``, ``ice``, ``rain``) |
+------------------------+---------------------------------------------+
| ``distributed``        | Any Dask cluster management                 |
+------------------------+---------------------------------------------+
| ``dask-jobqueue``      | SLURM cluster type                          |
+------------------------+---------------------------------------------+
| ``dask-gateway``       | Gateway cluster type                        |
+------------------------+---------------------------------------------+

All packages are optional; the server degrades gracefully when they are
absent. GeoViews static files are auto-detected from the installed
package location and mounted at ``/static/extensions/geoviews/``.
