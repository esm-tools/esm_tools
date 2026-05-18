ESM Catalog + Viz: User Guide
=============================

Getting Started
---------------

Browsing Collections
~~~~~~~~~~~~~~~~~~~~

When you open STAC Browser you land on the catalog home page, which
lists all registered experiment collections. Each card shows the
experiment name, the number of items (output files), the temporal
coverage, and a spatial extent thumbnail.

.. figure:: screenshots/catalog-home.png
   :alt: Catalog home page

   Catalog home page

Click any collection card to open the collection detail view, which
shows:

- Collection metadata (model, experiment, forcing scenario)
- A list of STAC items – individual output files grouped by variable and
  time period
- Quick action buttons: “Preview Collection” and “Compare”

Searching
~~~~~~~~~

The search bar at the top of the page accepts free-text queries that
match against collection and item metadata. For more targeted queries,
open the filter panel with the filter icon.

**Climate-specific quick filters** appear at the top of the filter
panel:

+--------------------------+-------------------------------------------+
| Filter                   | What it does                              |
+==========================+===========================================+
| Variable                 | Show only items containing a specific     |
|                          | output variable (e.g. ``temp``, ``sst``,  |
|                          | ``a_ice``)                                |
+--------------------------+-------------------------------------------+
| Model                    | Filter by model name or component         |
+--------------------------+-------------------------------------------+
| Experiment               | Filter by experiment identifier           |
+--------------------------+-------------------------------------------+
| Paleo Period             | Restrict to items matching a known paleo  |
|                          | time period (LGM, Mid-Holocene, etc.)     |
+--------------------------+-------------------------------------------+

Selecting a Paleo Period filter automatically translates the preset into
a STAC datetime filter so you do not need to know the exact
years-before-present value.

**Additional Filtering** (the “Advanced” toggle) opens the CQL2 query
builder. The field dropdowns are populated directly from the live
catalog so available values reflect the actual experiment output
registered on this server.

Viewing Data
------------

Static vs Interactive Previews
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every STAC item page shows a data preview panel. The preview has two
modes:

**Static PNG** – rendered immediately on page load. Shows a global map
of the first time step of the first variable found in the file. Use this
for a quick sanity check that the data looks reasonable.

.. figure:: screenshots/static-preview.png
   :alt: Static preview panel

   Static preview panel

**Interactive Panel** – click “Open Interactive” to launch a live Panel
session in a new tab (or an embedded iframe, depending on browser
configuration). The interactive view adds:

- Variable selector (all variables in the file)
- Time step slider
- Level / depth slider (for 3D fields)
- Colormap picker
- Zoom, pan, hover tools via Bokeh

The interactive view opens at ``/preview/{item_id}/panel`` on the viz
server and redirects to the live ``/_panel`` WebSocket endpoint. Each
browser tab gets an independent session.

FESOM Plots
~~~~~~~~~~~

Files from FESOM ocean model runs are automatically detected. Instead of
a regular lat/lon grid plot, the preview renders a triangulated mesh
using GeoViews and Datashader:

- Node values are averaged to element centroids for rendering
- Elements that span the date line (antimeridian) are removed to prevent
  visual artifacts
- Coastlines and land mask are overlaid
- Robinson projection is used by default

The mesh coordinates (``nod2d.out``, ``elem2d.out``) are read from the
path stored in the STAC item properties. If the mesh path is not
accessible from the viz server’s filesystem, the preview falls back to a
scatter plot of node values.

.. figure:: screenshots/fesom-interactive.png
   :alt: FESOM interactive plot

   FESOM interactive plot

Collection Preview
------------------

To explore an entire experiment collection at once, click **“Preview
Collection”** from the collection detail page. This opens the
collection-level Panel app.

.. figure:: screenshots/collection-preview.png
   :alt: Collection preview app

   Collection preview app

The collection app builds an xarray DataTree from all items in the
collection, grouped by output variable. This can take 10-30 seconds on
the first load for large collections; subsequent visits use a cached
DataTree.

Controls
~~~~~~~~

**Variable** – select which output variable to display. The dropdown
lists all variables found across the collection’s items.

**View Mode** – toggle between “Absolute” (time mean) and “Anomaly”
mode.

In **Absolute** mode: - The **Time Range** slider selects which time
steps to average. The indices refer to the position along the
concatenated time axis across all files for the selected variable. - The
plot shows the temporal mean over the selected range.

In **Anomaly** mode: - The **Time Range** slider is replaced by two
sliders: **Reference Period** and **Comparison Period**. - The plot
shows Comparison mean - Reference mean. - Use this to visualize trends
or the difference between two phases of a simulation.

**Level / Depth** – active only for 3D variables. Selects the vertical
level index to display before time averaging.

**Colormap** – the selector auto-populates with a physically appropriate
colormap based on the variable name (temperature variables default to
``thermal``, salinity to ``haline``, SSH to ``balance``, sea ice to
``ice``). You can override this with any available colormap.

Comparing Experiments
---------------------

Selecting Collections
~~~~~~~~~~~~~~~~~~~~~

From any collection detail page, click **“Compare”** to open the
comparison selector. Choose a second collection from the dropdown (only
collections with at least one variable in common are enabled) and click
**“Open Comparison”**.

Alternatively, you can initiate a comparison from the
``PersonalCollections`` sidebar by selecting two collections and
choosing “Compare selected”.

Parameter Diff
~~~~~~~~~~~~~~

The comparison app sidebar shows which variables are common to both
collections and flags variables that exist in only one. The **Variable**
dropdown is populated from the intersection so you are always comparing
the same physical field across experiments.

.. figure:: screenshots/comparison-controls.png
   :alt: Comparison controls

   Comparison controls

Visual Comparison
~~~~~~~~~~~~~~~~~

The main area has two tabs:

**Side by Side** – displays Collection A and Collection B next to each
other. Each panel has its own time range slider so you can align
different simulation periods (for example, years 1-50 of a piControl vs
years 1-50 of an abrupt4xCO2 run).

**Difference (B - A)** – shows the spatial difference field with a
diverging colormap (``RdBu_r`` by default). A separate colormap selector
is provided for the difference panel. This tab is disabled when the two
collections use different grid types (one gridded, one FESOM).

.. figure:: screenshots/comparison-side-by-side.png
   :alt: Side-by-side comparison

   Side-by-side comparison

Personal Collections
--------------------

Personal collections let you save and organize STAC items from the
global catalog into your own named groups, independent of the experiment
structure.

Creating a Collection
~~~~~~~~~~~~~~~~~~~~~

In the sidebar, click the **“+”** button next to “My Collections”. Enter
a name and optional description. The new collection appears as a node in
your tree.

You can also create sub-collections (folders within collections) by
right-clicking an existing collection and selecting “New
sub-collection”.

Adding Items
~~~~~~~~~~~~

When viewing a STAC item, click the **bookmark icon** or “Save to
collection” button. A dropdown shows your existing collections; select
one to add the item. You can add the same item to multiple collections.

To add multiple items at once, use the search results view: check the
items you want, then use the “Add to collection” bulk action.

Organizing Collections
~~~~~~~~~~~~~~~~~~~~~~

Drag and drop nodes in the sidebar tree to reorder them or move them
into folders. Right-click a node to rename, delete, or change its
parent.

Collections can be tagged with colored labels for visual grouping.
Manage your labels via the label editor (gear icon in the My Collections
section).

.. figure:: screenshots/personal-collections.png
   :alt: Personal collections sidebar

   Personal collections sidebar

Sharing
~~~~~~~

To share a collection with a colleague, right-click the collection and
select “Share”. Enter their username and choose a role:

============== ===============================================
Role           What the recipient can do
============== ===============================================
``viewer``     Browse the collection and view its items
``developer``  Also update the collection name and description
``maintainer`` Also add and remove items
============== ===============================================

Shared collections appear in the recipient’s sidebar under “Shared with
me”. You cannot grant ``owner`` role; to transfer ownership contact your
system administrator.

To revoke a share, open the sharing settings for the collection and
click the trash icon next to the grant.

Compute Clusters
----------------

For heavy analysis – time averaging across hundreds of files,
full-collection anomaly computation – you can register a Dask
distributed cluster with the viz server. Once registered, all compute
operations in your session automatically use the cluster.

Why Use a Cluster
~~~~~~~~~~~~~~~~~

The viz server applies a simple heuristic: if a collection has more than
5 files or more than 1 GB of data, it is recommended to have a cluster
registered. Without a cluster, computation runs on the viz server node
itself using local Dask threads, which may be slow or run out of memory
for large collections.

Registering a Cluster
~~~~~~~~~~~~~~~~~~~~~

Local Cluster (development / small jobs)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Starts worker processes on the same node as the viz server:

.. code:: bash

   curl -X POST http://viz-server:8001/compute/clusters \
       -H "Content-Type: application/json" \
       -d '{"type": "local", "workers": 4, "memory": "8GiB"}'

Connecting to an Existing SLURM Dask Scheduler
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have already started a ``dask-scheduler`` process on the cluster:

.. code:: bash

   curl -X POST http://viz-server:8001/compute/clusters \
       -H "Content-Type: application/json" \
       -d '{"scheduler_address": "tcp://n1234.hpc.example:8786"}'

SLURM Cluster (viz server submits SLURM jobs)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The viz server submits SLURM batch jobs as Dask workers:

.. code:: bash

   curl -X POST http://viz-server:8001/compute/clusters \
       -H "Content-Type: application/json" \
       -d '{
         "type": "slurm",
         "workers": 8,
         "cores": 4,
         "memory": "16GiB",
         "queue": "compute"
       }'

Note: this requires ``dask-jobqueue`` to be installed in the viz server
environment and the viz server process to have permission to submit
SLURM jobs.

Dask Gateway
^^^^^^^^^^^^

For sites running Dask Gateway:

.. code:: bash

   curl -X POST http://viz-server:8001/compute/clusters \
       -H "Content-Type: application/json" \
       -d '{
         "type": "gateway",
         "gateway_url": "http://dask-gateway.cluster.example",
         "workers": 10
       }'

Scaling and Monitoring
~~~~~~~~~~~~~~~~~~~~~~

List active clusters:

.. code:: bash

   curl http://viz-server:8001/compute/clusters

The response includes the scheduler address and Dask dashboard URL (if
available). Open the dashboard URL in a browser to monitor task
progress, worker memory, and throughput.

Scale a running cluster:

.. code:: bash

   # Fixed scale
   curl -X PATCH http://viz-server:8001/compute/clusters/{id} \
       -H "Content-Type: application/json" \
       -d '{"workers": 16}'

   # Adaptive scaling (Dask auto-adjusts between min and max)
   curl -X PATCH http://viz-server:8001/compute/clusters/{id} \
       -H "Content-Type: application/json" \
       -d '{"adapt_min": 2, "adapt_max": 20}'

Shutting Down
~~~~~~~~~~~~~

.. code:: bash

   curl -X DELETE http://viz-server:8001/compute/clusters/{id}

If the cluster was created by the viz server (local or SLURM type), it
is shut down. If you connected to an existing external scheduler, only
the client connection is closed; the scheduler itself continues running.
