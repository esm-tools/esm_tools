esm_catalog: Browsing the Catalogue
===================================

The web view of the catalogue is `STAC-browser
<https://github.com/radiantearth/stac-browser>`_ at
``https://stac-dev.awi.de``. It shows exactly what the API serves; there is
nothing in it that a script could not also get.

.. TODO screencast: open the browser, find an experiment, filter Items, copy an href

Landing Page, Items, and the Filter Panel
-------------------------------------------

- The landing page lists experiments (STAC Collections): title, description,
  authors, time covered, and the namelist inventory.
- Open one to list its files (Items). Each shows the variable, component,
  time span, the datacube dimensions, and — under *Properties* — every
  ``nml__`` namelist parameter and the ``paleo:`` fields.
- The *Filter* panel searches within an experiment on any registered field:
  variable, component, time, bounding box, and the namelist parameters.
- Each Item's ``data`` asset links to the file's path on the filesystem. The
  browser cannot open the data itself; copy the path and open it with
  ``xarray`` on the HPC side.

Filtering
---------

Filters in the browser are the same CQL2 filters ``pystac-client`` sends.
A filter you build by clicking can be reproduced in code:

.. code-block:: python

   # "component = echam and variable = temp2", as the browser would send it
   cat.search(
       collections=["pi-ctrl-001-3f9a1c2e"],
       filter="component = 'echam' AND variable = 'temp2'",
       filter_lang="cql2-text",
   )

More filter examples, each shown as the browser's filter panel would build
it and as ``pystac-client`` would send it:

.. code-block:: python

   # a time window, any component
   cat.search(
       collections=["pi-ctrl-001-3f9a1c2e"],
       datetime="1850-01-01/1859-12-31",
   )

   # a region (lon_min, lat_min, lon_max, lat_max): the North Atlantic
   cat.search(collections=["pi-ctrl-001-3f9a1c2e"], bbox=[-80, 30, 0, 70])

   # runs coupled to the ocean, across ALL experiments
   cat.search(filter="nml__echam__namelist_echam__runctl__lcouple = true",
              filter_lang="cql2-text")

   # the same thing as CQL2-JSON, which is what the panel actually sends
   cat.search(filter={"op": "=", "args": [
       {"property": "nml__echam__namelist_echam__runctl__lcouple"}, True]})

   # monthly echam output from experiments on the CORE2 mesh
   cat.search(filter=(
       "component = 'echam' AND frequency = 'mon' AND "
       "nml__fesom__namelist_config__paths__meshpath LIKE '%core2%'"
   ), filter_lang="cql2-text")

``search`` returns lazily; ``.items()`` iterates, ``.item_collection()``
fetches everything, and ``.matched()`` asks the server for the count first.

Namelist parameters can be filtered on from the moment they are pushed; they
appear in the *filter panel* once the operator has registered them.
``esm-catalog push`` prints the ones that still need registering and the
command to do it (see :doc:`esm_catalog_metadata`).

Reaching the browser
--------------------

The dev server sits in the same network zone as the API. If the page does
not load from your workstation, you are outside that zone; the same address
works from a login node's browser session or through the institute VPN.

Looking at a catalogue you have not pushed
------------------------------------------

The browser only shows what is on the server. To inspect a freshly scanned
catalogue before pushing, use ``esm-catalog status`` for the overview or read
the shards directly:

.. code-block:: bash

   esm-catalog status
   duckdb -c "SELECT component, variable, count(*) FROM read_parquet('catalog/items/*.parquet', union_by_name=true) GROUP BY 1,2"
