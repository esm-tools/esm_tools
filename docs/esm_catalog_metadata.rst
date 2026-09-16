esm_catalog: What Gets Recorded
===============================

This page tells you what ``esm-catalog scan`` reads from an experiment, what
it writes, and — the part you need when searching — what the fields are
called on the server.

Where the scan reads from
-------------------------

``scan`` reads the run-segment ``<expid>_finished_config.yaml`` files
ESM-Tools writes at the end of each run. Only the ``general`` block is used
for experiment metadata; a component's own ``metadata`` block describes the
model, not your run, and is ignored. Namelists come from
``config/<component>/namelist.*``. The list of output files comes from the
tidy-phase file-operations logs (with each component's ``outdata_targets``
as the fallback when no tidy log exists), so a file is catalogued because
ESM-Tools recorded producing it, not because it happened to be in the
directory.

To get good metadata, fill the ``general.metadata`` block of your runscript:

.. code-block:: yaml

   general:
       expid: pi-ctrl-001
       metadata:
           Description: Pre-industrial control, AWI-ESM 2.1, CMIP6 forcing
           Authors:
               - name: Paul Gierz
                 orcid: 0000-0002-4512-087X
           Institute: Alfred Wegener Institute
           License: CC-BY-4.0
       paleo:
           label: PI

Experiment → STAC Collection
----------------------------

One experiment becomes one Collection, covering all components. Its id is
``<expid>-<8-char hash of the experiment path>``, so two people who both call
a run ``test`` still get distinct entries.

.. list-table::
   :header-rows: 1
   :widths: 26 34 40

   * - You wrote (finished_config)
     - It becomes
     - Notes
   * - ``general.expid``
     - ``id`` (with path hash), ``title``
     - Filename prefix is the fallback
   * - ``general.metadata.Description``
     - ``description``
     - Falls back to the expid
   * - ``general.metadata.License``
     - ``license``
     - ``proprietary`` when absent
   * - ``general.metadata.Authors`` + ``Institute``
     - ``contacts`` (name, ORCID, organization; role
       ``principal_investigator``)
     - contacts extension; ``Authors`` entries are names or
       ``{name, orcid, institution}`` mappings; de-duplicated across segments
   * - ``general.start_date`` / ``end_date``
     - ``extent.temporal``
     - Min/max over all run segments; widened as Items arrive
   * - ``general.paleo``
     - ``paleo:datetime``, ``paleo:start_datetime``,
       ``paleo:end_datetime``, ``paleo:label``
     - paleo extension; first segment that declares it wins
   * - ``config/<component>/namelist.*``
     - ``nml:files``, ``nml:groups``, ``nml:parameters``
     - namelist extension; the inventory of what the Items carry

Output file → STAC Item
-----------------------

Every output file becomes one Item with a single ``data`` asset. Item ids
look like ``temp2.echam.185001.a91b3c4d``.

.. list-table::
   :header-rows: 1
   :widths: 26 34 40

   * - From the file
     - Field on the Item
     - Notes
   * - path
     - ``assets.data.href``
     - Local path or full URL (``sftp://…``) as seen from the scan root
   * - which component wrote it
     - ``component``
     - from the tidy log entry
   * - main variable
     - ``variable``; ``variables`` when the file holds several
     - from the file header
   * - file format
     - ``format``
     - ``netcdf`` or ``grib``
   * - variables with units and standard names
     - ``cube:variables``
     - datacube extension
   * - dimensions (time, level, lat/lon or unstructured mesh)
     - ``cube:dimensions``
     - datacube extension
   * - time covered
     - ``datetime``, ``start_datetime``, ``end_datetime``, ``frequency``
     - Time-invariant files get ``frequency: fx`` and span the whole
       experiment
   * - spatial extent
     - ``bbox``, ``geometry``
     -
   * - namelist parameters active for this file
     - ``nml__<component>__<file>__<group>__<key>``
     - one field per parameter; dots in file names become underscores
       (``namelist.echam`` → ``namelist_echam``); see below
   * - geological time
     - ``paleo:*``
     - inherited from the experiment

On disk, ``scan`` writes the time-series Items into a
``<expid>_stac_<start>-<end>.parquet`` shard stamped with the run span it
covered — one per scan that found new segments, never touched afterwards —
and the time-invariant files into a single ``<expid>_stac_fx.parquet`` that
is rewritten in full on every scan. Every Item property is a column, and the
``experiment`` column names the expid.

Searching on namelist parameters
--------------------------------

Every namelist parameter becomes a field named
``nml__<component>__<file>__<group>__<key>``, with the dot in the file name
replaced by an underscore. The CO₂ mixing ratio in ECHAM's
``namelist.echam``, group ``radctl``, key ``co2vmr``, is therefore:

.. code-block:: text

   nml__echam__namelist_echam__radctl__co2vmr

Locally you can see the fields the scan produced — this runs against the
demo experiment:

.. testsetup:: catalog

   from demo_experiment import build_demo
   catalog = build_demo() / "catalog"

.. testcode:: catalog

   import json, pyarrow.parquet as pq

   shard = next((catalog / "items").glob("*_stac_1850*.parquet"))
   nml = [c for c in pq.read_table(shard).column_names if c.startswith("nml__")]
   print(*nml, sep="\n")
   print(json.loads((catalog / "queryables.json").read_text())["properties"]
         ["nml__echam__namelist_echam__radctl__co2vmr"])

.. testoutput:: catalog

   nml__echam__namelist_echam__runctl__lcouple
   nml__echam__namelist_echam__radctl__co2vmr
   nml__echam__namelist_echam__radctl__ch4vmr
   nml__fesom__namelist_config__paths__meshpath
   nml__fesom__namelist_config__timestep__step_per_day
   {'type': 'number'}

One file, in full
-----------------

The same shard, one row, the fields a reader cares about:

.. testcode:: catalog

   row = pq.read_table(shard).slice(0, 1).to_pylist()[0]
   print(row["id"])
   print(row["variable"], row["component"], row["format"], row["frequency"])
   print(row["start_datetime"].date(), "→", row["end_datetime"].date())
   for name, dim in row["cube:dimensions"].items():
       print(f"  {name:5s} {dim['type']:9s} {dim['extent']}")
   print(row["cube:variables"])
   print(row["nml__echam__namelist_echam__radctl__co2vmr"],
         row["nml__fesom__namelist_config__paths__meshpath"])

.. testoutput:: catalog
   :options: +ELLIPSIS

   var.echam.185001...
   var echam netcdf mon
   1850-01-16 → 1850-01-16
     time  temporal  ['1850-01-16T00:00:00+00:00', '1850-01-16T00:00:00+00:00']
     lat   spatial   [-89.0, 89.0]
     lon   spatial   [-179.0, 179.0]
   {'var': {'dimensions': ['time', 'lat', 'lon'], 'unit': '1'}}
   0.0002847 /pool/data/meshes/core2/

Every file in the experiment carries the same ``nml__`` values — they
describe the run, not the file — which is what makes "all files from runs
with X" a one-line filter.

On the server the same field answers "every run with CO₂ above 400 ppm":

.. code-block:: python

   from pystac_client import Client

   cat = Client.open("https://stac-dev.dmawi.de")
   hits = cat.search(filter={
       "op": ">", "args": [
           {"property": "nml__echam__namelist_echam__radctl__co2vmr"}, 400e-6
       ]
   })
   for item in hits.items():
       print(item.collection_id, item.id)

Filtering on a field works as soon as the Items are pushed. Registering the
field on the server does one extra thing: it makes it appear in the web
browser's filter panel. ``push`` prints which fields are new to the server
and how the operator registers them:

.. code-block:: text

   $ esm-catalog push catalog/
   ...
   6 new queryable field(s) are not yet registered.
   Filtering already works without this — registration only makes these
   fields appear in the STAC Browser filter UI. A privileged operator runs
   on the pgstac host (adjust the ssh name if it differs from the API host):

     ssh stac-dev.dmawi.de sudo -u stac /usr/local/bin/esm-catalog-load-queryables - < catalog/queryables-delta.json

Send the operator ``catalog/queryables-delta.json``, or the command.

.. TODO screencast: fill in general.metadata, scan, and see the fields on the Item in the browser

Not recorded yet
----------------

- Which experiment a run was branched from, and when.
- The machine that hosted the run (``hosted_by`` in the machine YAML) is not
  yet copied onto the Collection.
- The git state of the model code.
