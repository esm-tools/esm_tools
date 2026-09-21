esm_catalog: Architecture
=========================

This is internals -- how ``esm_catalog`` builds and extends STAC objects. If
you just want to catalogue your experiment output, start with
:doc:`esm_catalog_services`; come back here when you're adding a new
metadata extension or reader. :doc:`esm_catalog_index` covers what a STAC
Item and Collection are, in general, with a bare-shape JSON and
:mod:`pystac` example -- start there if that's not already familiar.

Two Rules, Two Functions, Two Type Modules
--------------------------------------------

``esm_catalog`` populates that bare shape with two rules:

- **One experiment → one Collection.** The Collection includes every model
  component in the experiment (echam, fesom, …).
- **One output file → one Item.** Each file becomes an Item with a single
  ``data`` Asset. The file's variable, datacube dimensions, and the namelist and
  paleo context active for that file live on the Item.

The extensions below say what ``esm_catalog`` puts into
``item.properties``/``collection.extra_fields`` (and
``collection.summaries``) on top of this bare shape, and how.

Two functions build these objects: ``item.py``'s ``make_item(path,
file_metadata, exp_metadata)``, called once per output file, and
``collection.py``'s ``make_collection(exp_metadata)``, called once per
experiment. Both start from a bare STAC object -- an Item
carrying only the fields every file has (``variable``, ``experiment``,
``component``, ``format``), a Collection carrying only ``id``, ``title``,
``description``, ``extent``, ``license``, ``components``. From there, each
extension does one thing: add its own fields onto that object, in place.

``Item.properties``, before any extension:

.. code-block:: json

   {
     "variable": "t",
     "experiment": "PI",
     "component": "echam",
     "format": "grib"
   }

After ``datacube``, ``namelist``, and (paleo runs only) ``paleo`` have each
run (``paleo:datetime`` is geological time, an ISO-8601-like string with an
unbounded year -- see :doc:`esm_catalog_metadata` -- not a typo):

.. code-block:: json

   {
     "variable": "t",
     "experiment": "PI",
     "component": "echam",
     "format": "grib",
     "cube:dimensions": {"...": "..."},
     "cube:variables": {"...": "..."},
     "nml__echam__namelist_echam__runctl__lcouple": true,
     "paleo:datetime": "-21000-01-01T00:00:00"
   }

Four STAC extensions write fields onto an Item, a Collection, or both, and
register the extension's URL: ``datacube``, ``contacts``, ``paleo``,
``namelist``. They share one registry (``registry.py``), one pair of helpers
(``stac_ext.py``), and one pluggable contract (``plugins.py``, built on
`pluggy <https://pluggy.readthedocs.io/>`_ -- the same plugin framework
pytest uses) that ``item.py``/``collection.py`` dispatch through instead of
calling each extension directly. :ref:`Adding a New Extension` covers writing
another.

The code internally conforms to a typed vocabulary in the files ``types.py``
and ``models.py``:

- ``types.py`` — type aliases, ``TypedDict``\ s, and lightly-validated pydantic
  models that name recurring data shapes (``ExperimentId``, ``BBox``,
  ``FileMetadata``, ``ScannedVariable``); ``FileMetadata`` and
  ``ScannedVariable`` validate at the scan boundary but carry no behaviour of
  their own.
- ``models.py`` — pydantic models with methods and cross-field validation
  (``ExperimentMetadata``, ``Contact.to_stac()``).

.. note::

   This per-file Item shape is under review. ESGF-NG models STAC as
   Collection = project, Item = a CMIP Data Reference Syntax (DRS) dataset
   (component × stream × frequency), Assets = the files making up that
   dataset. A future redesign may move ``esm_catalog`` to the same
   Collection = simulation, Item = dataset shape -- fewer, richer Items
   instead of one per file -- to speak the same vocabulary as ESGF-NG and
   collapse per-file namelist/paleo duplication. Deferred to its own design;
   not started.

   References: `ESGF STAC tests <https://github.com/ESGF/esgf-stac-tests>`_;
   `CMIP6 DRS/CV specification <https://zenodo.org/records/15670624>`_
   (Taylor et al., DOI 10.5281/zenodo.15670624); `CMIP6_CVs
   <https://github.com/WCRP-CMIP/CMIP6_CVs>`_, the controlled-vocabulary
   source (concept DOI 10.5281/zenodo.12197150); `carbonplan/cmip7-virtualization
   <https://github.com/carbonplan/cmip7-virtualization>`_, virtual Zarr
   stores over CMIP7 with ESGF STAC catalog discovery.

The Extension Registry
----------------------

``registry.py`` holds two things: an ``Extension`` StrEnum listing the
extensions ``esm_catalog`` can attach, and ``EXTENSION_URLS``, mapping each to its
schema URL. Members use ``auto()``, so the enum value is the lowercase member
name.

.. code-block:: python

   class Extension(StrEnum):
       datacube = auto()
       contacts = auto()
       paleo = auto()
       namelist = auto()

   EXTENSION_URLS: dict[Extension, str] = {
       Extension.datacube: "https://stac-extensions.github.io/datacube/v2.2.0/schema.json",
       Extension.contacts: "https://stac-extensions.github.io/contacts/v0.1.1/schema.json",
       Extension.paleo: "https://esm-tools.github.io/stac-extensions/paleo/v1.0.0/schema.json",
       Extension.namelist: "https://esm-tools.github.io/stac-extensions/namelist/v1.0.0/schema.json",
   }

A URL is only reachable through ``EXTENSION_URLS[name]``, so an extension cannot
be applied under a name not in the enum.

Validation (``stac_ext.py``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``stac_ext.py`` has three public functions:

- ``register_extension(obj, url)`` appends ``url`` to ``obj.stac_extensions`` if
  absent.
- ``apply_extension(obj, name, *, validate=True)`` registers the URL, then, if
  ``validate`` is true, validates ``obj.to_dict()`` against the schema. The URL
  is registered before validation because the schema requires ``stac_extensions``
  to contain it.
- ``load_schema(name)`` reads a local schema file: it takes the URL from
  ``/stac-extensions/`` onward and opens it via ``esm_tools.get_config_filepath``.
  For a URL without that path segment (the upstream schemas on
  ``stac-extensions.github.io``) there is no local copy, and ``load_schema``
  raises ``ValueError``. The paired ``_validator(name)`` compiles a jsonschema
  validator from that schema. Both are cached.

``paleo`` and ``namelist`` ship their schema in esm_tools and use the default
``validate=True``. ``contacts`` and ``datacube`` use the upstream schema only and
call ``apply_extension(..., validate=False)``.

Data flow: building a catalog
-----------------------------

.. mermaid::

   flowchart TB
       EM["ExperimentMetadata<br/>(experiment_id, components, contacts,<br/>namelists_by_component, paleo_config)"]
       FM["FileMetadata<br/>(variable, component, dimensions,<br/>datetime_start/end, bbox, ...)"]

       MI["item.py: make_item(path, file_metadata, exp_metadata)"]
       MC["collection.py: make_collection(exp_metadata)"]

       EM --> MI
       FM --> MI
       EM --> MC

       ITEM["pystac.Item<br/>(geometry, bbox, datetime, properties, data Asset)"]
       COLL["pystac.Collection<br/>(id=collection_id, default extent, components)"]
       MI --> ITEM
       MC --> COLL

       IEXT["item extensions<br/>datacube, namelist, paleo"]
       CEXT["collection extensions<br/>contacts, namelist, paleo"]
       ITEM --> IEXT
       COLL --> CEXT

       CAT["Experiment catalog<br/>(Collection + N Items)"]
       IEXT --> CAT
       CEXT -->|update_extent, per item| CAT

.. note::

   - ``datacube`` is applied to Items only; there is no
     ``add_datacube_collection_extension``. ``contacts`` is applied to
     Collections only; there is no ``add_contacts_item_extension`` (an Item
     reaches its experiment's contacts through its ``rel="collection"`` link).
   - Every ``add_*_extension`` function is a no-op on empty input (no contacts,
     no dimensions, no paleo config, no namelist parameters): it adds the
     extension's URL and fields only when there is data.
   - ``update_extent(collection, item)`` grows the Collection's spatial and
     temporal ``Extent`` to cover one Item. The build orchestration (out of
     scope) calls it once per item; ``make_collection`` and ``make_item`` do not.
   - An fx (time-invariant) file still needs a datetime, because STAC forbids an
     Item with none. ``item.py::_build_datetime`` sets it across
     ``exp_metadata.run_start``/``run_end`` (taken from config, not derived from
     scanned items, to avoid a circular dependency) and raises ``ValueError`` if
     that span is also missing.

.. _Adding a New Extension:

Adding a new extension
----------------------

Recipe for a new ``lineage`` extension:

#. **Registry.** Add ``lineage = auto()`` to ``Extension`` in ``registry.py``,
   and ``Extension.lineage: "https://.../schema.json"`` to ``EXTENSION_URLS``.
#. **Schema.** If ESM-Tools owns the extension, put its JSON Schema at
   ``stac-extensions/lineage/vX.Y.Z/schema.json`` in the ``esm_tools`` config
   tree, matching the URL tail so ``load_schema`` finds it. For a third-party
   extension, skip this and pass ``validate=False``.
#. **Extension module.** Write ``lineage.py`` with
   ``add_lineage_item_extension(item, ...)`` and/or
   ``add_lineage_collection_extension(collection, ...)``, following the same
   structure as the existing extensions:

   - write item fields onto ``item.properties``. For a collection field,
     choose ``collection.summaries`` when the value is a per-Item scalar
     STAC is summarizing across the Collection (paleo does this: the same
     ``paleo:*`` values that live on each Item); choose
     ``collection.extra_fields`` for structured data with no such per-Item
     mapping (contacts, the namelist inventory);
   - return early on empty input;
   - call ``apply_extension(obj, Extension.lineage)`` last, with
     ``validate=False`` only if step 2 was skipped.

#. **Register it.** In ``lineage.py``, add ``@hookimpl`` wrapper(s) calling
   the new ``add_lineage_*_extension``:

   .. code-block:: python

      from esm_catalog.plugins import hookimpl

      @hookimpl
      def apply_to_item(item, file_metadata, exp_metadata, hints):
          add_lineage_item_extension(item, ...)

   Then register the module in ``plugins.py::_build_plugin_manager``
   (``pm.register(lineage)``). ``item.py``/``collection.py`` need no change —
   they dispatch ``pm.hook.apply_to_item``/``apply_to_collection`` without
   knowing which extensions are registered. ``esm-catalog list-plugins``
   shows what is registered and which hook(s) each plugin implements — use
   it to check the new extension picked up the right one(s).

``stac_ext.py`` needs no change; it looks up every extension through the
registry, never through a specific extension module.

Design Decisions
-----------------

- **A Collection is a whole experiment, not a component.** Per-component data is
  aggregated at the Collection level; there is no per-component Collection or
  sub-catalog. The namelist extension keys are component-qualified
  (``component:file:group:key``), so two components that ship a same-named
  namelist file do not collide.
- **One registry, one apply function, independent extension modules.**
  ``Extension``/``EXTENSION_URLS`` is the only place that lists the extensions
  and their schema URLs; ``apply_extension`` is the only place that registers and
  validates. No extension module imports another.
- **Pydantic for validated records, aliases for the rest.** ``models.py`` holds
  the pydantic models with behaviour and cross-field validation
  (``ExperimentMetadata``, ``Contact``); ``types.py`` holds aliases,
  ``TypedDict``\ s, and the lightly-validated shapes that only name data
  (``FileMetadata``, ``ScannedVariable``).
- **Validate only against a local schema.** ``paleo`` and ``namelist`` are
  ESM-Tools-owned and validated on every apply. ``contacts`` and ``datacube`` are
  upstream and remote-hosted, so they pass ``validate=False``: there is no local
  schema and no network call at build time.
- **Extensions no-op on empty input; they do not error.** An experiment with no
  paleo config or no contacts never declares that extension's URL.

Out of scope
------------

- ``src/esm_catalog/scan/*`` — walks real experiment output into
  ``FileMetadata`` (readers, sourcing, parallel scan, workspace state). This
  layer has its own, separate pluggy contract -- a GRIB reader can register a
  faster, model-specific read via ``scan/readers/grib/plugins.py``'s
  ``try_model_specific_read`` hook (see ``scan/readers/grib/echam.py`` for the
  only implementation so far) -- a different ``PluginManager``, scoped to
  ``"esm_catalog.grib"``, from the Item/Collection contract this document
  covers. Don't confuse the two.
- ``src/esm_catalog/storage/*`` — GeoParquet-backed catalog storage.
- ``src/esm_catalog/cli.py`` — command-line entry point.

This document covers what those layers hand to ``item.py``/``collection.py``
(``ExperimentMetadata``, ``FileMetadata``), not how they build it.
