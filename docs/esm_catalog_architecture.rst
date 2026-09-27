esm_catalog: Architecture
=========================

Overview
--------

``esm_catalog`` maps an ESM-Tools experiment onto a `STAC
<https://stacspec.org/>`_ catalog with two rules:

- **One experiment → one Collection.** The Collection covers the whole
  experiment: every model component (echam, fesom, …).
- **One output file → one Item.** Each file becomes an Item with a single
  ``data`` Asset. The file's variable, datacube dimensions, and the namelist and
  paleo context active for that file live on the Item.

Four STAC extensions write fields onto an Item, a Collection, or both, and
register the extension's URL: ``datacube``, ``contacts``, ``paleo``,
``namelist``. They share one registry (``registry.py``) and one pair of helpers
(``stac_ext.py``). :ref:`Adding a New Extension` covers writing another.

Two files hold the typed vocabulary:

- ``types.py`` — type aliases and ``TypedDict``\ s that name recurring data
  without methods (``ExperimentId``, ``FileMetadata``, ``BBox``, ...).
- ``models.py`` — pydantic models with methods and validation
  (``ExperimentMetadata``, ``Contact.to_stac()``).

How the pieces map
------------------

.. graphviz::

   digraph pieces {
       rankdir=TB;
       node [shape=box, fontsize=10];

       EM   [label="Experiment metadata\n(identity, contacts, components,\nnamelists, paleo)"];
       FILE [label="Output file\n(variable, dimensions, time, bbox)"];
       COLL [label="STAC Collection\n(the whole experiment)"];
       ITEM [label="STAC Item + data Asset\n(one per file)"];

       EM   -> COLL [label="make_collection"];
       FILE -> ITEM [label="make_item"];
       ITEM -> COLL [label="belongs to"];

       node [shape=ellipse];
       DC [label="datacube"]; CO [label="contacts"];
       NM [label="namelist"]; PA [label="paleo"];

       DC -> ITEM [label="from the file", style=dashed];
       CO -> ITEM [style=dashed];
       NM -> ITEM [style=dashed];
       PA -> ITEM [style=dashed];
       CO -> COLL [style=dashed];
       NM -> COLL [style=dashed];
       PA -> COLL [style=dashed];
   }

The Extension Registry
----------------------

``registry.py`` holds two things: an ``Extension`` StrEnum listing the
extensions esm_catalog can attach, and ``EXTENSION_URLS``, mapping each to its
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

.. graphviz::

   digraph dataflow {
       rankdir=TB;
       node [shape=box, fontsize=10];

       EM [label="ExperimentMetadata\n(experiment_id, components, contacts,\nnamelists_by_component, paleo_config)"];
       FM [label="FileMetadata\n(variable, component, dimensions,\ndatetime_start/end, bbox, ...)"];

       MI [label="item.py: make_item(path, file_metadata, exp_metadata)"];
       MC [label="collection.py: make_collection(exp_metadata)"];

       EM -> MI; FM -> MI; EM -> MC;

       ITEM [label="pystac.Item\n(geometry, bbox, datetime, properties, data Asset)"];
       COLL [label="pystac.Collection\n(id=collection_id, default extent, components)"];
       MI -> ITEM; MC -> COLL;

       ITEM -> "item extensions\ncontacts, datacube, namelist, paleo";
       COLL -> "collection extensions\ncontacts, namelist, paleo";

       "item extensions\ncontacts, datacube, namelist, paleo" -> CAT;
       "collection extensions\ncontacts, namelist, paleo" -> CAT [label="update_extent, per item"];
       CAT [label="Experiment catalog\n(Collection + N Items)"];
   }

.. note::

   - ``datacube`` is applied to Items only; there is no
     ``add_datacube_collection_extension``.
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

   - write fields onto ``item.properties``, ``collection.extra_fields``, or
     ``collection.summaries``;
   - return early on empty input;
   - call ``apply_extension(obj, Extension.lineage)`` last, with
     ``validate=False`` only if step 2 was skipped.

#. **Wire it in.** Call the new ``add_lineage_*_extension`` from
   ``item.py::make_item`` and/or ``collection.py::make_collection``.

``stac_ext.py`` needs no change; it works from the registry, not from any
specific extension.

Key design decisions
--------------------

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
  the pydantic models that validate and carry methods (``ExperimentMetadata``,
  ``Contact``); ``types.py`` holds aliases and ``TypedDict``\ s that only name
  data (``FileMetadata``, ``ScannedVariable``).
- **Validate only against a local schema.** ``paleo`` and ``namelist`` are
  ESM-Tools-owned and validated on every apply. ``contacts`` and ``datacube`` are
  upstream and remote-hosted, so they pass ``validate=False``: there is no local
  schema and no network call at build time.
- **Extensions no-op on empty input; they do not error.** An experiment with no
  paleo config or no contacts never declares that extension's URL.

Out of scope
------------

- ``src/esm_catalog/scan/*`` — walks real experiment output into
  ``FileMetadata`` (readers, sourcing, parallel scan, workspace state).
- ``src/esm_catalog/storage/*`` — GeoParquet-backed catalog storage.
- ``src/esm_catalog/cli.py`` — command-line entry point.

This document covers what those layers hand to ``item.py``/``collection.py``
(``ExperimentMetadata``, ``FileMetadata``), not how they build it.
