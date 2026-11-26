ESM Catalog
===========

STAC-based catalog for ESM-Tools experiment output.

.. epigraph::

   At its core, the SpatioTemporal Asset Catalog (STAC) specification
   provides a common structure for describing and cataloging spatiotemporal
   assets.

   A spatiotemporal asset is any file that represents information about the
   earth captured in a certain space and time.

   -- `stacspec.org <https://stacspec.org/en>`_

``esm_catalog`` transforms simulation outputs from an Earth System Model into
exactly that kind of spatiotemporal asset -- searchable, filterable,
discoverable by variable and time instead of by file path.

A STAC **Item** is one GeoJSON Feature: geometry, properties, links, and
Assets (files) describing one spatiotemporal "thing." A STAC **Collection**
is the metadata for a searchable group of Items -- extent, license,
description -- that Items belong to. Stripped of everything ``esm_catalog``
adds, a minimal Item and Collection look like this:

.. code-block:: json

   {
     "type": "Feature",
     "stac_version": "1.0.0",
     "id": "example-item",
     "collection": "example-collection",
     "geometry": {"type": "Point", "coordinates": [0.0, 0.0]},
     "bbox": [0.0, 0.0, 0.0, 0.0],
     "properties": {"datetime": "2000-01-01T00:00:00Z"},
     "assets": {"data": {"href": "file:///path/to/file.nc"}}
   }

.. code-block:: json

   {
     "type": "Collection",
     "stac_version": "1.0.0",
     "id": "example-collection",
     "description": "An example experiment",
     "license": "proprietary",
     "extent": {
       "spatial": {"bbox": [[-180.0, -90.0, 180.0, 90.0]]},
       "temporal": {"interval": [["2000-01-01T00:00:00Z", null]]}
     }
   }

``esm_catalog`` builds these through `pystac <https://pystac.readthedocs.io/>`_'s
:class:`~pystac.Item` and :class:`~pystac.Collection`, not by hand:

.. code-block:: python

   from datetime import datetime, timezone

   import pystac

   item = pystac.Item(
       id="example-item",
       geometry={"type": "Point", "coordinates": [0.0, 0.0]},
       bbox=[0.0, 0.0, 0.0, 0.0],
       datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
       properties={},
       collection="example-collection",
   )

   collection = pystac.Collection(
       id="example-collection",
       description="An example experiment",
       extent=pystac.Extent(
           spatial=pystac.SpatialExtent(bboxes=[[-180.0, -90.0, 180.0, 90.0]]),
           temporal=pystac.TemporalExtent(intervals=[[None, None]]),
       ),
       license="proprietary",
   )

:doc:`esm_catalog_architecture` covers how ``esm_catalog`` populates this
bare shape and how to extend it; start with :doc:`esm_catalog_services` if
you just want to catalogue your experiment output.

.. toctree::
   :maxdepth: 2

   esm_catalog_services
   esm_catalog_metadata
   esm_catalog_access_control
   esm_catalog_viewer
   esm_catalog_architecture
