"""Pluggable per-format readers.

Each reader module (``netcdf``, ``grib``) registers itself with the reader and
format-detection plugin managers (see ``esm_catalog.scan.readers.plugins`` and
``esm_catalog.scan.readers.format_plugins``) -- both build their
``PluginManager`` lazily, importing the reader modules themselves on first
use, so nothing needs importing here.
"""
