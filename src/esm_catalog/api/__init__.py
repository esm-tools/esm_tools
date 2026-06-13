"""STAC API serving layer for ESM-Catalog.

Public entry point::

    from esm_catalog.api.app import create_app

    api = create_app(catalogs=["catalog.duckdb"])
    # Pass api.app to uvicorn
"""
