from __future__ import annotations
"""
esm_viz - FastAPI-based visualization service for climate data.

Provides REST endpoints for generating static previews and metadata
from NetCDF and GRIB climate datasets referenced in STAC catalogs.
Also provides interactive Panel-based visualization applications.
"""

__version__ = "0.1.0"
__author__ = "ESM-Tools Development Team"

from esm_viz.readers import open_data, get_data_metadata
from esm_viz.static_preview import plot_spatial, generate_preview_png
from esm_viz.fesom import is_unstructured, plot_unstructured
from esm_viz.interactive import create_preview_app, create_comparison_app, get_smart_colormap
from esm_viz.collection import create_collection_preview_app

__all__ = [
    "open_data",
    "get_data_metadata",
    "plot_spatial",
    "generate_preview_png",
    "is_unstructured",
    "plot_unstructured",
    "create_preview_app",
    "create_comparison_app",
    "create_collection_preview_app",
    "get_smart_colormap",
]
