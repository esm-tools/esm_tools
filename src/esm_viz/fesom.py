"""
FESOM unstructured mesh support module.

Provides detection and plotting utilities for FESOM (Finite-volumE Sea ice-Ocean Model)
data which uses unstructured triangular meshes instead of regular lat/lon grids.
"""

from typing import Any

import matplotlib.pyplot as plt
import matplotlib.tri as tri
import numpy as np
import xarray as xr
from loguru import logger

import cartopy.crs as ccrs
import cartopy.feature as cfeature


def is_unstructured(ds: xr.Dataset) -> bool:
    """
    Detect if a dataset uses an unstructured mesh.

    Checks for common indicators of unstructured mesh data:
    - Presence of 'nod2' or 'node' dimensions (FESOM)
    - Presence of triangulation connectivity arrays
    - Single spatial dimension with 'ncells' or similar naming

    Parameters
    ----------
    ds : xr.Dataset
        The dataset to check.

    Returns
    -------
    bool
        True if the dataset appears to use an unstructured mesh.
    """
    # Common FESOM/unstructured mesh indicators
    unstructured_dims = {
        "nod2", "nod3", "node", "nodes", "ncells", "nelem",
        "n_node", "n_nodes", "n_cell", "n_cells", "npoints"
    }

    # Check dimensions
    dims_lower = {dim.lower() for dim in ds.dims}
    if dims_lower & unstructured_dims:
        logger.debug("Detected unstructured mesh via dimension names")
        return True

    # Check for triangulation/connectivity variables
    connectivity_vars = {"elem", "elements", "tri", "triangles", "face_node_connectivity"}
    vars_lower = {var.lower() for var in ds.data_vars} | {coord.lower() for coord in ds.coords}
    if vars_lower & connectivity_vars:
        logger.debug("Detected unstructured mesh via connectivity variables")
        return True

    # Check for FESOM-specific attributes
    if ds.attrs.get("source", "").lower().startswith("fesom"):
        logger.debug("Detected FESOM via source attribute")
        return True

    if any("fesom" in str(attr).lower() for attr in ds.attrs.values()):
        logger.debug("Detected FESOM via attributes")
        return True

    # Check for single spatial dimension (common in unstructured data)
    for var in ds.data_vars.values():
        spatial_dims = [d for d in var.dims if d not in ("time", "t", "level", "lev", "depth")]
        if len(spatial_dims) == 1:
            dim_name = spatial_dims[0].lower()
            if any(indicator in dim_name for indicator in ["nod", "cell", "elem", "point"]):
                logger.debug(f"Detected unstructured mesh via single spatial dimension: {spatial_dims[0]}")
                return True

    return False


def _get_mesh_coordinates(ds: xr.Dataset) -> tuple[np.ndarray | None, np.ndarray | None]:
    """
    Extract node coordinates from an unstructured mesh dataset.

    Returns
    -------
    tuple[np.ndarray | None, np.ndarray | None]
        (lon_values, lat_values) or (None, None) if not found.
    """
    # Common coordinate variable names for unstructured meshes
    lon_names = ["lon", "longitude", "x", "nod_x", "lon_nod2", "xc", "clon"]
    lat_names = ["lat", "latitude", "y", "nod_y", "lat_nod2", "yc", "clat"]

    lon_vals = None
    lat_vals = None

    all_vars = set(ds.data_vars.keys()) | set(ds.coords.keys())
    all_vars_lower = {v.lower(): v for v in all_vars}

    for lon_name in lon_names:
        if lon_name in all_vars_lower:
            actual_name = all_vars_lower[lon_name]
            lon_vals = ds[actual_name].values
            logger.debug(f"Found longitude coordinate: {actual_name}")
            break

    for lat_name in lat_names:
        if lat_name in all_vars_lower:
            actual_name = all_vars_lower[lat_name]
            lat_vals = ds[actual_name].values
            logger.debug(f"Found latitude coordinate: {actual_name}")
            break

    return lon_vals, lat_vals


def _get_triangulation(ds: xr.Dataset) -> np.ndarray | None:
    """
    Extract triangulation connectivity from an unstructured mesh dataset.

    Returns
    -------
    np.ndarray | None
        Triangle connectivity array (n_triangles, 3) or None if not found.
    """
    connectivity_names = [
        "elem", "elements", "tri", "triangles",
        "face_node_connectivity", "nv", "elem_nod2"
    ]

    all_vars = set(ds.data_vars.keys()) | set(ds.coords.keys())
    all_vars_lower = {v.lower(): v for v in all_vars}

    for name in connectivity_names:
        if name in all_vars_lower:
            actual_name = all_vars_lower[name]
            triangles = ds[actual_name].values
            logger.debug(f"Found triangulation: {actual_name} with shape {triangles.shape}")

            # Ensure correct shape (n_triangles, 3)
            if triangles.ndim == 2:
                if triangles.shape[1] == 3:
                    return triangles
                elif triangles.shape[0] == 3:
                    return triangles.T

    return None


def plot_unstructured(
    data_array: xr.DataArray,
    cmap: str = "viridis",
    ds: xr.Dataset | None = None,
    figsize: tuple[int, int] = (12, 8),
    add_coastlines: bool = True,
    title: str | None = None,
    vmin: float | None = None,
    vmax: float | None = None,
) -> plt.Figure:
    """
    Create a plot from unstructured mesh data using triangulation.

    This function attempts to use proper triangulation-based plotting for
    unstructured mesh data (like FESOM). If triangulation information is
    not available, it falls back to scatter plot or regular grid plotting.

    Parameters
    ----------
    data_array : xr.DataArray
        Data array from an unstructured mesh.
    cmap : str, optional
        Matplotlib colormap name. Default is 'viridis'.
    ds : xr.Dataset, optional
        Parent dataset containing mesh information (coordinates, triangulation).
        If None, attempts to extract from data_array coordinates.
    figsize : tuple[int, int], optional
        Figure size in inches. Default is (12, 8).
    add_coastlines : bool, optional
        Whether to add coastlines. Default is True.
    title : str, optional
        Plot title.
    vmin : float, optional
        Minimum value for colormap scaling.
    vmax : float, optional
        Maximum value for colormap scaling.

    Returns
    -------
    plt.Figure
        Matplotlib Figure object with the plot.

    Notes
    -----
    This is currently a basic implementation that supports:
    - Scatter plots when mesh coordinates are available
    - Triangulated contour plots when connectivity is available

    Full support for FESOM mesh visualization is planned for future versions.
    """
    fig, ax = plt.subplots(
        figsize=figsize,
        subplot_kw={"projection": ccrs.PlateCarree()},
    )

    # Try to get mesh coordinates from dataset
    lon_vals = None
    lat_vals = None
    triangles = None

    if ds is not None:
        lon_vals, lat_vals = _get_mesh_coordinates(ds)
        triangles = _get_triangulation(ds)

    # Also check data_array coordinates
    if lon_vals is None or lat_vals is None:
        lon_vals, lat_vals = _get_mesh_coordinates(
            xr.Dataset(coords=data_array.coords)
        )

    # Load data values
    values = data_array.values
    if hasattr(values, "compute"):
        values = values.compute()

    # Flatten if needed
    values = values.ravel()

    if lon_vals is not None and lat_vals is not None:
        lon_vals = lon_vals.ravel()
        lat_vals = lat_vals.ravel()

        # Ensure matching sizes
        if len(lon_vals) != len(values):
            logger.warning(
                f"Coordinate size mismatch: lon={len(lon_vals)}, values={len(values)}. "
                "Falling back to scatter plot."
            )
            triangles = None

        if triangles is not None:
            # Use triangulation for proper mesh visualization
            logger.debug("Creating triangulated plot")
            try:
                # Create triangulation object
                # Adjust indices if 1-based (common in Fortran-based models)
                if triangles.min() == 1:
                    triangles = triangles - 1

                triang = tri.Triangulation(lon_vals, lat_vals, triangles)

                im = ax.tripcolor(
                    triang,
                    values,
                    cmap=cmap,
                    vmin=vmin,
                    vmax=vmax,
                    transform=ccrs.PlateCarree(),
                    shading="flat",
                )
            except Exception as e:
                logger.warning(f"Triangulation plotting failed: {e}. Falling back to scatter.")
                im = ax.scatter(
                    lon_vals,
                    lat_vals,
                    c=values,
                    cmap=cmap,
                    vmin=vmin,
                    vmax=vmax,
                    s=1,
                    transform=ccrs.PlateCarree(),
                )
        else:
            # Scatter plot fallback
            logger.debug("Creating scatter plot (no triangulation available)")
            im = ax.scatter(
                lon_vals,
                lat_vals,
                c=values,
                cmap=cmap,
                vmin=vmin,
                vmax=vmax,
                s=1,
                transform=ccrs.PlateCarree(),
            )
    else:
        # No coordinates found - use index-based plot
        logger.warning(
            "No mesh coordinates found. Creating index-based plot. "
            "For proper FESOM visualization, ensure mesh coordinates are in the dataset."
        )
        im = ax.imshow(
            values.reshape(-1, 1).T,
            aspect="auto",
            cmap=cmap,
            vmin=vmin,
            vmax=vmax,
        )

    # Set global extent for global models
    if lon_vals is not None and lat_vals is not None:
        lon_range = float(lon_vals.max()) - float(lon_vals.min())
        lat_range = float(lat_vals.max()) - float(lat_vals.min())
        if lon_range > 300 and lat_range > 150:
            ax.set_global()

    # Add map features
    if add_coastlines:
        try:
            ax.add_feature(cfeature.COASTLINE, linewidth=0.5, edgecolor="black")
        except Exception as e:
            logger.warning(f"Could not add coastlines: {e}")

    # Add colorbar
    cbar = fig.colorbar(im, ax=ax, orientation="horizontal", pad=0.05, shrink=0.8)

    if "units" in data_array.attrs:
        cbar.set_label(data_array.attrs["units"])

    # Set title
    if title is None:
        title = data_array.attrs.get("long_name", data_array.name or "Unstructured Data")
    ax.set_title(title, fontsize=12)

    plt.tight_layout()
    return fig
