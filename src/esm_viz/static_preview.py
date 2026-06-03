from __future__ import annotations
"""
Static PNG generation module for climate data visualization.

Generates matplotlib/cartopy-based maps from xarray DataArrays,
suitable for quick previews of spatial climate data.
"""

import io
from typing import Any

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import xarray as xr
from loguru import logger

# Use non-interactive backend for server deployment
matplotlib.use("Agg")


def plot_spatial(
    data_array: xr.DataArray,
    cmap: str = "viridis",
    projection: ccrs.Projection | None = None,
    figsize: tuple[int, int] = (12, 8),
    add_coastlines: bool = True,
    add_gridlines: bool = True,
    title: str | None = None,
    colorbar_label: str | None = None,
    vmin: float | None = None,
    vmax: float | None = None,
) -> plt.Figure:
    """
    Create a spatial map plot from an xarray DataArray.

    Parameters
    ----------
    data_array : xr.DataArray
        2D array with lat/lon coordinates to plot.
    cmap : str, optional
        Matplotlib colormap name. Default is 'viridis'.
    projection : ccrs.Projection, optional
        Cartopy projection for the map. Default is PlateCarree.
    figsize : tuple[int, int], optional
        Figure size in inches. Default is (12, 8).
    add_coastlines : bool, optional
        Whether to add coastlines. Default is True.
    add_gridlines : bool, optional
        Whether to add gridlines. Default is True.
    title : str, optional
        Plot title. If None, derived from data_array attributes.
    colorbar_label : str, optional
        Colorbar label. If None, derived from data_array attributes.
    vmin : float, optional
        Minimum value for colormap scaling.
    vmax : float, optional
        Maximum value for colormap scaling.

    Returns
    -------
    plt.Figure
        Matplotlib Figure object with the plot.
    """
    if projection is None:
        projection = ccrs.PlateCarree()

    fig, ax = plt.subplots(
        figsize=figsize,
        subplot_kw={"projection": projection},
    )

    # Identify lat/lon coordinates
    lat_coord, lon_coord = _find_lat_lon_coords(data_array)

    if lat_coord is None or lon_coord is None:
        logger.warning("Could not identify lat/lon coordinates, using index-based plot")
        im = ax.imshow(
            data_array.values,
            cmap=cmap,
            vmin=vmin,
            vmax=vmax,
            origin="lower",
        )
    else:
        # Get coordinate values
        lons = data_array[lon_coord].values
        lats = data_array[lat_coord].values

        # Create 2D mesh if needed
        if lons.ndim == 1 and lats.ndim == 1:
            im = ax.pcolormesh(
                lons,
                lats,
                data_array.values,
                cmap=cmap,
                vmin=vmin,
                vmax=vmax,
                transform=ccrs.PlateCarree(),
                shading="auto",
            )
        else:
            # Already 2D coordinates (e.g., curvilinear grids)
            im = ax.pcolormesh(
                lons,
                lats,
                data_array.values,
                cmap=cmap,
                vmin=vmin,
                vmax=vmax,
                transform=ccrs.PlateCarree(),
                shading="auto",
            )

        # Set global extent if coordinates span full globe
        if _is_global_coverage(lons, lats):
            ax.set_global()

    # Add map features
    if add_coastlines:
        try:
            ax.add_feature(cfeature.COASTLINE, linewidth=0.5, edgecolor="black")
            ax.add_feature(cfeature.BORDERS, linewidth=0.3, linestyle=":", edgecolor="gray")
        except Exception as e:
            logger.warning(f"Could not add coastlines: {e}")

    if add_gridlines:
        try:
            gl = ax.gridlines(draw_labels=True, alpha=0.5, linestyle="--")
            gl.top_labels = False
            gl.right_labels = False
        except Exception as e:
            logger.warning(f"Could not add gridlines: {e}")

    # Add colorbar
    cbar = fig.colorbar(im, ax=ax, orientation="horizontal", pad=0.05, shrink=0.8)

    if colorbar_label is None:
        colorbar_label = _get_colorbar_label(data_array)
    if colorbar_label:
        cbar.set_label(colorbar_label)

    # Set title
    if title is None:
        title = _get_plot_title(data_array)
    if title:
        ax.set_title(title, fontsize=12)

    plt.tight_layout()
    return fig


def _find_lat_lon_coords(data_array: xr.DataArray) -> tuple[str | None, str | None]:
    """
    Identify latitude and longitude coordinate names in a DataArray.

    Returns
    -------
    tuple[str | None, str | None]
        (lat_coord_name, lon_coord_name) or (None, None) if not found.
    """
    lat_names = {"lat", "latitude", "LAT", "LATITUDE", "y", "Y", "rlat", "nav_lat"}
    lon_names = {"lon", "longitude", "LON", "LONGITUDE", "x", "X", "rlon", "nav_lon"}

    lat_coord = None
    lon_coord = None

    # Check dims and coords
    all_names = set(data_array.dims) | set(data_array.coords)

    for name in all_names:
        if name.lower() in {n.lower() for n in lat_names}:
            lat_coord = name
        elif name.lower() in {n.lower() for n in lon_names}:
            lon_coord = name

    # Also check standard_name attribute
    for coord_name, coord in data_array.coords.items():
        if "standard_name" in coord.attrs:
            std_name = coord.attrs["standard_name"].lower()
            if std_name in ("latitude", "grid_latitude"):
                lat_coord = coord_name
            elif std_name in ("longitude", "grid_longitude"):
                lon_coord = coord_name

    return lat_coord, lon_coord


def _is_global_coverage(lons: np.ndarray, lats: np.ndarray) -> bool:
    """Check if coordinates suggest global coverage."""
    try:
        lon_range = float(lons.max()) - float(lons.min())
        lat_range = float(lats.max()) - float(lats.min())
        return lon_range > 300 and lat_range > 150
    except Exception:
        return False


def _get_colorbar_label(data_array: xr.DataArray) -> str:
    """Generate colorbar label from data attributes."""
    parts = []

    if "long_name" in data_array.attrs:
        parts.append(str(data_array.attrs["long_name"]))
    elif data_array.name:
        parts.append(str(data_array.name))

    if "units" in data_array.attrs:
        parts.append(f"[{data_array.attrs['units']}]")

    return " ".join(parts)


def _get_plot_title(data_array: xr.DataArray) -> str:
    """Generate plot title from data attributes."""
    if "long_name" in data_array.attrs:
        return str(data_array.attrs["long_name"])
    elif "standard_name" in data_array.attrs:
        return str(data_array.attrs["standard_name"]).replace("_", " ").title()
    elif data_array.name:
        return str(data_array.name)
    return "Climate Data Preview"


def generate_preview_png(
    ds: xr.Dataset,
    variable: str,
    time_index: int = 0,
    level_index: int = 0,
    cmap: str = "viridis",
    dpi: int = 100,
    **plot_kwargs: Any,
) -> bytes:
    """
    Generate a PNG preview image from a dataset variable.

    Parameters
    ----------
    ds : xr.Dataset
        The xarray Dataset containing the variable.
    variable : str
        Name of the variable to plot.
    time_index : int, optional
        Index along the time dimension. Default is 0.
    level_index : int, optional
        Index along the level/depth dimension for 3D data. Default is 0.
    cmap : str, optional
        Matplotlib colormap name. Default is 'viridis'.
    dpi : int, optional
        Resolution for the output image. Default is 100.
    **plot_kwargs : Any
        Additional keyword arguments passed to plot_spatial.

    Returns
    -------
    bytes
        PNG image data as bytes.

    Raises
    ------
    KeyError
        If the variable is not found in the dataset.
    """
    if variable not in ds.data_vars:
        available = list(ds.data_vars.keys())
        raise KeyError(
            f"Variable '{variable}' not found in dataset. "
            f"Available variables: {available}"
        )

    data_array = ds[variable]
    logger.debug(f"Generating preview for variable '{variable}' with shape {data_array.shape}")

    # Select time and level slice to get 2D data
    data_2d = _select_2d_slice(data_array, time_index, level_index)

    # Load data into memory for plotting
    logger.debug("Loading data slice into memory")
    data_2d = data_2d.compute() if hasattr(data_2d, "compute") else data_2d

    # Check for valid data - raise error if all NaN to avoid returning blank PNG
    if np.all(np.isnan(data_2d.values)):
        raise ValueError(
            f"All values are NaN for variable '{variable}' at time_index={time_index}, "
            f"level_index={level_index}. Cannot generate preview."
        )

    # Generate the plot
    fig = plot_spatial(data_2d, cmap=cmap, **plot_kwargs)

    # Convert to PNG bytes
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=dpi, bbox_inches="tight")
    buf.seek(0)
    png_bytes = buf.read()
    buf.close()

    # Clean up
    plt.close(fig)

    logger.info(f"Generated PNG preview: {len(png_bytes)} bytes")
    return png_bytes


def _select_2d_slice(
    data_array: xr.DataArray,
    time_index: int = 0,
    level_index: int = 0,
) -> xr.DataArray:
    """
    Select a 2D spatial slice from a potentially higher-dimensional array.

    Parameters
    ----------
    data_array : xr.DataArray
        The input data array (may be 2D, 3D, or 4D).
    time_index : int
        Index to select along the time dimension.
    level_index : int
        Index to select along the level/depth dimension for 3D data.

    Returns
    -------
    xr.DataArray
        2D slice suitable for spatial plotting.
    """
    # Common dimension names to reduce
    time_dims = {"time", "t", "Time", "TIME"}
    level_dims = {"level", "lev", "depth", "z", "plev", "height", "pressure", "nz", "nz1"}

    result = data_array

    # Select along time dimension
    for dim in result.dims:
        if dim in time_dims or (hasattr(result[dim], "attrs") and
            result[dim].attrs.get("standard_name", "") == "time"):
            if result[dim].size > 1:
                idx = min(time_index, result[dim].size - 1)
                result = result.isel({dim: idx})
                logger.debug(f"Selected time index {idx} along dimension '{dim}'")
                break

    # If still more than 2D, select along level dimension using provided index
    if result.ndim > 2:
        for dim in result.dims:
            if dim.lower() in {d.lower() for d in level_dims}:
                idx = min(level_index, result[dim].size - 1)
                result = result.isel({dim: idx})
                logger.debug(f"Selected level index {idx} along dimension '{dim}'")
                break

    # Final fallback: take first slice of any extra dimensions
    while result.ndim > 2:
        extra_dim = result.dims[0]
        result = result.isel({extra_dim: 0})
        logger.debug(f"Selected index 0 along extra dimension '{extra_dim}'")

    return result
