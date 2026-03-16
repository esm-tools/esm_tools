"""
FESOM unstructured mesh support module.

Provides detection, mesh loading, and plotting utilities for FESOM
(Finite-volumE Sea ice-Ocean Model) data which uses unstructured
triangular meshes instead of regular lat/lon grids.

Mesh coordinates and triangulation are loaded from the standard FESOM
mesh files (nod2d.out, elem2d.out) whose path is extracted from
STAC item properties (nml:fesom:paths:meshpath).
"""

from __future__ import annotations

from functools import lru_cache
from pathlib import Path
from typing import Any

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.tri as tri
import numpy as np
import xarray as xr
from loguru import logger

import cartopy.crs as ccrs
import cartopy.feature as cfeature


# ---------------------------------------------------------------------------
# Detection
# ---------------------------------------------------------------------------

def is_unstructured(ds: xr.Dataset) -> bool:
    """
    Detect if a dataset uses an unstructured mesh.

    Checks for common indicators of unstructured mesh data:
    - Presence of 'nod2' or 'node' dimensions (FESOM)
    - Presence of triangulation connectivity arrays
    - Single spatial dimension with 'ncells' or similar naming
    """
    unstructured_dims = {
        "nod2", "nod3", "node", "nodes", "ncells", "nelem",
        "n_node", "n_nodes", "n_cell", "n_cells", "npoints"
    }

    dims_lower = {dim.lower() for dim in ds.dims}
    if dims_lower & unstructured_dims:
        return True

    connectivity_vars = {"elem", "elements", "tri", "triangles", "face_node_connectivity"}
    vars_lower = {var.lower() for var in ds.data_vars} | {coord.lower() for coord in ds.coords}
    if vars_lower & connectivity_vars:
        return True

    if ds.attrs.get("source", "").lower().startswith("fesom"):
        return True

    if any("fesom" in str(attr).lower() for attr in ds.attrs.values()):
        return True

    for var in ds.data_vars.values():
        spatial_dims = [d for d in var.dims if d not in ("time", "t", "level", "lev", "depth")]
        if len(spatial_dims) == 1:
            dim_name = spatial_dims[0].lower()
            if any(indicator in dim_name for indicator in ["nod", "cell", "elem", "point"]):
                return True

    return False


# ---------------------------------------------------------------------------
# Mesh loading from FESOM mesh files
# ---------------------------------------------------------------------------

def _euler_rotation(lon: np.ndarray, lat: np.ndarray,
                    alpha: float, beta: float, gamma: float) -> tuple[np.ndarray, np.ndarray]:
    """
    Rotate coordinates from rotated to geographical using Euler angles.

    This undoes the rotation applied by FESOM when force_rotation=True.
    Based on the pyfesom2 rotation implementation.
    """
    rad = np.pi / 180.0

    lon_r = lon * rad
    lat_r = lat * rad

    rotate_matrix = np.zeros((3, 3))
    rotate_matrix[0, 0] = (np.cos(gamma * rad) * np.cos(alpha * rad)
                           - np.sin(gamma * rad) * np.cos(beta * rad) * np.sin(alpha * rad))
    rotate_matrix[0, 1] = (np.cos(gamma * rad) * np.sin(alpha * rad)
                           + np.sin(gamma * rad) * np.cos(beta * rad) * np.cos(alpha * rad))
    rotate_matrix[0, 2] = np.sin(gamma * rad) * np.sin(beta * rad)
    rotate_matrix[1, 0] = (-np.sin(gamma * rad) * np.cos(alpha * rad)
                           - np.cos(gamma * rad) * np.cos(beta * rad) * np.sin(alpha * rad))
    rotate_matrix[1, 1] = (-np.sin(gamma * rad) * np.sin(alpha * rad)
                           + np.cos(gamma * rad) * np.cos(beta * rad) * np.cos(alpha * rad))
    rotate_matrix[1, 2] = np.cos(gamma * rad) * np.sin(beta * rad)
    rotate_matrix[2, 0] = np.sin(beta * rad) * np.sin(alpha * rad)
    rotate_matrix[2, 1] = -np.sin(beta * rad) * np.cos(alpha * rad)
    rotate_matrix[2, 2] = np.cos(beta * rad)

    # Convert to Cartesian
    x = np.cos(lat_r) * np.cos(lon_r)
    y = np.cos(lat_r) * np.sin(lon_r)
    z = np.sin(lat_r)

    # Apply inverse rotation (transpose)
    rot_inv = rotate_matrix.T
    xr = rot_inv[0, 0] * x + rot_inv[0, 1] * y + rot_inv[0, 2] * z
    yr = rot_inv[1, 0] * x + rot_inv[1, 1] * y + rot_inv[1, 2] * z
    zr = rot_inv[2, 0] * x + rot_inv[2, 1] * y + rot_inv[2, 2] * z

    lat_geo = np.arcsin(np.clip(zr, -1, 1)) / rad
    lon_geo = np.arctan2(yr, xr) / rad

    return lon_geo, lat_geo


@lru_cache(maxsize=8)
def load_fesom_mesh(meshpath: str, alpha: float = 0.0, beta: float = 0.0,
                    gamma: float = 0.0, force_rotation: bool = False
                    ) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Load FESOM mesh from nod2d.out and elem2d.out files.

    Results are cached by meshpath + rotation parameters so repeated
    requests for the same mesh are fast.

    Parameters
    ----------
    meshpath : str
        Path to the FESOM mesh directory containing nod2d.out and elem2d.out.
    alpha, beta, gamma : float
        Euler rotation angles (degrees). Only used when force_rotation is True.
    force_rotation : bool
        Whether the mesh uses a rotated grid that needs to be unrotated.

    Returns
    -------
    tuple[np.ndarray, np.ndarray, np.ndarray]
        (lon, lat, elem) where lon/lat are 1D arrays of node coordinates
        and elem is (n_triangles, 3) 0-based connectivity.
    """
    meshdir = Path(meshpath)
    nod2d_path = meshdir / "nod2d.out"
    elem2d_path = meshdir / "elem2d.out"

    if not nod2d_path.exists():
        raise FileNotFoundError(f"FESOM mesh file not found: {nod2d_path}")
    if not elem2d_path.exists():
        raise FileNotFoundError(f"FESOM mesh file not found: {elem2d_path}")

    # Read nod2d.out: first line is count, then "id lon lat flag"
    logger.info(f"Loading FESOM mesh from {meshpath}")
    nod2d = np.loadtxt(str(nod2d_path), skiprows=1)
    lon = nod2d[:, 1].astype(np.float64)
    lat = nod2d[:, 2].astype(np.float64)

    # Read elem2d.out: first line is count, then "node1 node2 node3" (1-based)
    elem = np.loadtxt(str(elem2d_path), skiprows=1, dtype=int) - 1  # convert to 0-based

    # Apply inverse Euler rotation if needed
    if force_rotation and (alpha != 0 or beta != 0 or gamma != 0):
        logger.info(f"Applying inverse Euler rotation: alpha={alpha}, beta={beta}, gamma={gamma}")
        lon, lat = _euler_rotation(lon, lat, alpha, beta, gamma)

    logger.info(f"Loaded mesh: {len(lon)} nodes, {len(elem)} elements")
    return lon, lat, elem


def get_mesh_from_stac_item(item: dict) -> tuple[np.ndarray, np.ndarray, np.ndarray] | None:
    """
    Load a FESOM mesh using metadata from a STAC item's properties.

    Extracts the mesh path from ``nml:fesom:paths:meshpath`` or
    ``file:FESOM_MeshPath``, and rotation parameters from the
    ``nml:fesom:geometry:*`` properties.

    Parameters
    ----------
    item : dict
        STAC item with properties containing mesh metadata.

    Returns
    -------
    tuple or None
        (lon, lat, elem) if mesh can be loaded, None otherwise.
    """
    props = item.get("properties", {})

    meshpath = props.get("nml:fesom:paths:meshpath") or props.get("file:FESOM_MeshPath")
    if not meshpath:
        logger.warning("No mesh path found in STAC item properties")
        return None

    force_rotation_raw = props.get("nml:fesom:geometry:force_rotation")
    # FESOM uses -1 for true in some contexts, or actual bool
    if isinstance(force_rotation_raw, bool):
        force_rotation = force_rotation_raw
    elif isinstance(force_rotation_raw, (int, float)):
        force_rotation = bool(force_rotation_raw)  # -1 -> True, 0 -> False
    else:
        force_rotation = False

    alpha = float(props.get("nml:fesom:geometry:alphaeuler", 0))
    beta = float(props.get("nml:fesom:geometry:betaeuler", 0))
    gamma = float(props.get("nml:fesom:geometry:gammaeuler", 0))

    try:
        return load_fesom_mesh(meshpath, alpha, beta, gamma, force_rotation)
    except FileNotFoundError as e:
        logger.error(f"Could not load FESOM mesh: {e}")
        return None


# ---------------------------------------------------------------------------
# Cyclic element filtering
# ---------------------------------------------------------------------------

def _trim_mesh_to_data(lon: np.ndarray, lat: np.ndarray, elem: np.ndarray,
                       n_data: int) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Trim mesh to match data size when there's a mismatch.

    FESOM mesh files (nod2d.out) sometimes include extra boundary nodes
    that aren't present in the output data. This filters elements that
    reference nodes beyond the data array size and truncates coordinates.
    """
    n_mesh = len(lon)
    if n_mesh == n_data:
        return lon, lat, elem

    logger.warning(f"Mesh/data size mismatch: mesh has {n_mesh} nodes, data has {n_data} values")

    if n_mesh > n_data:
        # Filter out elements that reference nodes beyond the data size
        valid_mask = np.all(elem < n_data, axis=1)
        elem = elem[valid_mask]
        lon = lon[:n_data]
        lat = lat[:n_data]
        n_removed = (~valid_mask).sum()
        logger.info(f"Trimmed mesh: removed {n_removed} elements referencing extra nodes")

    return lon, lat, elem


def _remove_cyclic_elements(lon: np.ndarray, elem: np.ndarray,
                            threshold: float = 100.0) -> np.ndarray:
    """
    Remove triangles that span the antimeridian (cyclic boundary).

    These triangles create visual artifacts -- long horizontal streaks
    across the entire plot.
    """
    elem_lon = lon[elem]
    lon_range = elem_lon.max(axis=1) - elem_lon.min(axis=1)
    mask = lon_range < threshold
    n_removed = (~mask).sum()
    if n_removed > 0:
        logger.debug(f"Removed {n_removed} cyclic elements")
    return elem[mask]


# ---------------------------------------------------------------------------
# Static (matplotlib) plotting
# ---------------------------------------------------------------------------

def plot_unstructured(
    data_array: xr.DataArray,
    cmap: str = "viridis",
    ds: xr.Dataset | None = None,
    figsize: tuple[int, int] = (12, 8),
    add_coastlines: bool = True,
    title: str | None = None,
    vmin: float | None = None,
    vmax: float | None = None,
    stac_item: dict | None = None,
) -> plt.Figure:
    """
    Create a matplotlib plot from unstructured FESOM mesh data.

    Uses the STAC item properties to locate the mesh files (nod2d.out,
    elem2d.out) and rotation parameters. Falls back to scatter plot
    if mesh cannot be loaded.

    Parameters
    ----------
    data_array : xr.DataArray
        Data on unstructured mesh nodes (1D spatial dimension).
    cmap : str
        Matplotlib colormap name.
    ds : xr.Dataset, optional
        Parent dataset (unused, kept for API compatibility).
    stac_item : dict, optional
        STAC item containing mesh path in properties.
    """
    matplotlib.use("Agg")

    fig, ax = plt.subplots(
        figsize=figsize,
        subplot_kw={"projection": ccrs.Robinson()},
    )

    values = data_array.values
    if hasattr(values, "compute"):
        values = values.compute()
    values = values.ravel()

    # Try to load mesh from STAC item
    mesh = None
    if stac_item is not None:
        mesh = get_mesh_from_stac_item(stac_item)

    if mesh is not None:
        lon, lat, elem = mesh
        lon, lat, elem = _trim_mesh_to_data(lon, lat, elem, len(values))
        elem_clean = _remove_cyclic_elements(lon, elem)

        try:
            triang = tri.Triangulation(lon, lat, elem_clean)

            # Compute element-mean values for flat shading
            elem_values = values[elem_clean].mean(axis=1)

            im = ax.tripcolor(
                triang,
                facecolors=elem_values,
                cmap=cmap,
                vmin=vmin,
                vmax=vmax,
                transform=ccrs.PlateCarree(),
                shading="flat",
            )
            ax.set_global()
            logger.info("Created triangulated FESOM plot")
        except Exception as e:
            logger.warning(f"Triangulation failed: {e}, using scatter")
            im = ax.scatter(
                lon, lat, c=values, cmap=cmap, s=0.5,
                vmin=vmin, vmax=vmax, transform=ccrs.PlateCarree(),
            )
            ax.set_global()
    else:
        logger.warning("No mesh available -- falling back to index-based plot")
        ax = fig.add_subplot(111)
        im = ax.plot(values)
        add_coastlines = False

    if add_coastlines:
        try:
            ax.add_feature(cfeature.COASTLINE, linewidth=0.5, edgecolor="black")
            ax.add_feature(cfeature.LAND, facecolor="lightgray", alpha=0.3)
        except Exception as e:
            logger.warning(f"Could not add coastlines: {e}")

    if hasattr(im, "colorbar") or not isinstance(im, list):
        try:
            cbar = fig.colorbar(im, ax=ax, orientation="horizontal", pad=0.05, shrink=0.8)
            if "units" in data_array.attrs:
                cbar.set_label(data_array.attrs["units"])
        except Exception:
            pass

    if title is None:
        title = data_array.attrs.get("long_name", data_array.name or "FESOM Data")
    ax.set_title(title, fontsize=12)

    plt.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Interactive (Panel/GeoViews) plotting
# ---------------------------------------------------------------------------

def create_interactive_fesom_plot(
    data_array: xr.DataArray,
    lon: np.ndarray,
    lat: np.ndarray,
    elem: np.ndarray,
    cmap: str = "viridis",
    title: str | None = None,
):
    """
    Create an interactive GeoViews TriMesh + datashader plot for FESOM data.

    This produces the same kind of visualization as the AWI-ESM documentation
    notebooks: rasterized triangulated data with proper geographic projection.

    Parameters
    ----------
    data_array : xr.DataArray
        1D data on mesh nodes.
    lon, lat : np.ndarray
        Node coordinates (geographic, after rotation).
    elem : np.ndarray
        Element connectivity (n_triangles, 3), 0-based.
    cmap : str
        Colormap name (matplotlib or colorcet).

    Returns
    -------
    holoviews.Element
        A rasterized TriMesh suitable for Panel serving.
    """
    import datashader as ds_module
    import geoviews as gv
    import holoviews as hv
    import pandas as pd
    from holoviews.operation.datashader import rasterize

    hv.extension("bokeh")

    values = data_array.values
    if hasattr(values, "compute"):
        values = values.compute()
    values = values.ravel()

    var_name = data_array.name or "value"
    long_name = data_array.attrs.get("long_name", var_name)
    units = data_array.attrs.get("units", "")

    if title is None:
        title = f"{long_name}" + (f" [{units}]" if units else "")

    # Trim mesh to match data size (FESOM mesh files may have extra nodes)
    lon, lat, elem = _trim_mesh_to_data(lon, lat, elem, len(values))

    # Remove cyclic elements
    elem_clean = _remove_cyclic_elements(lon, elem)

    # Compute element-mean values for flat shading
    elem_values = values[elem_clean].mean(axis=1)

    resolved_cmap = _resolve_cmap(cmap, var_name)
    projection = ccrs.Robinson()

    # Build TriMesh: elem_df columns are v0, v1, v2, value
    elem_df = pd.DataFrame(
        np.column_stack([elem_clean, elem_values]),
        columns=["v0", "v1", "v2", var_name],
    )
    elem_df[["v0", "v1", "v2"]] = elem_df[["v0", "v1", "v2"]].astype(int)

    nodes = gv.Points((lon, lat))
    trimesh = gv.TriMesh((elem_df, nodes)).redim(
        x="Longitude", y="Latitude",
    )

    projected = gv.project(trimesh, projection=projection)

    plot = rasterize(projected).opts(
        cmap=resolved_cmap,
        height=500,
        width=800,
        projection=projection,
        colorbar=True,
        colorbar_position="bottom",
        clabel=f"{long_name} ({units})" if units else long_name,
        bgcolor="darkgray",
        color_levels=25,
        title=title,
        tools=["hover", "wheel_zoom", "pan", "reset", "save"],
    )

    return plot


def _resolve_cmap(cmap: str, var_name: str) -> Any:
    """Resolve a colormap name, with smart defaults for climate variables."""
    # Smart defaults based on variable name
    climate_cmaps = {
        "sst": "thermal", "temp": "thermal", "temperature": "thermal",
        "salt": "haline", "sss": "haline", "salinity": "haline",
        "ssh": "balance", "zos": "balance",
        "u_ice": "balance", "v_ice": "balance",
        "m_ice": "ice", "a_ice": "ice",
        "precip": "rain", "evap": "rain",
    }

    if cmap == "viridis":
        # Only override default; if user chose a specific cmap, respect it
        cmap = climate_cmaps.get(var_name.lower(), cmap)

    try:
        import cmocean.cm as cmo
        if hasattr(cmo, cmap):
            return getattr(cmo, cmap)
    except ImportError:
        pass

    return cmap
