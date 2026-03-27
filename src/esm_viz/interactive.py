"""
Interactive visualization module using Panel/hvplot.

This module provides interactive web-based visualization apps for
exploring climate datasets in the browser using Panel, hvplot, and geoviews.
"""

from typing import Any

import numpy as np
import panel as pn
import xarray as xr
from loguru import logger

# Enable Panel extensions
pn.extension("bokeh")

# Attempt to import hvplot and geoviews - these are required for full functionality
try:
    import hvplot.xarray  # noqa: F401 - registers hvplot accessor
    import holoviews as hv
    import geoviews as gv
    import geoviews.feature as gf
    from cartopy import crs as ccrs

    HAS_GEOVIEWS = True
except ImportError as e:
    logger.warning(f"GeoViews not available, some features will be limited: {e}")
    HAS_GEOVIEWS = False

# Attempt to import cmocean colormaps
try:
    import cmocean.cm as cmo

    HAS_CMOCEAN = True
except ImportError:
    logger.warning("cmocean not available, using fallback colormaps")
    HAS_CMOCEAN = False


# Smart colormap mapping based on variable names
VARIABLE_COLORMAP_MAPPING: dict[str, str] = {
    # Temperature variables
    "temp": "cmo.thermal" if HAS_CMOCEAN else "inferno",
    "temperature": "cmo.thermal" if HAS_CMOCEAN else "inferno",
    "sst": "cmo.thermal" if HAS_CMOCEAN else "inferno",
    "t2m": "cmo.thermal" if HAS_CMOCEAN else "inferno",
    "tas": "cmo.thermal" if HAS_CMOCEAN else "inferno",
    "thetao": "cmo.thermal" if HAS_CMOCEAN else "inferno",
    # Salinity variables
    "salt": "cmo.haline" if HAS_CMOCEAN else "viridis",
    "salinity": "cmo.haline" if HAS_CMOCEAN else "viridis",
    "sss": "cmo.haline" if HAS_CMOCEAN else "viridis",
    "so": "cmo.haline" if HAS_CMOCEAN else "viridis",
    "sos": "cmo.haline" if HAS_CMOCEAN else "viridis",
    # Sea surface height / dynamic topography
    "ssh": "cmo.balance" if HAS_CMOCEAN else "RdBu_r",
    "zos": "cmo.balance" if HAS_CMOCEAN else "RdBu_r",
    "sea_surface_height": "cmo.balance" if HAS_CMOCEAN else "RdBu_r",
    "adt": "cmo.balance" if HAS_CMOCEAN else "RdBu_r",
    # Velocity / speed
    "u": "cmo.speed" if HAS_CMOCEAN else "plasma",
    "v": "cmo.speed" if HAS_CMOCEAN else "plasma",
    "speed": "cmo.speed" if HAS_CMOCEAN else "plasma",
    "uo": "cmo.speed" if HAS_CMOCEAN else "plasma",
    "vo": "cmo.speed" if HAS_CMOCEAN else "plasma",
    "current": "cmo.speed" if HAS_CMOCEAN else "plasma",
    # Ice variables
    "ice": "cmo.ice" if HAS_CMOCEAN else "Blues_r",
    "sic": "cmo.ice" if HAS_CMOCEAN else "Blues_r",
    "a_ice": "cmo.ice" if HAS_CMOCEAN else "Blues_r",
    "siconc": "cmo.ice" if HAS_CMOCEAN else "Blues_r",
    "ice_concentration": "cmo.ice" if HAS_CMOCEAN else "Blues_r",
    "m_ice": "cmo.ice" if HAS_CMOCEAN else "Blues_r",
    # Density
    "rho": "cmo.dense" if HAS_CMOCEAN else "viridis",
    "density": "cmo.dense" if HAS_CMOCEAN else "viridis",
    # Chlorophyll / bio
    "chl": "cmo.algae" if HAS_CMOCEAN else "Greens",
    "chlorophyll": "cmo.algae" if HAS_CMOCEAN else "Greens",
    # Oxygen
    "o2": "cmo.oxy" if HAS_CMOCEAN else "YlGnBu",
    "oxygen": "cmo.oxy" if HAS_CMOCEAN else "YlGnBu",
    # Bathymetry / depth
    "bathymetry": "cmo.deep" if HAS_CMOCEAN else "Blues",
    "depth": "cmo.deep" if HAS_CMOCEAN else "Blues",
    # Wind
    "wind": "cmo.amp" if HAS_CMOCEAN else "magma",
    "wspd": "cmo.amp" if HAS_CMOCEAN else "magma",
}

DEFAULT_COLORMAP = "viridis"

# Available colormaps for the picker widget
AVAILABLE_COLORMAPS = [
    "viridis",
    "plasma",
    "inferno",
    "magma",
    "cividis",
    "RdBu_r",
    "coolwarm",
    "Blues",
    "Reds",
    "Greens",
]

if HAS_CMOCEAN:
    AVAILABLE_COLORMAPS = [
        "cmo.thermal",
        "cmo.haline",
        "cmo.balance",
        "cmo.speed",
        "cmo.ice",
        "cmo.dense",
        "cmo.algae",
        "cmo.deep",
        "cmo.amp",
        "cmo.oxy",
    ] + AVAILABLE_COLORMAPS


def get_smart_colormap(variable_name: str) -> str:
    """
    Select an appropriate colormap based on the variable name.

    Uses pattern matching to identify common climate variable types
    and returns a scientifically appropriate colormap.

    Parameters
    ----------
    variable_name : str
        Name of the variable to get colormap for.

    Returns
    -------
    str
        Colormap name suitable for the variable.
    """
    var_lower = variable_name.lower()

    # Direct match
    if var_lower in VARIABLE_COLORMAP_MAPPING:
        return VARIABLE_COLORMAP_MAPPING[var_lower]

    # Pattern matching for partial matches
    for pattern, cmap in VARIABLE_COLORMAP_MAPPING.items():
        if pattern in var_lower:
            return cmap

    return DEFAULT_COLORMAP


def _get_colormap_object(cmap_name: str) -> Any:
    """
    Get the actual colormap object from a name string.

    Handles both matplotlib and cmocean colormap names.

    Parameters
    ----------
    cmap_name : str
        Colormap name (e.g., 'viridis', 'cmo.thermal').

    Returns
    -------
    Any
        Colormap object usable by hvplot/holoviews.
    """
    if cmap_name.startswith("cmo.") and HAS_CMOCEAN:
        cmap_attr = cmap_name.replace("cmo.", "")
        return getattr(cmo, cmap_attr, None)
    return cmap_name


def _get_plottable_variables(ds: xr.Dataset) -> list[str]:
    """
    Get list of variables suitable for plotting from a dataset.

    Filters out coordinate variables and non-numeric data.

    Parameters
    ----------
    ds : xr.Dataset
        The xarray Dataset to analyze.

    Returns
    -------
    list[str]
        List of variable names that can be plotted.
    """
    variables = []
    for var_name, var_data in ds.data_vars.items():
        # Skip non-numeric types
        if var_data.dtype.kind not in ("i", "u", "f"):
            continue
        # Skip 0D or 1D variables (likely scalars or coordinates)
        if var_data.ndim < 2:
            continue
        variables.append(var_name)

    return sorted(variables)


def _get_variable_display_names(ds: xr.Dataset, variables: list[str]) -> dict[str, str]:
    """
    Create a mapping of display names to variable names.

    Display names show both short name and long name for clarity.

    Parameters
    ----------
    ds : xr.Dataset
        The xarray Dataset containing the variables.
    variables : list[str]
        List of variable names to create display names for.

    Returns
    -------
    dict[str, str]
        Mapping of display name -> variable name.
    """
    display_map = {}
    for var_name in variables:
        var_data = ds[var_name]
        long_name = var_data.attrs.get("long_name", "")
        units = var_data.attrs.get("units", "")

        if long_name:
            if units:
                display_name = f"{var_name} ({long_name}, {units})"
            else:
                display_name = f"{var_name} ({long_name})"
        else:
            display_name = var_name

        display_map[display_name] = var_name

    return display_map


def _select_time_slice(
    data_array: xr.DataArray, time_index: int
) -> xr.DataArray:
    """
    Select a time slice from a data array if it has a time dimension.

    Parameters
    ----------
    data_array : xr.DataArray
        Input data array.
    time_index : int
        Time index to select.

    Returns
    -------
    xr.DataArray
        Data array with time dimension reduced.
    """
    time_dims = ["time", "t"]
    for dim in time_dims:
        if dim in data_array.dims:
            max_time = data_array.sizes[dim] - 1
            safe_index = min(time_index, max_time)
            return data_array.isel({dim: safe_index})
    return data_array


def _select_level_slice(
    data_array: xr.DataArray, level_index: int
) -> xr.DataArray:
    """
    Select a level/depth slice from a data array if present.

    Parameters
    ----------
    data_array : xr.DataArray
        Input data array.
    level_index : int
        Level index to select.

    Returns
    -------
    xr.DataArray
        Data array with level dimension reduced.
    """
    level_dims = ["level", "lev", "depth", "z", "nz", "nz1"]
    for dim in level_dims:
        if dim in data_array.dims:
            max_level = data_array.sizes[dim] - 1
            safe_index = min(level_index, max_level)
            return data_array.isel({dim: safe_index})
    return data_array


def _get_time_steps(ds: xr.Dataset) -> int:
    """
    Get the number of time steps in a dataset.

    Parameters
    ----------
    ds : xr.Dataset
        The xarray Dataset.

    Returns
    -------
    int
        Number of time steps, or 1 if no time dimension.
    """
    time_dims = ["time", "t"]
    for dim in time_dims:
        if dim in ds.dims:
            return ds.sizes[dim]
    return 1


def _get_level_steps(ds: xr.Dataset, variable: str) -> int:
    """
    Get the number of levels for a specific variable.

    Parameters
    ----------
    ds : xr.Dataset
        The xarray Dataset.
    variable : str
        Variable name to check.

    Returns
    -------
    int
        Number of levels, or 1 if no level dimension.
    """
    if variable not in ds.data_vars:
        return 1

    level_dims = ["level", "lev", "depth", "z", "nz", "nz1"]
    for dim in level_dims:
        if dim in ds[variable].dims:
            return ds[variable].sizes[dim]
    return 1


def _create_gridded_plot(
    data_array: xr.DataArray,
    cmap: str,
    projection: Any | None = None,
) -> Any:
    """
    Create an interactive plot for gridded (regular lat/lon) data.

    Parameters
    ----------
    data_array : xr.DataArray
        2D data array to plot.
    cmap : str
        Colormap name.
    projection : Any, optional
        Cartopy projection for geoviews plots.

    Returns
    -------
    Any
        HoloViews/GeoViews plot element.
    """
    cmap_obj = _get_colormap_object(cmap)

    # Determine if we have lat/lon coordinates
    has_geo_coords = any(
        coord in data_array.coords for coord in ["lat", "latitude", "lon", "longitude"]
    )

    if HAS_GEOVIEWS and has_geo_coords:
        # Use GeoViews for proper geographic projection
        if projection is None:
            projection = ccrs.Robinson()

        # Create quadmesh with geographic projection
        plot = data_array.hvplot.quadmesh(
            x="lon" if "lon" in data_array.coords else "longitude",
            y="lat" if "lat" in data_array.coords else "latitude",
            cmap=cmap_obj,
            geo=True,
            projection=projection,
            coastline=True,
            frame_width=700,
            frame_height=400,
            colorbar=True,
            title=data_array.attrs.get("long_name", data_array.name or "Data"),
        )
    elif data_array.ndim >= 2:
        # Fall back to regular hvplot quadmesh for 2D+ data
        try:
            plot = data_array.hvplot.quadmesh(
                cmap=cmap_obj,
                frame_width=700,
                frame_height=400,
                colorbar=True,
                title=data_array.attrs.get("long_name", data_array.name or "Data"),
            )
        except Exception:
            # quadmesh failed (e.g. no proper grid coordinates), try image
            plot = data_array.hvplot(
                kind="image",
                cmap=cmap_obj,
                frame_width=700,
                frame_height=400,
                colorbar=True,
                title=data_array.attrs.get("long_name", data_array.name or "Data"),
            )
    else:
        # 1D data or flat arrays -- line plot
        plot = data_array.hvplot.line(
            frame_width=700,
            frame_height=400,
            title=data_array.attrs.get("long_name", data_array.name or "Data"),
        )

    return plot


def _create_unstructured_plot(
    data_array: xr.DataArray,
    ds: xr.Dataset,
    cmap: str,
    projection: Any | None = None,
) -> Any:
    """
    Create an interactive plot for unstructured mesh data (e.g., FESOM).

    Uses GeoViews TriMesh for proper triangulated mesh rendering.

    Parameters
    ----------
    data_array : xr.DataArray
        Data array from unstructured mesh.
    ds : xr.Dataset
        Parent dataset containing mesh information.
    cmap : str
        Colormap name.
    projection : Any, optional
        Cartopy projection.

    Returns
    -------
    Any
        HoloViews/GeoViews plot element.
    """
    from esm_viz.fesom import _get_mesh_coordinates, _get_triangulation

    cmap_obj = _get_colormap_object(cmap)

    # Get mesh coordinates
    lon_vals, lat_vals = _get_mesh_coordinates(ds)
    triangles = _get_triangulation(ds)

    if lon_vals is None or lat_vals is None:
        logger.warning("Could not find mesh coordinates for unstructured plot")
        # Fall back to simple scatter plot
        values = data_array.values.ravel()
        return hv.Points(
            np.column_stack([np.arange(len(values)), values]),
            label="Unstructured Data (no coordinates)",
        )

    # Load and flatten data
    values = data_array.values
    if hasattr(values, "compute"):
        values = values.compute()
    values = values.ravel()

    lon_vals = lon_vals.ravel()
    lat_vals = lat_vals.ravel()

    if len(lon_vals) != len(values):
        logger.warning(
            f"Size mismatch: coords={len(lon_vals)}, values={len(values)}. "
            "Using scatter plot fallback."
        )
        # Scatter plot fallback
        if HAS_GEOVIEWS:
            points = gv.Points(
                (lon_vals[:len(values)], lat_vals[:len(values)], values[:len(lon_vals)]),
                vdims=["value"],
            )
            return points.opts(
                color="value",
                cmap=cmap_obj,
                size=2,
                colorbar=True,
                projection=projection or ccrs.Robinson(),
                global_extent=True,
            ) * gf.coastline()
        else:
            return hv.Points(
                (lon_vals[:len(values)], lat_vals[:len(values)], values[:len(lon_vals)]),
                vdims=["value"],
            ).opts(color="value", cmap=cmap_obj, size=2, colorbar=True)

    if triangles is not None and HAS_GEOVIEWS:
        # Create proper TriMesh with triangulation
        # Adjust indices if 1-based
        if triangles.min() == 1:
            triangles = triangles - 1

        try:
            # Build node data (vertices with values)
            nodes = gv.Dataset(
                (lon_vals, lat_vals, np.arange(len(lon_vals)), values),
                ["lon", "lat", "index", "value"],
            )

            # Build trimesh
            trimesh = gv.TriMesh((triangles, nodes), vdims=["value"])

            if projection is None:
                projection = ccrs.Robinson()

            plot = trimesh.opts(
                cmap=cmap_obj,
                colorbar=True,
                projection=projection,
                global_extent=True,
                frame_width=700,
                frame_height=400,
                title=data_array.attrs.get("long_name", data_array.name or "Data"),
            ) * gf.coastline()

            return plot

        except Exception as e:
            logger.warning(f"TriMesh creation failed: {e}. Using scatter fallback.")

    # Scatter plot fallback
    if HAS_GEOVIEWS:
        points = gv.Points(
            (lon_vals, lat_vals, values),
            vdims=["value"],
        )
        return points.opts(
            color="value",
            cmap=cmap_obj,
            size=2,
            colorbar=True,
            projection=projection or ccrs.Robinson(),
            global_extent=True,
            frame_width=700,
            frame_height=400,
            title=data_array.attrs.get("long_name", data_array.name or "Data"),
        ) * gf.coastline()
    else:
        return hv.Points(
            (lon_vals, lat_vals, values),
            vdims=["value"],
        ).opts(
            color="value",
            cmap=cmap_obj,
            size=2,
            colorbar=True,
            frame_width=700,
            frame_height=400,
            title=data_array.attrs.get("long_name", data_array.name or "Data"),
        )


def create_preview_app(
    href: str,
    variable: str | None = None,
    **kwargs: Any,
) -> pn.Column:
    """
    Create an interactive Panel app for exploring climate data.

    Parameters
    ----------
    href : str
        Path or URI to the data file.
    variable : str, optional
        Initial variable to display. If None, uses the first available variable.
    **kwargs : Any
        Additional configuration options for the app.

    Returns
    -------
    pn.Column
        A Panel Column containing controls and interactive plot.

    Examples
    --------
    >>> app = create_preview_app("/path/to/data.nc")
    >>> app.servable()  # Serve in Panel server

    >>> # Or embed in FastAPI
    >>> pn.io.fastapi.add_application("/app", create_preview_app, href=href)
    """
    from esm_viz.readers import open_data
    from esm_viz.fesom import is_unstructured

    logger.info(f"Creating interactive preview app for: {href}")

    try:
        # Open the dataset
        ds = open_data(href)
    except Exception as e:
        logger.error(f"Failed to open dataset: {e}")
        return pn.Column(
            pn.pane.Alert(
                f"Failed to open dataset: {e}",
                alert_type="danger",
            ),
            sizing_mode="stretch_width",
        )

    # Get plottable variables
    variables = _get_plottable_variables(ds)
    if not variables:
        return pn.Column(
            pn.pane.Alert(
                "No plottable variables found in dataset.",
                alert_type="warning",
            ),
            sizing_mode="stretch_width",
        )

    # Create display name mapping (shows long names alongside short names)
    var_display_map = _get_variable_display_names(ds, variables)
    # Reverse map to get variable name from display name
    display_to_var = var_display_map
    var_to_display = {v: k for k, v in var_display_map.items()}

    # Determine initial variable
    initial_var = variable if variable in variables else variables[0]
    initial_display = var_to_display.get(initial_var, initial_var)

    # Detect if unstructured mesh
    unstructured = is_unstructured(ds)
    mesh_type = "Unstructured Mesh (FESOM)" if unstructured else "Gridded"
    logger.info(f"Detected mesh type: {mesh_type}")

    # Get time and level info
    n_times = _get_time_steps(ds)
    n_levels = _get_level_steps(ds, initial_var)

    # Determine smart colormap for initial variable
    initial_cmap = get_smart_colormap(initial_var)

    # Create widgets
    var_selector = pn.widgets.Select(
        name="Variable",
        options=list(var_display_map.keys()),
        value=initial_display,
        width=300,  # Wider to accommodate long names
    )

    time_slider = pn.widgets.IntSlider(
        name="Time Step",
        start=0,
        end=max(0, n_times - 1),
        value=0,
        width=300,
        disabled=(n_times <= 1),
    )

    level_slider = pn.widgets.IntSlider(
        name="Level/Depth",
        start=0,
        end=max(0, n_levels - 1),
        value=0,
        width=300,
        disabled=(n_levels <= 1),
    )

    cmap_selector = pn.widgets.Select(
        name="Colormap",
        options=AVAILABLE_COLORMAPS,
        value=initial_cmap if initial_cmap in AVAILABLE_COLORMAPS else DEFAULT_COLORMAP,
        width=150,
    )

    # Info panel
    info_md = pn.pane.Markdown(
        f"""
**Dataset Info:**
- File: `{href.split('/')[-1]}`
- Mesh Type: {mesh_type}
- Variables: {len(variables)}
- Time Steps: {n_times}
        """,
        width=250,
    )

    # Create reactive plot function
    @pn.depends(
        var_selector.param.value,
        time_slider.param.value,
        level_slider.param.value,
        cmap_selector.param.value,
    )
    def update_plot(var_display: str, time_idx: int, level_idx: int, cmap: str) -> Any:
        """Reactive function to update plot based on widget values."""
        try:
            # Convert display name back to variable name
            var = display_to_var.get(var_display, var_display)
            # Get data array
            data_array = ds[var]

            # Apply selections
            data_array = _select_time_slice(data_array, time_idx)
            data_array = _select_level_slice(data_array, level_idx)

            # Create plot based on mesh type
            if unstructured:
                plot = _create_unstructured_plot(
                    data_array, ds, cmap, projection=ccrs.Robinson() if HAS_GEOVIEWS else None
                )
            else:
                plot = _create_gridded_plot(
                    data_array, cmap, projection=ccrs.Robinson() if HAS_GEOVIEWS else None
                )

            return plot

        except Exception as e:
            logger.error(f"Plot generation failed: {e}")
            return pn.pane.Alert(
                f"Plot generation failed: {e}",
                alert_type="danger",
            )

    # Update level slider when variable changes
    @pn.depends(var_selector.param.value, watch=True)
    def update_level_slider(var_display: str) -> None:
        """Update level slider range when variable changes."""
        var = display_to_var.get(var_display, var_display)
        n_lvls = _get_level_steps(ds, var)
        level_slider.end = max(0, n_lvls - 1)
        level_slider.disabled = (n_lvls <= 1)
        if level_slider.value > level_slider.end:
            level_slider.value = 0

    # Update colormap when variable changes (smart selection)
    @pn.depends(var_selector.param.value, watch=True)
    def update_colormap(var_display: str) -> None:
        """Update colormap based on variable name (smart selection)."""
        var = display_to_var.get(var_display, var_display)
        smart_cmap = get_smart_colormap(var)
        if smart_cmap in AVAILABLE_COLORMAPS:
            cmap_selector.value = smart_cmap

    # Build layout
    controls = pn.Column(
        pn.pane.Markdown("## Controls"),
        var_selector,
        time_slider,
        level_slider,
        cmap_selector,
        pn.layout.Divider(),
        info_md,
        width=280,
    )

    plot_pane = pn.panel(update_plot, sizing_mode="stretch_both")

    app = pn.Column(
        pn.pane.Markdown(f"# ESM-Viz Interactive Preview"),
        pn.Row(
            controls,
            plot_pane,
            sizing_mode="stretch_both",
        ),
        sizing_mode="stretch_both",
    )

    return app


def create_comparison_app(
    href_a: str,
    href_b: str,
    variable: str | None = None,
    **kwargs: Any,
) -> pn.Column:
    """
    Create an interactive comparison app for two datasets.

    Parameters
    ----------
    href_a : str
        Path or URI to the first data file.
    href_b : str
        Path or URI to the second data file.
    variable : str, optional
        Variable to compare.
    **kwargs : Any
        Additional configuration options.

    Returns
    -------
    pn.Column
        Panel Column with side-by-side comparison view.

    Notes
    -----
    This creates a side-by-side view of two datasets with synchronized
    controls for time, level, and colormap selection.
    """
    from esm_viz.readers import open_data
    from esm_viz.fesom import is_unstructured

    logger.info(f"Creating comparison app: {href_a} vs {href_b}")

    try:
        ds_a = open_data(href_a)
        ds_b = open_data(href_b)
    except Exception as e:
        logger.error(f"Failed to open datasets: {e}")
        return pn.Column(
            pn.pane.Alert(
                f"Failed to open datasets: {e}",
                alert_type="danger",
            ),
            sizing_mode="stretch_width",
        )

    # Find common variables
    vars_a = set(_get_plottable_variables(ds_a))
    vars_b = set(_get_plottable_variables(ds_b))
    common_vars = sorted(vars_a & vars_b)

    if not common_vars:
        return pn.Column(
            pn.pane.Alert(
                "No common plottable variables found between datasets.",
                alert_type="warning",
            ),
            sizing_mode="stretch_width",
        )

    initial_var = variable if variable in common_vars else common_vars[0]
    unstructured_a = is_unstructured(ds_a)
    unstructured_b = is_unstructured(ds_b)

    # Get time steps (use minimum)
    n_times = min(_get_time_steps(ds_a), _get_time_steps(ds_b))

    # Widgets
    var_selector = pn.widgets.Select(
        name="Variable",
        options=common_vars,
        value=initial_var,
        width=200,
    )

    time_slider = pn.widgets.IntSlider(
        name="Time Step",
        start=0,
        end=max(0, n_times - 1),
        value=0,
        width=300,
        disabled=(n_times <= 1),
    )

    cmap_selector = pn.widgets.Select(
        name="Colormap",
        options=AVAILABLE_COLORMAPS,
        value=get_smart_colormap(initial_var),
        width=150,
    )

    @pn.depends(var_selector.param.value, time_slider.param.value, cmap_selector.param.value)
    def plot_a(var: str, time_idx: int, cmap: str) -> Any:
        try:
            data_array = _select_time_slice(ds_a[var], time_idx)
            if unstructured_a:
                return _create_unstructured_plot(data_array, ds_a, cmap)
            return _create_gridded_plot(data_array, cmap)
        except Exception as e:
            return pn.pane.Alert(f"Error: {e}", alert_type="danger")

    @pn.depends(var_selector.param.value, time_slider.param.value, cmap_selector.param.value)
    def plot_b(var: str, time_idx: int, cmap: str) -> Any:
        try:
            data_array = _select_time_slice(ds_b[var], time_idx)
            if unstructured_b:
                return _create_unstructured_plot(data_array, ds_b, cmap)
            return _create_gridded_plot(data_array, cmap)
        except Exception as e:
            return pn.pane.Alert(f"Error: {e}", alert_type="danger")

    controls = pn.Row(var_selector, time_slider, cmap_selector)

    app = pn.Column(
        pn.pane.Markdown("# ESM-Viz Dataset Comparison"),
        controls,
        pn.Row(
            pn.Column(
                pn.pane.Markdown(f"**Dataset A:** `{href_a.split('/')[-1]}`"),
                pn.panel(plot_a, sizing_mode="stretch_both"),
            ),
            pn.Column(
                pn.pane.Markdown(f"**Dataset B:** `{href_b.split('/')[-1]}`"),
                pn.panel(plot_b, sizing_mode="stretch_both"),
            ),
            sizing_mode="stretch_both",
        ),
        sizing_mode="stretch_both",
    )

    return app
