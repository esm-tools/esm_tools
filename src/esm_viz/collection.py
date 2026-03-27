"""
Collection-level interactive visualization using xarray.DataTree.

Builds a DataTree from all items in a STAC collection, grouped by
variable name, and provides an interactive Panel app for exploring
the entire collection with time-averaging, anomaly views, and
per-variable depth/level selection.
"""

from __future__ import annotations

import hashlib
from collections import defaultdict
from functools import lru_cache
from typing import Any
from urllib.parse import urlparse

import httpx
import numpy as np
import panel as pn
import xarray as xr
from loguru import logger

from esm_viz.fesom import (
    create_interactive_fesom_plot,
    get_mesh_from_stac_item,
    is_unstructured,
    load_fesom_mesh,
)
from esm_viz.interactive import (
    AVAILABLE_COLORMAPS,
    DEFAULT_COLORMAP,
    _create_gridded_plot,
    _get_colormap_object,
    get_smart_colormap,
)

try:
    import hvplot.xarray  # noqa: F401
    import holoviews as hv
    import geoviews as gv
    from cartopy import crs as ccrs

    HAS_GEOVIEWS = True
except ImportError:
    HAS_GEOVIEWS = False


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _strip_file_uri(href: str) -> str:
    """Convert ``file://`` URIs to plain filesystem paths."""
    parsed = urlparse(href)
    if parsed.scheme == "file":
        return parsed.path
    return href


def _collection_cache_key(collection_id: str, stac_api: str) -> str:
    """Produce a hashable key for the LRU cache."""
    raw = f"{stac_api.rstrip('/')}|{collection_id}"
    return hashlib.sha256(raw.encode()).hexdigest()


# ---------------------------------------------------------------------------
# STAC fetching
# ---------------------------------------------------------------------------


def _fetch_collection_items(
    collection_id: str, stac_api: str, limit: int = 200
) -> list[dict]:
    """
    Fetch all items from a STAC collection.

    Paginates through the ``/collections/{id}/items`` endpoint until all
    items have been retrieved or *limit* is reached.

    Parameters
    ----------
    collection_id : str
        STAC collection ID.
    stac_api : str
        Base URL of the STAC API (trailing slash is stripped).
    limit : int
        Maximum number of items to fetch in total.

    Returns
    -------
    list[dict]
        List of STAC item dicts.
    """
    stac_api = stac_api.rstrip("/")
    items: list[dict] = []
    page_size = min(limit, 100)
    url: str | None = f"{stac_api}/collections/{collection_id}/items?limit={page_size}"

    with httpx.Client(timeout=60.0) as client:
        while url and len(items) < limit:
            logger.debug(f"Fetching items page: {url}")
            response = client.get(url)
            response.raise_for_status()
            data = response.json()

            features = data.get("features", [])
            if not features:
                break
            items.extend(features)

            # Follow "next" link if present
            url = None
            for link in data.get("links", []):
                if link.get("rel") == "next":
                    url = link.get("href")
                    break

    logger.info(
        f"Fetched {len(items)} items from collection '{collection_id}'"
    )
    return items[:limit]


# ---------------------------------------------------------------------------
# DataTree construction
# ---------------------------------------------------------------------------


# Cache keyed on (collection_id, stac_api) hash -- expensive to rebuild.
_datatree_cache: dict[str, tuple[xr.DataTree, dict[str, Any]]] = {}


def _build_datatree(
    collection_id: str, stac_api: str
) -> tuple[xr.DataTree, dict[str, Any]]:
    """
    Build an ``xr.DataTree`` from a STAC collection.

    Items are grouped by their ``properties.variable`` field.  Within each
    group the data files are opened with ``xr.open_mfdataset`` along the
    ``time`` dimension with Dask lazy loading.

    The second return value is a metadata dict carrying FESOM mesh info
    (if applicable) and the raw item list for downstream use.

    Parameters
    ----------
    collection_id : str
        STAC collection ID.
    stac_api : str
        Base URL of the STAC API.

    Returns
    -------
    tuple[xr.DataTree, dict]
        (tree, meta) where *tree* groups datasets by variable name and
        *meta* contains ``"items"``, ``"mesh"`` (lon, lat, elem or None),
        and ``"is_unstructured"`` flag.
    """
    cache_key = _collection_cache_key(collection_id, stac_api)
    if cache_key in _datatree_cache:
        logger.info("Returning cached DataTree for collection")
        return _datatree_cache[cache_key]

    items = _fetch_collection_items(collection_id, stac_api)
    if not items:
        raise ValueError(f"No items found in collection '{collection_id}'")

    # Group items by cube:variables keys — each variable maps to the files
    # that contain it, regardless of how many variables are in each file.
    var_groups: dict[str, list[str]] = defaultdict(list)
    fmt = "netcdf"
    for item in items:
        props = item.get("properties", {})
        cube_vars = props.get("cube:variables", {})
        href = _extract_href(item)
        if props.get("format") == "grib":
            fmt = "grib"
        if href and cube_vars:
            for var_name in cube_vars:
                var_groups[var_name].append(href)
        elif href:
            var_groups["unknown"].append(href)

    if not var_groups:
        raise ValueError("No data assets found in collection items")

    # Detect FESOM mesh from the first item that has mesh metadata
    mesh = None
    unstructured = False
    for item in items:
        mesh = get_mesh_from_stac_item(item)
        if mesh is not None:
            unstructured = True
            break

    # If mesh detection via STAC failed, check the first dataset directly
    if not unstructured:
        first_href = next(iter(next(iter(var_groups.values()))))
        try:
            _ds_probe = xr.open_dataset(_strip_file_uri(first_href), chunks="auto")
            unstructured = is_unstructured(_ds_probe)
            _ds_probe.close()
        except Exception:
            pass

    engine = "cfgrib" if fmt == "grib" else "netcdf4"

    # Group variables by their file set so we open each unique set only once.
    # Many variables share the same files (e.g., 143 vars in the same GRIB).
    fileset_to_vars: dict[tuple[str, ...], list[str]] = defaultdict(list)
    for var_name, hrefs in var_groups.items():
        paths = tuple(sorted(set(_strip_file_uri(h) for h in hrefs)))
        fileset_to_vars[paths].append(var_name)

    tree_dict: dict[str, xr.Dataset] = {}
    for paths, var_names in fileset_to_vars.items():
        logger.info(
            f"Opening {len(paths)} file(s) for {len(var_names)} variable(s)"
        )
        try:
            ds = xr.open_mfdataset(
                list(paths),
                concat_dim="time",
                combine="nested",
                chunks="auto",
                data_vars="minimal",
                coords="minimal",
                compat="override",
                engine=engine,
            )
        except Exception as e:
            logger.warning(
                f"Could not open multi-file dataset: {e}. "
                "Trying first file only."
            )
            try:
                ds = xr.open_dataset(paths[0], chunks="auto", engine=engine)
            except Exception as e2:
                logger.error(f"Skipping {len(var_names)} variables: {e2}")
                continue

        for var_name in var_names:
            if var_name in ds.data_vars:
                tree_dict[var_name] = ds[[var_name]]
            else:
                logger.debug(f"Variable '{var_name}' not found in dataset, skipping")

    if not tree_dict:
        raise ValueError("Could not open any datasets from collection items")

    tree = xr.DataTree.from_dict(tree_dict)
    meta: dict[str, Any] = {
        "items": items,
        "mesh": mesh,
        "is_unstructured": unstructured,
        "collection_id": collection_id,
        "n_items": len(items),
        "n_variables": len(tree_dict),
    }

    # Populate cache
    _datatree_cache[cache_key] = (tree, meta)
    logger.info(
        f"Built DataTree with {len(tree_dict)} variables for collection "
        f"'{collection_id}'"
    )
    return tree, meta


def _extract_href(item: dict) -> str | None:
    """Extract the data href from a STAC item's assets."""
    assets = item.get("assets", {})
    preferred = ["data", "netcdf", "nc", "grib"]
    for key in preferred:
        if key in assets and "href" in assets[key]:
            return assets[key]["href"]
    for asset in assets.values():
        if "href" in asset:
            return asset["href"]
    return None


# ---------------------------------------------------------------------------
# Panel app
# ---------------------------------------------------------------------------


def _create_loading_indicator(collection_id: str) -> pn.Column:
    """Create a loading screen shown while the DataTree is being built."""
    return pn.Column(
        pn.pane.Markdown(
            f"# ESM-Viz Collection Preview\n"
            f"**Collection:** `{collection_id}`",
        ),
        pn.indicators.LoadingSpinner(
            value=True, size=50, name="Building DataTree..."
        ),
        pn.pane.Markdown(
            "_Loading and indexing data files. This may take a minute "
            "for collections with many variables._",
            styles={"color": "#888"},
        ),
        sizing_mode="stretch_width",
    )


def create_collection_preview_app(
    collection_id: str,
    stac_api: str,
) -> pn.viewable.Viewable:
    """
    Create a Panel app for exploring an entire STAC collection.

    The app provides:
    - Variable selector (populated from DataTree node names)
    - Time range slider for computing temporal means
    - Anomaly mode with reference/comparison period selectors
    - Level/depth slider for 3D variables
    - Smart colormap selector

    Parameters
    ----------
    collection_id : str
        STAC collection ID.
    stac_api : str
        Base URL of the STAC API.

    Returns
    -------
    pn.viewable.Viewable
        The Panel application layout.
    """
    logger.info(
        f"Creating collection preview app: collection={collection_id}"
    )

    # Check cache first -- if already built, skip loading screen
    cache_key = _collection_cache_key(collection_id, stac_api)
    if cache_key in _datatree_cache:
        tree, meta = _datatree_cache[cache_key]
        return _create_collection_content(tree, meta, collection_id)

    # Show loading indicator, then build DataTree in background
    container = pn.Column(sizing_mode="stretch_width")
    container.append(_create_loading_indicator(collection_id))

    def _load_and_replace():
        try:
            tree, meta = _build_datatree(collection_id, stac_api)
            content = _create_collection_content(tree, meta, collection_id)
            container.clear()
            container.append(content)
        except Exception as e:
            logger.error(f"Failed to build DataTree: {e}")
            container.clear()
            container.append(pn.pane.Markdown("# ESM-Viz Collection Preview"))
            container.append(
                pn.pane.Alert(
                    f"Failed to load collection data: {e}",
                    alert_type="danger",
                )
            )

    pn.state.execute(_load_and_replace)
    return container


def _create_collection_content(
    tree: xr.DataTree, meta: dict, collection_id: str
) -> pn.viewable.Viewable:
    """Build the actual collection preview UI from a DataTree."""

    # Gather variable names from the tree (top-level children)
    variable_names = sorted(tree.children.keys())
    if not variable_names:
        return pn.Column(
            pn.pane.Alert(
                "No variables found in collection.",
                alert_type="warning",
            ),
        )

    mesh = meta.get("mesh")
    unstructured = meta.get("is_unstructured", False)

    # Determine global time range across all variables
    global_n_times = 1
    for var_name in variable_names:
        ds = tree[var_name].dataset
        for dim in ("time", "t"):
            if dim in ds.dims:
                global_n_times = max(global_n_times, ds.sizes[dim])
                break

    # Determine initial variable and its properties
    initial_var = variable_names[0]
    initial_cmap = get_smart_colormap(initial_var)

    def _n_levels_for(var_name: str) -> int:
        """Return the number of vertical levels for a given variable node."""
        ds = tree[var_name].dataset
        level_dims = {"level", "lev", "depth", "z", "nz", "nz1"}
        for dv in ds.data_vars.values():
            for dim in dv.dims:
                if dim.lower() in level_dims:
                    return dv.sizes[dim]
        return 1

    def _n_times_for(var_name: str) -> int:
        ds = tree[var_name].dataset
        for dim in ("time", "t"):
            if dim in ds.dims:
                return ds.sizes[dim]
        return 1

    # ---- Widgets ----

    var_selector = pn.widgets.Select(
        name="Variable",
        options=variable_names,
        value=initial_var,
        width=250,
    )

    view_mode = pn.widgets.RadioButtonGroup(
        name="View Mode",
        options=["Absolute", "Anomaly"],
        value="Absolute",
        button_type="default",
    )

    n_t_init = _n_times_for(initial_var)
    time_range_slider = pn.widgets.IntRangeSlider(
        name="Time Range (avg)",
        start=0,
        end=max(0, n_t_init - 1),
        value=(0, min(9, max(0, n_t_init - 1))),
        step=1,
        width=300,
    )

    ref_range_slider = pn.widgets.IntRangeSlider(
        name="Reference Period",
        start=0,
        end=max(0, n_t_init - 1),
        value=(0, min(9, max(0, n_t_init - 1))),
        step=1,
        width=300,
        visible=False,
    )

    cmp_range_slider = pn.widgets.IntRangeSlider(
        name="Comparison Period",
        start=0,
        end=max(0, n_t_init - 1),
        value=(
            min(10, max(0, n_t_init - 1)),
            min(19, max(0, n_t_init - 1)),
        ),
        step=1,
        width=300,
        visible=False,
    )

    n_l_init = _n_levels_for(initial_var)
    level_slider = pn.widgets.IntSlider(
        name="Level / Depth",
        start=0,
        end=max(1, n_l_init - 1),
        value=0,
        step=1,
        width=300,
        disabled=(n_l_init <= 1),
    )

    cmap_selector = pn.widgets.Select(
        name="Colormap",
        options=AVAILABLE_COLORMAPS,
        value=initial_cmap if initial_cmap in AVAILABLE_COLORMAPS else DEFAULT_COLORMAP,
        width=180,
    )

    # Info panel
    info_pane = pn.pane.Markdown(
        _build_info_md(meta, variable_names, global_n_times),
        width=280,
    )

    # ---- Reactive callbacks ----

    @pn.depends(view_mode.param.value, watch=True)
    def _toggle_anomaly(mode: str) -> None:
        is_anomaly = mode == "Anomaly"
        time_range_slider.visible = not is_anomaly
        ref_range_slider.visible = is_anomaly
        cmp_range_slider.visible = is_anomaly

    @pn.depends(var_selector.param.value, watch=True)
    def _on_variable_change(var_name: str) -> None:
        n_t = _n_times_for(var_name)
        for slider in (time_range_slider, ref_range_slider, cmp_range_slider):
            slider.start = 0
            slider.end = max(0, n_t - 1)

        time_range_slider.value = (0, min(9, max(0, n_t - 1)))
        ref_range_slider.value = (0, min(9, max(0, n_t - 1)))
        cmp_range_slider.value = (
            min(10, max(0, n_t - 1)),
            min(19, max(0, n_t - 1)),
        )

        n_l = _n_levels_for(var_name)
        level_slider.end = max(1, n_l - 1)
        level_slider.disabled = n_l <= 1
        if level_slider.value > level_slider.end:
            level_slider.value = 0

        smart_cmap = get_smart_colormap(var_name)
        if smart_cmap in AVAILABLE_COLORMAPS:
            cmap_selector.value = smart_cmap

    # ---- Plot function ----

    @pn.depends(
        var_selector.param.value,
        view_mode.param.value,
        time_range_slider.param.value,
        ref_range_slider.param.value,
        cmp_range_slider.param.value,
        level_slider.param.value,
        cmap_selector.param.value,
    )
    def _render_plot(
        var_name: str,
        mode: str,
        time_range: tuple[int, int],
        ref_range: tuple[int, int],
        cmp_range: tuple[int, int],
        level_idx: int,
        cmap: str,
    ) -> Any:
        try:
            ds = tree[var_name].dataset
            # Identify the data variable inside this single-variable dataset
            data_var = _pick_data_var(ds, var_name)
            da = ds[data_var]

            # Select level if applicable
            da = _isel_level(da, level_idx)

            if mode == "Anomaly":
                da = _compute_anomaly(da, ref_range, cmp_range)
                plot_title = f"{var_name} anomaly (t[{cmp_range[0]}:{cmp_range[1]}] - t[{ref_range[0]}:{ref_range[1]}])"
            else:
                da = _compute_time_mean(da, time_range)
                plot_title = f"{var_name} mean (t[{time_range[0]}:{time_range[1]}])"

            # Compute into memory for plotting.
            # When a distributed.Client exists it is automatically used
            # by Dask for all .compute() calls -- no explicit routing needed.
            if hasattr(da.data, "compute"):
                from esm_viz.compute import get_compute_manager

                manager = get_compute_manager()
                client = manager.get_client()
                if client:
                    logger.debug("Using distributed client for compute")
                da = da.compute()

            if unstructured and mesh is not None:
                lon, lat, elem = mesh
                plot = create_interactive_fesom_plot(
                    da, lon, lat, elem, cmap=cmap, title=plot_title
                )
                return pn.pane.HoloViews(plot, sizing_mode="stretch_both")

            # Gridded path
            plot = _create_gridded_plot(da, cmap, projection=ccrs.Robinson() if HAS_GEOVIEWS else None)
            return pn.pane.HoloViews(plot, sizing_mode="stretch_both")

        except Exception as e:
            logger.error(f"Collection plot failed: {e}")
            return pn.pane.Alert(f"Plot failed: {e}", alert_type="danger")

    # ---- Layout ----

    controls = pn.Column(
        pn.pane.Markdown("## Controls"),
        var_selector,
        view_mode,
        time_range_slider,
        ref_range_slider,
        cmp_range_slider,
        level_slider,
        cmap_selector,
        pn.layout.Divider(),
        info_pane,
        width=320,
    )

    plot_area = pn.panel(_render_plot, sizing_mode="stretch_both")

    app = pn.Column(
        pn.pane.Markdown("# ESM-Viz Collection Preview"),
        pn.Row(controls, plot_area, sizing_mode="stretch_both"),
        sizing_mode="stretch_both",
    )

    return app


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _pick_data_var(ds: xr.Dataset, hint: str) -> str:
    """
    Pick the most relevant data variable from a dataset.

    Prefers a variable whose name matches *hint*; falls back to the first
    plottable (ndim >= 2, numeric) variable.
    """
    if hint in ds.data_vars:
        return hint

    for name, var in ds.data_vars.items():
        if var.dtype.kind in ("i", "u", "f") and var.ndim >= 2:
            return str(name)

    # Last resort -- first data var
    return str(next(iter(ds.data_vars)))


def _isel_level(da: xr.DataArray, level_idx: int) -> xr.DataArray:
    """Select a single level/depth slice if a vertical dim exists."""
    level_dims = {"level", "lev", "depth", "z", "nz", "nz1"}
    for dim in list(da.dims):
        if dim.lower() in level_dims:
            safe_idx = min(level_idx, da.sizes[dim] - 1)
            da = da.isel({dim: safe_idx})
    return da


def _compute_time_mean(
    da: xr.DataArray, time_range: tuple[int, int]
) -> xr.DataArray:
    """Average over the selected time index range."""
    for dim in ("time", "t"):
        if dim in da.dims:
            start, end = time_range
            end = min(end, da.sizes[dim] - 1)
            start = min(start, end)
            da = da.isel({dim: slice(start, end + 1)}).mean(dim)
            break
    return da


def _compute_anomaly(
    da: xr.DataArray,
    ref_range: tuple[int, int],
    cmp_range: tuple[int, int],
) -> xr.DataArray:
    """Compute comparison_mean - reference_mean."""
    ref = _compute_time_mean(da, ref_range)
    cmp = _compute_time_mean(da, cmp_range)
    return cmp - ref


def _build_info_md(
    meta: dict[str, Any],
    variables: list[str],
    global_n_times: int,
) -> str:
    """Build a Markdown info block for the sidebar."""
    coll_id = meta.get("collection_id", "unknown")
    n_items = meta.get("n_items", 0)
    n_vars = meta.get("n_variables", 0)
    mesh_type = "Unstructured (FESOM)" if meta.get("is_unstructured") else "Gridded"
    return (
        f"**Collection:** `{coll_id}`\n\n"
        f"- Items: {n_items}\n"
        f"- Variables: {n_vars}\n"
        f"- Max time steps: {global_n_times}\n"
        f"- Mesh type: {mesh_type}\n"
    )


# ---------------------------------------------------------------------------
# Cross-experiment comparison
# ---------------------------------------------------------------------------


def create_comparison_preview_app(
    collection_a: str,
    collection_b: str,
    stac_api: str,
) -> pn.viewable.Viewable:
    """
    Create a Panel app comparing two STAC collections (experiments).

    Displays three panels: Collection A mean, Collection B mean, and
    the difference (B - A). Each collection gets its own time range
    slider for independent temporal averaging.

    Parameters
    ----------
    collection_a : str
        STAC collection ID for the reference experiment.
    collection_b : str
        STAC collection ID for the comparison experiment.
    stac_api : str
        Base URL of the STAC API.

    Returns
    -------
    pn.viewable.Viewable
        The Panel application layout with three-panel comparison view.
    """
    logger.info(
        f"Creating comparison app: {collection_a} vs {collection_b}"
    )

    # Build DataTrees for both collections
    try:
        tree_a, meta_a = _build_datatree(collection_a, stac_api)
        tree_b, meta_b = _build_datatree(collection_b, stac_api)
    except Exception as e:
        logger.error(f"Failed to build DataTrees for comparison: {e}")
        return pn.Column(
            pn.pane.Markdown("# ESM-Viz Experiment Comparison"),
            pn.pane.Alert(
                f"Failed to load collection data: {e}",
                alert_type="danger",
            ),
            sizing_mode="stretch_width",
        )

    # Find common variables
    vars_a = set(tree_a.children.keys())
    vars_b = set(tree_b.children.keys())
    common_vars = sorted(vars_a & vars_b)

    if not common_vars:
        return pn.Column(
            pn.pane.Markdown("# ESM-Viz Experiment Comparison"),
            pn.pane.Alert(
                f"No common variables between '{collection_a}' and '{collection_b}'.\n\n"
                f"Collection A variables: {sorted(vars_a)}\n\n"
                f"Collection B variables: {sorted(vars_b)}",
                alert_type="warning",
            ),
            sizing_mode="stretch_width",
        )

    # Determine mesh info -- use whichever has FESOM mesh
    mesh_a = meta_a.get("mesh")
    mesh_b = meta_b.get("mesh")
    unstructured_a = meta_a.get("is_unstructured", False)
    unstructured_b = meta_b.get("is_unstructured", False)

    # For difference plots, both must be on the same grid type
    can_difference = (unstructured_a == unstructured_b)

    # Time range helpers per collection
    def _n_times_for(tree: xr.DataTree, var_name: str) -> int:
        ds = tree[var_name].dataset
        for dim in ("time", "t"):
            if dim in ds.dims:
                return ds.sizes[dim]
        return 1

    initial_var = common_vars[0]
    initial_cmap = get_smart_colormap(initial_var)

    n_t_a = _n_times_for(tree_a, initial_var)
    n_t_b = _n_times_for(tree_b, initial_var)

    # Widgets
    var_selector = pn.widgets.Select(
        name="Variable",
        options=common_vars,
        value=initial_var,
        width=250,
    )

    range_a = pn.widgets.IntRangeSlider(
        name=f"Time Range: {collection_a}",
        start=0,
        end=max(0, n_t_a - 1),
        value=(0, max(0, n_t_a - 1)),
        step=1,
        width=250,
    )

    range_b = pn.widgets.IntRangeSlider(
        name=f"Time Range: {collection_b}",
        start=0,
        end=max(0, n_t_b - 1),
        value=(0, max(0, n_t_b - 1)),
        step=1,
        width=250,
    )

    level_slider = pn.widgets.IntSlider(
        name="Level / Depth",
        start=0,
        end=1,
        value=0,
        step=1,
        width=250,
        disabled=True,
    )

    cmap_selector = pn.widgets.Select(
        name="Colormap",
        options=AVAILABLE_COLORMAPS,
        value=initial_cmap if initial_cmap in AVAILABLE_COLORMAPS else DEFAULT_COLORMAP,
        width=180,
    )

    diff_cmap_selector = pn.widgets.Select(
        name="Difference Colormap",
        options=["RdBu_r", "coolwarm", "BrBG", "PiYG", "PRGn"] + AVAILABLE_COLORMAPS,
        value="RdBu_r",
        width=180,
    )

    # Update sliders when variable changes
    @pn.depends(var_selector.param.value, watch=True)
    def _on_var_change(var_name: str) -> None:
        n_a = _n_times_for(tree_a, var_name)
        n_b = _n_times_for(tree_b, var_name)
        range_a.end = max(0, n_a - 1)
        range_a.value = (0, max(0, n_a - 1))
        range_b.end = max(0, n_b - 1)
        range_b.value = (0, max(0, n_b - 1))

        # Level slider
        n_l = 1
        ds_a = tree_a[var_name].dataset
        level_dims = {"level", "lev", "depth", "z", "nz", "nz1"}
        for dv in ds_a.data_vars.values():
            for dim in dv.dims:
                if dim.lower() in level_dims:
                    n_l = max(n_l, dv.sizes[dim])
        level_slider.end = max(1, n_l - 1)
        level_slider.disabled = (n_l <= 1)
        if level_slider.value > level_slider.end:
            level_slider.value = 0

        smart_cmap = get_smart_colormap(var_name)
        if smart_cmap in AVAILABLE_COLORMAPS:
            cmap_selector.value = smart_cmap

    # Plotting helper
    def _make_plot(tree: xr.DataTree, meta: dict, var_name: str,
                   time_range: tuple[int, int], level_idx: int,
                   cmap: str, title: str) -> Any:
        ds = tree[var_name].dataset
        data_var = _pick_data_var(ds, var_name)
        da = ds[data_var]
        da = _isel_level(da, level_idx)
        da = _compute_time_mean(da, time_range)
        if hasattr(da.data, "compute"):
            from esm_viz.compute import get_compute_manager

            mgr = get_compute_manager()
            _client = mgr.get_client()
            if _client:
                logger.debug("Using distributed client for comparison compute")
            da = da.compute()

        mesh = meta.get("mesh")
        unstructured = meta.get("is_unstructured", False)

        if unstructured and mesh is not None:
            lon, lat, elem = mesh
            plot = create_interactive_fesom_plot(
                da, lon, lat, elem, cmap=cmap, title=title
            )
            return pn.pane.HoloViews(plot, sizing_mode="stretch_both")

        plot = _create_gridded_plot(
            da, cmap, projection=ccrs.Robinson() if HAS_GEOVIEWS else None
        )
        return pn.pane.HoloViews(plot, sizing_mode="stretch_both")

    # Panel A
    @pn.depends(
        var_selector.param.value,
        range_a.param.value,
        level_slider.param.value,
        cmap_selector.param.value,
    )
    def plot_a(var_name: str, tr: tuple, level_idx: int, cmap: str) -> Any:
        try:
            title = f"{collection_a}: {var_name} mean (t[{tr[0]}:{tr[1]}])"
            return _make_plot(tree_a, meta_a, var_name, tr, level_idx, cmap, title)
        except Exception as e:
            logger.error(f"Plot A failed: {e}")
            return pn.pane.Alert(f"Plot A failed: {e}", alert_type="danger")

    # Panel B
    @pn.depends(
        var_selector.param.value,
        range_b.param.value,
        level_slider.param.value,
        cmap_selector.param.value,
    )
    def plot_b(var_name: str, tr: tuple, level_idx: int, cmap: str) -> Any:
        try:
            title = f"{collection_b}: {var_name} mean (t[{tr[0]}:{tr[1]}])"
            return _make_plot(tree_b, meta_b, var_name, tr, level_idx, cmap, title)
        except Exception as e:
            logger.error(f"Plot B failed: {e}")
            return pn.pane.Alert(f"Plot B failed: {e}", alert_type="danger")

    # Difference panel
    @pn.depends(
        var_selector.param.value,
        range_a.param.value,
        range_b.param.value,
        level_slider.param.value,
        diff_cmap_selector.param.value,
    )
    def plot_diff(var_name: str, tr_a: tuple, tr_b: tuple,
                  level_idx: int, cmap: str) -> Any:
        if not can_difference:
            return pn.pane.Alert(
                "Difference not available: collections use different grid types.",
                alert_type="warning",
            )
        try:
            ds_a = tree_a[var_name].dataset
            ds_b = tree_b[var_name].dataset
            dv_a = _pick_data_var(ds_a, var_name)
            dv_b = _pick_data_var(ds_b, var_name)

            da_a = _isel_level(ds_a[dv_a], level_idx)
            da_b = _isel_level(ds_b[dv_b], level_idx)
            mean_a = _compute_time_mean(da_a, tr_a)
            mean_b = _compute_time_mean(da_b, tr_b)

            diff = mean_b - mean_a
            if hasattr(diff.data, "compute"):
                from esm_viz.compute import get_compute_manager

                mgr = get_compute_manager()
                _client = mgr.get_client()
                if _client:
                    logger.debug("Using distributed client for diff compute")
                diff = diff.compute()

            title = f"Difference: {collection_b} - {collection_a}"

            mesh = meta_a.get("mesh") or meta_b.get("mesh")
            unstructured = meta_a.get("is_unstructured") or meta_b.get("is_unstructured")

            if unstructured and mesh is not None:
                lon, lat, elem = mesh
                plot = create_interactive_fesom_plot(
                    diff, lon, lat, elem, cmap=cmap, title=title
                )
                return pn.pane.HoloViews(plot, sizing_mode="stretch_both")

            plot = _create_gridded_plot(
                diff, cmap, projection=ccrs.Robinson() if HAS_GEOVIEWS else None
            )
            return pn.pane.HoloViews(plot, sizing_mode="stretch_both")

        except Exception as e:
            logger.error(f"Difference plot failed: {e}")
            return pn.pane.Alert(f"Difference failed: {e}", alert_type="danger")

    # Layout
    controls = pn.Column(
        pn.pane.Markdown("## Controls"),
        var_selector,
        range_a,
        range_b,
        level_slider,
        cmap_selector,
        diff_cmap_selector,
        pn.layout.Divider(),
        pn.pane.Markdown(
            f"**Comparison:** `{collection_a}` vs `{collection_b}`\n\n"
            f"- Common variables: {len(common_vars)}\n"
            f"- Grid compatible: {'Yes' if can_difference else 'No'}\n"
        ),
        width=320,
    )

    plots = pn.Tabs(
        ("Side by Side", pn.Row(
            pn.Column(
                pn.pane.Markdown(f"### {collection_a}"),
                pn.panel(plot_a, sizing_mode="stretch_both"),
                sizing_mode="stretch_both",
            ),
            pn.Column(
                pn.pane.Markdown(f"### {collection_b}"),
                pn.panel(plot_b, sizing_mode="stretch_both"),
                sizing_mode="stretch_both",
            ),
            sizing_mode="stretch_both",
        )),
        ("Difference (B - A)", pn.panel(plot_diff, sizing_mode="stretch_both")),
        sizing_mode="stretch_both",
    )

    app = pn.Column(
        pn.pane.Markdown("# ESM-Viz Experiment Comparison"),
        pn.Row(controls, plots, sizing_mode="stretch_both"),
        sizing_mode="stretch_both",
    )

    return app
