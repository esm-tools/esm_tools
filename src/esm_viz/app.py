"""
FastAPI application for the ESM visualization service.

Provides REST endpoints for generating static previews and metadata
from climate datasets referenced in STAC catalogs. Also serves
interactive Panel applications for data exploration.
"""

from pathlib import Path
from typing import Annotated
from functools import partial
from urllib.parse import urlencode

import httpx
import panel as pn
from fastapi import FastAPI, HTTPException, Query, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import RedirectResponse
from fastapi.staticfiles import StaticFiles
from loguru import logger

from esm_viz import __version__
from esm_viz.collection import create_collection_preview_app, create_comparison_preview_app
from esm_viz.fesom import (
    is_unstructured, plot_unstructured,
    get_mesh_from_stac_item, create_interactive_fesom_plot,
)
from esm_viz.compute_routes import create_compute_router
from esm_viz.interactive import create_preview_app
from esm_viz.readers import get_data_metadata, open_data
from esm_viz.static_preview import generate_preview_png


app = FastAPI(
    title="ESM Visualization Service",
    description=(
        "REST API for generating visualizations and metadata from climate datasets. "
        "Integrates with STAC catalogs to fetch data references."
    ),
    version=__version__,
    docs_url="/docs",
    redoc_url="/redoc",
)

# Enable CORS for browser access
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Mount compute cluster management routes
app.include_router(create_compute_router())

# Serve GeoViews JS extension -- Panel looks for it at
# /static/extensions/geoviews/ but geoviews installs it elsewhere.
try:
    import geoviews as _gv
    _gv_dist = Path(_gv.__file__).parent / "dist"
    if _gv_dist.exists():
        # Panel expects /static/extensions/geoviews/geoviews.min.js
        # Mount the geoviews dist directory at that path
        app.mount(
            "/static/extensions/geoviews",
            StaticFiles(directory=str(_gv_dist)),
            name="geoviews-static",
        )
        logger.info(f"Mounted GeoViews static files from {_gv_dist}")
except ImportError:
    logger.warning("GeoViews not available, interactive maps will be limited")


@app.get("/health")
async def health_check() -> dict[str, str]:
    """
    Health check endpoint.

    Returns
    -------
    dict
        Status information including service version.
    """
    return {
        "status": "healthy",
        "service": "esm-viz",
        "version": __version__,
    }


async def _fetch_stac_item(
    stac_api: str, item_id: str, collection_id: str | None = None
) -> dict:
    """
    Fetch a STAC item from the API.

    Parameters
    ----------
    stac_api : str
        Base URL of the STAC API.
    item_id : str
        The STAC item ID to fetch.
    collection_id : str, optional
        The collection ID containing the item. If not provided, the item
        will be discovered via the search endpoint.

    Returns
    -------
    dict
        The STAC item as a dictionary.

    Raises
    ------
    HTTPException
        If the item cannot be fetched.
    """
    # Normalize API URL
    stac_api = stac_api.rstrip("/")

    async with httpx.AsyncClient(timeout=30.0) as client:
        # If collection_id is provided, try the direct endpoint first
        if collection_id:
            endpoint = f"{stac_api}/collections/{collection_id}/items/{item_id}"
            try:
                logger.debug(f"Trying STAC endpoint: {endpoint}")
                response = await client.get(endpoint)
                if response.status_code == 200:
                    data = response.json()
                    if "assets" in data:
                        return data
            except httpx.RequestError as e:
                logger.warning(f"Request failed for {endpoint}: {e}")

        # Use search endpoint to find the item across all collections
        # This is the most reliable method as items are collection-scoped in STAC
        search_endpoint = f"{stac_api}/search?ids={item_id}"
        try:
            logger.debug(f"Searching for item via: {search_endpoint}")
            response = await client.get(search_endpoint)
            if response.status_code == 200:
                data = response.json()
                if "features" in data and len(data["features"]) > 0:
                    item = data["features"][0]
                    # Log the collection for debugging
                    found_collection = item.get("collection", "unknown")
                    logger.debug(f"Found item in collection: {found_collection}")
                    return item
        except httpx.RequestError as e:
            logger.warning(f"Search request failed: {e}")

        # POST search as fallback (some STAC APIs require POST for search)
        try:
            logger.debug(f"Trying POST search for item: {item_id}")
            response = await client.post(
                f"{stac_api}/search",
                json={"ids": [item_id]},
            )
            if response.status_code == 200:
                data = response.json()
                if "features" in data and len(data["features"]) > 0:
                    return data["features"][0]
        except httpx.RequestError as e:
            logger.warning(f"POST search request failed: {e}")

        # Try to fetch all collections and search for item in each
        # This is a last resort for APIs that don't support item search
        try:
            logger.debug("Attempting to discover item by iterating collections")
            collections_response = await client.get(f"{stac_api}/collections")
            if collections_response.status_code == 200:
                collections_data = collections_response.json()
                collection_list = collections_data.get(
                    "collections", collections_data.get("features", [])
                )
                for coll in collection_list:
                    coll_id = coll.get("id")
                    if coll_id:
                        endpoint = f"{stac_api}/collections/{coll_id}/items/{item_id}"
                        try:
                            response = await client.get(endpoint)
                            if response.status_code == 200:
                                data = response.json()
                                if "assets" in data:
                                    logger.debug(f"Found item in collection: {coll_id}")
                                    return data
                        except httpx.RequestError:
                            continue
        except httpx.RequestError as e:
            logger.warning(f"Collection iteration failed: {e}")

    raise HTTPException(
        status_code=404,
        detail=f"Could not fetch STAC item '{item_id}' from {stac_api}",
    )


def _get_data_href(item: dict) -> str:
    """
    Extract the data href from a STAC item.

    Parameters
    ----------
    item : dict
        The STAC item.

    Returns
    -------
    str
        The href to the data file.

    Raises
    ------
    HTTPException
        If no suitable data asset is found.
    """
    assets = item.get("assets", {})

    # Priority order for data assets
    preferred_keys = ["data", "netcdf", "nc", "grib", "analysis", "forecast"]

    # Try preferred keys first
    for key in preferred_keys:
        if key in assets and "href" in assets[key]:
            return assets[key]["href"]

    # Fall back to first asset with href
    for asset_key, asset in assets.items():
        if "href" in asset:
            href = asset["href"]
            # Check if it looks like a data file
            if any(ext in href.lower() for ext in [".nc", ".nc4", ".grib", ".grb"]):
                return href

    # Last resort: return first available href
    for asset in assets.values():
        if "href" in asset:
            return asset["href"]

    raise HTTPException(
        status_code=400,
        detail="No data asset found in STAC item",
    )


@app.get("/preview/{item_id}.png")
async def get_preview_png(
    item_id: str,
    var: Annotated[str, Query(description="Variable name to plot")],
    stac_api: Annotated[str, Query(description="STAC API base URL")],
    time: Annotated[int, Query(description="Time index")] = 0,
    level: Annotated[int, Query(description="Level/depth index for 3D data")] = 0,
    cmap: Annotated[str, Query(description="Matplotlib colormap")] = "viridis",
    collection_id: Annotated[
        str | None, Query(description="STAC collection ID (optional, improves performance)")
    ] = None,
) -> Response:
    """
    Generate a static PNG preview of a dataset variable.

    Parameters
    ----------
    item_id : str
        STAC item ID.
    var : str
        Variable name to plot.
    stac_api : str
        Base URL of the STAC API.
    time : int, optional
        Time index to plot. Default is 0.
    cmap : str, optional
        Matplotlib colormap name. Default is 'viridis'.
    collection_id : str, optional
        STAC collection ID. If provided, enables direct item fetch
        via /collections/{collection_id}/items/{item_id}.

    Returns
    -------
    Response
        PNG image response.
    """
    logger.info(f"Preview request: item={item_id}, var={var}, time={time}, level={level}")

    ds = None
    try:
        # Fetch STAC item
        item = await _fetch_stac_item(stac_api, item_id, collection_id=collection_id)
        href = _get_data_href(item)
        logger.debug(f"Data href: {href}")

        # Open dataset
        ds = open_data(href)

        # Check if unstructured mesh (e.g., FESOM)
        if is_unstructured(ds):
            logger.info("Detected unstructured mesh data")
            data_array = ds[var]
            if "time" in data_array.dims:
                data_array = data_array.isel(time=min(time, data_array.sizes["time"] - 1))
            level_dims = {"nz", "nz1", "depth", "lev", "level", "z"}
            for dim in data_array.dims:
                if dim.lower() in level_dims:
                    data_array = data_array.isel({dim: min(level, data_array.sizes[dim] - 1)})

            import io
            import matplotlib
            matplotlib.use("Agg")

            fig = plot_unstructured(data_array, cmap=cmap, ds=ds, stac_item=item)
            buf = io.BytesIO()
            fig.savefig(buf, format="png", dpi=100, bbox_inches="tight")
            buf.seek(0)
            png_bytes = buf.read()
            plt.close(fig)
        else:
            # Regular gridded data
            png_bytes = generate_preview_png(ds, var, time_index=time, level_index=level, cmap=cmap)

        return Response(
            content=png_bytes,
            media_type="image/png",
            headers={
                "Cache-Control": "public, max-age=3600",
                "X-Item-ID": item_id,
                "X-Variable": var,
            },
        )

    except KeyError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception(f"Preview generation failed: {e}")
        raise HTTPException(status_code=500, detail=f"Preview generation failed: {e}")
    finally:
        # Ensure dataset is always closed to prevent file handle leaks
        if ds is not None:
            try:
                ds.close()
            except Exception:
                pass  # Ignore close errors


@app.get("/preview/{item_id}.json")
async def get_preview_metadata(
    item_id: str,
    stac_api: Annotated[str, Query(description="STAC API base URL")],
    collection_id: Annotated[
        str | None, Query(description="STAC collection ID (optional, improves performance)")
    ] = None,
) -> dict:
    """
    Get metadata for a dataset referenced by a STAC item.

    Parameters
    ----------
    item_id : str
        STAC item ID.
    stac_api : str
        Base URL of the STAC API.
    collection_id : str, optional
        STAC collection ID. If provided, enables direct item fetch
        via /collections/{collection_id}/items/{item_id}.

    Returns
    -------
    dict
        Metadata including variables, dimensions, and coordinate ranges.
    """
    logger.info(f"Metadata request: item={item_id}")

    try:
        # Fetch STAC item
        item = await _fetch_stac_item(stac_api, item_id, collection_id=collection_id)
        href = _get_data_href(item)
        logger.debug(f"Data href: {href}")

        # Open dataset
        ds = open_data(href)

        # Extract metadata
        metadata = get_data_metadata(ds)

        # Add STAC item info
        metadata["item_id"] = item_id
        metadata["href"] = href
        metadata["is_unstructured"] = is_unstructured(ds)

        # Close dataset
        ds.close()

        return metadata

    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception(f"Metadata extraction failed: {e}")
        raise HTTPException(status_code=500, detail=f"Metadata extraction failed: {e}")


# NOTE: Fixed-path routes MUST be defined BEFORE parameterized routes.
# /preview/compare/panel and /preview/collection/... must come before
# /preview/{item_id}/panel, otherwise FastAPI matches "compare" as an item_id.


@app.get("/preview/compare/panel")
async def preview_compare_redirect(
    collection_a: Annotated[str, Query(description="Reference collection ID")],
    collection_b: Annotated[str, Query(description="Comparison collection ID")],
    stac_api: Annotated[str | None, Query(description="STAC API base URL")] = None,
) -> RedirectResponse:
    """
    Redirect to the interactive comparison Panel for two STAC collections.

    Shows side-by-side views of two experiments with independent time
    range selectors, plus a difference panel (B - A).
    """
    logger.info(f"Compare redirect: {collection_a} vs {collection_b}")
    params = {
        "compare": "1",
        "collection_a": collection_a,
        "collection_b": collection_b,
    }
    if stac_api:
        params["stac_api"] = stac_api
    return RedirectResponse(url=f"/_panel?{urlencode(params)}", status_code=307)


@app.get("/preview/collection/{collection_id}/panel")
async def preview_collection_panel_redirect(
    collection_id: str,
    stac_api: Annotated[str | None, Query(description="STAC API base URL")] = None,
) -> RedirectResponse:
    """
    Redirect to the interactive Panel visualization for a whole STAC collection.
    """
    logger.info(f"Panel collection redirect: collection={collection_id}")
    params = {"collection_id": collection_id}
    if stac_api:
        params["stac_api"] = stac_api
    return RedirectResponse(url=f"/_panel?{urlencode(params)}", status_code=307)


@app.get("/preview/{item_id}/panel")
async def preview_panel_redirect(
    item_id: str,
    stac_api: Annotated[str | None, Query(description="STAC API base URL")] = None,
    var: Annotated[str | None, Query(description="Initial variable")] = None,
    collection_id: Annotated[
        str | None, Query(description="STAC collection ID (optional, improves performance)")
    ] = None,
) -> RedirectResponse:
    """
    Redirect to the interactive Panel visualization for a STAC item.
    """
    logger.info(f"Panel redirect: item={item_id}")
    params = {"item_id": item_id}
    if stac_api:
        params["stac_api"] = stac_api
    if var:
        params["var"] = var
    if collection_id:
        params["collection_id"] = collection_id
    return RedirectResponse(url=f"/_panel?{urlencode(params)}", status_code=307)


@app.get("/")
async def root() -> dict:
    """
    Root endpoint with API information.
    """
    return {
        "service": "ESM Visualization Service",
        "version": __version__,
        "endpoints": {
            "/health": "Health check",
            "/preview/{item_id}.png": "Static PNG preview",
            "/preview/{item_id}.json": "Dataset metadata",
            "/preview/{item_id}/panel": "Interactive Panel visualization",
            "/preview/collection/{collection_id}/panel": "Interactive collection-level Panel visualization",
            "/preview/compare/panel": "Cross-experiment comparison (two collections)",
            "/docs": "OpenAPI documentation",
        },
    }


# -----------------------------------------------------------------------------
# Panel Application Integration
# -----------------------------------------------------------------------------


def _create_panel_app_for_item(
    item_id: str,
    stac_api: str | None = None,
    var: str | None = None,
    collection_id: str | None = None,
) -> pn.viewable.Viewable:
    """
    Create a Panel app for a specific STAC item.

    This function is called by the Panel server to create apps dynamically.

    Parameters
    ----------
    item_id : str
        STAC item ID.
    stac_api : str, optional
        Base URL of the STAC API.
    var : str, optional
        Initial variable to display.
    collection_id : str, optional
        STAC collection ID.

    Returns
    -------
    pn.viewable.Viewable
        Panel application for the item.
    """
    logger.info(f"Creating Panel app for item: {item_id}")

    href = None
    item = None
    if stac_api:
        try:
            with httpx.Client(timeout=30.0) as client:
                stac_api_norm = stac_api.rstrip("/")
                item = None

                if collection_id:
                    try:
                        endpoint = f"{stac_api_norm}/collections/{collection_id}/items/{item_id}"
                        logger.debug(f"Trying direct endpoint: {endpoint}")
                        response = client.get(endpoint)
                        if response.status_code == 200:
                            data = response.json()
                            if "assets" in data:
                                item = data
                    except Exception as e:
                        logger.warning(f"Direct fetch failed: {e}")

                if not item:
                    try:
                        search_url = f"{stac_api_norm}/search?ids={item_id}"
                        logger.debug(f"Searching for item via: {search_url}")
                        response = client.get(search_url)
                        if response.status_code == 200:
                            data = response.json()
                            if "features" in data and len(data["features"]) > 0:
                                item = data["features"][0]
                    except Exception as e:
                        logger.warning(f"Search failed: {e}")

                if not item:
                    try:
                        response = client.post(
                            f"{stac_api_norm}/search",
                            json={"ids": [item_id]},
                        )
                        if response.status_code == 200:
                            data = response.json()
                            if "features" in data and len(data["features"]) > 0:
                                item = data["features"][0]
                    except Exception as e:
                        logger.warning(f"POST search failed: {e}")

                if item:
                    assets = item.get("assets", {})
                    preferred_keys = ["data", "netcdf", "nc", "grib"]
                    for key in preferred_keys:
                        if key in assets and "href" in assets[key]:
                            href = assets[key]["href"]
                            break
                    if not href:
                        for asset in assets.values():
                            if "href" in asset:
                                href = asset["href"]
                                break
        except Exception as e:
            logger.error(f"Failed to fetch STAC item: {e}")

    if not href:
        href = item_id
        logger.info(f"Using item_id as direct href: {href}")

    # Check if this is FESOM data -- use specialized interactive plot
    try:
        ds = open_data(href)
        if is_unstructured(ds) and item is not None:
            mesh = get_mesh_from_stac_item(item)
            if mesh is not None:
                lon, lat, elem = mesh
                # Select variable
                sel_var = var or next(iter(ds.data_vars), None)
                if sel_var and sel_var in ds.data_vars:
                    data_array = ds[sel_var]
                    if "time" in data_array.dims:
                        data_array = data_array.isel(time=0)
                    level_dims = {"nz", "nz1", "depth", "lev", "level", "z"}
                    for dim in data_array.dims:
                        if dim.lower() in level_dims:
                            data_array = data_array.isel({dim: 0})
                    plot = create_interactive_fesom_plot(data_array, lon, lat, elem)
                    ds.close()
                    logger.info("Created interactive FESOM Panel app")
                    return pn.pane.HoloViews(plot)
        ds.close()
    except Exception as e:
        logger.warning(f"FESOM interactive check failed, falling back to generic: {e}")

    return create_preview_app(href, variable=var)


def setup_panel_routes(fastapi_app: FastAPI) -> None:
    """
    Set up Panel application routes on the FastAPI app.

    Uses Panel's ``add_application`` decorator to mount an interactive
    Panel app at ``/_panel``. The public-facing endpoint at
    ``/preview/{item_id}/panel`` redirects here with query parameters.

    Parameters
    ----------
    fastapi_app : FastAPI
        The FastAPI application to add routes to.
    """
    try:
        from panel.io.fastapi import add_application

        @add_application("/_panel", fastapi_app, title="ESM-Viz Interactive Preview")
        def panel_app_factory():
            """Panel app factory -- reads query params from Bokeh session context."""
            try:
                from bokeh.io import curdoc

                doc = curdoc()
                if doc and hasattr(doc, "session_context"):
                    session_context = doc.session_context
                    if session_context and hasattr(session_context, "request"):
                        request = session_context.request
                        if request and hasattr(request, "arguments"):
                            args = request.arguments
                            item_id = args.get("item_id", [b""])[0].decode()
                            stac_api = args.get("stac_api", [b""])[0].decode() or None
                            var = args.get("var", [b""])[0].decode() or None
                            collection_id = args.get("collection_id", [b""])[0].decode() or None

                            # Cross-experiment comparison
                            compare = args.get("compare", [b""])[0].decode()
                            collection_a = args.get("collection_a", [b""])[0].decode()
                            collection_b = args.get("collection_b", [b""])[0].decode()

                            if compare and collection_a and collection_b and stac_api:
                                return create_comparison_preview_app(
                                    collection_a, collection_b, stac_api
                                )

                            if item_id:
                                return _create_panel_app_for_item(
                                    item_id, stac_api=stac_api, var=var, collection_id=collection_id
                                )

                            # Collection-level preview (no item_id, but collection_id present)
                            if collection_id and stac_api:
                                return create_collection_preview_app(
                                    collection_id, stac_api
                                )
            except Exception as e:
                logger.warning(f"Could not get request context: {e}")

            return pn.Column(
                pn.pane.Markdown("# ESM-Viz Interactive Preview"),
                pn.pane.Alert(
                    "No item or collection specified. Please provide an item_id or collection_id query parameter.",
                    alert_type="warning",
                ),
            )

        logger.info("Panel application routes configured at /_panel")

    except ImportError as e:
        logger.warning(
            f"Panel FastAPI integration not available: {e}. "
            "Install panel with FastAPI extras: pip install 'panel[fastapi]'"
        )
    except Exception as e:
        logger.error(f"Failed to set up Panel routes: {e}")


# Set up Panel routes when the module loads
try:
    setup_panel_routes(app)
except Exception as e:
    logger.warning(f"Panel route setup deferred: {e}")
