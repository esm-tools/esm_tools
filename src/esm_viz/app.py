"""
FastAPI application for the ESM visualization service.

Provides REST endpoints for generating static previews and metadata
from climate datasets referenced in STAC catalogs. Also serves
interactive Panel applications for data exploration.
"""

from typing import Annotated
from functools import partial

import httpx
import panel as pn
from fastapi import FastAPI, HTTPException, Query, Response
from fastapi.middleware.cors import CORSMiddleware
from loguru import logger

from esm_viz import __version__
from esm_viz.fesom import is_unstructured, plot_unstructured
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


async def _fetch_stac_item(stac_api: str, item_id: str) -> dict:
    """
    Fetch a STAC item from the API.

    Parameters
    ----------
    stac_api : str
        Base URL of the STAC API.
    item_id : str
        The STAC item ID to fetch.

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

    # Try different endpoint patterns
    endpoints = [
        f"{stac_api}/items/{item_id}",
        f"{stac_api}/collections/default/items/{item_id}",
        f"{stac_api}/search?ids={item_id}",
    ]

    async with httpx.AsyncClient(timeout=30.0) as client:
        for endpoint in endpoints:
            try:
                logger.debug(f"Trying STAC endpoint: {endpoint}")
                response = await client.get(endpoint)

                if response.status_code == 200:
                    data = response.json()

                    # Handle search response (returns FeatureCollection)
                    if "features" in data and len(data["features"]) > 0:
                        return data["features"][0]

                    # Direct item response
                    if "assets" in data:
                        return data

            except httpx.RequestError as e:
                logger.warning(f"Request failed for {endpoint}: {e}")
                continue

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
    cmap: Annotated[str, Query(description="Matplotlib colormap")] = "viridis",
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

    Returns
    -------
    Response
        PNG image response.
    """
    logger.info(f"Preview request: item={item_id}, var={var}, time={time}")

    try:
        # Fetch STAC item
        item = await _fetch_stac_item(stac_api, item_id)
        href = _get_data_href(item)
        logger.debug(f"Data href: {href}")

        # Open dataset
        ds = open_data(href)

        # Check if unstructured mesh (e.g., FESOM)
        if is_unstructured(ds):
            logger.info("Detected unstructured mesh data")
            # For unstructured data, we need special handling
            # This is a simplified path - full implementation would use plot_unstructured
            data_array = ds[var]
            if "time" in data_array.dims:
                data_array = data_array.isel(time=min(time, data_array.sizes["time"] - 1))

            import io
            import matplotlib
            matplotlib.use("Agg")
            import matplotlib.pyplot as plt

            fig = plot_unstructured(data_array, cmap=cmap, ds=ds)
            buf = io.BytesIO()
            fig.savefig(buf, format="png", dpi=100, bbox_inches="tight")
            buf.seek(0)
            png_bytes = buf.read()
            plt.close(fig)
        else:
            # Regular gridded data
            png_bytes = generate_preview_png(ds, var, time_index=time, cmap=cmap)

        # Close dataset
        ds.close()

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


@app.get("/preview/{item_id}.json")
async def get_preview_metadata(
    item_id: str,
    stac_api: Annotated[str, Query(description="STAC API base URL")],
) -> dict:
    """
    Get metadata for a dataset referenced by a STAC item.

    Parameters
    ----------
    item_id : str
        STAC item ID.
    stac_api : str
        Base URL of the STAC API.

    Returns
    -------
    dict
        Metadata including variables, dimensions, and coordinate ranges.
    """
    logger.info(f"Metadata request: item={item_id}")

    try:
        # Fetch STAC item
        item = await _fetch_stac_item(stac_api, item_id)
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


@app.get("/app/{item_id}")
async def get_interactive_app(
    item_id: str,
    stac_api: Annotated[str | None, Query(description="STAC API base URL")] = None,
    var: Annotated[str | None, Query(description="Initial variable")] = None,
) -> dict:
    """
    Get information about the interactive preview app.

    This endpoint returns metadata about the interactive app. To access
    the actual interactive Panel application, use the /panel/{item_id} endpoint
    which serves a full interactive visualization.

    Parameters
    ----------
    item_id : str
        STAC item ID.
    stac_api : str, optional
        Base URL of the STAC API.
    var : str, optional
        Initial variable to display.

    Returns
    -------
    dict
        Information about the interactive app including a link to the Panel app.
    """
    logger.info(f"Interactive app info request: item={item_id}")

    href = None
    if stac_api:
        try:
            item = await _fetch_stac_item(stac_api, item_id)
            href = _get_data_href(item)
        except HTTPException:
            pass

    # Build the Panel app URL
    panel_url = f"/panel/{item_id}"
    if stac_api:
        panel_url += f"?stac_api={stac_api}"
        if var:
            panel_url += f"&var={var}"
    elif var:
        panel_url += f"?var={var}"

    return {
        "item_id": item_id,
        "href": href,
        "panel_app_url": panel_url,
        "status": "available",
        "message": (
            "Interactive Panel application is available. "
            f"Visit {panel_url} to access the interactive visualization."
        ),
        "features": [
            "Variable selector dropdown",
            "Time slider for temporal navigation",
            "Level/depth selector for 3D data",
            "Smart colormap selection based on variable type",
            "Interactive pan/zoom with geographic projections",
            "Support for both gridded and unstructured mesh data (FESOM)",
        ],
    }


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
            "/app/{item_id}": "Interactive app info",
            "/panel/{item_id}": "Interactive Panel visualization",
            "/docs": "OpenAPI documentation",
        },
    }


# -----------------------------------------------------------------------------
# Panel Application Integration
# -----------------------------------------------------------------------------

# Store for dynamically created Panel apps based on item_id
_panel_app_cache: dict[str, pn.viewable.Viewable] = {}


def _create_panel_app_for_item(
    item_id: str,
    stac_api: str | None = None,
    var: str | None = None,
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

    Returns
    -------
    pn.viewable.Viewable
        Panel application for the item.
    """
    import asyncio

    logger.info(f"Creating Panel app for item: {item_id}")

    # We need to fetch the STAC item synchronously for Panel
    # This is a limitation of mixing async FastAPI with sync Panel
    href = None
    if stac_api:
        try:
            # Use synchronous httpx client
            with httpx.Client(timeout=30.0) as client:
                stac_api_norm = stac_api.rstrip("/")
                endpoints = [
                    f"{stac_api_norm}/items/{item_id}",
                    f"{stac_api_norm}/collections/default/items/{item_id}",
                ]
                for endpoint in endpoints:
                    try:
                        response = client.get(endpoint)
                        if response.status_code == 200:
                            data = response.json()
                            if "features" in data and len(data["features"]) > 0:
                                item = data["features"][0]
                            elif "assets" in data:
                                item = data
                            else:
                                continue

                            # Extract href from assets
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
                            if href:
                                break
                    except Exception as e:
                        logger.warning(f"Failed to fetch from {endpoint}: {e}")
                        continue
        except Exception as e:
            logger.error(f"Failed to fetch STAC item: {e}")

    if not href:
        # If no STAC API or fetch failed, treat item_id as a direct file path
        href = item_id
        logger.info(f"Using item_id as direct href: {href}")

    # Create the Panel app
    return create_preview_app(href, variable=var)


def setup_panel_routes(fastapi_app: FastAPI) -> None:
    """
    Set up Panel application routes on the FastAPI app.

    This function adds a route that serves Panel applications dynamically
    based on the item_id path parameter.

    Parameters
    ----------
    fastapi_app : FastAPI
        The FastAPI application to add routes to.
    """
    try:
        from panel.io.fastapi import add_application

        # Create a factory function that Panel can call
        def panel_app_factory() -> pn.viewable.Viewable:
            """
            Factory function for Panel app creation.

            Note: This gets query parameters from Panel's state.
            """
            # Get parameters from Panel's curdoc or state
            # In Panel/Bokeh context, we can access query params
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

                            if item_id:
                                return _create_panel_app_for_item(
                                    item_id, stac_api=stac_api, var=var
                                )
            except Exception as e:
                logger.warning(f"Could not get request context: {e}")

            # Return a placeholder if we can't get the item_id
            return pn.Column(
                pn.pane.Markdown("# ESM-Viz Interactive Preview"),
                pn.pane.Alert(
                    "No item specified. Please provide an item_id query parameter.",
                    alert_type="warning",
                ),
            )

        # Add the Panel application at /panel/
        add_application(
            "/panel/",
            panel_app_factory,
            title="ESM-Viz Interactive Preview",
        )
        logger.info("Panel application routes configured at /panel/")

    except ImportError as e:
        logger.warning(
            f"Panel FastAPI integration not available: {e}. "
            "Install panel with FastAPI extras: pip install 'panel[fastapi]'"
        )
    except Exception as e:
        logger.error(f"Failed to set up Panel routes: {e}")


# Alternative simpler approach: redirect to Panel server
@app.get("/panel/{item_id}")
async def panel_redirect(
    item_id: str,
    stac_api: Annotated[str | None, Query(description="STAC API base URL")] = None,
    var: Annotated[str | None, Query(description="Initial variable")] = None,
) -> dict:
    """
    Interactive Panel visualization endpoint.

    This endpoint provides information on how to access the interactive
    Panel application. For full interactivity, the Panel app should be
    served using Panel's server or embedded in the FastAPI app using
    panel.io.fastapi integration.

    Parameters
    ----------
    item_id : str
        STAC item ID or direct file path.
    stac_api : str, optional
        Base URL of the STAC API.
    var : str, optional
        Initial variable to display.

    Returns
    -------
    dict
        Information about accessing the Panel app.
    """
    logger.info(f"Panel endpoint accessed: item={item_id}")

    # Try to get the data href
    href = None
    if stac_api:
        try:
            item = await _fetch_stac_item(stac_api, item_id)
            href = _get_data_href(item)
        except HTTPException:
            href = item_id
    else:
        href = item_id

    # Check if we can load the data
    try:
        ds = open_data(href)
        variables = list(ds.data_vars.keys())
        is_unstructured_data = is_unstructured(ds)
        n_times = ds.dims.get("time", 1)
        ds.close()

        return {
            "status": "ready",
            "item_id": item_id,
            "href": href,
            "is_unstructured": is_unstructured_data,
            "variables": variables,
            "time_steps": n_times,
            "message": (
                "Data loaded successfully. To view the interactive Panel application, "
                "use: panel serve esm_viz/interactive.py --args --href={href}"
            ),
            "alternative": (
                "You can also use the Python API directly:\n"
                "from esm_viz.interactive import create_preview_app\n"
                f"app = create_preview_app('{href}')\n"
                "app.servable()"
            ),
        }
    except Exception as e:
        return {
            "status": "error",
            "item_id": item_id,
            "href": href,
            "error": str(e),
            "message": f"Failed to load data: {e}",
        }


# Set up Panel routes when the module loads
# This integrates Panel apps directly into FastAPI
try:
    setup_panel_routes(app)
except Exception as e:
    logger.warning(f"Panel route setup deferred: {e}")
