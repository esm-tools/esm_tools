"""
FastAPI application for the ESM visualization service.

Provides REST endpoints for generating static previews and metadata
from climate datasets referenced in STAC catalogs.
"""

from typing import Annotated

import httpx
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
        Information about the interactive app (placeholder for now).

    Notes
    -----
    This endpoint currently returns placeholder information.
    Full Panel app serving is planned for a future release.
    """
    logger.info(f"Interactive app request: item={item_id}")

    href = None
    if stac_api:
        try:
            item = await _fetch_stac_item(stac_api, item_id)
            href = _get_data_href(item)
        except HTTPException:
            pass

    app_info = create_preview_app(href or item_id, variable=var)
    app_info["item_id"] = item_id

    return app_info


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
            "/app/{item_id}": "Interactive app (coming soon)",
            "/docs": "OpenAPI documentation",
        },
    }
