"""MCP tool implementations for ESM catalog access.

Each function is decorated with @mcp.tool() in server.py and exposed to LLMs
via the Model Context Protocol. Tools communicate with the running STAC API
via HTTP (pystac_client.Client).
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import uuid
from pathlib import Path


def _client(catalog_url: str):
    """Return a pystac_client.Client for the given catalog URL."""
    import pystac_client

    return pystac_client.Client.open(catalog_url)


def list_collections(catalog_url: str) -> str:
    """List all experiment collections available in the ESM catalog.

    Returns a JSON array of objects with id, title, and description.
    """
    import httpx

    resp = httpx.get(f"{catalog_url}/collections", timeout=30)
    resp.raise_for_status()
    body = resp.json()
    collections = body.get("collections", [])
    summary = [
        {
            "id": c.get("id"),
            "title": c.get("title"),
            "description": c.get("description", ""),
        }
        for c in collections
    ]
    return json.dumps(summary, indent=2)


def search_items(
    catalog_url: str,
    collection: str,
    variable: str | None = None,
    start_date: str | None = None,
    end_date: str | None = None,
    limit: int = 20,
) -> str:
    """Search for data files in a collection.

    Returns a JSON array of items with id, datetime, variable, and file path.

    Args:
        catalog_url: STAC API base URL.
        collection: Collection ID (e.g. "basic-001-fesom").
        variable: Filter by variable name (e.g. "ssh", "tas").
        start_date: ISO-8601 start date (e.g. "0850-01-01").
        end_date: ISO-8601 end date (e.g. "0860-12-31").
        limit: Maximum number of items to return (default 20).
    """
    import httpx

    params: dict = {"collections": collection, "limit": limit}

    if start_date or end_date:
        start = start_date or ".."
        end = end_date or ".."
        params["datetime"] = f"{start}/{end}"

    filter_parts = []
    if variable:
        filter_parts.append(f"variable = '{variable}'")
    if filter_parts:
        params["filter"] = " AND ".join(filter_parts)
        params["filter-lang"] = "cql2-text"

    resp = httpx.get(f"{catalog_url}/search", params=params, timeout=30)
    resp.raise_for_status()
    body = resp.json()

    features = body.get("features", [])
    items = []
    for f in features:
        props = f.get("properties", {})
        assets = f.get("assets", {})
        href = ""
        for asset in assets.values():
            href = asset.get("href", "")
            if href:
                # Strip file:// prefix for display
                href = href.removeprefix("file://")
                break
        items.append(
            {
                "id": f.get("id"),
                "datetime": props.get("datetime"),
                "variable": props.get("variable"),
                "path": href,
            }
        )

    total = body.get("numberMatched", len(items))
    return json.dumps({"total_matched": total, "returned": len(items), "items": items}, indent=2)


def get_collection_info(catalog_url: str, collection_id: str) -> str:
    """Get metadata for a specific collection.

    Returns JSON with title, description, time range, spatial extent,
    variables, and item count.

    Args:
        catalog_url: STAC API base URL.
        collection_id: Collection ID (e.g. "basic-001-fesom").
    """
    import httpx

    resp = httpx.get(f"{catalog_url}/collections/{collection_id}", timeout=30)
    resp.raise_for_status()
    col = resp.json()

    # Also fetch queryables for variable list
    variables: list[str] = []
    try:
        qresp = httpx.get(
            f"{catalog_url}/collections/{collection_id}/queryables", timeout=10
        )
        if qresp.status_code == 200:
            qbody = qresp.json()
            props = qbody.get("properties", {})
            if "variable" in props:
                variables = props["variable"].get("enum", [])
    except Exception:
        pass

    # Item count via search with limit=0 (use numberMatched)
    item_count = None
    try:
        iresp = httpx.get(
            f"{catalog_url}/search",
            params={"collections": collection_id, "limit": 1},
            timeout=10,
        )
        if iresp.status_code == 200:
            item_count = iresp.json().get("numberMatched")
    except Exception:
        pass

    extent = col.get("extent", {})
    spatial = extent.get("spatial", {}).get("bbox", [[]])
    temporal = extent.get("temporal", {}).get("interval", [[]])

    summary = {
        "id": col.get("id"),
        "title": col.get("title"),
        "description": col.get("description", ""),
        "item_count": item_count,
        "variables": variables,
        "spatial_extent": spatial[0] if spatial else None,
        "temporal_extent": temporal[0] if temporal else None,
    }
    return json.dumps(summary, indent=2)


def run_python(code: str, timeout: int = 120) -> str:
    """Execute Python code for data analysis or plotting.

    xarray, numpy, matplotlib, and pandas are available.
    Call plt.show() to save a plot — it returns the file path automatically.

    IMPORTANT: always obtain real file paths first by calling search_items, then
    paste the actual path strings directly into the code. Never use placeholder
    strings like 'path/to/file.nc' — use the exact paths returned by search_items.

    Args:
        code: Python source code to execute. Must use real file paths.
        timeout: Execution timeout in seconds (default 120).

    Returns:
        JSON with stdout, stderr, returncode, and a list of generated PNG file paths.
    """
    plot_dir = Path(tempfile.gettempdir())
    plot_id = uuid.uuid4().hex[:8]
    plot_prefix = f"plot_{plot_id}"

    # Inject matplotlib non-interactive backend and savefig path helper
    preamble = f"""\
import os, sys, pathlib
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
try:
    import xarray as xr
except ImportError:
    pass
try:
    import pandas as pd
except ImportError:
    pass

_PLOT_DIR = pathlib.Path("/tmp")
_PLOT_PREFIX = "{plot_prefix}"
_plot_counter = [0]

def _auto_savefig():
    _plot_counter[0] += 1
    path = _PLOT_DIR / f"{{_PLOT_PREFIX}}_{{_plot_counter[0]}}.png"
    plt.savefig(str(path), bbox_inches="tight", dpi=150)
    plt.close("all")
    print(f"[plot saved] {{path}}")
    return str(path)

# Patch plt.show() to save instead
plt.show = _auto_savefig
"""

    full_code = preamble + "\n" + code

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".py", delete=False, prefix="esm_mcp_"
    ) as f:
        f.write(full_code)
        script_path = f.name

    try:
        result = subprocess.run(
            [sys.executable, script_path],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return json.dumps(
            {"error": f"Execution timed out after {timeout}s", "stdout": "", "stderr": ""}
        )
    finally:
        Path(script_path).unlink(missing_ok=True)

    # Collect generated PNGs
    pngs = sorted(plot_dir.glob(f"{plot_prefix}_*.png"))
    png_paths = [str(p) for p in pngs]

    return json.dumps(
        {
            "returncode": result.returncode,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "plots": png_paths,
        },
        indent=2,
    )
