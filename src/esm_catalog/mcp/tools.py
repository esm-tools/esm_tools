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


import re as _re

_PLACEHOLDER_PATTERNS = [
    "REPLACE_WITH_FILE_PATH",
    "REPLACE_WITH_PATH",
    "PATH_FROM_SEARCH",
    "FILE_FROM_SEARCH",
    "FILE_PATH_HERE",
    "/path/to/file",
    "path/to/file",
    "your_file_path",
    "<file_path>",
    "<path>",
    # lowercase variants commonly produced by weaker models
    "'file_path'",
    '"file_path"',
    "'path_to_file'",
    '"path_to_file"',
    "'actual_path'",
    '"actual_path"',
    "= 'file_path'",
    '= "file_path"',
]

# Match any ALL_CAPS_SNAKE_CASE token ≥6 chars used as a string literal —
# these are almost always placeholders the model forgot to substitute.
_PLACEHOLDER_RE = _re.compile(r"""['"][A-Z][A-Z0-9_]{5,}['"]""")


def open_and_run(
    catalog_url: str,
    collection: str,
    code: str,
    variable: str | None = None,
    start_date: str | None = None,
    end_date: str | None = None,
    timeout: int = 120,
) -> str:
    """Find matching files and execute Python code on them.

    Placeholders substituted in code before execution:
      {path}  — path of the first matching file (string)
      {paths} — Python list of all matching file paths, e.g. ['/a.nc', '/b.nc']

    Use {path} for single-file analysis, {paths} for multi-file with open_mfdataset.

    Example (single file):
        ds = open_dataset('{path}')
        print(ds['SST'].min().values, ds['SST'].max().values)

    Example (all files — FESOM, correct geographic plot):
        import numpy as np, matplotlib.pyplot as plt, xarray as xr
        mesh = np.loadtxt('/albedo/pool/FESOM2/core2/nod2d.out', skiprows=1, usecols=(1,2))
        lon, lat = mesh[:,0], mesh[:,1]
        ds = xr.open_mfdataset({paths}, combine='by_coords')
        sst = ds['sst'].mean('time').values
        plt.figure(figsize=(14,7))
        plt.tripcolor(lon, lat, sst, cmap='coolwarm', shading='gouraud')
        plt.colorbar(label='SST (°C)'); plt.title('Mean SST'); plt.show()

    Args:
        collection: Collection ID (e.g. "basic-001-fesom").
        code: Python source code. Use {path} or {paths} as placeholders.
        variable: Filter by variable name (e.g. "SST", "ssh"). Optional.
        start_date: ISO-8601 start date. Optional.
        end_date: ISO-8601 end date. Optional.
        timeout: Execution timeout in seconds (default 120).
    """
    # FESOM guard: unstructured-grid collections require plt.tripcolor, not .plot()
    if "fesom" in collection.lower() and ".plot(" in code and "tripcolor" not in code:
        return json.dumps({
            "error": (
                "REJECTED: FESOM data is on an unstructured grid — .plot() would produce "
                "a 1-D node-index chart, not a geographic map.\n"
                "You MUST use plt.tripcolor with mesh node coordinates. "
                "Call open_and_run again with this pattern:\n\n"
                "    import numpy as np, matplotlib.pyplot as plt, xarray as xr\n"
                "    mesh = np.loadtxt('/albedo/pool/FESOM2/core2/nod2d.out', skiprows=1, usecols=(1,2))\n"
                "    lon, lat = mesh[:,0], mesh[:,1]\n"
                "    ds = xr.open_mfdataset({paths}, combine='by_coords')\n"
                "    sst = ds['sst'].mean('time').values\n"
                "    plt.figure(figsize=(14,7))\n"
                "    plt.tripcolor(lon, lat, sst, cmap='coolwarm', shading='gouraud')\n"
                "    plt.colorbar(label='SST (°C)'); plt.title('Mean SST'); plt.show()\n\n"
                "Replace 'sst' with the actual variable name if different."
            ),
            "stdout": "",
            "stderr": "",
            "plots": [],
        })

    # Use limit=1 for {path}-only code, full fetch when {paths} is needed
    limit = 200 if "{paths}" in code else 1
    result = json.loads(
        search_items(catalog_url, collection, variable, start_date, end_date, limit=limit)
    )
    items = result.get("items", [])
    if not items:
        return json.dumps({
            "error": (
                f"No files found for collection='{collection}'"
                + (f", variable='{variable}'" if variable else "")
            ),
            "stdout": "",
            "stderr": "",
            "plots": [],
        })

    first_path = items[0].get("path", "")
    if not first_path:
        return json.dumps({
            "error": "search_items returned an item with no path",
            "stdout": "",
            "stderr": "",
            "plots": [],
        })

    all_paths = [it.get("path", "") for it in items if it.get("path")]

    resolved_code = code.replace("{path}", first_path)
    resolved_code = resolved_code.replace("{paths}", repr(all_paths))
    return run_python(resolved_code, timeout=timeout)


def run_python(code: str, timeout: int = 120) -> str:
    """Execute Python code for data analysis or plotting.

    xarray, numpy, matplotlib, and pandas are available.
    Use open_dataset(path) (not xr.open_dataset) to open NetCDF files — it
    handles engine selection automatically. Call plt.show() to save a plot.

    IMPORTANT: always obtain real file paths first by calling search_items, then
    paste the actual path strings directly into the code. Never use placeholder
    strings like 'path/to/file.nc' — use the exact paths returned by search_items.

    Args:
        code: Python source code to execute. Must use real file paths.
        timeout: Execution timeout in seconds (default 120).

    Returns:
        JSON with stdout, stderr, returncode, and a list of generated PNG file paths.
    """
    for pat in _PLACEHOLDER_PATTERNS:
        if pat in code:
            return json.dumps({
                "error": (
                    f"REJECTED: code contains placeholder '{pat}'. "
                    "⚠️ DO NOT write a text response. You MUST use the tools: "
                    "Step 1 — call search_items with the relevant collection and variable to get real file paths. "
                    "Step 2 — copy the exact 'path' string from those results into the code. "
                    "Step 3 — call run_python again with the literal path embedded. "
                    "Never invent, guess, or use placeholder strings for paths."
                ),
                "stdout": "",
                "stderr": "",
                "plots": [],
            })
    m = _PLACEHOLDER_RE.search(code)
    if m:
        return json.dumps({
            "error": (
                f"REJECTED: code contains unsubstituted placeholder {m.group()}. "
                "⚠️ DO NOT write a text response. You MUST use the tools: "
                "Step 1 — call search_items with the relevant collection and variable to get real file paths. "
                "Step 2 — copy the exact 'path' string from those results into the code. "
                "Step 3 — call run_python again with the literal path embedded. "
                "Never invent, guess, or use placeholder strings for paths."
            ),
            "stdout": "",
            "stderr": "",
            "plots": [],
        })

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
    _xr_open_dataset_orig = xr.open_dataset

    def open_dataset(path, **kwargs):
        # Open a NetCDF file, trying netcdf4/h5netcdf engines automatically.
        kwargs.setdefault("decode_times", True)
        for engine in ("netcdf4", "h5netcdf", "scipy"):
            try:
                return _xr_open_dataset_orig(path, engine=engine, **kwargs)
            except Exception:
                pass
        return _xr_open_dataset_orig(path, **kwargs)

    # Patch xr.open_dataset so both open_dataset() and xr.open_dataset() work
    xr.open_dataset = open_dataset
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
