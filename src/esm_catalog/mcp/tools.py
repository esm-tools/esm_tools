"""MCP tool implementations for ESM catalog access.

Each function is decorated with @mcp.tool() in server.py and exposed to LLMs
via the Model Context Protocol. Tools communicate with the running STAC API
via HTTP (pystac_client.Client).
"""

from __future__ import annotations

import json
import re as _re
import subprocess
import sys
import tempfile
import uuid
from pathlib import Path


# ---------------------------------------------------------------------------
# Placeholder detection — catches LLMs that forget to substitute real paths
# ---------------------------------------------------------------------------

# ALL_CAPS strings in quotes (≥6 chars) are almost always unsubstituted
# placeholders (e.g. "REPLACE_WITH_FILE_PATH"). Explicit patterns below cover
# lowercase/special-char cases the regex cannot catch.
_PLACEHOLDER_PATTERNS = [
    # Common template-style placeholders
    "/path/to/file",
    "path/to/file",
    "your_file_path",
    "<file_path>",
    "<path>",
    # open_and_run substitution tokens — different error message
    "'{path}'",
    '"{path}"',
    "'{paths}'",
    '"{paths}"',
    "{path}",
    "{paths}",
    # Lowercase string literals produced by weaker models
    "('path'",
    '("path"',
    "= 'path'",
    '= "path"',
    "['path'",
    '["path"',
    "'file_path'",
    '"file_path"',
    "'path_to_file'",
    '"path_to_file"',
    "'actual_path'",
    '"actual_path"',
]

_PLACEHOLDER_RE = _re.compile(r"""['"][A-Z][A-Z0-9_]{5,}['"]""")
_OPEN_AND_RUN_TOKENS = frozenset(("{path}", "{paths}"))


def _check_placeholder(code: str) -> str | None:
    """Return a REJECTED error JSON string if code contains a placeholder path.

    Returns None if the code looks clean.
    """
    for pat in _PLACEHOLDER_PATTERNS:
        if pat in code:
            if pat.strip("'\"") in _OPEN_AND_RUN_TOKENS:
                msg = (
                    f"REJECTED: code contains unsubstituted open_and_run token '{pat}'. "
                    "⚠️ You called run_python directly with a {path}/{paths} token that only "
                    "open_and_run substitutes. DO NOT call run_python directly for catalog data. "
                    "Call open_and_run instead — it finds the files and substitutes {path}/{paths} "
                    "before executing the code."
                )
            else:
                msg = (
                    f"REJECTED: code contains placeholder '{pat}'. "
                    "⚠️ DO NOT write a text response. You MUST use the tools: "
                    "Step 1 — call search_items with the relevant collection and variable to get real file paths. "
                    "Step 2 — copy the exact 'path' string from those results into the code. "
                    "Step 3 — call run_python again with the literal path embedded. "
                    "Never invent, guess, or use placeholder strings for paths."
                )
            return json.dumps({"error": msg, "stdout": "", "stderr": "", "plots": []})

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

    return None


# Error returned when FESOM code omits elem2d.out or misuses .plot().
_FESOM_REJECTION = json.dumps({
    "error": (
        "REJECTED: FESOM data is on an unstructured grid — .plot() would produce "
        "a 1-D node-index chart, not a geographic map.\n"
        "You MUST use plt.tripcolor with the FESOM mesh triangulation from elem2d.out. "
        "Using nod2d.out alone (without elem2d.out) creates spurious triangles across "
        "land and poles, producing streak artifacts and no land-sea mask.\n"
        "Call open_and_run again with this pattern:\n\n"
        "    import numpy as np, matplotlib.pyplot as plt, matplotlib.tri as tri\n"
        "    import xarray as xr\n"
        "    import cartopy.crs as ccrs, cartopy.feature as cfeature\n"
        "    # Load mesh nodes and triangulation\n"
        "    mesh = np.loadtxt('/albedo/pool/FESOM2/core2/nod2d.out', skiprows=1, usecols=(1,2))\n"
        "    lon, lat = mesh[:,0], mesh[:,1]\n"
        "    elems = np.loadtxt('/albedo/pool/FESOM2/core2/elem2d.out', skiprows=1, dtype=int) - 1\n"
        "    triang = tri.Triangulation(lon, lat, triangles=elems)\n"
        "    # Mask triangles that span the dateline (pole artifacts)\n"
        "    lon_tri = lon[elems]\n"
        "    triang.set_mask(np.max(lon_tri, axis=1) - np.min(lon_tri, axis=1) > 180)\n"
        "    # Load data\n"
        "    ds = xr.open_mfdataset({paths}, combine='by_coords')\n"
        "    sst = ds['sst'].mean('time').values\n"
        "    # Plot with cartopy for land masking\n"
        "    fig = plt.figure(figsize=(14,7))\n"
        "    ax = fig.add_subplot(111, projection=ccrs.Robinson())\n"
        "    ax.set_global()\n"
        "    im = ax.tripcolor(triang, sst, cmap='coolwarm', transform=ccrs.PlateCarree())\n"
        "    ax.add_feature(cfeature.LAND, color='lightgray', zorder=1)\n"
        "    ax.add_feature(cfeature.COASTLINE, linewidth=0.5, zorder=2)\n"
        "    plt.colorbar(im, ax=ax, label='SST (°C)'); ax.set_title('Mean SST'); plt.show()\n\n"
        "Replace 'sst' with the actual variable name if different."
    ),
    "stdout": "",
    "stderr": "",
    "plots": [],
})


# ---------------------------------------------------------------------------
# Catalog helpers
# ---------------------------------------------------------------------------

def _client(catalog_url: str):
    """Return a pystac_client.Client for the given catalog URL."""
    import pystac_client

    return pystac_client.Client.open(catalog_url)


def _get_collection_variables(catalog_url: str, collection_id: str) -> list[str]:
    """Return the distinct variable names for a collection.

    Tries the queryables endpoint first (fast, cached by the API).
    Falls back to scanning items directly if queryables returns nothing
    (e.g. right after an API restart before the cache is warm).
    """
    import httpx

    # --- Primary: queryables enum (O(1), cached) ---
    try:
        qresp = httpx.get(
            f"{catalog_url}/collections/{collection_id}/queryables", timeout=10
        )
        if qresp.status_code == 200:
            props = qresp.json().get("properties", {})
            variables = props.get("variable", {}).get("enum", [])
            if variables:
                return sorted(variables)
    except Exception:
        pass

    # --- Fallback: scan up to 200 items for distinct variable values ---
    try:
        resp = httpx.get(
            f"{catalog_url}/search",
            params={"collections": collection_id, "limit": 200},
            timeout=15,
        )
        if resp.status_code == 200:
            features = resp.json().get("features", [])
            variables = sorted(
                {
                    f["properties"]["variable"]
                    for f in features
                    if f.get("properties", {}).get("variable")
                }
            )
            return variables
    except Exception:
        pass

    return []


def list_collections(catalog_url: str) -> str:
    """List all experiment collections available in the ESM catalog.

    Returns a JSON array of objects with id, title, and variables.
    oasis3mct collections (coupler output) are excluded as they are not
    directly useful for scientific analysis.
    """
    import httpx
    from collections import defaultdict

    resp = httpx.get(f"{catalog_url}/collections", timeout=30)
    resp.raise_for_status()
    body = resp.json()
    collections = body.get("collections", [])

    # Exclude oasis3mct: it is coupler exchange data, not model output
    collections = [c for c in collections if "oasis3mct" not in c.get("id", "")]

    # Get variables for all collections in ONE search request (avoids N queries)
    collection_vars: dict[str, set] = defaultdict(set)
    try:
        sresp = httpx.get(
            f"{catalog_url}/search", params={"limit": 500}, timeout=30
        )
        if sresp.status_code == 200:
            for f in sresp.json().get("features", []):
                cid = f.get("collection", "")
                var = f.get("properties", {}).get("variable", "")
                if cid and var:
                    collection_vars[cid].add(var)
    except Exception:
        pass

    summary = []
    for c in collections:
        cid = c.get("id", "")
        entry: dict = {"id": cid, "title": c.get("title", "")}
        variables = sorted(collection_vars.get(cid, set()))
        if variables:
            entry["variables"] = variables
        summary.append(entry)

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

    if variable:
        # Variable names are stored lowercase; normalise to avoid 0-result misses
        params["filter"] = f"variable = '{variable.lower()}'"
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
    variables, item count, and namelist (run configuration) parameters.

    Args:
        catalog_url: STAC API base URL.
        collection_id: Collection ID (e.g. "basic-001-fesom").
    """
    import httpx

    resp = httpx.get(f"{catalog_url}/collections/{collection_id}", timeout=30)
    resp.raise_for_status()
    col = resp.json()

    # Fetch variable list (queryables with item-scan fallback)
    variables = _get_collection_variables(catalog_url, collection_id)

    # Item count via search with limit=1 (use numberMatched)
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

    nml_params = col.get("nml:parameters", {})
    summary = {
        "id": col.get("id"),
        "title": col.get("title"),
        "description": col.get("description", ""),
        "item_count": item_count,
        "variables": variables,
        "spatial_extent": spatial[0] if spatial else None,
        "temporal_extent": temporal[0] if temporal else None,
        "nml_parameters": nml_params,
    }
    return json.dumps(summary, indent=2)


def search_collections(
    catalog_url: str,
    filter_expr: str | None = None,
    variable: str | None = None,
) -> str:
    """Search collections by metadata, namelist parameters, or variable name.

    Supports CQL2-text filter expressions. Use this to find experiments that
    share a configuration setting — e.g. all runs with ice enabled, a specific
    CO2 concentration, or a particular time step.

    NML parameter names use the format ``nml:group.param``, e.g.:
      - ``nml:run_config.use_ice = true``
      - ``nml:run_config.nyear = 1``
      - ``nml:radctl.co2vmr > 0.00028``

    Use ``variable`` to find all collections that contain a specific variable,
    e.g. variable="sst" returns all collections that have SST output.

    Args:
        catalog_url: STAC API base URL.
        filter_expr: CQL2-text filter expression. Optional.
                     Examples: "nml:run_config.use_ice = true"
                               "nml:run_config.use_ice = true AND nml:run_config.nyear = 1"
        variable: Variable name to filter by (e.g. "sst", "temp"). Optional.
    """
    import httpx

    params: dict = {}
    if filter_expr:
        params["filter"] = filter_expr
        params["filter-lang"] = "cql2-text"

    resp = httpx.get(f"{catalog_url}/collections", params=params, timeout=30)
    resp.raise_for_status()
    body = resp.json()

    collections = body.get("collections", [])

    # If variable filter requested, use a single STAC search to find all
    # collections that contain at least one item with that variable.
    if variable:
        var_lower = variable.lower()
        sresp = httpx.get(
            f"{catalog_url}/search",
            params={
                "filter": f"variable = '{var_lower}'",
                "filter-lang": "cql2-text",
                "limit": 500,
            },
            timeout=30,
        )
        sresp.raise_for_status()
        features = sresp.json().get("features", [])
        # Deduplicate collection IDs while preserving order
        seen: set = set()
        matching_ids: list[str] = []
        for f in features:
            cid = f.get("collection", "")
            if cid and cid not in seen:
                seen.add(cid)
                matching_ids.append(cid)
        # Build titles from the collections list
        title_map = {c.get("id", ""): c.get("title", "") for c in collections}
        matching = [
            {"id": cid, "title": title_map.get(cid, cid)}
            for cid in matching_ids
        ]
        return json.dumps({"total_matched": len(matching), "collections": matching}, indent=2)

    summary = [{"id": c.get("id"), "title": c.get("title")} for c in collections]
    total = body.get("numberMatched", len(summary))
    return json.dumps({"total_matched": total, "collections": summary}, indent=2)


def compare_collections(
    catalog_url: str,
    collection_ids: list[str],
) -> str:
    """Compare NML parameters and metadata across multiple collections.

    For each collection, fetches its namelist parameters and temporal extent.
    Then computes which NML parameters are identical across all collections
    and which vary — making it easy to understand what distinguishes each run.

    Call this after search_collections returns a list of matching IDs to get
    a side-by-side scientific comparison.

    Args:
        catalog_url: STAC API base URL.
        collection_ids: List of collection IDs to compare (e.g. from search_collections).
    """
    import httpx

    records = []
    for cid in collection_ids:
        try:
            resp = httpx.get(f"{catalog_url}/collections/{cid}", timeout=30)
            resp.raise_for_status()
            col = resp.json()
        except Exception as e:
            records.append({"id": cid, "error": str(e)})
            continue

        nml_raw = col.get("nml:parameters", {})
        # Flatten {"run_config": {"use_ice": true}} → {"run_config.use_ice": true}
        nml_flat: dict = {}
        for group, params in nml_raw.items():
            if isinstance(params, dict):
                for k, v in params.items():
                    nml_flat[f"{group}.{k}"] = v
            else:
                nml_flat[group] = params

        extent = col.get("extent", {})
        temporal = extent.get("temporal", {}).get("interval", [[]])

        records.append({
            "id": cid,
            "title": col.get("title", ""),
            "temporal_extent": temporal[0] if temporal else None,
            "nml_parameters": nml_flat,
        })

    # Compute which NML params are identical vs varying across all valid records
    valid = [r for r in records if "nml_parameters" in r]
    all_keys = sorted({k for r in valid for k in r["nml_parameters"]})

    identical: dict = {}
    varying: dict = {}
    for key in all_keys:
        values = {r["id"]: r["nml_parameters"].get(key) for r in valid}
        if len({str(v) for v in values.values()}) == 1:
            identical[key] = next(iter(values.values()))
        else:
            varying[key] = values

    return json.dumps({
        "total": len(records),
        "collections": records,
        "nml_comparison": {
            "identical_across_all": identical,
            "varying": varying,
        },
    }, indent=2)


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
    # FESOM guard: unstructured-grid collections require plt.tripcolor with elem2d.out
    # Trigger if: using .plot() (wrong method), OR using tripcolor without elem2d.out
    # (nod2d.out alone causes streak artifacts and no land-sea mask)
    _fesom = "fesom" in collection.lower()
    _bad_plot = ".plot(" in code and "tripcolor" not in code
    _bad_tripcolor = "tripcolor" in code and "elem2d.out" not in code
    if _fesom and (_bad_plot or _bad_tripcolor):
        return _FESOM_REJECTION

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
    resolved_code = code.replace("{path}", first_path).replace("{paths}", repr(all_paths))
    return run_python(resolved_code, timeout=timeout)


def preview_item(
    catalog_url: str,
    viz_url: str,
    collection: str,
    variable: str,
    time: int = 0,
    level: int = 0,
    cmap: str = "RdBu_r",
    start_date: str | None = None,
    end_date: str | None = None,
) -> str:
    """Generate a PNG preview of a variable via the ESM Visualization Service.

    Finds the first matching STAC item and renders it using the viz server,
    which handles FESOM unstructured grids, land-sea masking, and projections
    automatically. Prefer this over open_and_run for spatial plots.

    Args:
        catalog_url: STAC API base URL.
        viz_url: ESM Visualization Service base URL (e.g. "http://localhost:23001").
        collection: Collection ID (e.g. "basic-001-fesom").
        variable: Variable name to plot (e.g. "sst", "ssh", "temp").
        time: Time step index (default 0 = first time step).
        level: Vertical level index (default 0 = surface).
        cmap: Matplotlib colormap name (default "RdBu_r").
        start_date: ISO-8601 start date to pick item from. Optional.
        end_date: ISO-8601 end date to pick item from. Optional.
    """
    import httpx

    # Find first matching item to get a valid item_id
    result = json.loads(
        search_items(catalog_url, collection, variable, start_date, end_date, limit=1)
    )
    items = result.get("items", [])
    if not items:
        return json.dumps({
            "error": (
                f"No items found for collection={collection!r}"
                + (f", variable={variable!r}" if variable else "")
            ),
            "plots": [],
        })

    item_id = items[0]["id"]

    params = {
        "var": variable,
        "stac_api": catalog_url,
        "time": time,
        "level": level,
        "cmap": cmap,
        "collection_id": collection,
    }
    try:
        resp = httpx.get(
            f"{viz_url}/preview/{item_id}.png",
            params=params,
            timeout=120,
            follow_redirects=True,
        )
        resp.raise_for_status()
    except Exception as e:
        return json.dumps({"error": f"Viz server error: {e}", "plots": []})

    # Save PNG to temp file
    plot_id = uuid.uuid4().hex[:8]
    png_path = Path(tempfile.gettempdir()) / f"viz_preview_{plot_id}.png"
    png_path.write_bytes(resp.content)

    return json.dumps({
        "item_id": item_id,
        "variable": variable,
        "time_index": time,
        "plots": [str(png_path)],
    })


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
    rejection = _check_placeholder(code)
    if rejection:
        return rejection

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
