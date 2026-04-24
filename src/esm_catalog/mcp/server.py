"""MCP server exposing ESM catalog tools via FastMCP.

Start with streamable-http transport (Open WebUI "MCP Streamable HTTP"):
    esm-catalog mcp --transport streamable-http --port 23007
    → endpoint: http://localhost:23007/mcp

Start with SSE transport:
    esm-catalog mcp --transport sse --port 23007

Start with stdio transport (default — process-based MCP clients):
    esm-catalog mcp
"""

from __future__ import annotations

import json as _json
from pathlib import Path as _Path

from . import tools as _tools


def _with_images(result_json: str):
    """Attach Image objects for any PNG paths listed in a tool result JSON."""
    from mcp.server.fastmcp import Image

    try:
        data = _json.loads(result_json)
    except Exception:
        return result_json

    plots = data.get("plots", [])
    if not plots:
        return result_json

    contents: list = [result_json]
    for png_path in plots:
        try:
            contents.append(Image(path=png_path))
        except Exception:
            pass
    return contents


def _build_instructions(viz_url: str | None) -> str:
    viz_available = viz_url is not None
    viz_tool_line = (
        "- preview_item: PREFERRED for spatial plots — renders a variable directly "
        "via the viz server (handles FESOM grids, land-sea mask, projections automatically)\n"
        if viz_available else ""
    )
    rule_3 = (
        "3. For spatial/map plots, ALWAYS call preview_item first.\n"
        if viz_available else
        "3. For analysis and plots, use open_and_run.\n"
    )
    return (
        "You have access to an ESM (Earth System Model) catalog containing climate "
        "simulation output files stored on an HPC cluster.\n\n"
        "Available tools:\n"
        "- list_collections: discover available experiments\n"
        "- search_collections: find collections by run configuration / namelist parameters\n"
        "- get_collection_info: get variables, time range, spatial extent, item count\n"
        "- search_items: find file paths by collection, variable, and date range\n"
        + viz_tool_line +
        "- compare_collections: side-by-side NML parameter comparison across multiple collections\n"
        "- open_and_run: find files and run Python code on them\n"
        "- run_python: execute arbitrary Python code\n\n"
        "RULES — FOLLOW EXACTLY:\n"
        "1. NEVER write a text response before calling a tool. "
        "Call the tool FIRST, THEN summarise the results. "
        "Do NOT say 'I don't have enough information' — you have tools; use them.\n"
        "2. ANY request involving files or data (find, open, inspect, analyse, plot) "
        "MUST be answered by calling a tool. "
        "NEVER output a Python code block (```python ... ```) to the user under any circumstances. "
        "If you need to run code, call open_and_run or run_python — do NOT show the code.\n"
        + rule_3 +
        "4. If a tool returns an error or 0 results, call another tool to fix it — "
        "do NOT write a text explanation. If search_items returns 0, retry with "
        "lowercase variable name (e.g. 'sst' not 'SST').\n"
        "5. To find collections by namelist/NML parameter, call search_collections with a "
        "CQL2-text filter (e.g. nml:run_config.use_ice = true). "
        "To find collections by variable, call search_collections(variable='sst').\n"
        "6. When search_collections returns multiple collections, call compare_collections "
        "with their IDs to show what NML parameters differ between the runs.\n"
        "7. When a tool returns a list of N results, ALWAYS report ALL N results in your "
        "response. NEVER truncate, abbreviate, or say 'and X more' — list every item.\n\n"
        "FESOM UNSTRUCTURED GRID — if you must use open_and_run for FESOM spatial plots, "
        "you MUST load elem2d.out for the triangulation (nod2d.out alone causes artifacts):\n"
        "  import numpy as np, matplotlib.pyplot as plt, matplotlib.tri as tri\n"
        "  import cartopy.crs as ccrs, cartopy.feature as cfeature\n"
        "  mesh = np.loadtxt('/albedo/pool/FESOM2/core2/nod2d.out', skiprows=1, usecols=(1,2))\n"
        "  lon, lat = mesh[:,0], mesh[:,1]\n"
        "  elems = np.loadtxt('/albedo/pool/FESOM2/core2/elem2d.out', skiprows=1, dtype=int) - 1\n"
        "  triang = tri.Triangulation(lon, lat, triangles=elems)\n"
        "  lon_tri = lon[elems]; triang.set_mask(np.max(lon_tri,axis=1)-np.min(lon_tri,axis=1)>180)\n"
        "  fig = plt.figure(figsize=(14,7)); ax = fig.add_subplot(111, projection=ccrs.Robinson())\n"
        "  ax.set_global(); im = ax.tripcolor(triang, values, cmap='RdBu_r', transform=ccrs.PlateCarree())\n"
        "  ax.add_feature(cfeature.LAND, color='lightgray', zorder=1)\n"
        "  ax.add_feature(cfeature.COASTLINE, linewidth=0.5, zorder=2)\n"
        "  plt.colorbar(im, ax=ax); plt.show()"
    )


def create_server(catalog_url: str, viz_url: str | None = None):
    """Build and return a configured FastMCP server instance."""
    try:
        from mcp.server.fastmcp import FastMCP
    except ImportError as e:
        raise ImportError(
            "The 'mcp' package is required for the MCP server.\n"
            "Install with: pip install 'esm-catalog[mcp]'"
        ) from e

    # Use host="0.0.0.0" at construction to prevent FastMCP from auto-enabling
    # DNS rebinding protection (which would reject non-localhost Host headers
    # from reverse proxies such as Traefik).
    mcp = FastMCP("ESM Catalog", instructions=_build_instructions(viz_url), host="0.0.0.0")

    # Bind catalog_url into each tool so the LLM only needs to supply scientific parameters.
    @mcp.tool()
    def list_collections() -> str:
        """List all experiment collections available in the ESM catalog."""
        return _tools.list_collections(catalog_url)

    @mcp.tool()
    def get_collection_info(collection_id: str) -> str:
        """Get metadata for a specific collection: variables, time range, spatial extent,
        item count, and the number of namelist parameters available.

        To search collections by namelist parameter values, use search_collections.

        Args:
            collection_id: Collection ID, e.g. "basic-001-fesom".
        """
        return _tools.get_collection_info(catalog_url, collection_id)

    @mcp.tool()
    def search_collections(filter_expr: str = None, variable: str = None) -> str:
        """Find collections that match a run configuration, namelist parameter, or variable.

        Use this to answer questions like:
        - "Which experiments have ice enabled?"  → filter_expr="nml:run_config.use_ice = true"
        - "Which runs used CO2 above 400 ppm?"   → filter_expr="nml:radctl.co2vmr > 0.000400"
        - "Find all 1-year runs"                 → filter_expr="nml:run_config.nyear = 1"
        - "Which collections have SST?"          → variable="sst"

        Namelist parameter names use the format ``nml:group.param``.
        Multiple conditions can be combined with AND/OR.
        Without a filter, returns all collections.

        Args:
            filter_expr: CQL2-text filter expression. Optional.
                         Example: "nml:run_config.use_ice = true"
            variable: Variable name to filter by (e.g. "sst", "temp"). Optional.
        """
        return _tools.search_collections(catalog_url, filter_expr, variable)

    if viz_url is not None:
        @mcp.tool()
        def preview_item(
            collection: str,
            variable: str,
            time: int = 0,
            level: int = 0,
            cmap: str = "RdBu_r",
            start_date: str = None,
            end_date: str = None,
        ):
            """Generate a PNG map of a variable using the ESM Visualization Service.

            PREFERRED tool for all spatial/map plots. Handles FESOM unstructured grids,
            land-sea masking, and map projections automatically — no matplotlib code needed.

            Args:
                collection: Collection ID (e.g. "basic-001-fesom").
                variable: Variable name to plot (e.g. "sst", "ssh", "temp").
                time: Time step index (default 0 = first time step).
                level: Vertical level index (default 0 = surface).
                cmap: Colormap name (default "RdBu_r"). Use "viridis" for non-diverging data.
                start_date: ISO-8601 start date to select a specific item. Optional.
                end_date: ISO-8601 end date to select a specific item. Optional.
            """
            return _with_images(
                _tools.preview_item(
                    catalog_url, viz_url, collection, variable, time, level, cmap,
                    start_date, end_date,
                )
            )

    @mcp.tool()
    def search_items(
        collection: str,
        variable: str = None,
        start_date: str = None,
        end_date: str = None,
        limit: int = 20,
    ) -> str:
        """Search for data files in a collection. Returns paths, dates, and variables.

        Args:
            collection: Collection ID (e.g. "basic-001-fesom").
            variable: Filter by variable name (e.g. "ssh", "tas"). Optional.
            start_date: ISO-8601 start date (e.g. "0850-01-01"). Optional.
            end_date: ISO-8601 end date (e.g. "0860-12-31"). Optional.
            limit: Maximum number of items to return (default 20, max 200).
        """
        return _tools.search_items(catalog_url, collection, variable, start_date, end_date, limit)

    @mcp.tool()
    def compare_collections(collection_ids: list[str]) -> str:
        """Compare NML parameters side-by-side across multiple collections.

        Call this after search_collections returns several matching IDs to understand
        what scientifically distinguishes each run — which parameters are identical
        across all experiments and which vary.

        Returns per-collection metadata plus a split view:
          - identical_across_all: NML params with the same value in every collection
          - varying: params that differ, with each collection's value shown

        Args:
            collection_ids: List of collection IDs (e.g. from search_collections results).
        """
        return _tools.compare_collections(catalog_url, collection_ids)

    @mcp.tool()
    def open_and_run(
        collection: str,
        code: str,
        variable: str = None,
        start_date: str = None,
        end_date: str = None,
    ):
        """Find matching files and run Python code on them — one step, no search needed.

        Two placeholders are substituted in code before execution:
          {path}  — first matching file path (for single-file analysis)
          {paths} — Python list of all matching paths (for multi-file with open_mfdataset)

        FESOM data is on an UNSTRUCTURED grid. For any spatial plot you MUST load
        the mesh node coordinates and use plt.tripcolor — NEVER use ds[var].plot():

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
            code: Python code with {path} or {paths} placeholder.
            variable: Variable name to filter by (e.g. "SST"). Optional.
            start_date: ISO-8601 start date. Optional.
            end_date: ISO-8601 end date. Optional.
        """
        return _with_images(
            _tools.open_and_run(catalog_url, collection, code, variable, start_date, end_date)
        )

    @mcp.tool()
    def run_python(code: str):
        """Execute Python for data analysis or plotting.

        xarray, numpy, matplotlib, and pandas are available.
        Call plt.show() to save a plot — the image is returned automatically.
        Return values are captured in stdout; use print() to communicate results.

        PREREQUISITE: You MUST call search_items first to obtain real file paths.
        Embed the exact 'path' string from those results directly in the code.
        If this tool returns REJECTED, call search_items immediately — do NOT
        write a text response explaining what you would do.

        Args:
            code: Python source code to execute. Must contain literal file paths
                  obtained from search_items, never placeholders.
        """
        return _with_images(_tools.run_python(code))

    return mcp


def run(
    catalog_url: str,
    transport: str = "stdio",
    port: int = 8001,
    viz_url: str | None = None,
    base_url: str | None = None,
    path: str | None = None,
) -> None:
    """Start the MCP server with the given transport.

    transport="streamable-http" — FastMCP Streamable HTTP; endpoint at /mcp (or --path)
                                   Open WebUI "MCP Streamable HTTP" integration
    transport="openapi"         — FastAPI REST server; /openapi.json for Open WebUI
    transport="sse"             — FastMCP SSE server
    transport="stdio"           — FastMCP stdio; process-based MCP clients (default)
    """
    if transport == "openapi":
        from .openapi_server import run as openapi_run

        openapi_run(catalog_url=catalog_url, port=port, base_url=base_url)
        return

    mcp = create_server(catalog_url, viz_url=viz_url)

    if transport == "streamable-http":
        mcp.settings.host = "0.0.0.0"
        mcp.settings.port = port
        if path is not None:
            mcp.settings.streamable_http_path = path
        mcp.run(transport="streamable-http")
    elif transport == "sse":
        mcp.settings.host = "0.0.0.0"
        mcp.settings.port = port
        mcp.run(transport="sse")
    else:
        mcp.run(transport="stdio")
