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


def create_server(catalog_url: str):
    """Build and return a configured FastMCP server instance."""
    try:
        from mcp.server.fastmcp import FastMCP
    except ImportError as e:
        raise ImportError(
            "The 'mcp' package is required for the MCP server.\n"
            "Install with: pip install 'esm-catalog[mcp]'"
        ) from e

    mcp = FastMCP(
        "ESM Catalog",
        instructions=(
            "You have access to an ESM (Earth System Model) catalog containing climate "
            "simulation output files stored on an HPC cluster.\n\n"
            "Available tools:\n"
            "- list_collections: discover available experiments\n"
            "- get_collection_info: get variables, time range, spatial extent, item count\n"
            "- search_items: find file paths by collection, variable, and date range\n"
            "- open_and_run: find the first matching file and run Python code on it in one step\n"
            "- run_python: execute Python code using xarray and matplotlib\n\n"
            "RULES YOU MUST FOLLOW:\n"
            "1. ANY request that involves files or data — open, load, read, find, inspect, "
            "examine, print, check, analyse, visualise, or plot — MUST be handled by calling "
            "a tool. Do NOT write code blocks in your response. "
            "Do NOT show Python code to the user. ALWAYS call the tool and report the output.\n"
            "2. PREFERRED: use open_and_run(collection, variable, code) for all single-file "
            "analysis and plots. Write {path} in your code where the file path belongs — "
            "it is substituted automatically. You do NOT need to call search_items first.\n"
            "3. Only use search_items + run_python when you need to inspect multiple files "
            "or need the file listing before writing code.\n"
            "4. If a tool returns an error, call another tool to fix it. "
            "Do NOT write a text response explaining what you would do.\n\n"
            "FESOM UNSTRUCTURED GRID — spatial plots MUST use mesh coordinates:\n"
            "  import numpy as np, matplotlib.pyplot as plt\n"
            "  mesh = np.loadtxt('/albedo/pool/FESOM2/core2/nod2d.out', skiprows=1, usecols=(1, 2))\n"
            "  lon, lat = mesh[:, 0], mesh[:, 1]\n"
            "  sst_mean = ds['sst'].mean('time').values\n"
            "  plt.figure(figsize=(14, 7))\n"
            "  plt.tripcolor(lon, lat, sst_mean, cmap='coolwarm', shading='gouraud')\n"
            "  plt.colorbar(label='SST (°C)'); plt.title('Mean SST'); plt.show()\n"
            "NEVER use ds['sst'].plot() directly — it plots vs node index, not geography."
        ),
    )

    # Bind catalog_url into each tool so the LLM only needs to supply scientific parameters.
    @mcp.tool()
    def list_collections() -> str:
        """List all experiment collections available in the ESM catalog."""
        return _tools.list_collections(catalog_url)

    @mcp.tool()
    def get_collection_info(collection_id: str) -> str:
        """Get metadata for a specific collection: variables, time range, spatial extent, item count.

        Args:
            collection_id: Collection ID, e.g. "basic-001-fesom".
        """
        return _tools.get_collection_info(catalog_url, collection_id)

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
        from mcp.server.fastmcp import Image

        result_json = _tools.open_and_run(
            catalog_url, collection, code, variable, start_date, end_date
        )
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
        from mcp.server.fastmcp import Image

        result_json = _tools.run_python(code)
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

    return mcp


def run(catalog_url: str, transport: str = "stdio", port: int = 8001) -> None:
    """Start the MCP server with the given transport.

    transport="streamable-http" — FastMCP Streamable HTTP; endpoint at /mcp
                                   Open WebUI "MCP Streamable HTTP" integration
    transport="openapi"         — FastAPI REST server; /openapi.json for Open WebUI
    transport="sse"             — FastMCP SSE server
    transport="stdio"           — FastMCP stdio; process-based MCP clients (default)
    """
    if transport == "openapi":
        from .openapi_server import run as openapi_run

        openapi_run(catalog_url=catalog_url, port=port)
        return

    mcp = create_server(catalog_url)

    if transport == "streamable-http":
        mcp.settings.host = "0.0.0.0"
        mcp.settings.port = port
        mcp.run(transport="streamable-http")
    elif transport == "sse":
        mcp.settings.host = "0.0.0.0"
        mcp.settings.port = port
        mcp.run(transport="sse")
    else:
        mcp.run(transport="stdio")
