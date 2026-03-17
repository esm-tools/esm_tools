"""MCP server exposing ESM catalog tools via FastMCP.

Start with stdio transport (default — used by Open WebUI and most MCP clients):
    esm-catalog mcp

Start with SSE transport (HTTP-based MCP clients):
    esm-catalog mcp --transport sse --port 8001
"""

from __future__ import annotations

from functools import partial

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
            "simulation output files. Use list_collections to discover available experiments, "
            "get_collection_info to understand a dataset, search_items to find specific files, "
            "and run_python to analyse data or create plots with xarray and matplotlib."
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
    def run_python(code: str) -> str:
        """Execute Python for data analysis or plotting.

        xarray, numpy, matplotlib, and pandas are available.
        Call plt.show() to save a plot — it returns the file path automatically.
        Return values are captured in stdout; use print() to communicate results.

        Args:
            code: Python source code to execute.
        """
        return _tools.run_python(code)

    return mcp


def run(catalog_url: str, transport: str = "stdio", port: int = 8001) -> None:
    """Start the MCP server with the given transport."""
    mcp = create_server(catalog_url)

    if transport == "sse":
        mcp.settings.port = port
        mcp.run(transport="sse")
    else:
        mcp.run(transport="stdio")
