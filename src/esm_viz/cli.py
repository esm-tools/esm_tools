"""
CLI entry point for the ESM visualization service.

Provides command-line interface for starting the visualization server
and related utilities.
"""

import sys
from typing import Annotated, Optional

import typer
from loguru import logger

from esm_viz import __version__


app = typer.Typer(
    name="esm-viz",
    help="ESM visualization service for climate data.",
    no_args_is_help=True,
)


def _configure_logging(verbose: bool = False, quiet: bool = False) -> None:
    """Configure loguru logging based on verbosity settings."""
    logger.remove()

    if quiet:
        level = "ERROR"
    elif verbose:
        level = "DEBUG"
    else:
        level = "INFO"

    logger.add(
        sys.stderr,
        level=level,
        format="<green>{time:YYYY-MM-DD HH:mm:ss}</green> | <level>{level: <8}</level> | <cyan>{name}</cyan>:<cyan>{function}</cyan>:<cyan>{line}</cyan> - <level>{message}</level>",
    )


def version_callback(value: bool) -> None:
    """Print version and exit."""
    if value:
        typer.echo(f"esm-viz version {__version__}")
        raise typer.Exit()


@app.callback()
def main(
    version: Annotated[
        Optional[bool],
        typer.Option(
            "--version",
            "-V",
            callback=version_callback,
            is_eager=True,
            help="Show version and exit.",
        ),
    ] = None,
) -> None:
    """
    ESM Visualization Service.

    A FastAPI-based service for generating visualizations and metadata
    from climate datasets referenced in STAC catalogs.
    """
    pass


@app.command()
def serve(
    port: Annotated[
        int,
        typer.Option("--port", "-p", help="Port to run the server on."),
    ] = 8001,
    host: Annotated[
        str,
        typer.Option("--host", "-h", help="Host to bind the server to."),
    ] = "0.0.0.0",
    reload: Annotated[
        bool,
        typer.Option("--reload", "-r", help="Enable auto-reload for development."),
    ] = False,
    workers: Annotated[
        int,
        typer.Option("--workers", "-w", help="Number of worker processes."),
    ] = 1,
    verbose: Annotated[
        bool,
        typer.Option("--verbose", "-v", help="Enable verbose logging."),
    ] = False,
    quiet: Annotated[
        bool,
        typer.Option("--quiet", "-q", help="Suppress non-error output."),
    ] = False,
) -> None:
    """
    Start the visualization server.

    Runs a uvicorn server hosting the FastAPI application for
    generating climate data visualizations.

    Examples
    --------
    Start server on default port:

        esm-viz serve

    Start on custom port with auto-reload:

        esm-viz serve --port 8080 --reload

    Production deployment with multiple workers:

        esm-viz serve --workers 4 --host 0.0.0.0
    """
    import uvicorn

    _configure_logging(verbose=verbose, quiet=quiet)

    logger.info(f"Starting ESM Visualization Service v{__version__}")
    logger.info(f"Server: http://{host}:{port}")
    logger.info(f"Documentation: http://{host}:{port}/docs")

    log_level = "debug" if verbose else ("error" if quiet else "info")

    uvicorn.run(
        "esm_viz.app:app",
        host=host,
        port=port,
        reload=reload,
        workers=workers if not reload else 1,
        log_level=log_level,
    )


@app.command()
def preview(
    path: Annotated[
        str,
        typer.Argument(help="Path to NetCDF or GRIB file."),
    ],
    variable: Annotated[
        str,
        typer.Option("--var", "-v", help="Variable to plot."),
    ],
    output: Annotated[
        Optional[str],
        typer.Option("--output", "-o", help="Output PNG file path."),
    ] = None,
    time_index: Annotated[
        int,
        typer.Option("--time", "-t", help="Time index to plot."),
    ] = 0,
    cmap: Annotated[
        str,
        typer.Option("--cmap", "-c", help="Matplotlib colormap."),
    ] = "viridis",
    verbose: Annotated[
        bool,
        typer.Option("--verbose", help="Enable verbose logging."),
    ] = False,
) -> None:
    """
    Generate a preview image from a local data file.

    This command creates a PNG visualization without starting the server,
    useful for quick previews and testing.

    Examples
    --------
    Generate preview of temperature variable:

        esm-viz preview data.nc --var tas --output preview.png

    Use a different colormap:

        esm-viz preview ocean.nc --var sst --cmap coolwarm
    """
    from pathlib import Path

    from esm_viz.readers import open_data
    from esm_viz.static_preview import generate_preview_png

    _configure_logging(verbose=verbose)

    path_obj = Path(path)
    if not path_obj.exists():
        logger.error(f"File not found: {path}")
        raise typer.Exit(1)

    # Determine output path
    if output is None:
        output = str(path_obj.with_suffix(".png"))

    logger.info(f"Generating preview: {path} -> {output}")
    logger.info(f"Variable: {variable}, Time: {time_index}, Colormap: {cmap}")

    try:
        ds = open_data(path)
        png_bytes = generate_preview_png(ds, variable, time_index=time_index, cmap=cmap)
        ds.close()

        with open(output, "wb") as f:
            f.write(png_bytes)

        logger.info(f"Preview saved to: {output}")
        typer.echo(f"Preview saved: {output}")

    except KeyError as e:
        logger.error(f"Variable error: {e}")
        raise typer.Exit(1)
    except Exception as e:
        logger.exception(f"Preview generation failed: {e}")
        raise typer.Exit(1)


@app.command()
def info(
    path: Annotated[
        str,
        typer.Argument(help="Path to NetCDF or GRIB file."),
    ],
    json_output: Annotated[
        bool,
        typer.Option("--json", "-j", help="Output as JSON."),
    ] = False,
) -> None:
    """
    Display metadata for a data file.

    Shows variables, dimensions, and coordinate ranges for a given
    climate data file.

    Examples
    --------
    Display file info:

        esm-viz info data.nc

    Output as JSON:

        esm-viz info data.nc --json
    """
    import json
    from pathlib import Path

    from esm_viz.fesom import is_unstructured
    from esm_viz.readers import get_data_metadata, open_data

    _configure_logging(quiet=True)

    path_obj = Path(path)
    if not path_obj.exists():
        typer.echo(f"Error: File not found: {path}", err=True)
        raise typer.Exit(1)

    try:
        ds = open_data(path)
        metadata = get_data_metadata(ds)
        metadata["is_unstructured"] = is_unstructured(ds)
        ds.close()

        if json_output:
            typer.echo(json.dumps(metadata, indent=2, default=str))
        else:
            typer.echo(f"\nFile: {path}")
            typer.echo("=" * 60)

            typer.echo(f"\nMesh type: {'Unstructured' if metadata['is_unstructured'] else 'Regular grid'}")

            typer.echo(f"\nDimensions ({len(metadata['dimensions'])}):")
            for dim_name, size in metadata["dimensions"].items():
                typer.echo(f"  {dim_name}: {size}")

            typer.echo(f"\nVariables ({len(metadata['variables'])}):")
            for var in metadata["variables"]:
                name = var["name"]
                dims = ", ".join(var["dims"])
                units = var.get("units", "")
                long_name = var.get("long_name", "")
                typer.echo(f"  {name} ({dims})")
                if long_name:
                    typer.echo(f"    {long_name}")
                if units:
                    typer.echo(f"    Units: {units}")

            typer.echo(f"\nCoordinates ({len(metadata['coordinates'])}):")
            for coord_name, coord_info in metadata["coordinates"].items():
                if "min" in coord_info and "max" in coord_info:
                    typer.echo(f"  {coord_name}: [{coord_info['min']:.4g}, {coord_info['max']:.4g}]")
                else:
                    typer.echo(f"  {coord_name}: size={coord_info['size']}")

    except Exception as e:
        typer.echo(f"Error: {e}", err=True)
        raise typer.Exit(1)


def cli_main() -> None:
    """Entry point for the CLI."""
    app()


if __name__ == "__main__":
    cli_main()
