#!/usr/bin/env python3
"""
Minimal CLI for event-driven Snakemake workflow.

This parses ONLY the user's runscript YAML, without loading the full
esm_runscripts machinery. We start from scratch.
"""

import sys
from pathlib import Path

import click
import snakemake
import yaml
from loguru import logger


def parse_runscript(runscript_path: str) -> dict:
    """Parse runscript YAML with minimal processing.

    Args:
        runscript_path: Path to user runscript YAML

    Returns:
        Config dict from YAML file
    """
    with open(runscript_path) as f:
        config = yaml.safe_load(f)

    if config is None:
        config = {}

    return config


@click.command()
@click.argument("runscript", type=click.Path(exists=True))
@click.option("-e", "--expid", default="test", help="Experiment ID")
@click.option("-c", "--cores", default=1, help="Number of cores for Snakemake")
@click.option("-n", "--dry-run", is_flag=True, help="Dry run (don't execute)")
@click.option("-d", "--debug", is_flag=True, help="Enable debug logging")
@click.option("--unlock", is_flag=True, help="Unlock working directory")
@click.option("--dag", is_flag=True, help="Print DAG in dot format")
def main(runscript, expid, cores, dry_run, debug, unlock, dag):
    """Run ESM workflow with event-driven Snakemake architecture.

    This is a minimal implementation that parses only the user's runscript,
    without loading the full esm_runscripts configuration system.

    Example:
        python cli.py experiment.yaml -e test001 --dry-run
        python cli.py experiment.yaml -e test001 --dag | dot -Tpng > dag.png
    """
    logger.remove()  # Remove default handler
    # When generating DAG, send logs to stderr so stdout has only the dot graph
    log_sink = sys.stderr if dag else sys.stdout
    logger.add(log_sink, level="DEBUG" if debug else "INFO", format="{message}")

    try:
        # Parse user's runscript only
        logger.info(f"Parsing runscript: {runscript}")
        config = parse_runscript(runscript)

        # Inject expid into config
        if "general" not in config:
            config["general"] = {}
        config["general"]["expid"] = expid

        # Set up working directory (use current dir if not specified)
        workdir = Path(config.get("general", {}).get("base_dir", ".")).resolve()
        workdir.mkdir(parents=True, exist_ok=True)

        logger.info(f"Working directory: {workdir}")

        # Write config for Snakemake to read
        config_file = workdir / "config.yaml"
        with open(config_file, "w") as f:
            yaml.dump(config, f, default_flow_style=False)

        logger.info(f"Wrote config to: {config_file}")

        # Determine Snakefile location (same directory as this CLI)
        snakefile = Path(__file__).parent / "Snakefile"

        if not snakefile.exists():
            logger.error(f"Snakefile not found: {snakefile}")
            sys.exit(1)

        # Run Snakemake workflow
        if dag:
            logger.info("Generating DAG")
        else:
            logger.info("Launching Snakemake workflow")
        logger.info(f"  Snakefile: {snakefile}")
        logger.info(f"  Config: {config_file}")
        logger.info(f"  Cores: {cores}")
        logger.info(f"  Dry run: {dry_run}")
        logger.info(f"  Unlock: {unlock}")
        logger.info(f"  Workdir: {workdir}")

        # dynamicaly create teh snakemake
        jinja2. .... c

        success = snakemake.snakemake(
            snakefile=str(snakefile),
            configfiles=[str(config_file)],
            cores=cores,
            dryrun=dry_run,
            printshellcmds=True,
            keepgoing=False,
            unlock=unlock,
            workdir=str(workdir),
            printdag=dag,
        )

        if not success:
            logger.error("Workflow execution failed")
            sys.exit(1)

        if not dag:
            logger.success("Workflow completed successfully")

    except Exception as e:
        logger.exception(f"Fatal error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
