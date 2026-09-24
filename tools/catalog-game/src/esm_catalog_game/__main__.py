"""CLI entry point: ``esm-catalog-game --exp-root <path>``."""

from __future__ import annotations

from pathlib import Path

import click

from .app import DEFAULT_TIME_SECONDS, run
from .catalog_data import SourcingError


@click.command()
@click.option(
    "--exp-root",
    required=True,
    type=click.Path(exists=True, file_okay=False, path_type=Path),
    help="Root of a completed ESM-Tools experiment (must hold config/*_finished_config.yaml).",
)
@click.option(
    "--time",
    "time_budget",
    default=DEFAULT_TIME_SECONDS,
    show_default=True,
    type=int,
    help="Countdown, in seconds, before the run ends.",
)
def main(exp_root: Path, time_budget: int) -> None:
    """Gamified terminal cataloging of an ESM-Tools experiment.

    Drives esm_catalog's real scan-sourcing layer (esm_catalog.scan.sourcing)
    against EXP_ROOT to find every file a real 'esm-catalog scan' would
    catalog, then turns finding and classifying them into a ranger-style
    navigation game: h/j/k/l to move, 'c' to capture the file under the
    cursor and pick its (component / stream).
    """
    try:
        run(exp_root, time_budget=time_budget)
    except SourcingError as exc:
        raise click.ClickException(str(exc)) from exc


if __name__ == "__main__":
    main()
