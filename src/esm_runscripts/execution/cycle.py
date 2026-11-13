"""Advancing between cycles: incrementing the run date/number,
persisting it to disk, and checking whether the experiment is over.
Native replacement for legacy's
``resubmit._increment_date_and_run_number`` (split here into
``advance_date``/``advance_run_number``/``sync_command_line_config``,
three granular steps instead of one function doing all three),
``_write_date_file``, and ``end_of_experiment``.

Deliberately NOT covering legacy's ``end_of_experiment_all_models`` --
iterative coupling's multi-model completion check. Traced it while
designing this: it greps each sibling model's log for the literal string
``"# Experiment over"``, but the only place that message gets written
(``logger.progress("Experiment over")``) uses the format string
``"{time} | {level} | {message}"`` (``logfiles.py``) -- no ``"# "``
prefix at all, so the substring check can never match. Confirmed broken,
not just fragile; iterative coupling is getting redesigned separately,
so this isn't reproduced here.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger

from .. import chunky_parts


def advance_date(config: dict) -> dict:
    """Increment ``current_date`` by ``delta_date``, for the next cycle.
    Mutates and returns ``config``."""
    general = config["general"]
    for key in ("current_date", "delta_date"):
        if key not in general:
            raise KeyError(f"advance_date needs general.{key!r}, not present.")
    general["current_date"] += general["delta_date"]
    return config


def advance_run_number(config: dict) -> dict:
    """Increment ``run_number`` by one, for the next cycle. Mutates and
    returns ``config``."""
    general = config["general"]
    if "run_number" not in general:
        raise KeyError("advance_run_number needs general['run_number'], not present.")
    general["run_number"] += 1
    return config


def sync_command_line_config(config: dict) -> dict:
    """Stash the current ``current_date``/``run_number`` into
    ``general.command_line_config`` -- ``chunky_parts.
    update_command_line_config`` (iterative coupling's model-switch
    bookkeeping, unchanged) reads them from there, not from ``general``
    directly. Call after :func:`advance_date`/:func:`advance_run_number`.
    """
    general = config["general"]
    general["command_line_config"]["current_date"] = general["current_date"].format(
        form=9, givenph=False, givenpm=False, givenps=False
    )
    general["command_line_config"]["run_number"] = general["run_number"]
    return chunky_parts.update_command_line_config(config)


def write_date_file(config: dict) -> None:
    """Persist ``<expid>_<setup_name>.date`` as ``"<date> <run_number>"``,
    so a fresh CLI invocation can recover where the experiment left off.
    """
    general = config["general"]
    path = Path(general["experiment_scripts_dir"]) / f"{general['expid']}_{general['setup_name']}.date"
    path.write_text(f"{general['current_date'].output()} {general['run_number']}")


def is_experiment_over(config: dict) -> bool:
    """Whether this run's ``next_date`` has reached ``final_date``.

    ``next_date`` is computed once at config-build time (``prepare.py``,
    ``current_date + delta_date``) -- checking it here, rather than
    re-deriving from the just-``advance_date``'d ``current_date``,
    matches what this run was already configured to produce.
    """
    if config["general"]["next_date"] >= config["general"]["final_date"]:
        logger.progress("Experiment over")
        return True
    return False
