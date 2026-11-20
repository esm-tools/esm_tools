"""Named, single-source groups of filetypes each job type's ``run_job``
sets as ``general.relevant_filetypes`` before running its recipe.

These overlap with, but are not identical to, ``prepare.py``'s
``_add_all_folders``-computed ``general.in_filetypes``/``out_filetypes``
(``PREPCOMPUTE_COMPUTE`` matches ``in_filetypes`` minus ``"scripts"``;
``TIDY`` is a deliberate subset of ``out_filetypes`` missing ``"analysis"``/
``"scripts"`` -- tidy's own commented-out history shows it intentionally
excludes the input-side types, and the ``analysis``/``scripts`` gap
predates every commit in this file's history, so it's long-standing
intent, not drift). Named here instead of inlined so both the legacy
``run_job`` wrapper (``prepcompute.py``/``compute.py``/``tidy.py``) and
native ``execution.steps.run_steps`` (which calls straight into the
recipe, bypassing ``run_job``) read the same single list instead of each
carrying their own copy.
"""

from __future__ import annotations

PREPCOMPUTE_COMPUTE = ("bin", "config", "forcing", "input", "restart_in")
TIDY = ("log", "mon", "outdata", "restart_out", "ignore", "unknown")
