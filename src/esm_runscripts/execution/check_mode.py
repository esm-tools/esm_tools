"""Check-mode (``-c``/``--check``): build everything a dispatch strategy
would build, run/submit nothing. Shared by :mod:`.dispatch` and
:mod:`.launch` -- not folded into either to avoid a circular import
between them.
"""

from __future__ import annotations

from loguru import logger


def is_check_mode(config: dict) -> bool:
    """Whether this run was launched with ``-c``/``--check``. Native
    replacement for legacy's ``resubmit.check_if_check``."""
    if config.get("general", {}).get("check", False):
        logger.info(
            "Actually not submitting/running anything, this job preparation "
            "was launched in 'check' mode (-c)."
        )
        return True
    return False
