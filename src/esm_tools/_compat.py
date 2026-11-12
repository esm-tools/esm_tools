"""Small compatibility shims for the Python version range esm-tools targets.

Not part of esm_tools' public API for anything depending on esm-tools as an
installed library -- but esm_runscripts/esm_catalog/etc. are siblings in the
same distribution, so importing from here is an in-repo import, not reaching
across a real API boundary (same convention as e.g. pandas._compat).
"""

from __future__ import annotations

import sys

if sys.version_info >= (3, 11):
    from enum import StrEnum
else:  # pragma: no cover - py<3.11 fallback
    from enum import Enum

    class StrEnum(str, Enum):
        __str__ = str.__str__

        @staticmethod
        def _generate_next_value_(name, start, count, last_values):
            return name.lower()  # matches enum.StrEnum: auto() -> lowercase member name


__all__ = ["StrEnum"]
