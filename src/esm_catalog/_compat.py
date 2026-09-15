"""Small compatibility shims for the Python version range esm_catalog targets."""

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
