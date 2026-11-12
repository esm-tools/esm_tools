"""Small compatibility shims for the Python version range esm_catalog targets.

Re-exports from esm_tools._compat (the shared, project-wide home for these)
so existing ``from esm_catalog._compat import StrEnum`` imports keep working.
"""

from esm_tools._compat import StrEnum

__all__ = ["StrEnum"]
