"""Input validation utilities for the ESM-Catalog API.

Provides validation helpers for paths, request parameters, and other inputs.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger


class ValidationError(Exception):
    """Validation error with user-friendly message."""

    pass


def validate_catalog_path(
    path: str,
    must_exist: bool = False,
    resolve: bool = True,
) -> str:
    """Validate and normalize a catalog path.

    Args:
        path: Path to the catalog file (absolute or relative).
        must_exist: If True, raise ValidationError if path doesn't exist.
        resolve: If True, resolve symlinks and normalize the path.

    Returns:
        Normalized absolute path string.

    Raises:
        ValidationError: If validation fails.
    """
    if not path or not path.strip():
        raise ValidationError("Catalog path cannot be empty")

    try:
        p = Path(path)
        if resolve:
            p = p.resolve()
        else:
            p = p.absolute()
    except Exception as e:
        raise ValidationError(f"Invalid path format: {e}")

    path_str = str(p)

    if must_exist and not p.exists():
        raise ValidationError(f"Catalog file does not exist: {path_str}")

    if p.exists() and p.is_dir():
        raise ValidationError(f"Path is a directory, expected file: {path_str}")

    # Log for audit trail
    logger.debug("Validated catalog path: {}", path_str)

    return path_str


def validate_catalog_id(catalog_id: str) -> str:
    """Validate a catalog ID.

    Args:
        catalog_id: The catalog ID to validate.

    Returns:
        The validated catalog ID.

    Raises:
        ValidationError: If validation fails.
    """
    if not catalog_id or not catalog_id.strip():
        raise ValidationError("Catalog ID cannot be empty")

    # IDs should be alphanumeric with optional hyphens (UUID format)
    clean_id = catalog_id.strip()
    if not all(c.isalnum() or c == "-" for c in clean_id):
        raise ValidationError(
            f"Invalid catalog ID format: {catalog_id}. "
            "Must contain only alphanumeric characters and hyphens."
        )

    return clean_id


def validate_limit(limit: int | None, default: int = 10, max_value: int = 10000) -> int:
    """Validate and bound a limit parameter.

    Args:
        limit: Requested limit, or None for default.
        default: Default limit if None provided.
        max_value: Maximum allowed limit.

    Returns:
        Validated limit value.
    """
    if limit is None:
        return default
    if limit < 0:
        return default
    if limit > max_value:
        logger.warning("Limit {} exceeds maximum {}, capping", limit, max_value)
        return max_value
    return limit


def validate_offset(offset: int | str | None, default: int = 0) -> int:
    """Validate an offset/token parameter.

    Args:
        offset: Offset value (int or string token).
        default: Default offset if None or invalid.

    Returns:
        Validated offset as integer.
    """
    if offset is None:
        return default

    if isinstance(offset, str):
        if not offset.isdigit():
            return default
        offset = int(offset)

    if offset < 0:
        return default

    return offset


import re

# Pattern for safe property names in SQL queries.
# Allows alphanumeric, underscores, hyphens, colons (for namespaced properties).
# Must start with alphanumeric or underscore.
_SAFE_PROPERTY_PATTERN = re.compile(r"^[a-zA-Z_][a-zA-Z0-9_:\-]*$")

# Maximum length for property names
_MAX_PROPERTY_NAME_LENGTH = 256


def is_safe_property_name(name: str) -> bool:
    """Check if a property name is safe for use in SQL queries.

    Property names discovered from JSON keys are generally safe, but this
    provides an additional layer of validation before string interpolation.

    Args:
        name: Property name to validate.

    Returns:
        True if the name is safe for SQL interpolation.
    """
    if not name or len(name) > _MAX_PROPERTY_NAME_LENGTH:
        return False
    return bool(_SAFE_PROPERTY_PATTERN.match(name))


def validate_property_name(name: str) -> str:
    """Validate a property name for use in SQL queries.

    Args:
        name: Property name to validate.

    Returns:
        The validated property name.

    Raises:
        ValidationError: If the property name is invalid.
    """
    if not is_safe_property_name(name):
        raise ValidationError(
            f"Invalid property name: {name!r}. "
            "Must start with a letter/underscore and contain only "
            "alphanumeric characters, underscores, colons, or hyphens."
        )
    return name
