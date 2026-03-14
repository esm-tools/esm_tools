"""Scan Fortran namelists and extract metadata for STAC catalog.

Parses Fortran namelists using f90nml and extracts key-value pairs organized
by namelist group (called "chapter" in Fortran terminology).

Example namelist file::

    &runctl
        dt_start = 1850, 1, 1
        delta_time = 450
        lcouple = .true.
    /

    &radctl
        co2vmr = 0.000284316986084
        yr_perp = 1850
    /

Produces metadata structure::

    {
        "runctl": {
            "dt_start": [1850, 1, 1],
            "delta_time": 450,
            "lcouple": True
        },
        "radctl": {
            "co2vmr": 0.000284316986084,
            "yr_perp": 1850
        }
    }

The namelist data is stored at the collection level since it describes
simulation parameters that apply to all output files from that component.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

from loguru import logger

if TYPE_CHECKING:
    pass

# Lazy import f90nml to avoid hard dependency
_f90nml = None


def _get_f90nml():
    """Lazy import f90nml."""
    global _f90nml
    if _f90nml is None:
        try:
            import f90nml

            _f90nml = f90nml
        except ImportError:
            raise ImportError(
                "f90nml is required for namelist parsing. "
                "Install with: pip install f90nml"
            )
    return _f90nml


def scan_namelist(path: Path) -> dict:
    """Parse a single Fortran namelist file.

    Args:
        path: Path to namelist file.

    Returns:
        Dict mapping namelist group names to their key-value pairs.
        Values are converted to Python types (bool, int, float, list, str).

    Raises:
        ImportError: If f90nml is not installed.
        ValueError: If file cannot be parsed as a valid namelist.
    """
    path = Path(path)
    if not path.exists():
        raise ValueError(f"Namelist file not found: {path}")

    f90nml = _get_f90nml()

    try:
        nml = f90nml.read(path)
    except Exception as e:
        raise ValueError(f"Failed to parse namelist {path}: {e}") from e

    # Convert f90nml.Namelist to plain dict recursively
    return _nml_to_dict(nml)


def scan_namelist_directory(
    config_dir: Path,
    component: str | None = None,
) -> dict:
    """Scan all namelist files in a component's config directory.

    ESM-Tools convention: config/{component}/ contains namelist files
    named like "namelist.echam", "namelist.config", etc.

    Args:
        config_dir: Path to config directory (e.g., /exp/config/echam/).
        component: Optional component name for filtering/logging.

    Returns:
        Dict with structure::

            {
                "namelist.echam": {
                    "runctl": {...},
                    "radctl": {...},
                },
                "namelist.jsbach": {
                    "jsbach_ctl": {...},
                }
            }

    Files that fail to parse are logged and skipped.
    """
    config_dir = Path(config_dir)
    if not config_dir.is_dir():
        logger.debug("Config directory not found: {}", config_dir)
        return {}

    result: dict = {}

    # Find namelist files (various naming conventions)
    namelist_patterns = [
        "namelist.*",
        "*.nml",
        "*.namelist",
    ]

    seen_files: set[Path] = set()
    for pattern in namelist_patterns:
        for nml_path in config_dir.glob(pattern):
            # Skip symlinks pointing outside (often point to cluster paths)
            if nml_path.is_symlink():
                try:
                    target = nml_path.resolve()
                    if not target.exists():
                        logger.debug("Skipping broken symlink: {}", nml_path)
                        continue
                except Exception:
                    continue

            # Skip already processed (patterns may overlap)
            if nml_path in seen_files:
                continue
            seen_files.add(nml_path)

            # Skip timestamp-suffixed versions if base exists
            # e.g., namelist.echam_18500101-18500131 vs namelist.echam
            base_name = _strip_timestamp_suffix(nml_path.name)
            if base_name != nml_path.name:
                # This is a timestamped version; prefer the base symlink
                base_path = config_dir / base_name
                if base_path.exists() and base_path not in seen_files:
                    continue

            try:
                nml_data = scan_namelist(nml_path)
                if nml_data:
                    # Use clean name without timestamp suffix
                    clean_name = _strip_timestamp_suffix(nml_path.name)
                    result[clean_name] = nml_data
                    logger.debug(
                        "Parsed namelist {} ({} groups)",
                        nml_path.name,
                        len(nml_data),
                    )
            except ValueError as e:
                logger.warning("Skipping invalid namelist {}: {}", nml_path, e)
            except ImportError:
                logger.warning("f90nml not installed, skipping namelist parsing")
                return {}

    if result and component:
        logger.info(
            "Scanned {} namelist files for component {}",
            len(result),
            component,
        )

    return result


def _strip_timestamp_suffix(name: str) -> str:
    """Remove ESM-Tools timestamp suffix from filename.

    Examples:
        namelist.echam_18500101-18500131 -> namelist.echam
        namelist.config_18500301-18500331 -> namelist.config
        namelist.oce -> namelist.oce (unchanged)
    """
    import re

    # Match _YYYYMMDD-YYYYMMDD suffix
    pattern = r"_\d{8}-\d{8}$"
    return re.sub(pattern, "", name)


def _nml_to_dict(nml) -> dict:
    """Convert f90nml.Namelist to plain dict recursively."""
    result = {}
    for group_name, group_data in nml.items():
        if hasattr(group_data, "todict"):
            result[group_name] = group_data.todict()
        elif isinstance(group_data, dict):
            result[group_name] = dict(group_data)
        else:
            result[group_name] = group_data
    return result


def flatten_namelists(namelists: dict, prefix: str = "nml") -> dict:
    """Flatten nested namelist structure for queryable properties.

    Converts nested structure to dot-notation keys suitable for
    JSON path queries and STAC property filtering.

    Args:
        namelists: Output from scan_namelist_directory().
        prefix: Prefix for flattened keys (default: "nml").

    Returns:
        Flat dict with keys like::

            {
                "nml:namelist.echam:runctl:delta_time": 450,
                "nml:namelist.echam:runctl:lcouple": True,
                "nml:namelist.echam:radctl:co2vmr": 0.000284,
            }

    Example:
        >>> namelists = {
        ...     "namelist.echam": {
        ...         "runctl": {"delta_time": 450, "lcouple": True}
        ...     }
        ... }
        >>> flatten_namelists(namelists)
        {
            "nml:namelist.echam:runctl:delta_time": 450,
            "nml:namelist.echam:runctl:lcouple": True,
        }
    """
    result = {}
    for filename, groups in namelists.items():
        for group_name, values in groups.items():
            for key, value in values.items():
                # Skip complex nested values (arrays of derived types, etc.)
                if isinstance(value, (dict, list)) and value:
                    if isinstance(value, list) and all(
                        isinstance(v, (int, float, str, bool, type(None)))
                        for v in value
                    ):
                        # Simple list of scalars - keep it
                        pass
                    else:
                        # Complex structure - skip for now
                        continue

                flat_key = f"{prefix}:{filename}:{group_name}:{key}"
                result[flat_key] = value

    return result


def get_searchable_namelist_keys(namelists: dict) -> set[str]:
    """Extract unique searchable keys from namelist structure.

    Returns keys in the format "group:key" for use in queryables.
    This allows searching by namelist group and parameter name.

    Args:
        namelists: Output from scan_namelist_directory().

    Returns:
        Set of searchable keys like {"runctl:delta_time", "radctl:co2vmr"}.
    """
    keys = set()
    for _filename, groups in namelists.items():
        for group_name, values in groups.items():
            for key in values.keys():
                keys.add(f"{group_name}:{key}")
    return keys


def extract_fesom_mesh_info(config_dir: Path) -> dict:
    """Extract FESOM mesh information from namelist.config.

    FESOM uses an unstructured triangular mesh. The mesh path and rotation
    parameters are stored in the namelist.config file under geometry/paths groups.

    Args:
        config_dir: Path to FESOM config directory (e.g., /exp/config/fesom/).

    Returns:
        Dict with FESOM mesh properties for STAC item/collection:
            - fesom:mesh_path: Path to mesh directory
            - fesom:rotated: Whether mesh is rotated (force_rotation=False)
            - fesom:alpha: Euler alpha rotation angle
            - fesom:beta: Euler beta rotation angle
            - fesom:gamma: Euler gamma rotation angle

    Example output::

        {
            "fesom:mesh_path": "/pool/data/AWICM/FESOM2/meshes/core2",
            "fesom:rotated": False,
            "fesom:alpha": 0.0,
            "fesom:beta": 0.0,
            "fesom:gamma": 0.0
        }
    """
    config_dir = Path(config_dir)

    # Try namelist.config first, then namelist.nml
    namelist_path = config_dir / "namelist.config"
    if not namelist_path.exists():
        namelist_path = config_dir / "namelist.nml"
    if not namelist_path.exists():
        logger.debug("No FESOM namelist found in {}", config_dir)
        return {}

    try:
        nml = scan_namelist(namelist_path)
    except (ValueError, ImportError) as e:
        logger.warning("Failed to parse FESOM namelist: {}", e)
        return {}

    result: dict = {}

    # Extract mesh path from paths group
    paths = nml.get("paths", {})
    mesh_path = paths.get("meshpath") or paths.get("mesh_path")
    if mesh_path:
        result["fesom:mesh_path"] = str(mesh_path).rstrip("/")

    # Extract geometry/rotation info
    geometry = nml.get("geometry", {})

    # force_rotation=.true. means mesh is NOT rotated (confusing naming)
    # force_rotation=.false. means mesh IS rotated using Euler angles
    force_rotation = geometry.get("force_rotation", True)
    result["fesom:rotated"] = not force_rotation

    # Euler rotation angles (only meaningful when rotated=True)
    result["fesom:alpha"] = float(geometry.get("alphaeuler", 0.0))
    result["fesom:beta"] = float(geometry.get("betaeuler", 0.0))
    result["fesom:gamma"] = float(geometry.get("gammaeuler", 0.0))

    # Extract resolution info if available
    if "res" in geometry:
        result["fesom:resolution"] = geometry["res"]

    logger.debug("Extracted FESOM mesh info: {}", result)
    return result
