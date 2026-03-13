#!/usr/bin/env python3
"""
ECHAM GRIB Reading Example
==========================

This example demonstrates how to read ECHAM GRIB files with proper metadata
extraction, including support for .codes files that define variable names
and units.

ECHAM outputs GRIB files where variables are identified by paramId. The
.codes files (e.g., echam6.codes) provide the mapping from paramId to
human-readable variable names, units, and descriptions.

Usage:
    python read_echam_grib.py /path/to/file.grb [/path/to/echam.codes]

Requirements:
    - xarray
    - cfgrib (GRIB engine for xarray)
    - eccodes (ECMWF GRIB library)
"""

import pathlib
import re
import warnings
from collections import defaultdict
from typing import Optional

import xarray as xr


# -----------------------------------------------------------------------------
# Part 1: Parsing .codes files
# -----------------------------------------------------------------------------

def parse_codes_file(codes_path: pathlib.Path) -> dict[int, dict]:
    """
    Parse an ECHAM-style .codes file.

    These files define the mapping from GRIB paramId to variable metadata.
    Format: paramId levels shortName offset scale long_name [units]

    Example line:
        130    1 st      0.      1. surface temperature [K]

    Parameters
    ----------
    codes_path : pathlib.Path
        Path to the .codes file (e.g., echam6.codes)

    Returns
    -------
    dict[int, dict]
        Dictionary mapping paramId -> {shortName, levels, longName, units}
    """
    params = {}

    with open(codes_path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue

            parts = line.split()
            if len(parts) < 6:
                continue

            param_id = int(parts[0])
            levels = int(parts[1])
            short_name = parts[2]

            # Reconstruct long name and extract units in brackets
            long_name_with_units = " ".join(parts[5:])
            match = re.match(r"(.+?)\s*\[(.+?)\]$", long_name_with_units)
            if match:
                long_name = match.group(1).strip()
                units = match.group(2)
            else:
                long_name = long_name_with_units
                units = ""

            params[param_id] = {
                "shortName": short_name,
                "levels": levels,
                "longName": long_name,
                "units": units,
            }

    return params


# -----------------------------------------------------------------------------
# Part 2: Scanning GRIB structure with eccodes
# -----------------------------------------------------------------------------

def scan_grib_structure(file_path: pathlib.Path) -> dict:
    """
    Scan a GRIB file to discover its internal structure.

    GRIB files can contain multiple "hypercubes" - combinations of grid type
    and level type. For example, an ECHAM file might contain:
    - (regular_ll, surface) - Surface variables on regular lat/lon grid
    - (regular_ll, hybrid) - 3D variables on hybrid model levels
    - (reduced_gg, surface) - Surface variables on reduced Gaussian grid

    This function scans all messages to discover these combinations.

    Parameters
    ----------
    file_path : pathlib.Path
        Path to the GRIB file

    Returns
    -------
    dict
        Nested dict: {(gridType, levelType): {paramId: {metadata}}}
    """
    import eccodes

    structure = defaultdict(lambda: defaultdict(dict))

    with open(file_path, "rb") as f:
        msg_count = 0
        while True:
            gid = eccodes.codes_grib_new_from_file(f)
            if gid is None:
                break

            msg_count += 1
            try:
                # Key identifiers for grouping
                grid_type = eccodes.codes_get(gid, "gridType")
                level_type = eccodes.codes_get(gid, "typeOfLevel")
                param_id = eccodes.codes_get(gid, "paramId")

                # Variable identification
                short_name = eccodes.codes_get(gid, "shortName")
                name = eccodes.codes_get(gid, "name")

                # Time information (may not exist in all files)
                try:
                    data_date = eccodes.codes_get(gid, "dataDate")
                    data_time = eccodes.codes_get(gid, "dataTime")
                except Exception:
                    data_date = None
                    data_time = None

                # Grid dimensions
                try:
                    ni = eccodes.codes_get(gid, "Ni")
                    nj = eccodes.codes_get(gid, "Nj")
                except Exception:
                    ni = None
                    nj = None

                structure[(grid_type, level_type)][param_id] = {
                    "shortName": short_name,
                    "name": name,
                    "dataDate": data_date,
                    "dataTime": data_time,
                    "gridDimensions": (ni, nj) if ni and nj else None,
                }

            finally:
                eccodes.codes_release(gid)

    print(f"Scanned {msg_count} GRIB messages")
    print(f"Found {len(structure)} distinct (gridType, levelType) combinations")

    return dict(structure)


# -----------------------------------------------------------------------------
# Part 3: Opening GRIB with xarray
# -----------------------------------------------------------------------------

def open_grib_datasets(
    file_path: pathlib.Path,
    structure: Optional[dict] = None,
) -> dict[tuple[str, str], xr.Dataset]:
    """
    Open a GRIB file as multiple xarray Datasets.

    Because GRIB files can contain multiple hypercubes (different grid/level
    combinations), we need to open each combination as a separate Dataset.
    cfgrib's filter_by_keys parameter allows this selection.

    Parameters
    ----------
    file_path : pathlib.Path
        Path to the GRIB file
    structure : dict, optional
        Pre-scanned structure from scan_grib_structure(). If None, will scan.

    Returns
    -------
    dict[tuple[str, str], xr.Dataset]
        Dictionary mapping (gridType, levelType) -> xr.Dataset
    """
    if structure is None:
        structure = scan_grib_structure(file_path)

    datasets = {}

    for (grid_type, level_type), params in structure.items():
        # Filter to select only this hypercube
        filter_keys = {
            "gridType": grid_type,
            "typeOfLevel": level_type,
        }

        try:
            with warnings.catch_warnings():
                # cfgrib often warns about missing eccodes definitions
                warnings.filterwarnings("ignore", message=".*ecCodes provides no.*")

                ds = xr.open_dataset(
                    file_path,
                    engine="cfgrib",
                    backend_kwargs={
                        "filter_by_keys": filter_keys,
                        "errors": "ignore",  # Skip variables that fail
                        "indexpath": "",      # Don't create .idx files
                    },
                )

                datasets[(grid_type, level_type)] = ds
                print(
                    f"Opened: gridType={grid_type}, "
                    f"levelType={level_type}, "
                    f"variables={list(ds.data_vars)}"
                )

        except Exception as e:
            print(f"Failed to open ({grid_type}, {level_type}): {e}")

    return datasets


# -----------------------------------------------------------------------------
# Part 4: Extracting metadata with codes table enrichment
# -----------------------------------------------------------------------------

def collect_variable_metadata(
    datasets: dict[tuple[str, str], xr.Dataset],
    codes_table: Optional[dict[int, dict]] = None,
) -> list[dict]:
    """
    Extract metadata for all variables, enriching from codes table if available.

    Parameters
    ----------
    datasets : dict
        Dictionary of xarray Datasets from open_grib_datasets()
    codes_table : dict, optional
        Parsed codes table from parse_codes_file()

    Returns
    -------
    list[dict]
        List of variable metadata dictionaries
    """
    variables = []

    for (grid_type, level_type), ds in datasets.items():
        for var_name in ds.data_vars:
            var = ds[var_name]

            # Get paramId from GRIB attributes
            param_id = var.attrs.get("GRIB_paramId")

            # Try to get better names from codes table
            if codes_table and param_id in codes_table:
                code_info = codes_table[param_id]
                short_name = code_info["shortName"]
                long_name = code_info["longName"]
                units = code_info["units"]
                source = "codes_table"
            else:
                # Fall back to GRIB attributes
                short_name = var_name
                long_name = var.attrs.get("long_name", var.attrs.get("GRIB_name", ""))
                units = var.attrs.get("units", var.attrs.get("GRIB_units", ""))
                source = "grib_attrs"

            var_meta = {
                "name": short_name,
                "original_name": var_name,
                "long_name": long_name,
                "units": units,
                "grid_type": grid_type,
                "level_type": level_type,
                "dimensions": list(var.dims),
                "shape": list(var.shape),
                "paramId": param_id,
                "metadata_source": source,
            }

            # Add CF standard name if present
            if "standard_name" in var.attrs:
                var_meta["standard_name"] = var.attrs["standard_name"]

            variables.append(var_meta)

    return variables


# -----------------------------------------------------------------------------
# Part 5: Main example
# -----------------------------------------------------------------------------

def main():
    import sys

    if len(sys.argv) < 2:
        print("Usage: python read_echam_grib.py <grib_file> [codes_file]")
        print("")
        print("Example:")
        print("  python read_echam_grib.py experiment_197001.grb echam6.codes")
        sys.exit(1)

    grib_path = pathlib.Path(sys.argv[1])
    codes_path = pathlib.Path(sys.argv[2]) if len(sys.argv) > 2 else None

    if not grib_path.exists():
        print(f"Error: GRIB file not found: {grib_path}")
        sys.exit(1)

    # Parse codes file if provided
    codes_table = None
    if codes_path:
        if codes_path.exists():
            print(f"Loading codes table from: {codes_path}")
            codes_table = parse_codes_file(codes_path)
            print(f"Loaded {len(codes_table)} parameter definitions")
        else:
            print(f"Warning: Codes file not found: {codes_path}")

    print("")
    print("=" * 60)
    print("Scanning GRIB structure...")
    print("=" * 60)
    structure = scan_grib_structure(grib_path)

    print("")
    print("Structure discovered:")
    for (grid_type, level_type), params in structure.items():
        print(f"  ({grid_type}, {level_type}): {len(params)} variables")

    print("")
    print("=" * 60)
    print("Opening as xarray Datasets...")
    print("=" * 60)
    datasets = open_grib_datasets(grib_path, structure)

    print("")
    print("=" * 60)
    print("Extracting variable metadata...")
    print("=" * 60)
    variables = collect_variable_metadata(datasets, codes_table)

    print("")
    print(f"Found {len(variables)} variables:")
    print("-" * 60)
    for var in variables:
        source_indicator = "*" if var["metadata_source"] == "codes_table" else " "
        print(
            f"{source_indicator} {var['name']:12} | "
            f"{var['long_name'][:30]:30} | "
            f"{var['units']:10} | "
            f"({var['grid_type']}, {var['level_type']})"
        )

    print("")
    print("* = metadata enriched from .codes file")

    # Close datasets
    for ds in datasets.values():
        ds.close()


if __name__ == "__main__":
    main()
