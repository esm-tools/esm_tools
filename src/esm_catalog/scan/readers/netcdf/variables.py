"""Extract the per-variable metadata from a NetCDF dataset."""

from __future__ import annotations

import xarray as xr

from esm_catalog.types import ScannedVariable


def _extract_variables(dataset: xr.Dataset) -> list[ScannedVariable]:
    """Return one :class:`ScannedVariable` per data variable in *dataset*."""
    variables: list[ScannedVariable] = []
    for name, array in dataset.data_vars.items():
        entry: ScannedVariable = {"name": str(name)}
        for attr in ("standard_name", "long_name", "units", "description"):
            if attr in array.attrs:
                entry[attr] = array.attrs[attr]  # type: ignore[literal-required]
        entry["dimensions"] = [str(dim) for dim in array.dims]
        variables.append(entry)
    return variables
