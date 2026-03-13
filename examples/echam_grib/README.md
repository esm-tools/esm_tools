# ECHAM GRIB Reading Example

This example demonstrates how to read ECHAM GRIB files with proper metadata
extraction, including support for `.codes` files.

## Files

- `read_echam_grib.py` - Main example script with documented functions
- `echam.codes` - Real ECHAM codes file from basic-001 simulation
- `echam_sample.grb` - Sample GRIB file for testing

## Usage

```bash
python read_echam_grib.py echam_sample.grb echam.codes
```

## Key Concepts

### 1. GRIB Structure

ECHAM GRIB files contain multiple "hypercubes" - combinations of grid type
and level type. For example:
- `(regular_ll, surface)` - Surface variables on regular lat/lon grid
- `(regular_ll, hybrid)` - 3D variables on hybrid model levels

### 2. The .codes File

ECHAM uses parameter IDs (paramId) to identify variables. The `.codes` file
provides the mapping to human-readable names:

```
# paramId levels shortName offset scale long_name [units]
130    1 st      0.      1. surface temperature [K]
```

### 3. Opening with xarray

Because of the multiple hypercubes, we open each combination as a separate
xarray Dataset using cfgrib's `filter_by_keys`:

```python
ds = xr.open_dataset(
    file_path,
    engine="cfgrib",
    backend_kwargs={
        "filter_by_keys": {"gridType": "regular_ll", "typeOfLevel": "surface"},
    },
)
```

## Dependencies

- xarray
- cfgrib
- eccodes
