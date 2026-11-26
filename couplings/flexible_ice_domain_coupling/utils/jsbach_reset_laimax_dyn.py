#!/usr/bin/env python3
"""Reset JSBACH's dynamic maximum LAI where the glacier mask changed.

JSBACH (echam-6.3.05p2, mo_phenology.f90) keeps the dynamic maximum LAI
``laiMax_dyn`` (veg restart, phenStream) per tile. It is set only once, on a
cold start, where it is still negative: laiMax_dyn = laiMax_stat = lctlib
MaxLAI(cover_type). The yearly update (update_maximum_lai) is commented out,
so afterwards the field is never raised again; values below LAI_negligible are
only floored to LAI_negligible, and the LAI is capped by it.

Under a glacier every tile has cover type 1 (glacier, MaxLAI 0), so
laiMax_dyn = 0 there. When the coupling turns such a cell into land
(new cover types from the new init file), the 0 from the restart stays and the
cell can never grow leaves: deglaciated land stays bare, dry and too warm
(T2 deglaciation runs, 127 ka: about 75 % desert, LAI 0.1-0.3, July T2m
7-15 K too warm).

This script sets laiMax_dyn on deglaciated cells to the value a run carries
on land (checked on flextest_20261123: on non-glacier land laiMax_dyn equals
MaxLAI(cover_type) on every tile, on glacier cells it is LAI_negligible = 1e-5
on every tile):

    new land    laiMax_dyn(tile) = MaxLAI(cover_type(tile))   (lctlib.def)
    new glacier laiMax_dyn unchanged

New glacier cells must keep their laiMax_dyn: the coupling does not reset the
LAI in the jsbach restart, so they still carry the LAI of their vegetation
(1.0-1.6 in flextest_20261123 3143), and update_phenology stops with "LAI
larger than maximum LAI on input" wherever lai > laiMax_dyn. Lowering it to the
glacier value killed 3143 of flextest_20261123 on 4 new glacier cells.

Cells deglaciated by an earlier coupling that did not reset the field (an
older coupling version, or a restart from another run) are repaired too: every
cell that is not a glacier in INIT_FILE but still carries the glacier value
(laiMax_dyn <= TRAPPED_MAX on all tiles) gets MaxLAI(cover_type) as well. On
non-glacier land a cold start never gives such a cell (MaxLAI > 0 for every
vegetation type), so only trapped cells match.

It does not write -1 ("initialise from laiMax_stat"), because JSBACH then
re-initialises the whole nproma block, not only the changed cells. All other
cells are left as they are.

Usage
-----
    jsbach_reset_laimax_dyn.py LAIMAX_2D INIT_FILE LCTLIB NEW_LAND_MASK NEW_GLACIER_MASK

LAIMAX_2D        unpacked veg restart variable laiMax_dyn(tiles, lat, lon), changed in place
INIT_FILE        the new JSBACH init file (cover_type(ntiles, lat, lon))
LCTLIB           lctlib.def of the run (line "MaxLAI v1 v2 ...", one value per LctNumber)
NEW_LAND_MASK    (lat, lon), > 0.5 where a glacier cell became land
NEW_GLACIER_MASK (lat, lon), > 0.5 where land became glacier (only checked
                 against NEW_LAND_MASK and excluded from the repair; laiMax_dyn
                 is not changed there)
"""

import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:  # pragma: no cover
    sys.exit("jsbach_reset_laimax_dyn.py: needs netCDF4 (module load python3)")


def read_lctlib_maxlai(path):
    """MaxLAI per land cover type number (index 0 = LctNumber 1)."""
    numbers = values = None
    with open(path) as f:
        for line in f:
            parts = line.split("#")[0].split()
            if not parts:
                continue
            if parts[0] == "LctNumber":
                numbers = [int(float(p)) for p in parts[1:]]
            elif parts[0] == "MaxLAI":
                values = [float(p) for p in parts[1:]]
    if values is None:
        sys.exit(f"jsbach_reset_laimax_dyn.py: no MaxLAI line in {path}")
    if numbers is None:
        numbers = list(range(1, len(values) + 1))
    if len(numbers) != len(values):
        sys.exit(f"jsbach_reset_laimax_dyn.py: {path}: {len(numbers)} LctNumber but"
                 f" {len(values)} MaxLAI values")
    return dict(zip(numbers, values))


def first_2d_field(path):
    with Dataset(path) as ds:
        for name, var in ds.variables.items():
            if var.ndim >= 2 and name not in ("lat", "lon", "lat_bnds", "lon_bnds"):
                return np.squeeze(np.ma.filled(np.ma.asarray(var[:], dtype="f8"), 0.0)), \
                    np.asarray(ds.variables["lat"][:]) if "lat" in ds.variables else None
    sys.exit(f"jsbach_reset_laimax_dyn.py: no 2-D field in {path}")


def same_orientation(lat_a, lat_b, what):
    """True if equal, False if reversed; stop otherwise."""
    if lat_a is None or lat_b is None:
        return True
    if lat_a.shape == lat_b.shape and np.allclose(lat_a, lat_b, atol=1e-6):
        return True
    if lat_a.shape == lat_b.shape and np.allclose(lat_a, lat_b[::-1], atol=1e-6):
        return False
    sys.exit(f"jsbach_reset_laimax_dyn.py: latitudes of {what} do not match the restart")


def read_mask(path, lat_r, shape):
    field, lat_m = first_2d_field(path)
    if field.shape != shape:
        sys.exit(f"jsbach_reset_laimax_dyn.py: {path} {field.shape} does not match"
                 f" the restart grid {shape}")
    if not same_orientation(lat_r, lat_m, path):
        field = field[::-1, :]
    return field > 0.5


TRAPPED_MAX = 1.0e-4   # > LAI_negligible (1e-5), far below any MaxLAI of vegetation


def main(argv):
    if len(argv) != 5:
        sys.exit(__doc__)
    laimax_file, init_file, lctlib_file, land_mask_file, glacier_mask_file = argv
    maxlai = read_lctlib_maxlai(lctlib_file)

    with Dataset(init_file) as ds:
        ctype = np.ma.filled(np.ma.asarray(ds.variables["cover_type"][:], dtype="f8"), 0.0)
        lat_init = np.asarray(ds.variables["lat"][:])
        glac_new = (np.squeeze(np.ma.filled(np.ma.asarray(ds.variables["glac"][:], dtype="f8"), 0.0)) > 0.5
                    if "glac" in ds.variables else None)
    ctype = np.rint(np.squeeze(ctype)).astype(int)
    if glac_new is None:
        glac_new = (ctype == 1).any(axis=0)

    with Dataset(laimax_file, "r+") as ds:
        var = ds.variables["laiMax_dyn"]
        lat_r = np.asarray(ds.variables["lat"][:]) if "lat" in ds.variables else None
        raw = np.ma.asarray(var[:], dtype="f8")
        fill_mask = np.ma.getmaskarray(raw)
        lai = np.squeeze(np.ma.filled(raw, np.nan))
        if lai.ndim != 3 or ctype.shape != lai.shape:
            sys.exit(f"jsbach_reset_laimax_dyn.py: laiMax_dyn {lai.shape} and cover_type"
                     f" {ctype.shape} differ in shape")
        if not same_orientation(lat_r, lat_init, init_file):
            ctype = ctype[:, ::-1, :]
            glac_new = glac_new[::-1, :]

        land = np.isfinite(lai[0])
        to_land = read_mask(land_mask_file, lat_r, lai.shape[1:]) & land
        to_glac = read_mask(glacier_mask_file, lat_r, lai.shape[1:]) & land
        both = to_land & to_glac
        if both.any():
            sys.exit(f"jsbach_reset_laimax_dyn.py: {int(both.sum())} cell(s) in both masks")
        trapped = land & ~glac_new & ~to_land & ~to_glac & np.all(lai <= TRAPPED_MAX, axis=0)
        print(f" * laiMax_dyn: {int(to_land.sum())} deglaciated cell(s) -> MaxLAI(cover_type),"
              f" {int(to_glac.sum())} new glacier cell(s) unchanged,"
              f" {int(trapped.sum())} earlier deglaciated cell(s) still at the glacier value -> MaxLAI(cover_type)")
        to_land = to_land | trapped
        if not to_land.any():
            return
        unknown = set()
        new = lai.copy()
        for t in range(lai.shape[0]):
            before = new[t][to_land]
            ct = ctype[t][to_land]
            vals = np.array([maxlai.get(int(c), np.nan) for c in ct])
            unknown |= {int(c) for c, v in zip(ct, vals) if not np.isfinite(v)}
            vals = np.where(np.isfinite(vals), vals, 0.0)  # unknown type: keep the old value (fmax below)
            if (ct == 1).any():
                print(f" ! tile {t + 1}: {int((ct == 1).sum())} deglaciated cell(s) still"
                      f" have cover type 1 (glacier) in {init_file}")
            # never below what is there: the LAI in the jsbach restart must stay <= laiMax_dyn
            new[t][to_land] = np.fmax(vals, before)
            after = new[t][to_land]
            print(f"   tile {t + 1:2d}: before {np.nanmin(before):6.3f}..{np.nanmax(before):6.3f}"
                  f"  after {after.min():6.3f}..{after.max():6.3f}")
        if unknown:
            print(f" ! cover types without MaxLAI in {lctlib_file}: {sorted(unknown)} -> laiMax_dyn left as it was")
        var[:] = np.ma.masked_array(new.reshape(raw.shape), mask=fill_mask)
        var.setncattr("reset_on_glacier_change",
                      "deglaciated (now or trapped earlier): max(MaxLAI(cover_type) from lctlib.def, old value);"
                      " new glacier: unchanged")


if __name__ == "__main__":
    main(sys.argv[1:])
