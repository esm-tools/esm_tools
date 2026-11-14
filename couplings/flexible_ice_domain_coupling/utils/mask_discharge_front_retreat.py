#!/usr/bin/env python3
"""Set PISM's discharge to 0 where the front-retreat file allows no ice.

Usage:
    mask_discharge_front_retreat.py DISCHARGE_FILE FRONT_RETREAT_FILE

DISCHARGE_FILE is changed in place: tendency_of_ice_amount_due_to_discharge
becomes 0 wherever land_ice_area_fraction_retreat of FRONT_RETREAT_FILE (the
file given to PISM as geometry.front_retreat.prescribed.file) is 0.

Why: PISM books the ice it removes there as discharge. Over a masked-out
land region (e.g. Tibet in the Northern Hemisphere domain: ~2200 Gt/yr in a
110k run) that is snow being cleared, not calving, and must not be seeded
as icebergs or subtracted from the ice mass loss.

The two files may have different resolutions on the same projection; every
discharge cell takes the nearest retreat-file cell by projection x/y. The
script stops if the matched cells are more than 0.5 degrees apart in
latitude (different projections).
"""
import sys

import numpy as np
from netCDF4 import Dataset

VAR = "tendency_of_ice_amount_due_to_discharge"
MASK = "land_ice_area_fraction_retreat"


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    dfile, rfile = sys.argv[1], sys.argv[2]

    with Dataset(rfile) as r:
        if MASK not in r.variables:
            sys.exit(f" !!! no {MASK} in {rfile}")
        mask = r[MASK]
        if mask.ndim == 3 and mask.shape[0] != 1:
            sys.exit(f" !!! {rfile} has {mask.shape[0]} time steps; only a fixed mask is supported")
        allowed = np.ma.filled(mask[:].reshape(mask.shape[-2:]), 1.0) > 0.5
        rx, ry = np.asarray(r["x"][:]), np.asarray(r["y"][:])
        rlat = np.asarray(r["lat"][:])

    with Dataset(dfile, "r+") as d:
        if VAR not in d.variables:
            sys.exit(f" !!! no {VAR} in {dfile}")
        xi = np.abs(rx[None, :] - np.asarray(d["x"][:])[:, None]).argmin(1)
        yi = np.abs(ry[None, :] - np.asarray(d["y"][:])[:, None]).argmin(1)
        if "lat" in d.variables:
            dlat = np.abs(rlat[np.ix_(yi, xi)] - np.asarray(d["lat"][:])).max()
            if dlat > 0.5:
                sys.exit(f" !!! {rfile} and {dfile} differ by {dlat:.1f} deg in latitude: not the same projection")
        removed = ~allowed[np.ix_(yi, xi)]

        var = d[VAR]
        data = var[:]
        full = np.broadcast_to(removed, data.shape)
        before = float(np.ma.sum(data))
        cut = float(np.ma.sum(np.ma.masked_where(~full, data)))
        data[full] = 0.0
        var[:] = data
        var.setncattr("comment_front_retreat", f"set to 0 where {MASK} = 0 in {rfile}")

    share = 100.0 * cut / before if before else 0.0
    print(f" * {VAR}: set to 0 in {int(removed.sum())} cells without ice allowed "
          f"({share:.1f}% of the field sum)")


if __name__ == "__main__":
    main()
