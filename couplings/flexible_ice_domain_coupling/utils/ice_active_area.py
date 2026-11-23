#!/usr/bin/env python3
"""Write where an ice domain may hold ice: its footprint for ECHAM.

ice2echam lets the ice sheet decide about ECHAM's glaciers, orography
sub-grid parameters and albedo inside each domain's footprint, and keeps the
prescribed (unit.24) state outside it. The footprint used to be the whole PISM
grid rectangle. Parts of that rectangle where PISM is not allowed to grow ice
(the prescribed front-retreat mask, e.g. Tibet or the Canadian Arctic in a
Greenland grid) then lost their prescribed glaciers. This script marks them
as outside.

Usage:
    ice_active_area.py GRID_FILE OUT_FILE [--front-retreat FILE]

GRID_FILE  any file on the domain's PISM grid with x, y (and lat) coordinate
           variables, e.g. ice_mask_current.nc
OUT_FILE   gets ice_domain(y, x): 1 where the domain may hold ice, 0 where
           land_ice_area_fraction_retreat of the front-retreat file is 0.
           Without --front-retreat the whole grid is 1.

The front-retreat file may have a different resolution on the same
projection; every grid cell takes the nearest front-retreat cell by x/y (as
mask_discharge_front_retreat.py does). Stops if matched cells are more than
0.5 degrees apart in latitude (different projections).
"""

import argparse
import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:  # pragma: no cover
    sys.exit("ice_active_area.py: needs netCDF4 (module load python3)")

MASK = "land_ice_area_fraction_retreat"


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("grid_file")
    parser.add_argument("out_file")
    parser.add_argument("--front-retreat", default="")
    args = parser.parse_args(argv)

    with Dataset(args.grid_file) as g:
        for name in ("x", "y"):
            if name not in g.variables:
                sys.exit(f" !!! no {name} coordinate in {args.grid_file}")
        gx, gy = np.asarray(g["x"][:]), np.asarray(g["y"][:])
        glat = np.asarray(g["lat"][:]) if "lat" in g.variables else None
        glon = np.asarray(g["lon"][:]) if "lon" in g.variables else None
        latdims = g["lat"].dimensions if "lat" in g.variables else None

    active = np.ones((gy.size, gx.size), dtype="f4")
    if args.front_retreat:
        with Dataset(args.front_retreat) as r:
            if MASK not in r.variables:
                sys.exit(f" !!! no {MASK} in {args.front_retreat}")
            mask = r[MASK]
            if mask.ndim == 3 and mask.shape[0] != 1:
                sys.exit(f" !!! {args.front_retreat} has {mask.shape[0]} time steps;"
                         " only a fixed mask is supported")
            allowed = np.ma.filled(mask[:].reshape(mask.shape[-2:]), 1.0) > 0.5
            rx, ry = np.asarray(r["x"][:]), np.asarray(r["y"][:])
            rlat = np.asarray(r["lat"][:]) if "lat" in r.variables else None
        xi = np.abs(rx[None, :] - gx[:, None]).argmin(1)
        yi = np.abs(ry[None, :] - gy[:, None]).argmin(1)
        if rlat is not None and glat is not None and latdims == ("y", "x"):
            dlat = np.abs(rlat[np.ix_(yi, xi)] - glat).max()
            if dlat > 0.5:
                sys.exit(f" !!! {args.front_retreat} and {args.grid_file} differ by"
                         f" {dlat:.1f} deg in latitude: not the same projection")
        active = allowed[np.ix_(yi, xi)].astype("f4")

    with Dataset(args.out_file, "w") as out:
        out.createDimension("y", gy.size)
        out.createDimension("x", gx.size)
        for name, values in (("y", gy), ("x", gx)):
            var = out.createVariable(name, "f8", (name,))
            var[:] = values
            var.units = "m"
        if glat is not None and latdims == ("y", "x"):
            for name, values, units in (("lat", glat, "degrees_north"),
                                        ("lon", glon, "degrees_east")):
                var = out.createVariable(name, "f8", ("y", "x"))
                var[:] = values
                var.units = units
        var = out.createVariable("ice_domain", "f4", ("y", "x"))
        var[:] = active
        var.long_name = "1 where this ice domain may hold ice"
        if glat is not None and latdims == ("y", "x"):
            var.coordinates = "lat lon"
        if args.front_retreat:
            var.comment = f"0 where {MASK} = 0 in {args.front_retreat}"
    print(f" * {args.out_file}: {int(active.sum())} of {active.size} cells may hold ice")


if __name__ == "__main__":
    main(sys.argv[1:])
