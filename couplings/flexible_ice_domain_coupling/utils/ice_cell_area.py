#!/usr/bin/env python3
"""Write the cell area of a PISM grid as PISM itself counts it: dx * dy.

PISM grids are projected (polar stereographic) and equidistant in projection
space, and PISM books every mass on the projected cell, dx * dy. ``cdo
gridarea`` gives the true area on the sphere instead, which differs from it by
the square of the projection's scale factor (a few percent on the usual
Greenland and Antarctic grids). A flux integrated with the true area therefore
does not add up to the mass PISM lost; integrated with dx * dy it does.

The grid spacing is read from the 1-D coordinate variables of the dimensions
of VARNAME (x and y in PISM files), so no cell corners are needed.

Usage
-----
    ice_cell_area.py FILE OUT_FILE [--varname total_ice_mass_loss_flux]

OUT_FILE gets ``cell_area(<ydim>, <xdim>)`` [m2] on the same dimensions as
VARNAME, so it can be used as weights for it (apply_hosing_correction.py,
conserve_remapped_flux.py).
"""

import argparse
import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:  # pragma: no cover
    sys.exit("ice_cell_area.py: needs netCDF4 (module load python3)")


def spacing(ds, dim):
    if dim not in ds.variables:
        sys.exit(f"ice_cell_area.py: no coordinate variable for dimension {dim!r}")
    coord = np.asarray(ds.variables[dim][:], dtype="f8")
    if coord.size < 2:
        sys.exit(f"ice_cell_area.py: dimension {dim!r} has fewer than 2 points")
    step = np.abs(np.diff(coord))
    if not np.allclose(step, step[0], rtol=1e-6):
        sys.exit(f"ice_cell_area.py: {dim!r} is not equidistant; not a PISM grid?")
    units = getattr(ds.variables[dim], "units", "m")
    factor = {"m": 1.0, "km": 1000.0}.get(units)
    if factor is None:
        sys.exit(f"ice_cell_area.py: unknown units {units!r} of {dim!r}")
    return float(step[0]) * factor


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("file")
    parser.add_argument("out_file")
    parser.add_argument("--varname", default="total_ice_mass_loss_flux")
    args = parser.parse_args(argv)

    with Dataset(args.file) as ds:
        if args.varname not in ds.variables:
            sys.exit(f"ice_cell_area.py: no {args.varname} in {args.file}")
        dims = [d for d in ds.variables[args.varname].dimensions if ds.dimensions[d].size > 1]
        if len(dims) != 2:
            sys.exit(f"ice_cell_area.py: {args.varname} has dimensions {dims}, expected (y, x)")
        ydim, xdim = dims
        dy, dx = spacing(ds, ydim), spacing(ds, xdim)
        ny, nx = ds.dimensions[ydim].size, ds.dimensions[xdim].size

    with Dataset(args.out_file, "w") as out:
        out.createDimension(ydim, ny)
        out.createDimension(xdim, nx)
        var = out.createVariable("cell_area", "f8", (ydim, xdim))
        var[:] = np.full((ny, nx), dx * dy)
        var.units = "m2"
        var.long_name = "projected grid cell area (dx * dy), as PISM accounts mass"
        out.source = args.file
    print(f" * {args.out_file}: dx={dx:g} m, dy={dy:g} m, cell area {dx * dy:g} m2")


if __name__ == "__main__":
    main(sys.argv[1:])
