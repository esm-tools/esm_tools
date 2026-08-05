#!/usr/bin/env python3
"""Add lon_bnds and lat_bnds to a file on the PISM grid.

The iceberg generator reads the corners of each discharge cell to place bergs
inside it (`_get_coords` wants `lon_bnds`/`lat_bnds` shaped (y, x, 4)). Nothing
upstream of it writes them: the PISM griddes in the pool carries centres only,
so `cdo -setgrid` cannot add them, and `latest_discharge.nc` comes out without.

The corners are the same ones ocp-tool computes for the OASIS grid, so this uses
that code rather than a second implementation of the projection.

Usage:
  add_cell_bounds.py <file.nc> <pism_grid_file.nc>
"""
import sys

import numpy as np
import netCDF4 as nc


def main(argv=None):
    argv = argv if argv is not None else sys.argv[1:]
    if len(argv) < 2:
        raise SystemExit(__doc__)
    target, grid_file = argv[0], argv[1]

    from ocp_tool.grids.pism import PISM

    grid = PISM(grid_file)
    corners = grid.cell_corners()          # (2, 4, ny, nx), 0=lat 1=lon
    lat_c = np.transpose(corners[0], (1, 2, 0))   # -> (ny, nx, 4)
    lon_c = np.transpose(corners[1], (1, 2, 0))

    with nc.Dataset(target, "a") as d:
        ny, nx = lat_c.shape[:2]
        if d.dimensions["y"].size != ny or d.dimensions["x"].size != nx:
            raise SystemExit(
                f"add_cell_bounds: {target} is "
                f"{d.dimensions['y'].size}x{d.dimensions['x'].size}, "
                f"grid is {ny}x{nx}")
        if "nv4" not in d.dimensions:
            d.createDimension("nv4", 4)
        for name, data in (("lon_bnds", lon_c), ("lat_bnds", lat_c)):
            if name not in d.variables:
                d.createVariable(name, "f8", ("y", "x", "nv4"))
            d[name][:] = data
        for base, bnds in (("lon", "lon_bnds"), ("lat", "lat_bnds")):
            if base in d.variables:
                d[base].bounds = bnds

    print(f" *   added lon_bnds/lat_bnds ({ny}x{nx}x4) to {target}")


if __name__ == "__main__":
    main()
