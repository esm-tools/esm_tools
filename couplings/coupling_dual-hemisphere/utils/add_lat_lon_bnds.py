#!/usr/bin/env python3
"""
add_lat_lon_bnds.py

Add CF-compliant lat_bnds(y, x, nv4) and lon_bnds(y, x, nv4) to a curvilinear
netCDF file that already has 2D lat(y, x) and lon(y, x) coordinate variables.

Background:
  PISM 1.x's extra files do not emit lat_bnds/lon_bnds, and the antarct_cr 08 km
  griddes in the pool does not contain xbounds/ybounds blocks, so 'cdo setgrid'
  leaves the cell vertex coordinates undefined.  Downstream tools (e.g.
  icebergs/.../icb_apply_distribution_functions.py) read lat_bnds/lon_bnds
  directly.

Approach (no projection math):
  Cell vertices are computed by averaging the cell-centre lat/lon values of the
  (up to) four neighbouring cells that meet at each vertex.  Linear
  extrapolation handles the outer boundary of the grid.  Longitudes are
  averaged on the unit circle (sin/cos -> atan2) to handle wrap-around safely.

  This sidesteps any ambiguity in the projection convention (CF's
  polar_stereographic without an explicit straight_vertical_longitude_from_pole
  is interpreted differently by PROJ than by PISM here) and reproduces the
  file's own lat/lon convention exactly.

Operates in place.  Idempotent.

Usage:
    python add_lat_lon_bnds.py path/to/latest_discharge.nc
"""
import argparse
import sys

import numpy as np
import netCDF4


def _pad_extrapolate(arr):
    """Return arr padded by 1 on every side using linear extrapolation.

    arr is shape (ny, nx); the returned array is shape (ny+2, nx+2).
    Edges are filled with 2*edge - inner, corners are filled by extending
    from the diagonal neighbour.
    """
    ny, nx = arr.shape
    out = np.empty((ny + 2, nx + 2), dtype=np.float64)
    out[1:-1, 1:-1] = arr
    # left / right edges
    out[1:-1, 0] = 2.0 * arr[:, 0] - arr[:, 1]
    out[1:-1, -1] = 2.0 * arr[:, -1] - arr[:, -2]
    # bottom / top edges
    out[0, 1:-1] = 2.0 * arr[0, :] - arr[1, :]
    out[-1, 1:-1] = 2.0 * arr[-1, :] - arr[-2, :]
    # corners via diagonal extrapolation from the inner-most diagonal neighbour
    out[0, 0] = 2.0 * arr[0, 0] - arr[1, 1]
    out[0, -1] = 2.0 * arr[0, -1] - arr[1, -2]
    out[-1, 0] = 2.0 * arr[-1, 0] - arr[-2, 1]
    out[-1, -1] = 2.0 * arr[-1, -1] - arr[-2, -2]
    return out


def _vertex_grid_lat(lat_2d):
    """Compute lat values at the (ny+1, nx+1) cell vertices.

    Each vertex is the mean of the 4 surrounding cell centres.  At the outer
    boundary, the missing neighbours are linearly extrapolated first so that
    the corner cells of the grid have full 4-point averages.
    """
    padded = _pad_extrapolate(lat_2d)  # (ny+2, nx+2)
    # The vertex grid has shape (ny+1, nx+1).  Vertex (j, i) sits at the
    # SW corner of cell (j, i) in the original grid, i.e. it is the centre
    # of the 2x2 block padded[j:j+2, i:i+2].
    v = 0.25 * (
        padded[:-1, :-1] + padded[1:, :-1] + padded[:-1, 1:] + padded[1:, 1:]
    )
    return v


def _vertex_grid_lon(lon_2d):
    """Same as _vertex_grid_lat but averaging on the unit circle (wrap-safe)."""
    lon_rad = np.deg2rad(lon_2d)
    s = _pad_extrapolate(np.sin(lon_rad))
    c = _pad_extrapolate(np.cos(lon_rad))
    s_v = 0.25 * (s[:-1, :-1] + s[1:, :-1] + s[:-1, 1:] + s[1:, 1:])
    c_v = 0.25 * (c[:-1, :-1] + c[1:, :-1] + c[:-1, 1:] + c[1:, 1:])
    return np.rad2deg(np.arctan2(s_v, c_v))


def _cell_corners(vertex_grid):
    """From a (ny+1, nx+1) vertex grid, return cell corners as (ny, nx, 4).

    Vertex order, counter-clockwise starting at SW (CF convention):
        nv4=0: SW = vertex (j,   i)
        nv4=1: SE = vertex (j,   i+1)
        nv4=2: NE = vertex (j+1, i+1)
        nv4=3: NW = vertex (j+1, i)
    """
    sw = vertex_grid[:-1, :-1]
    se = vertex_grid[:-1, 1:]
    ne = vertex_grid[1:, 1:]
    nw = vertex_grid[1:, :-1]
    return np.stack([sw, se, ne, nw], axis=-1)


def add_bounds(filename, vertex_dim="nv4", verbose=True):
    with netCDF4.Dataset(filename, "r+") as ds:
        if "lat_bnds" in ds.variables and "lon_bnds" in ds.variables:
            if verbose:
                print(
                    "{}: lat_bnds and lon_bnds already present, skipping".format(
                        filename
                    )
                )
            return

        for needed in ("lat", "lon"):
            if needed not in ds.variables:
                raise RuntimeError(
                    "{}: missing required coordinate variable {!r}".format(
                        filename, needed
                    )
                )

        lat_var = ds.variables["lat"]
        lon_var = ds.variables["lon"]
        if lat_var.dimensions != lon_var.dimensions or len(lat_var.dimensions) != 2:
            raise RuntimeError(
                "{}: expected 2D curvilinear lat/lon sharing the same "
                "two dimensions".format(filename)
            )
        y_dim, x_dim = lat_var.dimensions

        lat_2d = np.asarray(lat_var[:], dtype=np.float64)
        lon_2d = np.asarray(lon_var[:], dtype=np.float64)
        if lat_2d.shape != lon_2d.shape:
            raise RuntimeError(
                "{}: lat and lon shape mismatch ({} vs {})".format(
                    filename, lat_2d.shape, lon_2d.shape
                )
            )
        ny, nx = lat_2d.shape
        if ny < 2 or nx < 2:
            raise RuntimeError(
                "{}: degenerate grid ({}x{})".format(filename, ny, nx)
            )

        lat_v = _cell_corners(_vertex_grid_lat(lat_2d))
        lon_v = _cell_corners(_vertex_grid_lon(lon_2d))

        # Vertex dimension.
        if vertex_dim not in ds.dimensions:
            ds.createDimension(vertex_dim, 4)
        elif len(ds.dimensions[vertex_dim]) != 4:
            raise RuntimeError(
                "{}: dimension {!r} exists with size {} (expected 4)".format(
                    filename, vertex_dim, len(ds.dimensions[vertex_dim])
                )
            )

        lat_bnds = ds.createVariable(
            "lat_bnds", "f8", (y_dim, x_dim, vertex_dim)
        )
        lon_bnds = ds.createVariable(
            "lon_bnds", "f8", (y_dim, x_dim, vertex_dim)
        )
        lat_bnds[:] = lat_v
        lon_bnds[:] = lon_v

        lat_var.bounds = "lat_bnds"
        lon_var.bounds = "lon_bnds"

        if verbose:
            print(
                "{}: added lat_bnds and lon_bnds "
                "({}={}, lat/lon dims=({}, {}))".format(
                    filename, vertex_dim, 4, y_dim, x_dim
                )
            )


def main(argv=None):
    ap = argparse.ArgumentParser(
        description=(
            "Add lat_bnds/lon_bnds to a netCDF file with 2D curvilinear "
            "lat(y,x) and lon(y,x), by averaging the existing cell-centre "
            "coordinates of the 4 surrounding cells at each vertex."
        )
    )
    ap.add_argument("file", help="netCDF file to modify in place")
    ap.add_argument(
        "--vertex-dim",
        default="nv4",
        help="name of the vertex dimension to create (default: nv4)",
    )
    ap.add_argument(
        "-q", "--quiet", action="store_true", help="suppress informational output"
    )
    args = ap.parse_args(argv)
    try:
        add_bounds(args.file, vertex_dim=args.vertex_dim, verbose=not args.quiet)
    except Exception as exc:
        print("add_lat_lon_bnds: {}".format(exc), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
