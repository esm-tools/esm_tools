#!/usr/bin/env python3
"""Write a CDO grid description with cell corners for a PISM grid.

The griddes files shipped in the PISM pools describe the ice grid by cell
*centres* only. Several consumers need the cell *corners* as well:

  * ``cdo remapcon2`` (ice2echam masks and discharge) aborts with
    "Source grid cell corner coordinates missing!",
  * ``cdo gridarea`` (hosing correction) aborts with
    "gridcell_areas: Cell corner coordinates missing!",
  * the fesom_icb_pism iceberg plugin reads ``lon_bnds``/``lat_bnds`` from the
    discharge file and fails with ``AttributeError`` without them.

PISM grids are projected and equidistant, so the corners are computable: take
the cell centres in projection space, offset them by half a cell, and convert
back to geographic coordinates.

The projection is read from the file's ``mapping`` variable (polar
stereographic: origin, standard parallel, central longitude, ellipsoid). How
the array axes relate to it is not always as stated -- the Antarctic 8 km file
has x and y swapped and is turned by 90 degrees -- so the axis order and a
turn by a multiple of 90 degrees around the stated central longitude are tried,
and the one that reproduces the file's own ``lon``/``lat`` is used. If none
does, the script fails rather than emit a plausible-looking but wrongly
rotated grid.

Usage
-----
    make_ice_griddes.py PISM_FILE OUTPUT.griddes [--tolerance DEG]
"""

import argparse
import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:  # pragma: no cover
    sys.exit("make_ice_griddes.py: needs netCDF4")
try:
    from pyproj import CRS, Transformer
except ImportError:  # pragma: no cover
    sys.exit("make_ice_griddes.py: needs pyproj")


def projection_from_mapping(ds):
    """Build a proj4 string from the CF grid mapping variable of a PISM file."""
    mapping = None
    for name in ("mapping", "crs", "polar_stereographic"):
        if name in ds.variables:
            mapping = ds.variables[name]
            break
    if mapping is None:
        sys.exit(" !!! no grid mapping variable (mapping/crs) in the file")

    attrs = {k: mapping.getncattr(k) for k in mapping.ncattrs()}
    name = str(attrs.get("grid_mapping_name", "")).lower()
    if "polar_stereographic" not in name:
        sys.exit(f" !!! unsupported grid_mapping_name {name!r}; only polar stereographic")

    lat0 = float(attrs.get("latitude_of_projection_origin", -90.0))
    lat_ts = float(attrs.get("standard_parallel", lat0))
    lon_pole = float(
        attrs.get(
            "straight_vertical_longitude_from_pole",
            attrs.get("longitude_of_projection_origin", 0.0),
        )
    )
    ellps = str(attrs.get("ellipsoid", attrs.get("ellps", "WGS84")))
    x0 = float(attrs.get("false_easting", 0.0))
    y0 = float(attrs.get("false_northing", 0.0))
    return lat0, lat_ts, lon_pole, ellps, x0, y0


_GEOGRAPHIC = "+proj=longlat +ellps={ellps} +no_defs"


def to_geographic(proj, swap, xx, yy, ellps):
    """Project (xx, yy) metre arrays to lon/lat; swap: array x is projection y.

    The target is given as proj4 rather than EPSG:4326 on purpose: pyproj on
    this machine has no PROJ database context, so EPSG codes cannot be looked
    up ("Internal Proj Error: proj_create: no database context specified").
    """
    t = Transformer.from_crs(
        CRS.from_proj4(proj),
        CRS.from_proj4(_GEOGRAPHIC.format(ellps=ellps)),
        always_xy=True,
    )
    lon, lat = t.transform(yy, xx) if swap else t.transform(xx, yy)
    return np.asarray(lon), np.asarray(lat)


def angular_diff(a, b):
    return np.abs(((np.asarray(a) - np.asarray(b) + 180.0) % 360.0) - 180.0)


def projection(ds, x, y, tolerance):
    """(proj4, swap, ellps) of the file's grid, checked against its lon/lat."""
    lat0, lat_ts, lon_pole, ellps, x0, y0 = projection_from_mapping(ds)
    lon_file = np.asarray(ds.variables["lon"][:], dtype=float)
    lat_file = np.asarray(ds.variables["lat"][:], dtype=float)
    xx, yy = np.meshgrid(x, y)
    away = np.hypot(xx, yy) > 2.0 * abs(x[1] - x[0])  # lon is degenerate at the pole
    best = None
    for turn in (0.0, 90.0, 180.0, 270.0):
        proj = (
            f"+proj=stere +lat_0={lat0} +lat_ts={lat_ts} +lon_0={lon_pole + turn} "
            f"+k=1 +x_0={x0} +y_0={y0} +ellps={ellps} +units=m"
        )
        for swap in (False, True):
            lon, lat = to_geographic(proj, swap, xx, yy, ellps)
            if lon.shape != lon_file.shape:
                continue
            dlon = float(np.nanmedian(angular_diff(lon[away], lon_file[away])))
            dlat = float(np.nanmedian(np.abs(lat - lat_file)))
            if best is None or max(dlon, dlat) < best[0]:
                best = (max(dlon, dlat), proj, swap, dlon, dlat)
    if best is None:
        sys.exit(" !!! lon/lat and x/y of the file do not have matching shapes")
    score, proj, swap, dlon, dlat = best
    print(f" * {proj}  (array axes swapped: {swap})")
    print(f"   median |dlon|={dlon:.3e} deg, |dlat|={dlat:.3e} deg")
    if score > tolerance:
        sys.exit(
            f" !!! no axis convention reproduces the file's lon/lat (> {tolerance:.1e} deg).\n"
            " !!! Refusing to write a griddes that may be rotated."
        )
    return proj, swap, ellps


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("pism_file")
    ap.add_argument("output")
    ap.add_argument("--tolerance", type=float, default=1e-3,
                    help="max tolerated centre mismatch in degrees (default 1e-3)")
    args = ap.parse_args()

    ds = Dataset(args.pism_file)
    for needed in ("x", "y", "lon", "lat"):
        if needed not in ds.variables:
            sys.exit(f" !!! {args.pism_file} has no variable {needed!r}")
    x = np.asarray(ds.variables["x"][:], dtype=float)
    y = np.asarray(ds.variables["y"][:], dtype=float)
    dx = np.diff(x)
    dy = np.diff(y)
    if not (np.allclose(dx, dx[0]) and np.allclose(dy, dy[0])):
        sys.exit(" !!! x/y are not equidistant; corners cannot be derived this way")
    dx, dy = float(dx[0]), float(dy[0])
    nx, ny = len(x), len(y)
    print(f" * grid {nx} x {ny}, dx={dx:g} m, dy={dy:g} m")

    proj, swap, ellps = projection(ds, x, y, args.tolerance)

    # Centres straight from the file, so they are correct by construction.
    lon_c = np.asarray(ds.variables["lon"][:], dtype=float).ravel()
    lat_c = np.asarray(ds.variables["lat"][:], dtype=float).ravel()

    # Corners. The offsets below walk the cell counterclockwise in projection
    # space; swapped axes or a south polar projection turn that into
    # clockwise in lon/lat, and CDO then builds self-inconsistent polygons
    # (absurd cell areas, and conservative remapping with them). So the
    # orientation is measured on the result and reversed if needed.
    offs = ((-0.5, -0.5), (+0.5, -0.5), (+0.5, +0.5), (-0.5, +0.5))
    xx, yy = np.meshgrid(x, y)
    lon_b = np.empty((ny * nx, 4))
    lat_b = np.empty((ny * nx, 4))
    for k, (fx, fy) in enumerate(offs):
        lo, la = to_geographic(proj, swap, xx + fx * dx, yy + fy * dy, ellps)
        lon_b[:, k] = np.asarray(lo).ravel()
        lat_b[:, k] = np.asarray(la).ravel()

    # Shoelace signed area per cell, on longitudes unwrapped relative to the
    # first corner so the dateline does not corrupt the sign.
    rel = ((lon_b - lon_b[:, [0]] + 180.0) % 360.0) - 180.0
    nxt = [1, 2, 3, 0]
    signed = 0.5 * np.sum(
        rel * lat_b[:, nxt] - rel[:, nxt] * lat_b, axis=1
    )
    finite = np.isfinite(signed) & (np.abs(signed) > 0)
    orientation = np.sign(np.median(signed[finite])) if finite.any() else 1.0
    if orientation < 0:
        print(" * corner order was clockwise in lon/lat -- reversing")
        lon_b = lon_b[:, ::-1]
        lat_b = lat_b[:, ::-1]
    else:
        print(" * corner order is counterclockwise in lon/lat")

    def block(label, values, per_line=6):
        out = [f"{label:<10}= "]
        flat = np.asarray(values).ravel()
        chunks = []
        for i in range(0, flat.size, per_line):
            chunks.append(" ".join(f"{v:.8f}" for v in flat[i:i + per_line]))
        out[0] += ("\n" + " " * 12).join(chunks)
        return out[0]

    with open(args.output, "w") as fh:
        fh.write("#\n# gridID 1 -- written by make_ice_griddes.py\n#\n")
        fh.write("gridtype  = curvilinear\n")
        fh.write(f"gridsize  = {nx * ny}\n")
        fh.write(f"xsize     = {nx}\n")
        fh.write(f"ysize     = {ny}\n")
        fh.write('xname     = lon\nxdimname  = x\n')
        fh.write('xlongname = "longitude"\nxunits    = "degrees_east"\n')
        fh.write('yname     = lat\nydimname  = y\n')
        fh.write('ylongname = "latitude"\nyunits    = "degrees_north"\n')
        fh.write("nvertex   = 4\n")
        fh.write(block("xvals", lon_c) + "\n")
        fh.write(block("xbounds", lon_b) + "\n")
        fh.write(block("yvals", lat_c) + "\n")
        fh.write(block("ybounds", lat_b) + "\n")
    print(f" * wrote {args.output}")


if __name__ == "__main__":
    main()
