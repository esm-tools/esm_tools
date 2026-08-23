#!/usr/bin/env python3
"""Build the OASIS grid/mask/area entries for a PISM domain.

The ISM-mapper is an OASIS component, so the PISM grid has to appear in
grids.nc, masks.nc and areas.nc alongside the atmosphere and ocean grids.
Unlike the FESOM mesh and the OIFS land-sea mask, the PISM grid never moves:
antarct_cr is 761x761 at 8 km on a fixed polar stereographic projection. So
this runs ONCE and the result is appended to the per-leg OASIS files that
ocp-tool regenerates (see couplings/oifs/coupling_ice2oifs.functions).

Cell centres come straight out of the PISM file, which already carries lat/lon
with pism_intent="mapping". Only the corners have to be derived, by inverse-
projecting the cell corner (x,y) with the projection named in `mapping`.

Areas are computed as spherical excess (Girard) over the two triangles each
quadrilateral splits into. A plane-projection formula is wrong here for the
same reason it was wrong on the FESOM mesh: cells spanning the +/-180 seam get
a lon span of ~2*pi instead of ~0.

Usage:
  make_oasis_pism_grid.py <pism_file.nc> <out_dir> [grid_name]
    grid_name: 4-character OASIS grid name (default ismp)

Writes <out_dir>/pism_grids.nc, pism_masks.nc, pism_areas.nc, each holding
only the new grid's variables so they can be appended with `ncks -A`.
"""
import os
import sys

import numpy as np
import netCDF4 as nc
import pyproj

R_EARTH = 6371229.0  # m, OASIS/SCRIP sphere


def read_pism_grid(path):
    """Return (x, y, lat, lon, proj4) from a PISM restart or bootstrap file."""
    d = nc.Dataset(path)
    x = np.array(d["x"][:], dtype=float)
    y = np.array(d["y"][:], dtype=float)
    lat = np.array(d["lat"][:], dtype=float)
    lon = np.array(d["lon"][:], dtype=float)
    m = d["mapping"]
    attrs = {k: m.getncattr(k) for k in m.ncattrs()}
    d.close()

    name = attrs.get("grid_mapping_name", "").lower()
    if name != "polar_stereographic":
        raise SystemExit(f"make_oasis_pism_grid: unsupported projection {name!r}")
    proj4 = (
        f"+proj=stere +lat_0={attrs['latitude_of_projection_origin']} "
        f"+lat_ts={attrs['standard_parallel']} "
        f"+lon_0={attrs.get('straight_vertical_longitude_from_pole', 0.0)} "
        f"+x_0={attrs.get('false_easting', 0.0)} "
        f"+y_0={attrs.get('false_northing', 0.0)} "
        f"+ellps={attrs.get('ellipsoid', 'WGS84')} +units=m +no_defs"
    )
    return x, y, lat, lon, proj4


def corners(x, y, proj4):
    """Inverse-project the four (x,y) cell corners -> (clo, cla), shape (4,ny,nx).

    Corner order is counterclockwise in projected space:
    (x-,y-), (x+,y-), (x+,y+), (x-,y+).
    """
    dx = float(np.diff(x).mean())
    dy = float(np.diff(y).mean())
    xc = np.concatenate([x - dx / 2.0, [x[-1] + dx / 2.0]])
    yc = np.concatenate([y - dy / 2.0, [y[-1] + dy / 2.0]])
    XC, YC = np.meshgrid(xc, yc)

    tf = pyproj.Transformer.from_crs(pyproj.CRS.from_proj4(proj4), "EPSG:4326",
                                     always_xy=True)
    LONC, LATC = tf.transform(XC, YC)

    clo = np.stack([LONC[:-1, :-1], LONC[:-1, 1:], LONC[1:, 1:], LONC[1:, :-1]])
    cla = np.stack([LATC[:-1, :-1], LATC[:-1, 1:], LATC[1:, 1:], LATC[1:, :-1]])
    return clo, cla


def cell_areas(clo, cla):
    """Spherical-excess area [m2] of each quadrilateral, split into 2 triangles."""
    def unit(lon, lat):
        lo, la = np.radians(lon), np.radians(lat)
        return np.stack([np.cos(la) * np.cos(lo), np.cos(la) * np.sin(lo),
                         np.sin(la)], axis=0)

    v = [unit(clo[k], cla[k]) for k in range(4)]

    def tri(a, b, c):
        num = np.abs(np.einsum("i...,i...->...", a, np.cross(b, c, axis=0)))
        den = (1.0 + np.einsum("i...,i...->...", a, b)
                   + np.einsum("i...,i...->...", b, c)
                   + np.einsum("i...,i...->...", c, a))
        return 2.0 * np.arctan2(num, den) * R_EARTH * R_EARTH

    return tri(v[0], v[1], v[2]) + tri(v[0], v[2], v[3])


def write(out_dir, name, lon, lat, clo, cla, msk, srf):
    ny, nx = lon.shape
    os.makedirs(out_dir, exist_ok=True)

    def newfile(fname):
        d = nc.Dataset(os.path.join(out_dir, fname), "w", format="NETCDF3_CLASSIC")
        d.createDimension(f"x_{name}", nx)
        d.createDimension(f"y_{name}", ny)
        return d

    d = newfile("pism_grids.nc")
    d.createDimension(f"crn_{name}", 4)
    for vn, arr, units in ((f"{name}.lon", lon, "degrees_east"),
                           (f"{name}.lat", lat, "degrees_north")):
        v = d.createVariable(vn, "f8", (f"y_{name}", f"x_{name}"))
        v.units = units
        v[:] = arr
    for vn, arr, units in ((f"{name}.clo", clo, "degrees_east"),
                           (f"{name}.cla", cla, "degrees_north")):
        v = d.createVariable(vn, "f8", (f"crn_{name}", f"y_{name}", f"x_{name}"))
        v.units = units
        v[:] = arr
    d.close()

    d = newfile("pism_masks.nc")
    v = d.createVariable(f"{name}.msk", "i4", (f"y_{name}", f"x_{name}"))
    v.units = "1"
    # OASIS convention: 1 = masked out, 0 = active. Every PISM cell is active;
    # which cells carry ice is PISM's business, not the coupler's.
    v[:] = msk
    d.close()

    d = newfile("pism_areas.nc")
    v = d.createVariable(f"{name}.srf", "f8", (f"y_{name}", f"x_{name}"))
    v.units = "m2"
    v[:] = srf
    d.close()


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    pism_file, out_dir = sys.argv[1], sys.argv[2]
    name = sys.argv[3] if len(sys.argv) > 3 else "ismp"
    if len(name) != 4:
        raise SystemExit(f"make_oasis_pism_grid: grid name {name!r} must be 4 chars")

    x, y, lat, lon, proj4 = read_pism_grid(pism_file)
    clo, cla = corners(x, y, proj4)
    srf = cell_areas(clo, cla)
    msk = np.zeros(lat.shape, dtype=np.int32)

    write(out_dir, name, lon, lat, clo, cla, msk, srf)

    # Cross-check against the projected-plane area corrected by the polar
    # stereographic scale factor k. The two should agree to well under a
    # percent; a gross mismatch means the corner ordering or the projection
    # string is wrong.
    dx = float(np.diff(x).mean())
    lat_ts = np.radians(float(proj4.split("+lat_ts=")[1].split()[0]))
    la = np.radians(lat)
    k = (1.0 + np.sin(np.abs(lat_ts))) / (1.0 + np.sin(np.abs(la)))
    approx = float((dx * dx / (k * k)).sum())

    print(f"grid {name}: {lat.shape[1]}x{lat.shape[0]} at {dx/1000:.0f} km")
    print(f"  lon {lon.min():.2f} .. {lon.max():.2f}   "
          f"lat {lat.min():.2f} .. {lat.max():.2f}")
    print(f"  area  spherical excess : {srf.sum()/1e12:10.2f} x10^6 km2")
    print(f"        plane + k factor : {approx/1e12:10.2f} x10^6 km2  "
          f"({100*(srf.sum()-approx)/approx:+.2f} %)")
    print(f"  cell area min/max: {srf.min()/1e6:.2f} / {srf.max()/1e6:.2f} km2 "
          f"(nominal {dx*dx/1e6:.0f})")
    print(f"  wrote pism_grids.nc, pism_masks.nc, pism_areas.nc to {out_dir}")


if __name__ == "__main__":
    main()
