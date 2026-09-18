#!/usr/bin/env python
"""Build the OIFS orography field (usurf_oifs) from PISM's ice surface.

suorog reads `usurf_oifs` from {prefix}_pism2ece.nc at leg initialisation, in
native OIFS grid-point order, and patches the model's grid-point orography

    WHERE (ZLOC_USURF > 0) ZZOROG = ZLOC_USURF

so this is a plain file read, not an OASIS field. It therefore updates once per
leg, at restart, which is the right cadence for an ice sheet.

Two things about that WHERE test drive the design here.

It is a value test, not a mask, so a cell written as 0 is skipped and keeps
OIFS's own orography. Every cell inside the PISM footprint is therefore written
with a floor of EPS metres rather than 0. Without that floor, a cell that
deglaciates to bedrock below sea level has usurf = 0 in PISM, is skipped, and
keeps its glaciated elevation for ever: retreat would be invisible, and an
ice-free start would leave a phantom ice sheet in the atmosphere. On the
current geometry that is 21% of the ice sheet area, holding a mean 890 m and up
to 3246 m of orography that could never be removed.

The floor also makes the region boundary smooth. suorog runs the patched field
through REESPE and SPEREE, a T95 spectral round trip, and a cliff at the cap
edge would ring across the globe. With PISM's ocean at about 0 m meeting OIFS's
ocean at about 0 m there is no cliff. Measured on the real leg-1 step, a 217 m
RMS patch leaves 2.0 m RMS outside Antarctica, which is about 0.9% of the patch
and scales linearly with it.

Cells outside the footprint are written as 0 so suorog leaves them alone.

Usage:
  make_usurf_oifs.py <latest_ice_geometry.nc> <ICMGG*INIT> <out_pism2ece.nc>
"""
import os
import sys

import numpy as np
import netCDF4 as nc

# Metres. Small enough to read as sea level, large enough to pass "> 0" in
# every floating-point path suorog takes.
EPS = 0.01

# PISM's polar stereographic, with the y axis as PISM writes it.
PISM_CRS = "EPSG:3031"

# PISM defaults, for deriving usurf on a restart that does not carry it.
RHO_ICE = 910.0
RHO_SEAWATER = 1028.0


def oifs_grid(icmgg_path):
    """Row structure of the OIFS reduced Gaussian grid.

    Read from the file rather than derived from N, so it is right for the
    octahedral grids, whose row lengths are not the classic table.
    Returns (points per row, latitude of each row, index of each row's first
    point, total points).
    """
    import eccodes as ec

    with open(icmgg_path, "rb") as f:
        while True:
            gid = ec.codes_grib_new_from_file(f)
            if gid is None:
                break
            try:
                if ec.codes_get(gid, "gridType") != "reduced_gg":
                    continue
                pl = np.asarray(ec.codes_get_array(gid, "pl"), dtype=np.int64)
                lat = np.asarray(ec.codes_get_array(gid, "latitudes"), float)
            finally:
                ec.codes_release(gid)
            start = np.concatenate([[0], np.cumsum(pl)[:-1]])
            return pl, lat[start], start, int(pl.sum())
    raise SystemExit(f"make_usurf_oifs: no reduced Gaussian message in {icmgg_path}")


def main():
    if len(sys.argv) < 4:
        raise SystemExit(__doc__)
    geom_path, icmgg_path, out_path = sys.argv[1:4]

    from pyproj import Transformer

    ds = nc.Dataset(geom_path)

    def last(name):
        v = np.array(ds[name][:])
        return v[-1] if v.ndim == 3 else v

    if "usurf" in ds.variables:
        usurf = last("usurf")
        how = "usurf"
    elif "thk" in ds.variables and "topg" in ds.variables:
        # A PISM restart carries thk and topg but not usurf, which is
        # diagnostic. Rebuild it the way PISM does, so the same writer can seed
        # chunk 1 from a spin-up file and run per leg from the coupler's
        # ice-geometry extract.
        thk, topg = last("thk"), last("topg")
        floating = RHO_ICE * thk < RHO_SEAWATER * np.maximum(-topg, 0.0)
        usurf = np.where(floating,
                         thk * (1.0 - RHO_ICE / RHO_SEAWATER),
                         topg + thk)
        # Ice-free ocean sits at sea level, not at the bed.
        usurf = np.where((thk <= 0.0) & (topg < 0.0), 0.0, usurf)
        how = "thk and topg"
    else:
        ds.close()
        raise SystemExit(
            f"make_usurf_oifs: {geom_path} has neither usurf nor thk+topg. The "
            "cdo -expr in pism2esm has to carry 'usurf=usurf;'."
        )
    x = np.array(ds["x"][:])
    y = np.array(ds["y"][:])
    ds.close()
    print(f"make_usurf_oifs: surface elevation from {how}")

    # PISM writes y in the opposite sense to EPSG:3031. Taking the axes as
    # written puts East and West Antarctica on each other's longitudes: it
    # scores 1100 m RMS against OIFS's own orography where flipping y scores
    # 285 m. Nothing else in the coupling does this flip, so it has to be here.
    X, Y = np.meshgrid(x, -y)
    lon_p, lat_p = Transformer.from_crs(
        PISM_CRS, "EPSG:4326", always_xy=True).transform(X, Y)

    pl, row_lat, row_start, n_oifs = oifs_grid(icmgg_path)

    # Area mean of PISM onto the OIFS cells. At 8 km into cells of about
    # 100 km there are ~150 source cells per target cell, so assigning each
    # source cell to the OIFS cell containing it is an area mean to within the
    # boundary sliver. Row by latitude, then column by longitude within it.
    r = np.abs(row_lat[None, :] - lat_p.ravel()[:, None]).argmin(axis=1)
    col = np.floor((lon_p.ravel() % 360.0) / (360.0 / pl[r])).astype(np.int64) % pl[r]
    idx = row_start[r] + col

    flat = np.maximum(usurf, 0.0).ravel()
    count = np.bincount(idx, minlength=n_oifs)
    total = np.bincount(idx, weights=flat, minlength=n_oifs)
    covered = count > 0

    out = np.zeros(n_oifs)
    out[covered] = np.maximum(total[covered] / count[covered], EPS)

    print(f"make_usurf_oifs: {os.path.basename(geom_path)} -> "
          f"{os.path.basename(out_path)}")
    print(f"  OIFS cells in the PISM footprint: {int(covered.sum())} of {n_oifs}")
    print(f"  usurf over the footprint: mean {out[covered].mean():.1f} m, "
          f"max {out[covered].max():.1f} m, "
          f"at the {EPS} m floor: {int((out[covered] <= EPS).sum())}")
    if covered.sum() == 0:
        raise SystemExit(
            "make_usurf_oifs: the PISM footprint covers no OIFS cell. Refusing "
            "to write a field that would leave suorog patching nothing."
        )

    # Append, never truncate. This file has three consumers: suorog reads
    # usurf_oifs and plit_oifs in OIFS grid-point order, the ISM-mapper reads
    # plit(time, y, x) on the PISM grid.
    mode = "a" if os.path.exists(out_path) else "w"
    with nc.Dataset(out_path, mode, format="NETCDF4") as tgt:
        if "ny" not in tgt.dimensions:
            tgt.createDimension("ny", 1)
        if "nx" not in tgt.dimensions:
            tgt.createDimension("nx", n_oifs)
        elif tgt.dimensions["nx"].size != n_oifs:
            raise SystemExit(
                f"make_usurf_oifs: {out_path} has nx="
                f"{tgt.dimensions['nx'].size}, this ICMGG gives {n_oifs}")
        v = (tgt["usurf_oifs"] if "usurf_oifs" in tgt.variables
             else tgt.createVariable("usurf_oifs", "f8", ("ny", "nx")))
        v.units = "m"
        v.long_name = "ice sheet surface elevation, OIFS GP order, 0 outside the region"
        v[:] = out.reshape(1, -1)
        print(f"  mode={mode}, kept {[n for n in tgt.variables if n != 'usurf_oifs']}")


if __name__ == "__main__":
    main()
