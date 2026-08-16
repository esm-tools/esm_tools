#!/usr/bin/env python3
"""Sanitize the A096/atm-grid fields of an OASIS restart (rstos.nc) against a
NEW atmosphere land-sea mask.

When the moving-cavity coupling regenerates the OIFS ICMGG (new coastline), a
handful of atm cells flip land->ocean. The OASIS restart's A096-side fields
(A_SST, A_Ice_temp, ...) are carried over from the previous leg untouched, so
those newly-ocean cells still hold land values (A_SST = 0 K). OIFS ingests
A_SST at its first step and the voskin cool-skin scheme overflows on a 0 K
ocean cell (forrtl (72), voskin_mod.F90:282).

Fix: for every cell that the NEW lsm calls ocean but whose A_SST is
non-physical (< 100 K), fill A_SST from the nearest valid ocean cell
(3D-cartesian nearest neighbour on the reduced gaussian grid) and floor
A_Ice_temp at the same cells. All other fields are left untouched (ice
fraction/albedo/snow are legitimately 0 there; fluxes are benign).

Usage: sanitize_oasis_restart_atm.py <rstos.nc> <new_icmgg_grib>
Edits <rstos.nc> in place; prints the number of filled cells.
"""
import sys
import numpy as np
import xarray as xr
import eccodes as ec
from scipy.spatial import cKDTree

TFREEZE = 271.35  # [K] sea-water freezing point; floor for filled ice temps
TVALID = 100.0    # [K] below this an SST/skin value is stale land garbage

def read_lsm_latlon(icmgg):
    """First lsm record of the ICMGG + its lat/lon (reduced gaussian)."""
    with open(icmgg, "rb") as f:
        while True:
            h = ec.codes_grib_new_from_file(f)
            if h is None:
                raise RuntimeError(f"no lsm record found in {icmgg}")
            if ec.codes_get(h, "shortName") == "lsm":
                it = ec.codes_grib_iterator_new(h, 0)
                lats, lons, vals = [], [], []
                while True:
                    r = ec.codes_grib_iterator_next(it)
                    if not r:
                        break
                    lats.append(r[0]); lons.append(r[1]); vals.append(r[2])
                ec.codes_grib_iterator_delete(it)
                ec.codes_release(h)
                return (np.asarray(vals), np.asarray(lats), np.asarray(lons))
            ec.codes_release(h)

def to_xyz(lat, lon):
    la, lo = np.radians(lat), np.radians(lon)
    return np.column_stack((np.cos(la) * np.cos(lo),
                            np.cos(la) * np.sin(lo),
                            np.sin(la)))

def main():
    rst_path, icmgg_path = sys.argv[1], sys.argv[2]
    lsm, lat, lon = read_lsm_latlon(icmgg_path)
    npts = lsm.size

    ds = xr.load_dataset(rst_path)
    if "A_SST" not in ds:
        print(f"sanitize_oasis_restart_atm: no A_SST in {rst_path}; nothing to do")
        return
    sst_da = ds["A_SST"]
    sst = np.asarray(sst_da.values, dtype=float)
    flat = sst.reshape(-1)
    if flat.size != npts:
        print(f"sanitize_oasis_restart_atm: A_SST size {flat.size} != lsm size "
              f"{npts}; grid mismatch, refusing to touch {rst_path}")
        sys.exit(1)

    ocean = lsm <= 0.5
    bad = ocean & (flat < TVALID)          # ocean per NEW lsm, stale land value
    good = ocean & (flat >= TVALID)        # donor cells
    if not bad.any():
        print("sanitize_oasis_restart_atm: no stale ocean cells; nothing to do")
        return
    if not good.any():
        print("sanitize_oasis_restart_atm: no valid ocean donors; refusing")
        sys.exit(1)

    tree = cKDTree(to_xyz(lat[good], lon[good]))
    _, idx = tree.query(to_xyz(lat[bad], lon[bad]), k=1)
    flat[bad] = flat[good][idx]
    ds["A_SST"].values[...] = flat.reshape(sst.shape)
    print(f"sanitize_oasis_restart_atm: A_SST filled {int(bad.sum())} "
          f"newly-ocean cells from nearest ocean neighbours "
          f"(range now {flat[ocean].min():.2f}..{flat[ocean].max():.2f} K)")

    # Ice skin temperature: same cells, floored at freezing (voskin/ice-tile safe)
    if "A_Ice_temp" in ds:
        it = np.asarray(ds["A_Ice_temp"].values, dtype=float).reshape(-1)
        n_it = int((bad & (it < TVALID)).sum())
        it[bad & (it < TVALID)] = np.minimum(flat[bad & (it < TVALID)], TFREEZE)
        ds["A_Ice_temp"].values[...] = it.reshape(ds["A_Ice_temp"].shape)
        print(f"sanitize_oasis_restart_atm: A_Ice_temp floored at {n_it} cells")

    ds.to_netcdf(rst_path)

if __name__ == "__main__":
    main()
