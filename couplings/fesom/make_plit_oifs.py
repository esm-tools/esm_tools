#!/usr/bin/env python
"""Build the OIFS land-ice mask (plit_oifs) for a moving-cavity leg.

On a mesh-change leg ice2fesom regenerates, via ocp-tool, a land-sea mask for
the new FESOM submesh that is consistent with the OASIS A096<->feom coupling:
the modified OIFS initial file ICMGG*INIT_feomdyn carries that mask as its `lsm`
field (paramId 172) on the OIFS reduced-Gaussian grid, in native OIFS
gridpoint order.

OIFS warm-starts its own land-sea mask from the parallel srf restart, so it does
NOT see this regenerated coastline. suorog reads plit_oifs from
{prefix}_pism2ece.nc and the updtim KSTEP=0 hook flips OIFS cells sea->land
(and seeds a freezing-point surface temperature) where plit>threshold but the
restarted mask still says sea -- i.e. exactly the cells the moving coastline
grounded, which otherwise lose their OASIS SST and crash SURFPP with a
divide-by-zero.

So the correct, per-leg plit is simply this regenerated OIFS land mask,
restricted to the Antarctic cap (the only place the PISM coastline moves) so a
flip can never fire spuriously anywhere else on the globe.

Usage:
  make_plit_oifs.py <ICMGG*INIT_feomdyn> <out_pism2ece.nc> [lat_max]
    lat_max: northern latitude bound of the cap (default -60.0)
"""
import sys
import numpy as np
import eccodes as ec


def read_lsm(icmgg_path):
    """Return (values, latitudes) of the lsm field in native OIFS GP order."""
    with open(icmgg_path, "rb") as f:
        while True:
            gid = ec.codes_grib_new_from_file(f)
            if gid is None:
                break
            sn = ec.codes_get(gid, "shortName")
            pid = ec.codes_get(gid, "paramId")
            if sn == "lsm" or pid == 172:
                vals = ec.codes_get_values(gid)
                lats = ec.codes_get_array(gid, "latitudes")
                ec.codes_release(gid)
                return np.asarray(vals, dtype=float), np.asarray(lats, dtype=float)
            ec.codes_release(gid)
    raise SystemExit(f"make_plit_oifs: no lsm (paramId 172) field in {icmgg_path}")


def main():
    icmgg, out = sys.argv[1], sys.argv[2]
    lat_max = float(sys.argv[3]) if len(sys.argv) > 3 else -60.0

    lsm, lats = read_lsm(icmgg)
    if lsm.size != lats.size:
        raise SystemExit(
            f"make_plit_oifs: lsm ({lsm.size}) / latitudes ({lats.size}) size mismatch")

    plit = np.where(lats < lat_max, lsm, 0.0)
    nland = int((plit > 0.5).sum())
    print(f"make_plit_oifs: {icmgg} -> {out}; grid={lsm.size}, cap lat<{lat_max}, "
          f"plit active cells (>0.5)={nland}, sum={plit.sum():.1f}")
    if nland == 0:
        raise SystemExit(
            "make_plit_oifs: 0 active cells in the Antarctic cap -- the "
            "regenerated ICMGG lsm looks wrong; refusing to write an empty mask.")

    # netCDF4 via xarray-free write to match the (ny=1, nx) layout suorog reads.
    import netCDF4 as nc
    with nc.Dataset(out, "w", format="NETCDF4") as ds:
        ds.createDimension("ny", 1)
        ds.createDimension("nx", plit.size)
        v = ds.createVariable("plit_oifs", "f8", ("ny", "nx"))
        v.units = "1"
        v.long_name = "Antarctic land-ice mask (1=ice), OIFS GP order"
        v[:] = plit.reshape(1, -1)


if __name__ == "__main__":
    main()
