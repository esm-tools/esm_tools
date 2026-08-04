#!/usr/bin/env python3
"""Add the ice sheet's freshwater fluxes to a {region}_pism2ece.nc feedback file.

The ISM-mapper reads `tendency_of_ice_amount_due_to_basal_mass_flux` and
`tendency_of_ice_amount_due_to_discharge` from this file when
LSendToRunoffMapper is on, flips their sign, and sends them to the runoff
mapper as `bmelt` and `discharge`. The mapper sums bmelt into runoff and
discharge into calving.

Only the GROUNDED basal melt is written. The ocean computes the cavity melt
itself and the ice sheet consumes it, so returning the floating part would send
the same water back where it came from.

PISM writes kg m-2 year-1, positive for ice gain. The runoff mapper works in
m/s of water alongside the atmosphere's runoff, so the values are divided by
the water density and the year length. The sign is left as PISM has it, since
the ISM-mapper does the flip.

Written time-invariant over the chunk, like `plit` and for the same reason:
UpdateISMForcing indexes the record as month+1 and whether month counts from 0
or 1 decides which record a given month lands on. A chunk mean sidesteps it,
and a decade of ice sheet discharge has no seasonal cycle worth resolving here.

Usage:
  make_pism2ece_fwf.py <pism_extra.nc> <target_pism2ece.nc> [n_records]
"""
import sys

import numpy as np
import netCDF4 as nc

RHO_W = 1000.0          # kg m-3
SEC_PER_YEAR = 365 * 86400.0

# (name in the target file, name in the PISM extra file)
FIELDS = (
    ("tendency_of_ice_amount_due_to_basal_mass_flux", "basal_mass_flux_grounded"),
    ("tendency_of_ice_amount_due_to_discharge", "tendency_of_ice_amount_due_to_discharge"),
)


def _chunk_mean(src, name):
    if name not in src.variables:
        raise SystemExit(f"make_pism2ece_fwf: {name} not in {src.filepath()}")
    a = np.ma.filled(src[name][:], np.nan).astype(np.float64)
    if a.ndim == 2:
        a = a[None]
    return np.nan_to_num(np.nanmean(a, axis=0), nan=0.0, posinf=0.0, neginf=0.0)


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    src_path, tgt_path = sys.argv[1], sys.argv[2]
    nrec = int(sys.argv[3]) if len(sys.argv) > 3 else 14

    src = nc.Dataset(src_path)
    means = {tgt_name: _chunk_mean(src, pism_name) for tgt_name, pism_name in FIELDS}
    src.close()

    ny, nx = next(iter(means.values())).shape

    tgt = nc.Dataset(tgt_path, "a")
    try:
        for name, size in (("time", nrec), ("y", ny), ("x", nx)):
            if name not in tgt.dimensions:
                tgt.createDimension(name, size)
            elif tgt.dimensions[name].size != size:
                raise SystemExit(
                    f"make_pism2ece_fwf: existing dim {name} is "
                    f"{tgt.dimensions[name].size}, need {size}")

        for name, field in means.items():
            field = field / (RHO_W * SEC_PER_YEAR)
            if name in tgt.variables:
                v = tgt[name]
            else:
                v = tgt.createVariable(name, "f8", ("time", "y", "x"))
            v.units = "m s-1"
            v.long_name = "water flux to the ocean, PISM sign (negative = ice loss)"
            v[:] = np.broadcast_to(field, (nrec, ny, nx))
            total = -field.sum() * 8000.0 * 8000.0 * SEC_PER_YEAR * RHO_W / 1e12
            print(f"  wrote {name}: {total:8.1f} Gt/yr to the ocean")
    finally:
        tgt.close()


if __name__ == "__main__":
    main()
