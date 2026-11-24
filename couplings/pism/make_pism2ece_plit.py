#!/usr/bin/env python3
"""Add the PISM-grid `plit` to a {region}_pism2ece.nc feedback file.

The feedback file has two consumers on two grids. OIFS suorog reads
`plit_oifs` and `usurf_oifs` in OIFS gridpoint order; the ISM-mapper reads
`plit` (and, when the runoff-mapper exchange is on, the
tendency_of_ice_amount_due_to_* fields) on the PISM grid, with dimensions
named time/y/x.

Our chunk-1 file was built for suorog only, so it carries just the OIFS half.
This adds the PISM half, seeded from an initial ice thickness.

`plit` is written time-invariant. UpdateISMForcing indexes it as month+1, so
the record count is deliberately generous rather than 12: a fixed initial
geometry has no seasonal cycle, and this sidesteps the off-by-one.

Usage:
  make_pism2ece_plit.py <pism_initial.nc> <target_pism2ece.nc> [n_records]
"""
import sys

import numpy as np
import netCDF4 as nc

THK_MIN = 1.0  # m; below this a cell is not land ice


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    src_path, tgt_path = sys.argv[1], sys.argv[2]
    nrec = int(sys.argv[3]) if len(sys.argv) > 3 else 14

    src = nc.Dataset(src_path)
    if "thk" in src.variables:
        field = np.array(src["thk"][:])
        src.close()
        if field.ndim == 3:
            field = field[-1]
        plit = (field > THK_MIN).astype(np.float64)
        how = f"thk > {THK_MIN} m"
    elif "mask" in src.variables:
        # latest_ice_geometry.nc, which is what couple_in has to hand, carries
        # PISM's ice-type mask rather than thickness: 2 grounded, 3 floating,
        # 4 ice-free ocean. Land ice is 2 or 3, and that agrees with the
        # thickness test to within 40 cells of 214k on the same state.
        field = np.array(src["mask"][:])
        src.close()
        if field.ndim == 3:
            field = field[-1]
        plit = np.isin(field, (2, 3)).astype(np.float64)
        how = "mask in (2,3)"
    else:
        src.close()
        raise SystemExit(f"make_pism2ece_plit: {src_path} has neither thk nor mask")
    ny, nx = plit.shape

    tgt = nc.Dataset(tgt_path, "a")
    try:
        # Deliberately NOT skipped when already present: the ice sheet moves, so
        # each leg's plit has to come from that leg's geometry. Only the
        # variable's values are rewritten; dimensions are reused.
        refreshed = "plit" in tgt.variables
        for name, size in (("time", nrec), ("y", ny), ("x", nx)):
            if name not in tgt.dimensions:
                tgt.createDimension(name, size)
            elif tgt.dimensions[name].size != size:
                raise SystemExit(
                    f"make_pism2ece_plit: existing dim {name} is "
                    f"{tgt.dimensions[name].size}, need {size}")
        v = (tgt["plit"] if refreshed
             else tgt.createVariable("plit", "f8", ("time", "y", "x")))
        v.units = "1"
        v.long_name = "land ice mask on the PISM grid (1=ice)"
        v[:] = np.broadcast_to(plit, (nrec, ny, nx))
    finally:
        tgt.close()

    print(f"  {'refreshed' if refreshed else 'wrote'} plit {nrec}x{ny}x{nx} "
          f"from {how}, ice cells {int(plit.sum())} of {plit.size} "
          f"({100*plit.mean():.1f}%)")


if __name__ == "__main__":
    main()
