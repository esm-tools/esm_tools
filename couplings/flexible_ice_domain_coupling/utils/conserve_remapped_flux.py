#!/usr/bin/env python3
"""Make a remapped flux carry exactly the integral of the native flux.

Remapping an ice sheet flux to the ECHAM grid does not keep its integral:
``remapbil`` (the fallback without cell corners) is not conservative at all,
and even ``remapcon2`` only conserves with respect to the true areas on the
sphere, not the projected dx * dy cells that PISM books its mass on. The
ocean would then receive a different amount of water than the ice sheet lost.

This script corrects the remapped field in place:

    N = sum(F_native * A_native)          (A_native from ice_cell_area.py)
    R = sum(F_remapped * A_remapped)      (A_remapped from cdo gridarea)
    F_remapped += (N - R) * |F_remapped| / sum(|F_remapped| * A_remapped)

The difference is spread in proportion to |F|, so it stays where the flux is,
cells without flux stay at 0, and opposite signs cancelling in the sum do not
blow the correction up (as a plain scaling factor N / R would).

Usage
-----
    conserve_remapped_flux.py NATIVE NATIVE_AREA REMAPPED REMAPPED_AREA
                              [--varname total_ice_mass_loss_flux]
    conserve_remapped_flux.py - - FIELD FIELD_AREA --target-integral N

With --target-integral the field is corrected to N [kg/s] instead of to the
integral of a native field (pism2atmosphere: the mass change PISM's own
budget reports for the chunk).

The correction fixes the total, not a broken pattern. When it exceeds
--warn-percent (default 10) a warning says so: after a remapping that usually
means the ice grid had no cell corners and was remapped bilinearly; against
PISM's budget, that the chunk's input differed from where PISM started.
"""

import argparse
import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:  # pragma: no cover
    sys.exit("conserve_remapped_flux.py: needs netCDF4 (module load python3)")


def read(path, varname):
    with Dataset(path) as ds:
        if varname not in ds.variables:
            sys.exit(f"conserve_remapped_flux.py: no {varname} in {path}")
        data = np.ma.filled(np.ma.asarray(ds.variables[varname][:], dtype="f8"), 0.0)
    data = np.squeeze(data)
    return np.where(np.isfinite(data), data, 0.0)


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("native")
    parser.add_argument("native_area")
    parser.add_argument("remapped")
    parser.add_argument("remapped_area")
    parser.add_argument("--varname", default="total_ice_mass_loss_flux")
    parser.add_argument("--target-integral", type=float, default=None)
    parser.add_argument("--warn-percent", type=float, default=10.0)
    args = parser.parse_args(argv)

    if args.target_integral is None:
        f_nat = read(args.native, args.varname)
        a_nat = read(args.native_area, "cell_area")
        if f_nat.shape != a_nat.shape:
            sys.exit(f"conserve_remapped_flux.py: {args.native} {f_nat.shape} and "
                     f"{args.native_area} {a_nat.shape} differ in shape")
    with Dataset(args.remapped_area) as ds:
        area_name = "cell_area" if "cell_area" in ds.variables else [
            v for v in ds.variables if ds.variables[v].ndim >= 2][0]
    a_rem = read(args.remapped_area, area_name)

    with Dataset(args.remapped, "r+") as ds:
        var = ds.variables[args.varname]
        raw = np.ma.asarray(var[:], dtype="f8")
        f_rem = np.squeeze(np.where(np.isfinite(np.ma.filled(raw, 0.0)), np.ma.filled(raw, 0.0), 0.0))
        if f_rem.shape != a_rem.shape:
            sys.exit(f"conserve_remapped_flux.py: {args.remapped} {f_rem.shape} and "
                     f"{args.remapped_area} {a_rem.shape} differ in shape")

        n = (args.target_integral if args.target_integral is not None
             else float(np.sum(f_nat * a_nat)))
        r = float(np.sum(f_rem * a_rem))
        w = float(np.sum(np.abs(f_rem) * a_rem))
        rel = (r - n) / abs(n) if n != 0 else 0.0
        label = "target" if args.target_integral is not None else "native"
        print(f" * {args.varname}: {label} {n:.6e} kg/s, field {r:.6e} kg/s "
              f"({100 * rel:+.3f} %)")
        if abs(100 * rel) > args.warn_percent:
            print(f" !!! WARNING: correction of {100 * rel:+.1f} % (more than"
                  f" {args.warn_percent:g} %) in {args.remapped}.")
            if args.target_integral is None:
                print(" !!!   The total is fixed, the pattern is not: the remapping was"
                      " probably bilinear because the ice grid's ice.griddes has no"
                      " cell corners (see 'using remapbil' above, and"
                      " ice_griddes_ensure_corners).")
            else:
                print(" !!!   The thickness difference and PISM's budget disagree: the"
                      " chunk's input file is probably not the state PISM started"
                      " from (regridding, bootstrapping, a restart from elsewhere).")
        if w == 0.0:
            if n != 0.0:
                print(" ! the remapped field is zero everywhere, the native one is not;"
                      " left unchanged")
            return
        f_new = f_rem + (n - r) * np.abs(f_rem) / w
        var[:] = f_new.reshape(var.shape)
        var.setncattr(f"conserved_to_{label}_integral_kg_s", n)
        check = float(np.sum(f_new * a_rem))
        print(f" * corrected to {check:.6e} kg/s ({label} integral)")


if __name__ == "__main__":
    main(sys.argv[1:])
