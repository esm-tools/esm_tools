#!/usr/bin/env python3
"""Generate the iceberg set for one leg from the ice sheet's discharge.

Runs under the ocp-tool conda python, not the one esm_runscripts uses, the same
way the OASIS weight regeneration does. The generator pulls in pyfesom2, which
pulls pyresample, pyproj and cmocean, and that stack does not import in the
spack python: libproj needs a newer libstdc++ than /lib64 provides, and cmocean
calls matplotlib.cm.register_cmap, gone since matplotlib 3.9.

The upstream fesom_icb_pism plugin does this work in-process instead. That is
why we do not use it: it registers an esm_tools.plugins entry point, and
esm_plugin_manager imports every registered plugin at startup, so installing it
drags pyfesom2 into every esm_runscripts call whether or not icebergs are on.
ocp-tool avoids that by registering no entry point and being called as a
subprocess, which is what this does.

Writes icb_{longitude,latitude,length,height,scaling,felem}.dat into <icb_dir>.
Counting them into namelist.config icebergs%ib_num is a separate, light step.

Usage:
  make_icebergs.py <discharge.nc> <mesh_dir> <icb_dir> <basin_file> \
                   [--restart FILE] [--seed N] [--cavities 0|1] [--ibareamax N] \
                   [--scaling "1,1,1,1,1,1"]
"""
import argparse
import os
import sys


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("discharge", help="latest_discharge.nc from the ice sheet")
    p.add_argument("mesh_dir", help="FESOM mesh of THIS leg (elem2d.out lives here)")
    p.add_argument("icb_dir", help="where the icb_*.dat files go")
    p.add_argument("basin_file", help="ice sheet drainage basins")
    p.add_argument("--restart", default="", help="icb_restart_ISM from the previous leg")
    p.add_argument("--seed", type=int, default=0)
    p.add_argument("--cavities", type=int, default=1)
    p.add_argument("--ibareamax", type=float, default=400)
    p.add_argument("--scaling", default="1,1,1,1,1,1")
    args = p.parse_args(argv)

    for path, what in ((args.discharge, "discharge file"),
                       (args.mesh_dir, "mesh dir"),
                       (args.basin_file, "basin file")):
        if not os.path.exists(path):
            sys.exit(f"make_icebergs: missing {what}: {path}")

    from fesom_icb_pism.icb_apply_distribution_functions import IcebergCalving

    os.makedirs(args.icb_dir, exist_ok=True)
    ib = IcebergCalving(
        args.discharge,
        args.mesh_dir,
        args.icb_dir,
        args.basin_file,
        args.restart,
        scaling_factor=[float(s) for s in args.scaling.split(",")],
        seed=args.seed,
        bcavities=bool(args.cavities),
        ibareamax=args.ibareamax,
    )
    ib.create_dataframe()
    ib._icb_generator(fmode="w")

    lon = os.path.join(args.icb_dir, "icb_longitude.dat")
    n = sum(1 for _ in open(lon)) if os.path.exists(lon) else 0

    # icb_calving_day.dat, the day of year each berg leaves the front. FESOM
    # opens it status='old' and reads ib_num values, so it has to exist and be
    # as long as the rest, and the generator does not write it. Spread over the
    # year rather than all on day 1: PISM gives an annual discharge, and calving
    # the whole of it in one timestep is a freshwater pulse the ocean would feel.
    day = os.path.join(args.icb_dir, "icb_calving_day.dat")
    with open(day, "w") as fh:
        for i in range(n):
            fh.write(f"{1.0 + 364.0 * i / max(n - 1, 1):.4f}\n")

    print(f" *   generated {n} icebergs -> {args.icb_dir}")


if __name__ == "__main__":
    main()
