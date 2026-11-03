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

Two phases, for the PISM chain prebuilding the next leg (prebuild_couple_in):
  --prepare STATE     everything that needs only the discharge, the basins and the
                      mesh (~98% of the time: reading the discharge, the basin and
                      FESOM element search), saved to STATE; no restart, no bergs.
  --from-state STATE  the rest, which needs the previous leg's iceberg restart: the
                      full elements, the bergs themselves, the FESOM-rule filter.
The generator draws all its random numbers after create_dataframe, so reseeding
before it gives exactly the bergs the one-phase run gives.

Usage:
  make_icebergs.py <discharge.nc> <mesh_dir> <icb_dir> <basin_file> \
                   [--restart FILE] [--seed N] [--cavities 0|1] [--ibareamax N] \
                   [--scaling "1,1,1,1,1,1"] [--prepare STATE | --from-state STATE]
"""
import argparse
import os
import pickle
import sys
import types


def _save_state(ib, path):
    """The generator's inputs, without the open datasets and the full mesh."""
    state, skipped = {}, []
    for key, value in vars(ib).items():
        if key == "mesh":
            # _get_full_cells needs only the element areas
            value = types.SimpleNamespace(voltri=ib.mesh.voltri)
        try:
            pickle.dumps(value)
        except Exception:
            skipped.append(key)
            continue
        state[key] = value
    tmp = path + ".tmp"
    with open(tmp, "wb") as fh:
        pickle.dump(state, fh, protocol=pickle.HIGHEST_PROTOCOL)
    os.replace(tmp, path)
    print(f" *   iceberg state -> {path} (not kept: {', '.join(skipped) or 'nothing'})")


def _load_state(cls, path, icb_dir, restart):
    ib = cls.__new__(cls)
    with open(path, "rb") as fh:
        vars(ib).update(pickle.load(fh))
    ib.icb_path = icb_dir
    ib.latest_restart_file = restart
    # as IcebergCalving.__init__ does it
    if restart != "" and os.path.exists(restart):
        print(" * restart file ", restart)
        ib._get_full_cells()
    else:
        print("* no restart file found, continue anyway...")
        ib.full_elems = []
    return ib


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
    phase = p.add_mutually_exclusive_group()
    phase.add_argument("--prepare", metavar="STATE", default="",
                       help="stop after the restart-independent part, save it to STATE")
    phase.add_argument("--from-state", metavar="STATE", default="",
                       help="continue from a --prepare STATE")
    args = p.parse_args(argv)

    needed = [(args.mesh_dir, "mesh dir")]
    if args.from_state:
        needed.append((args.from_state, "iceberg state"))
    else:
        needed += [(args.discharge, "discharge file"), (args.basin_file, "basin file")]
    for path, what in needed:
        if not os.path.exists(path):
            sys.exit(f"make_icebergs: missing {what}: {path}")

    from fesom_icb_pism.icb_apply_distribution_functions import IcebergCalving

    os.makedirs(args.icb_dir, exist_ok=True)
    if args.from_state:
        ib = _load_state(IcebergCalving, args.from_state, args.icb_dir, args.restart)
        import numpy.random
        numpy.random.seed(args.seed)
    else:
        ib = IcebergCalving(
            args.discharge,
            args.mesh_dir,
            args.icb_dir,
            args.basin_file,
            "" if args.prepare else args.restart,
            scaling_factor=[float(s) for s in args.scaling.split(",")],
            seed=args.seed,
            bcavities=bool(args.cavities),
            ibareamax=args.ibareamax,
        )
        ib.create_dataframe()
        if args.prepare:
            _save_state(ib, args.prepare)
            return
    ib._icb_generator(fmode="w")

    # FESOM re-derives each berg's element from its lon/lat and refuses elements
    # whose nodes are all cavity or all boundary. The generator's own cavity
    # exclusion is a different test on a different object, so it can pass a berg
    # FESOM will reject, and a rejected berg hangs every rank rather than
    # aborting. Apply FESOM's rule here so the two agree.
    _filter = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "filter_icebergs_fesom_rule.py")
    if os.path.isfile(_filter):
        import runpy
        _argv = sys.argv
        sys.argv = [_filter, args.mesh_dir, args.icb_dir]
        try:
            runpy.run_path(_filter, run_name="__main__")
        finally:
            sys.argv = _argv
    else:
        print(f"make_icebergs: {_filter} missing, bergs not filtered")

    lon = os.path.join(args.icb_dir, "icb_longitude.dat")
    n = sum(1 for _ in open(lon)) if os.path.exists(lon) else 0

    # icb_calving_day.dat, the day of year each berg leaves the front. FESOM
    # opens it status='old' and reads ib_num values, so it has to exist and be
    # as long as the rest, and the generator does not write it. Spread over the
    # year rather than all on day 1: PISM gives an annual discharge, and calving
    # the whole of it in one timestep is a freshwater pulse the ocean would feel.
    #
    # The last day of the year is deliberately left free. FESOM calves on
    # istep > step_per_day*calving_day, so day 365 needs a step the year does not
    # have, and such a berg never calves. It still reaches the restart, carrying
    # an iceberg_elem that was never assigned, which makes the record unreadable.
    day = os.path.join(args.icb_dir, "icb_calving_day.dat")
    with open(day, "w") as fh:
        for i in range(n):
            fh.write(f"{1.0 + 363.0 * i / max(n - 1, 1):.4f}\n")

    # num_non_melted_icb_file: how many of the bergs FESOM reads are carried over
    # from the previous leg rather than newly calved. It is staged into work/ and
    # opened there, so it has to exist alongside the .dat files. The generator
    # does not write it either.
    carried = 0
    if args.restart and os.path.exists(args.restart):
        with open(args.restart) as fh:
            carried = sum(1 for line in fh if line.strip())
    with open(os.path.join(args.icb_dir, "num_non_melted_icb_file"), "w") as fh:
        fh.write(f"{carried}\n")

    print(f" *   generated {n} icebergs ({carried} carried over) -> {args.icb_dir}")


if __name__ == "__main__":
    main()
