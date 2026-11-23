#!/usr/bin/env python3
"""Take a regenerated JSBACH init file only where the glacier mask changed.

ice2echam regenerates the JSBACH init file from the pool's present-day maps
(jsbach_init_file, cover types of one reference year) and the new glacier
mask. Used as a whole, that file also replaces the land surface everywhere
else, which in a paleo run means present-day cover types, soils and lakes
outside the ice sheets. This script keeps the current init file and takes
the regenerated values only on the cells whose glacier mask changed, where
the land surface has to follow the ice sheet.

Usage:
    jsbach_merge_changed_cells.py CURRENT REGENERATED OUT [--mask-var glac]

Every variable whose last two dimensions are (lat, lon) is taken from
REGENERATED on the changed cells (all tiles, layers and months); everything
else comes from CURRENT. OUT may not be one of the inputs.
"""

import argparse
import shutil
import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:  # pragma: no cover
    sys.exit("jsbach_merge_changed_cells.py: needs netCDF4 (module load python3)")


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("current")
    parser.add_argument("regenerated")
    parser.add_argument("out")
    parser.add_argument("--mask-var", default="glac")
    args = parser.parse_args(argv)

    shutil.copyfile(args.current, args.out)
    with Dataset(args.regenerated) as new, Dataset(args.out, "r+") as out:
        for ds, name in ((new, args.regenerated), (out, args.current)):
            if args.mask_var not in ds.variables:
                sys.exit(f" !!! no {args.mask_var} in {name}")
        old_glac = np.ma.filled(out[args.mask_var][:], 0.0) > 0.5
        new_glac = np.ma.filled(new[args.mask_var][:], 0.0) > 0.5
        if old_glac.shape != new_glac.shape:
            sys.exit(f" !!! {args.mask_var}: {old_glac.shape} in {args.current},"
                     f" {new_glac.shape} in {args.regenerated} (different grids)")
        changed = old_glac != new_glac
        spatial = out[args.mask_var].dimensions[-2:]
        taken = []
        for name, var in out.variables.items():
            if var.dimensions[-2:] != spatial or name not in new.variables:
                continue
            if new[name].shape != var.shape:
                sys.exit(f" !!! {name}: shape {var.shape} in {args.current},"
                         f" {new[name].shape} in {args.regenerated}")
            if not changed.any():
                continue
            data = var[:]
            data[..., changed] = new[name][:][..., changed]
            var[:] = data
            taken.append(name)
        out.setncattr("glacier_update",
                      f"{int(changed.sum())} cells with a changed {args.mask_var} taken from "
                      f"{args.regenerated}, the rest from {args.current}")
    print(f" * {int(changed.sum())} cells changed {args.mask_var} "
          f"({int((~old_glac & new_glac).sum())} new glacier, "
          f"{int((old_glac & ~new_glac).sum())} deglaciated); "
          f"{len(taken)} variables updated there")


if __name__ == "__main__":
    main(sys.argv[1:])
