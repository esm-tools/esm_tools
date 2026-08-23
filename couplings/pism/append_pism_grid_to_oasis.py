#!/usr/bin/env python3
"""Append a PISM OASIS grid into an existing grids/masks/areas set.

ocp-tool writes the ice grid itself for the per-leg regeneration
(oasis_writer._append_pism_grid_to_oasis_files). This is for the pools that
ocp-tool does not produce: the chunk-1 bootstrap set and the fixed-mesh pool.

Idempotent: a grid already present is left alone.

Usage:
  append_pism_grid_to_oasis.py <reference_dir> <target_dir> [grid_name]

    reference_dir   holds pism_grids.nc / pism_masks.nc / pism_areas.nc
    target_dir      holds grids.nc / masks.nc / areas.nc to append into
"""
import os
import sys

import netCDF4 as nc

FILES = (("pism_grids.nc", "grids.nc", ("lon", "lat", "clo", "cla")),
         ("pism_masks.nc", "masks.nc", ("msk",)),
         ("pism_areas.nc", "areas.nc", ("srf",)))


def append(ref_path, tgt_path, name, suffixes):
    src = nc.Dataset(ref_path)
    dst = nc.Dataset(tgt_path, "a")
    added = []
    try:
        for suf in suffixes:
            vn = f"{name}.{suf}"
            if vn not in src.variables:
                raise SystemExit(f"append: {vn} missing from {ref_path}")
            if vn in dst.variables:
                continue
            svar = src.variables[vn]
            for d in svar.dimensions:
                if d not in dst.dimensions:
                    dst.createDimension(d, src.dimensions[d].size)
            dvar = dst.createVariable(vn, svar.dtype, svar.dimensions)
            for a in svar.ncattrs():
                dvar.setncattr(a, svar.getncattr(a))
            dvar[:] = svar[:]
            added.append(vn)
    finally:
        src.close()
        dst.close()
    return added


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    ref, tgt = sys.argv[1], sys.argv[2]
    name = sys.argv[3] if len(sys.argv) > 3 else "ismp"

    for ref_file, tgt_file, suffixes in FILES:
        rp, tp = os.path.join(ref, ref_file), os.path.join(tgt, tgt_file)
        if not os.path.exists(tp):
            raise SystemExit(f"append: {tp} does not exist")
        added = append(rp, tp, name, suffixes)
        print(f"  {tgt_file:9s} added: {', '.join(added) if added else '(already present)'}")


if __name__ == "__main__":
    main()
