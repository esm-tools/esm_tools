#!/usr/bin/env python3
"""Add zero-valued FESOM2 tracer restart files to an existing oce restart directory.

FESOM registers every tracer as a required restart variable (io_restart.F90,
ini_ocean_io: def_node_var), so a run that switches on a new tracer, e.g. the
transient tracers SF6 and CFC-12 (namelist.config use_transit, namelist.transit
l_sf6 / l_f12), aborts when it restarts from a directory written without it.

This script writes <tracer>.nc (and by default <tracer>_M1.nc, optionally
<tracer>_carry.nc) next to the existing files, as copies of the structure of a
template tracer (temp by default): same netCDF format, dimensions, chunking,
time and iter records, with the tracer variable set to zero everywhere.

The tracer names are the ones io_restart.F90 uses for the tracer id:
sf6 (6), cfc11 (11), cfc12 (12). The _M1 field (Adams-Bashforth) and the
_carry field are optional on read; _M1 is written anyway so that the first
step after the restart sees a consistent zero state.

Existing files are never overwritten unless --force is given.

Example, for the leg that starts on 2850-01-01:

    python fesom_add_zero_tracer_restart.py \\
        <exp>/restart/fesom/fesom.2850-01-01.oce.restart sf6 cfc12
"""
import argparse
import os
import sys

import netCDF4
import numpy as np

# trname -> (long name, units) as in io_restart.F90 ini_ocean_io
TRACERS = {
    "sf6": ("sulfur hexafluoride", "mol / m**3"),
    "cfc11": ("chlorofluorocarbon CFC-11", "mol / m**3"),
    "cfc12": ("chlorofluorocarbon CFC-12", "mol / m**3"),
}


def variants(name, long_name, units, with_m1, with_carry):
    yield "", name, long_name, units
    if with_m1:
        yield "_M1", name + "_M1", long_name + ", Adams-Bashforth", units
    if with_carry:
        yield "_carry", name + "_carry", "rounding carry of " + name + " times layer thickness", units + " m"


def write_zero_copy(src_path, src_var, dst_path, dst_var, long_name, units):
    with netCDF4.Dataset(src_path) as src:
        tmp_path = dst_path + ".tmp"
        with netCDF4.Dataset(tmp_path, "w", format=src.data_model) as dst:
            dst.setncatts({k: src.getncattr(k) for k in src.ncattrs()})
            for dim in src.dimensions.values():
                dst.createDimension(dim.name, None if dim.isunlimited() else len(dim))
            for var in src.variables.values():
                name = dst_var if var.name == src_var else var.name
                chunks = var.chunking()
                kwargs = {}
                if chunks not in (None, "contiguous"):
                    kwargs["chunksizes"] = chunks
                filters = var.filters() or {}
                if filters.get("zlib"):
                    kwargs.update(zlib=True, complevel=filters.get("complevel", 4), shuffle=filters.get("shuffle", False))
                fill = var.getncattr("_FillValue") if "_FillValue" in var.ncattrs() else None
                out = dst.createVariable(name, var.dtype, var.dimensions, fill_value=fill, **kwargs)
                attrs = {k: var.getncattr(k) for k in var.ncattrs() if k != "_FillValue"}
                if var.name == src_var:
                    attrs.update(units=units, long_name=long_name)
                    out.setncatts(attrs)
                    # one record at a time keeps the memory to one 3D field
                    for rec in range(var.shape[0]):
                        out[rec] = np.zeros(var.shape[1:], dtype=var.dtype)
                else:
                    out.setncatts(attrs)
                    out[:] = var[:]
    os.replace(tmp_path, dst_path)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("restart_dir", help="FESOM oce restart directory, e.g. fesom.2850-01-01.oce.restart")
    ap.add_argument("tracers", nargs="*", default=["sf6", "cfc12"], help="tracer restart names (default: sf6 cfc12)")
    ap.add_argument("--template", default="temp", help="existing tracer to copy the structure from (default: temp)")
    ap.add_argument("--no-m1", dest="m1", action="store_false", help="do not write <tracer>_M1.nc")
    ap.add_argument("--carry", action="store_true", help="also write <tracer>_carry.nc")
    ap.add_argument("--force", action="store_true", help="overwrite existing tracer files")
    ap.add_argument("--dry-run", action="store_true", help="only print what would be written")
    args = ap.parse_args()

    rdir = args.restart_dir
    if not os.path.isdir(rdir):
        sys.exit(f"not a directory: {rdir}")
    jobs = []
    for tracer in args.tracers:
        if tracer not in TRACERS:
            sys.exit(f"unknown tracer {tracer}; known: {', '.join(TRACERS)}")
        long_name, units = TRACERS[tracer]
        for suffix, dst_var, ln, un in variants(tracer, long_name, units, args.m1, args.carry):
            src_var = args.template + suffix
            src_path = os.path.join(rdir, src_var + ".nc")
            dst_path = os.path.join(rdir, dst_var + ".nc")
            if not os.path.isfile(src_path):
                sys.exit(f"template file missing: {src_path}")
            if os.path.exists(dst_path) and not args.force:
                sys.exit(f"exists, not overwritten (use --force): {dst_path}")
            jobs.append((src_path, src_var, dst_path, dst_var, ln, un))
    for src_path, src_var, dst_path, dst_var, ln, un in jobs:
        print(f"{'would write' if args.dry_run else 'writing'} {dst_path} (structure of {os.path.basename(src_path)})")
        if not args.dry_run:
            write_zero_copy(src_path, src_var, dst_path, dst_var, ln, un)


if __name__ == "__main__":
    main()
