#!/usr/bin/env python3
"""
Interpolate ICON model-level ("ml") 3D output onto pressure levels.

The ICON *_atm_3d_ml_daily_*.nc files already contain the full 3D
pressure field (variable "pres", dims time x height x ncells), so no
vertical coordinate needs to be reconstructed from hyai/hybi/surface
pressure. This script therefore applies the same linear-in-pressure
vertical interpolation used and validated in pyicon
(pyicon.calc_vertical_interp_weights, see pyicon/pyicon/pyicon_tb.py),
directly to that pressure field:

  - for each target pressure level, find the two bracketing model
    (full) levels for every column,
  - linearly interpolate in pressure between them,
  - mask (NaN) target levels that lie above the model top or below
    the surface for a given column.

Weights are computed once per time step (from "pres") and re-used for
every 3D variable in that time step, which is the expensive-but-shared
part of the interpolation -- this is what pyicon does internally, and
is what makes this efficient.

One output file per input file is written (streamed, one time step at
a time, so peak memory stays at a few (nlev, ncells) / (nplev, ncells)
arrays regardless of how many time steps or input files there are).
The output is first written to <outfile>.tmp and renamed only after the
interpolation finished, so an existing output file is always complete.

What happens to the *_ml_* input file afterwards (--ml-files):
  move    (default) move it into a "3d_ml" subdirectory of --path
  remove  delete it
  keep    leave it in place (same as the old --keep-ml-files)
An input file is only moved or removed after the output file passed a
check against it (same time axis, all interpolated variables present
with the full shape, finite values in the last time step). This check
also applies to output files that already existed (skipped). If any
file fails, its input file is kept and the script exits with status 1.

--dry-run only reads: it lists what would be interpolated, checks
existing output files and reports what would be moved or removed.

--start-date / --end-date (YYYYMMDD, inclusive) select input files by
the date in their name (<...>_YYYYMMDDThhmmssZ.nc). ESM-Tools uses this
to process only the files of the run that just finished, not those of
a later run that may already be in the outdata directory.

Usage
-----
  python interp_to_pressure_levels.py \\
      --path /path/to/data \\
      --pattern 'icon_atm_3d_ml_daily_1850010*T000000Z.nc' \\
      --outdir /path/to/output

  # custom pressure levels [Pa], comma-separated
  python interp_to_pressure_levels.py --plevels 100000,85000,50000,10000

  # see what removing the model-level files would do, without changing anything
  python interp_to_pressure_levels.py --path ... --ml-files remove --dry-run
"""
import argparse
import glob
import os
import re
import shutil
import sys

import numpy as np
from netCDF4 import Dataset

# standard 26 pressure levels [Pa]
DEFAULT_PLEVELS = [
    100000, 92500, 85000, 77500, 70000, 60000, 50000, 40000, 30000,
    25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000,
    700, 500, 300, 200, 100, 50, 20, 10,
]

FILE_DATE_RE = re.compile(r'_(\d{8})T\d{6}Z\.nc$')


def calc_vertical_interp_weights(zdata, levs):
    """Vertical interpolation weights (algorithm from pyicon.pyicon_tb).

    Parameters
    ----------
    zdata : ndarray, shape (nlev, ncells)
        Pressure [Pa] at model full levels, increasing with level
        index (index 0 = model top, index -1 = surface), as written
        out by ICON.
    levs : ndarray, shape (nplev,)
        Target pressure levels [Pa].

    Returns
    -------
    icall, ind_lev, fac such that for any variable `data` with the
    same shape as `zdata`:

        data_i = data[ind_lev, icall] * fac + data[ind_lev + 1, icall] * (1 - fac)

    is `data` linearly interpolated (in pressure) to `levs`. `fac` is
    a masked array, masked wherever a target level is out of the
    model's pressure range for that column (above model top or below
    the surface).
    """
    nlev, ncells = zdata.shape
    nplev = levs.size
    icall = np.arange(ncells, dtype=int)[np.newaxis, :]
    ind_lev = np.zeros((nplev, ncells), dtype=int)
    fac = np.ma.zeros((nplev, ncells))
    for k in range(nplev):
        ind_lev[k, :] = (zdata < levs[k]).sum(axis=0) - 1
        ind_lev[k, ind_lev[k, :] == (nlev - 1)] = -1
        zd1 = zdata[ind_lev[k, :], icall[0]]
        zd2 = zdata[ind_lev[k, :] + 1, icall[0]]
        fac[k, :] = (0. - 1.) / (zd2 - zd1) * (levs[k] - zd1) + 1.
    fac[ind_lev == -1] = np.ma.masked
    return icall, ind_lev, fac


def apply_weights(data, icall, ind_lev, fac):
    """Apply precomputed weights to one (nlev, ncells) variable slice."""
    di = data[ind_lev, icall] * fac + data[ind_lev + 1, icall] * (1. - fac)
    return np.ma.filled(di.astype(np.float32), np.nan)


def make_outfile_name(infile, outdir):
    """icon_atm_3d_ml_daily_*.nc -> ..._pl_daily_*.nc

    If "_ml_" is not present in the file name, "_pl" is inserted
    before the ".nc" extension instead.
    """
    base = os.path.basename(infile)
    if '_ml_' in base:
        out_base = base.replace('_ml_', '_pl_')
    else:
        stem, ext = os.path.splitext(base)
        out_base = f'{stem}_pl{ext}'
    return os.path.join(outdir, out_base)


def move_ml_file(infile, path, dry_run=False, ml_subdir='3d_ml'):
    """Move a successfully-processed *_ml_* input file into <path>/<ml_subdir>."""
    dest_dir = os.path.join(path, ml_subdir)
    dest = os.path.join(dest_dir, os.path.basename(infile))
    if os.path.exists(dest):
        print(f'  [move skipped: {dest} already exists]')
        return
    if dry_run:
        print(f'  [dry run] would move -> {dest}')
        return
    os.makedirs(dest_dir, exist_ok=True)
    shutil.move(infile, dest)
    print(f'  moved -> {dest}')


def remove_ml_file(infile, outfile, dry_run=False):
    """Delete a successfully-processed *_ml_* input file."""
    if os.path.realpath(infile) == os.path.realpath(outfile):
        raise RuntimeError(f'refusing to remove {infile}: it is the output file')
    if dry_run:
        print(f'  [dry run] would remove {infile}')
        return
    os.remove(infile)
    print(f'  removed {infile}')


def file_date(path):
    """YYYYMMDD from <...>_YYYYMMDDThhmmssZ.nc, or None."""
    m = FILE_DATE_RE.search(os.path.basename(path))
    return m.group(1) if m else None


def find_files(path, pattern, start_date=None, end_date=None):
    files = sorted(glob.glob(os.path.join(path, pattern)))
    # never take our own output as input (e.g. a pattern like '*daily*')
    files = [f for f in files if '_pl_' not in os.path.basename(f)
             and not f.endswith('.tmp')]
    if start_date or end_date:
        selected = []
        for f in files:
            d = file_date(f)
            if d is None:
                print(f'{f}\n  [skip: no _YYYYMMDDThhmmssZ.nc date in the file name]')
                continue
            if start_date and d < start_date:
                continue
            if end_date and d > end_date:
                continue
            selected.append(f)
        files = selected
    if not files:
        raise FileNotFoundError(
            f"No files matching pattern {pattern!r} found in {path!r}"
            + (f" with dates {start_date or '...'} to {end_date or '...'}"
               if (start_date or end_date) else "")
        )
    return files


def copy_var_attrs(src_var, dst_var, skip=('_FillValue',)):
    for name in src_var.ncattrs():
        if name in skip:
            continue
        dst_var.setncattr(name, src_var.getncattr(name))


def find_vert_vars(src, pres_name, exclude):
    """Variables with exactly the dims of the pressure field (minus pres itself).

    This correctly excludes bounds variables like height_bnds.
    """
    if pres_name not in src.variables:
        raise KeyError(f"Pressure variable {pres_name!r} not found in {src.filepath()}")
    pres_dims = src.variables[pres_name].dimensions
    return [name for name, var in src.variables.items()
            if var.dimensions == pres_dims and name != pres_name and name not in exclude]


def interp_file(infile, outfile, plevels, pres_name, vert_dim, exclude, complevel):
    print(f'{infile}\n  -> {outfile}')
    tmpfile = outfile + '.tmp'
    try:
        with Dataset(infile, 'r') as src, Dataset(tmpfile, 'w', format='NETCDF4') as dst:
            # --- figure out which variables live on the vertical model-level
            #     grid and need interpolating, vs. which can be copied as-is
            vert_vars = find_vert_vars(src, pres_name, exclude)

            # --- copy global attributes
            dst.setncatts({a: src.getncattr(a) for a in src.ncattrs()})
            dst.setncattr(
                'history_vertical_interp',
                f'Interpolated from model levels to pressure levels using '
                f'the pyicon calc_vertical_interp_weights algorithm '
                f'(linear-in-pressure), variable {pres_name!r}.'
            )

            # --- copy all dimensions except the vertical model-level one,
            #     which is replaced by "plev"
            for name, dim in src.dimensions.items():
                if name == vert_dim:
                    continue
                dst.createDimension(name, (None if dim.isunlimited() else len(dim)))
            dst.createDimension('plev', len(plevels))

            # --- copy all variables that do not depend on the vertical
            #     model-level dimension (coordinates, bounds, time, ...)
            for name, var in src.variables.items():
                if vert_dim in var.dimensions:
                    continue
                dst_var = dst.createVariable(
                    name, var.dtype, var.dimensions,
                    zlib=complevel > 0, complevel=complevel,
                )
                copy_var_attrs(var, dst_var)
                dst_var[:] = var[:]

            # --- create the plev coordinate variable
            plev_var = dst.createVariable('plev', 'f8', ('plev',))
            plev_var[:] = plevels
            plev_var.standard_name = 'air_pressure'
            plev_var.long_name = 'pressure'
            plev_var.units = 'Pa'
            plev_var.positive = 'down'
            plev_var.axis = 'Z'

            # --- create the interpolated variables
            out_vars = {}
            for name in vert_vars:
                src_var = src.variables[name]
                dims = tuple('plev' if d == vert_dim else d for d in src_var.dimensions)
                dst_var = dst.createVariable(
                    name, 'f4', dims,
                    zlib=complevel > 0, complevel=complevel,
                    fill_value=np.nan,
                )
                copy_var_attrs(src_var, dst_var)
                out_vars[name] = dst_var

            # --- stream over time steps: compute interpolation weights once
            #     per time step from "pres", re-use for every 3D variable
            pres_var = src.variables[pres_name]
            ntime = src.dimensions['time'].size if 'time' in src.dimensions else 1
            has_time = 'time' in pres_var.dimensions

            for it in range(ntime):
                pres_t = pres_var[it, :, :] if has_time else pres_var[:, :]
                pres_t = np.ma.filled(pres_t, np.nan).astype(np.float64)
                icall, ind_lev, fac = calc_vertical_interp_weights(pres_t, plevels)

                for name in vert_vars:
                    src_var = src.variables[name]
                    data_t = src_var[it, :, :] if has_time else src_var[:, :]
                    data_t = np.ma.filled(data_t, np.nan).astype(np.float64)
                    result = apply_weights(data_t, icall, ind_lev, fac)
                    if has_time:
                        out_vars[name][it, :, :] = result
                    else:
                        out_vars[name][:, :] = result

                print(f'  time step {it + 1}/{ntime} done')
    except BaseException:
        if os.path.exists(tmpfile):
            os.remove(tmpfile)
        raise
    os.replace(tmpfile, outfile)


def check_output(infile, outfile, pres_name, vert_dim, exclude):
    """Return a list of problems of outfile compared to infile (empty = OK)."""
    problems = []
    try:
        with Dataset(infile, 'r') as src, Dataset(outfile, 'r') as dst:
            vert_vars = find_vert_vars(src, pres_name, exclude)
            if 'plev' not in dst.dimensions:
                return ['no plev dimension']
            nplev = len(dst.dimensions['plev'])
            has_time = 'time' in src.variables[pres_name].dimensions
            if has_time:
                ntime = len(src.dimensions['time'])
                if 'time' not in dst.dimensions or len(dst.dimensions['time']) != ntime:
                    return [f'time length differs from the input ({ntime})']
                if 'time' in src.variables and not np.array_equal(
                        src.variables['time'][:], dst.variables['time'][:]):
                    problems.append('time values differ from the input')
            for name in vert_vars:
                if name not in dst.variables:
                    problems.append(f'{name} missing')
                    continue
                src_shape = src.variables[name].shape
                src_dims = src.variables[name].dimensions
                expect = tuple(nplev if d == vert_dim else n
                               for d, n in zip(src_dims, src_shape))
                var = dst.variables[name]
                if var.shape != expect:
                    problems.append(f'{name} shape {var.shape}, expected {expect}')
                    continue
                last = var[-1] if has_time else var[:]
                if not np.isfinite(np.ma.filled(last, np.nan)).any():
                    problems.append(f'{name}: no finite values in the last time step')
    except Exception as e:  # unreadable or truncated file
        problems.append(f'cannot read: {e}')
    return problems


def handle_ml_file(infile, outfile, args, exclude):
    """Check outfile and then move, remove or keep infile. Returns True if OK."""
    if args.ml_files == 'keep':
        return True
    problems = check_output(infile, outfile, args.pres_var, args.vert_dim, exclude)
    if problems:
        print(f'  [ERROR: check of {outfile} failed, keeping {infile}: '
              + '; '.join(problems) + ']')
        return False
    print('  output check OK')
    if args.ml_files == 'move':
        move_ml_file(infile, args.path, dry_run=args.dry_run)
    elif args.ml_files == 'remove':
        remove_ml_file(infile, outfile, dry_run=args.dry_run)
    return True


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument('--path', default='.',
                         help='Directory containing the input files (default: current dir)')
    parser.add_argument('--pattern', default='icon_atm_3d_ml_daily_*T000000Z.nc',
                         help='Glob pattern (relative to --path) matching input files')
    parser.add_argument('--outdir', default=None,
                         help='Output directory (default: same as --path)')
    parser.add_argument('--start-date', default=None,
                         help='Only process files dated YYYYMMDD or later (date in the file name)')
    parser.add_argument('--end-date', default=None,
                         help='Only process files dated YYYYMMDD or earlier (date in the file name)')
    parser.add_argument('--force', action='store_true',
                         help='Overwrite output files that already exist '
                              '(default: skip them)')
    parser.add_argument('--ml-files', choices=('move', 'remove', 'keep'), default='move',
                         help='What to do with a *_ml_* input file after its output '
                              'file passed the check: move it into a "3d_ml" '
                              'subdirectory of --path (default), remove it, or keep it')
    parser.add_argument('--keep-ml-files', action='store_true',
                         help='Same as --ml-files keep')
    parser.add_argument('--missing-ok', action='store_true',
                         help='Exit with status 0 if no input file matches (e.g. monthly '
                              'output of a run that ends within a month)')
    parser.add_argument('--dry-run', action='store_true',
                         help='Do not write, move or remove anything; only report '
                              'what would be done (existing output files are checked)')
    parser.add_argument('--plevels', default=None,
                         help='Comma-separated target pressure levels [Pa] '
                              '(default: standard 26 levels 100000..10)')
    parser.add_argument('--pres-var', default='pres', dest='pres_var',
                         help='Name of the 3D pressure variable [Pa] (default: pres)')
    parser.add_argument('--vert-dim', default='height', dest='vert_dim',
                         help='Name of the model-level vertical dimension (default: height)')
    parser.add_argument('--exclude', default='',
                         help='Comma-separated extra variable names to skip')
    parser.add_argument('--complevel', type=int, default=4,
                         help='zlib compression level for output (0 = off, default: 4)')
    args = parser.parse_args()
    if args.keep_ml_files:
        args.ml_files = 'keep'
    for name in ('start_date', 'end_date'):
        value = getattr(args, name)
        if value is not None and not re.fullmatch(r'\d{8}', value):
            parser.error(f'--{name.replace("_", "-")} must be YYYYMMDD, got {value!r}')

    plevels = np.array(
        DEFAULT_PLEVELS if args.plevels is None
        else [float(x) for x in args.plevels.split(',')],
        dtype=np.float64,
    )
    exclude = {x.strip() for x in args.exclude.split(',') if x.strip()}
    outdir = args.outdir or args.path
    if not args.dry_run:
        os.makedirs(outdir, exist_ok=True)

    try:
        files = find_files(args.path, args.pattern, args.start_date, args.end_date)
    except FileNotFoundError as e:
        if not args.missing_ok:
            raise
        print(f'{e}: nothing to do (--missing-ok)')
        return
    print(f'Found {len(files)} input file(s) for pattern {args.pattern!r} in {args.path!r}'
          f' (ml files: {args.ml_files}{", DRY RUN" if args.dry_run else ""})')

    failed = []
    for infile in files:
        outfile = make_outfile_name(infile, outdir)
        if os.path.exists(outfile) and not args.force:
            print(f'{infile}\n  -> {outfile}  [skip: already exists, use --force to overwrite]')
        elif args.dry_run:
            print(f'{infile}\n  -> {outfile}  [dry run: would interpolate]')
            if args.ml_files != 'keep':
                print(f'  [dry run] would then {args.ml_files} {infile} '
                      f'if the output check passes')
            continue
        else:
            try:
                interp_file(infile, outfile, plevels, args.pres_var, args.vert_dim,
                            exclude, args.complevel)
            except Exception as e:
                print(f'  [ERROR: interpolation failed, keeping {infile}: {e}]')
                failed.append(infile)
                continue
        if not handle_ml_file(infile, outfile, args, exclude):
            failed.append(infile)

    if failed:
        print(f'Done with errors in {len(failed)} file(s), their input files were kept:')
        for f in failed:
            print(f'  {f}')
        sys.exit(1)
    print('Done.')


if __name__ == '__main__':
    main()
