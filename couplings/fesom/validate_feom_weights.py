#!/usr/bin/env python
"""Validate freshly generated feom exchange weights against FESOM's RUNTIME node
ordering. Cheap (seconds) and decisive: apply the weights to the runtime-ordered
node LATITUDES and check the result lands at the right latitudes on the other
grid. A nod2d-ordered weight file fails this loudly (median error ~60 deg); a
correct one passes at interpolation accuracy (~0.03 deg).

This is the check that would have caught the tile-translocated exchange (OIFS's
jigsaw ocean) on the day the offline weight engine was introduced. Non-zero exit
on failure -> the caller treats it as a coupling failure (couple_fail).

Usage: validate_feom_weights.py <oasis_dir> <mesh_dir> <nproc>
"""
import sys
import glob
import numpy as np
import netCDF4

TOL_DEG = 1.0


def runtime_lat_lon(mesh_dir, nproc):
    vals = np.array(open(f"{mesh_dir}/dist_{nproc}/rpart.out").read().split(),
                    dtype=int)
    npes = vals[0]
    L = vals[1 + npes:]
    perm = np.argsort(L, kind="stable")
    ao = np.loadtxt(f"{mesh_dir}/nod2d.out", skiprows=1)
    return ao[perm, 2], ao[perm, 1], len(L)


def apply_weights(W, src_field, ndst):
    sa = np.asarray(W["src_address"][:]).astype(int) - 1
    da = np.asarray(W["dst_address"][:]).astype(int) - 1
    wm = np.squeeze(np.asarray(W["remap_matrix"][:]))
    acc = np.zeros(ndst)
    ws = np.zeros(ndst)
    np.add.at(acc, da, wm * src_field[sa])
    np.add.at(ws, da, wm)
    ok = ws > 1e-6
    out = np.full(ndst, np.nan)
    out[ok] = acc[ok] / ws[ok]
    return out, ok


def main():
    oasis_dir, mesh_dir, nproc = sys.argv[1], sys.argv[2], sys.argv[3]
    flat, flon, nfeom = runtime_lat_lon(mesh_dir, nproc)
    failures = []
    checked = 0
    for wf in sorted(glob.glob(f"{oasis_dir}/rmp_*.nc")):
        W = netCDF4.Dataset(wf)
        sdim = int(np.asarray(W["src_grid_dims"][:]).ravel()[0])
        ddim = int(np.asarray(W["dst_grid_dims"][:]).ravel()[0])
        name = wf.split("/")[-1]
        if sdim == nfeom:                      # feom -> X : remap runtime lat
            dlat = np.asarray(W["dst_grid_center_lat"][:]).ravel()
            if np.abs(dlat).max() < 7:
                dlat = np.degrees(dlat)
            got, ok = apply_weights(W, flat, len(dlat))
            err = np.nanmedian(np.abs(got[ok] - dlat[ok]))
            checked += 1
        elif ddim == nfeom:                    # X -> feom : compare dst coords
            slat = np.asarray(W["src_grid_center_lat"][:]).ravel()
            if np.abs(slat).max() < 7:
                slat = np.degrees(slat)
            got, ok = apply_weights(W, slat, nfeom)
            err = np.nanmedian(np.abs(got[ok] - flat[ok]))
            checked += 1
        else:
            continue
        status = "OK" if err < TOL_DEG else "FAIL"
        print(f"  {name}: median lat error {err:.3f} deg  [{status}]")
        if err >= TOL_DEG:
            failures.append(name)
    if checked == 0:
        sys.exit("validate_feom_weights: no feom weight files found -- refusing to pass")
    if failures:
        sys.exit("validate_feom_weights: FEOM WEIGHTS DO NOT MATCH FESOM'S RUNTIME "
                 f"NODE ORDERING: {failures}. The coupled exchange would be "
                 "geographically scrambled (jigsaw ocean). Aborting.")
    print(f"validate_feom_weights: OK ({checked} weight files consistent with runtime ordering)")


if __name__ == "__main__":
    main()
