#!/usr/bin/env python
"""Permute the feom.* arrays in OASIS grids/masks/areas.nc from nod2d.out order
into FESOM's RUNTIME exchange order.

WHY: FESOM registers its OASIS partition rank-contiguously (Apple partition,
cpl_driver.F90): the exchange field is the concatenation of each rank's owned
nodes in my_list order -- NOT nod2d.out order. The offline weight engine builds
the feom geometry from the mesh files (nod2d order); weights generated from that
geometry are applied to a permuted field at runtime and every A096<->feom
exchange is tile-translocated (OIFS saw a jigsaw ocean: Weddell ice fraction
0.04 while FESOM had 0.95; tropical SST tiles at 55S). The canonical pool
weights (TCO95-CORE3/1792/) are in runtime order because they were harvested
from a real coupled run -- which is also why the pool is organised per-nproc.

The permutation comes from dist_<nproc>/rpart.out, written by fesom_meshpart in
build_submesh BEFORE this runs, and read by FESOM itself at runtime from the
same mesh dir:  layout [npes, counts(npes), L(n2d)] where L[n] is the 1-based
runtime position of nod2d-node n;  runtime_field = nod2d_field[argsort(L)].

Idempotent: stamps `feom_node_order = "runtime"` as a global attribute and
refuses to permute twice.

Usage: permute_feom_to_runtime.py <oasis_dir> <mesh_dir> <nproc>
"""
import sys
import numpy as np
import netCDF4


def main():
    oasis_dir, mesh_dir, nproc = sys.argv[1], sys.argv[2], sys.argv[3]
    vals = np.array(open(f"{mesh_dir}/dist_{nproc}/rpart.out").read().split(),
                    dtype=int)
    npes = vals[0]
    if npes != int(nproc):
        sys.exit(f"permute_feom_to_runtime: rpart.out npes={npes} != nproc={nproc}")
    L = vals[1 + npes:]
    perm = np.argsort(L, kind="stable")
    n = len(L)
    total = 0
    already = 0
    for fn in ("grids.nc", "masks.nc", "areas.nc"):
        path = f"{oasis_dir}/{fn}"
        try:
            d = netCDF4.Dataset(path, "a")
        except OSError:
            continue
        if getattr(d, "feom_node_order", "") == "runtime":
            print(f"  {fn}: already runtime-ordered, skipping")
            d.close()
            already += 1
            continue
        k = 0
        for v in list(d.variables):
            if v.startswith("feom") and d[v].shape[-1] == n:
                d[v][:] = np.asarray(d[v][:])[..., perm]
                k += 1
        d.feom_node_order = "runtime"
        d.close()
        total += k
        print(f"  {fn}: {k} feom vars permuted to runtime order")
    if total == 0 and already == 0:
        sys.exit("permute_feom_to_runtime: no feom vars found/permuted -- refusing to continue")
    print(f"permute_feom_to_runtime: OK ({total} vars permuted, {already} files already runtime-ordered, n={n}, npes={npes})")


if __name__ == "__main__":
    main()
