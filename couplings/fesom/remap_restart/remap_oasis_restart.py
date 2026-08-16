#!/usr/bin/env python
"""Remap OASIS coupling-restart feom-grid fields from the old FESOM submesh to
the new submesh after a dynamic-ocean mesh change.

When ice2fesom carves a new ocean submesh, the OASIS coupling restarts written
on the previous mesh (rstos.nc = ocean->atm fields like sst_feom/sie_feom...,
rstas.nc = atm->ocean fields on the feom grid) no longer match the new feom node
count. Reading them would mismatch; relying on $NNOREST T just zeroes the fields
(a coupling transient). This remaps them instead, the same way the FESOM ocean
restart is remapped, so the coupling state is carried across the mesh change.

ORDERING (the bug this file used to have): OASIS restarts are written in
FESOM's RUNTIME exchange order (Apple partition: concatenated my_list per
rank), NOT nod2d order -- and the runtime-ordered remap weights expect the
output in the new mesh's runtime order too. The map_nod gather, however, lives
in nod2d space. So the transform is a sandwich:

    old_nod2d = old_runtime[L_old - 1]        # un-permute (rpart.out of OLD mesh)
    new_nod2d[i] = old_nod2d[g[i]]            # map_nod gather (+ nn fill)
    new_runtime  = new_nod2d[argsort(L_new)]  # re-permute (rpart.out of NEW mesh)

Skipping the outer two steps (the old behaviour) emits a compound-scrambled
restart: OIFS's first coupling interval then sees a jigsaw SST (tropical water
under polar air), which nondeterministically detonates the convection scheme
within hours-to-weeks (movcav16 attempts 2/3: forrtl overflow / div-by-zero in
cubasen.F90). Verified on the chunk-1 pool: the old output correlated 0.15 with
the restart's own level-1 temperature; this transform correlates 0.995.

Both submeshes are subsets of the same max-mesh ("base"); each submesh's
map_nod.out maps submesh-local node -> base node (1-based). New nodes absent
from the old submesh are filled by nearest-neighbour on the sphere.

Any variable whose node axis length equals the old feom size is remapped; all
other variables (the fixed A096/atm grid fields, LOCTRANS aux arrays, ...) are
copied unchanged.

Usage:
  remap_oasis_restart.py <in_rst.nc> <out_rst.nc> \
      <old_map_nod.out> <new_map_nod.out> <new_nod2d.out> \
      <old_feom_size> <new_feom_size> <old_rpart.out> <new_rpart.out>
"""
import sys
import numpy as np
import xarray as xr


def _read_map(path):
    """submesh-local node index -> base (max-mesh) node, 1-based, as read."""
    return np.loadtxt(path, dtype=np.int64).ravel()


def _read_nod2d_lonlat(path):
    """nod2d.out: first line is the node count, then 'idx lon lat coast'."""
    arr = np.loadtxt(path, skiprows=1)
    return arr[:, 1], arr[:, 2]


def _read_rpart(path, expect_n):
    """rpart.out: [npes, counts(npes), L(n2d)] with L[n] = 1-based runtime
    position of nod2d node n. Returns L."""
    vals = np.array(open(path).read().split(), dtype=np.int64)
    npes = vals[0]
    L = vals[1 + npes:]
    if len(L) != expect_n:
        sys.exit(f"remap_oasis_restart: rpart {path} carries {len(L)} nodes, "
                 f"expected {expect_n}")
    return L


def _build_gather(old_map, new_map, new_nod2d, old_n, new_n):
    """Return an index array g of length new_n with old-node index per new node."""
    base_to_old = {int(b): j for j, b in enumerate(old_map)}
    g = np.fromiter((base_to_old.get(int(b), -1) for b in new_map),
                    dtype=np.int64, count=new_n)
    missing = g < 0
    if missing.any():
        lon, lat = _read_nod2d_lonlat(new_nod2d)

        def xyz(lo, la):
            lo = np.radians(lo)
            la = np.radians(la)
            return np.c_[np.cos(la) * np.cos(lo),
                         np.cos(la) * np.sin(lo),
                         np.sin(la)]

        good = ~missing
        good_idx = np.where(good)[0]
        try:
            from scipy.spatial import cKDTree
            tree = cKDTree(xyz(lon[good], lat[good]))
            _, nn = tree.query(xyz(lon[missing], lat[missing]))
        except ImportError:
            # brute force fallback (fine for the few hundred carved nodes)
            G = xyz(lon[good], lat[good])
            M = xyz(lon[missing], lat[missing])
            nn = np.array([np.argmax(G @ m) for m in M])
        g[missing] = g[good_idx[nn]]
    return g, int(missing.sum())


def main():
    (in_rst, out_rst, old_map_p, new_map_p, new_nod2d_p,
     old_n, new_n, old_rpart_p, new_rpart_p) = sys.argv[1:10]
    old_n, new_n = int(old_n), int(new_n)

    old_map = _read_map(old_map_p)
    new_map = _read_map(new_map_p)
    g, n_fill = _build_gather(old_map, new_map, new_nod2d_p, old_n, new_n)
    L_old = _read_rpart(old_rpart_p, old_n)
    L_new = _read_rpart(new_rpart_p, new_n)
    # composite index: for each new RUNTIME position, the old RUNTIME position
    # feeding it (see module docstring for the three-step sandwich).
    comp = (L_old - 1)[g][np.argsort(L_new, kind="stable")]

    ds = xr.open_dataset(in_rst)
    data_vars = {}
    n_remapped = 0
    for v in ds.data_vars:
        da = ds[v]
        axes = [ax for ax, n in enumerate(da.shape) if n == old_n]
        if axes:
            ax = axes[-1]  # node axis
            arr = np.take(da.values, comp, axis=ax)
            data_vars[v] = (da.dims, arr)
            n_remapped += 1
        else:
            data_vars[v] = (da.dims, da.values)

    # Guard: if this restart carries feom-grid fields (rstos: *_feom) but none
    # matched old_n, then old_map / old_feom_size describes a different mesh than
    # the one the source was written on -- e.g. restart/oasis3mct/rstos.nc was
    # left at a different node count by an earlier submesh attempt. Every field
    # would then be copied through unchanged at the wrong size and exit 0, and
    # OASIS only catches it as an "av gsize nx ny mismatch in file = rstos.nc"
    # abort several init layers later. Fail loudly here instead of emitting a
    # mis-sized restart. (rstas.nc has no *_feom fields, so 0 remapped is fine.)
    feom_vars = [v for v in ds.data_vars if "_feom" in str(v)]
    if feom_vars and n_remapped == 0:
        sizes = sorted({int(n) for v in feom_vars for n in ds[v].shape if n > 1})
        sys.exit(
            f"remap_oasis_restart: FATAL: {in_rst} carries feom fields "
            f"{feom_vars[:3]} with node size(s) {sizes}, none matching "
            f"old_feom_size={old_n}. Source restart does not match old_map; "
            f"refusing to write a mis-sized OASIS restart.")

    out = xr.Dataset(data_vars)
    # carry attributes
    for v in out.data_vars:
        out[v].attrs = ds[v].attrs
    out.attrs = ds.attrs
    out.attrs["node_order_note"] = ("feom fields in new-mesh runtime order "
                                    "(rpart sandwich; remap_oasis_restart.py)")
    out.to_netcdf(out_rst)
    print(f"remap_oasis_restart: {in_rst} ({old_n}) -> {out_rst} ({new_n}); "
          f"{n_remapped} feom fields remapped, {n_fill} nn-filled")


if __name__ == "__main__":
    main()
