#!/usr/bin/env python
"""Remap OASIS coupling-restart feom-grid fields from the old FESOM submesh to
the new submesh after a dynamic-ocean mesh change.

When ice2fesom carves a new ocean submesh, the OASIS coupling restarts written
on the previous mesh (rstos.nc = ocean->atm fields like sst_feom/sie_feom...,
rstas.nc = atm->ocean fields on the feom grid) no longer match the new feom node
count. Reading them would mismatch; relying on $NNOREST T just zeroes the fields
(a coupling transient). This remaps them instead, the same way the FESOM ocean
restart is remapped, so the coupling state is carried across the mesh change.

Both submeshes are subsets of the same max-mesh ("base"); each submesh's
map_nod.out maps submesh-local node -> base node (1-based). A feom field is
remapped by gather: new[i] = old[j] where base(new i) == base(old j). New nodes
absent from the old submesh (only possible when the old mesh is itself a
submesh, not the full base) are filled by nearest-neighbour on the sphere from
the new nodes that did map.

Any variable whose node axis length equals the old feom size is remapped; all
other variables (the fixed A096/atm grid fields, LOCTRANS aux arrays, ...) are
copied unchanged.

Usage:
  remap_oasis_restart.py <in_rst.nc> <out_rst.nc> \
      <old_map_nod.out> <new_map_nod.out> <new_nod2d.out> \
      <old_feom_size> <new_feom_size>
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
     old_n, new_n) = sys.argv[1:8]
    old_n, new_n = int(old_n), int(new_n)

    old_map = _read_map(old_map_p)
    new_map = _read_map(new_map_p)
    g, n_fill = _build_gather(old_map, new_map, new_nod2d_p, old_n, new_n)

    ds = xr.open_dataset(in_rst)
    data_vars = {}
    n_remapped = 0
    for v in ds.data_vars:
        da = ds[v]
        axes = [ax for ax, n in enumerate(da.shape) if n == old_n]
        if axes:
            ax = axes[-1]  # node axis
            arr = np.take(da.values, g, axis=ax)
            data_vars[v] = (da.dims, arr)
            n_remapped += 1
        else:
            data_vars[v] = (da.dims, da.values)
    out = xr.Dataset(data_vars)
    # carry attributes
    for v in out.data_vars:
        out[v].attrs = ds[v].attrs
    out.attrs = ds.attrs
    out.to_netcdf(out_rst)
    print(f"remap_oasis_restart: {in_rst} ({old_n}) -> {out_rst} ({new_n}); "
          f"{n_remapped} feom fields remapped, {n_fill} nn-filled")


if __name__ == "__main__":
    main()
