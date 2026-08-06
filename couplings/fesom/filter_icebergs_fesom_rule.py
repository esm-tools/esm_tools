#!/usr/bin/env python3
"""Drop icebergs FESOM would refuse to place, using FESOM's own test.

FESOM does not read `icb_felem.dat`. At initialisation it re-derives the element
from the berg's lon/lat with `point_in_triangle`, and then rejects that element
when every one of its nodes is either under a cavity or on the mesh boundary
(`icb_step.F90`):

    reject = all( cavity_depth(elem2D_nodes(:,e)) /= 0 .OR.
                  bc_index_nod2D(elem2D_nodes(:,e)) == 0 )

A rejected berg leaves `iceberg_elem = 0`, and FESOM then calls `par_ex` followed
by `stop`, which in a coupled run hangs every rank instead of aborting.

The generator excludes cavities too, but per element from `cavity_elvls.out`, and
that is a different criterion on a different object, so it can keep a berg whose
containing element FESOM rejects. This applies FESOM's rule to the generated set
so the two agree by construction.

`bc_index_nod2D` is 1 everywhere and 0 on boundary-edge nodes, which is the
boundary flag in column 4 of `nod2d.out`.

Usage:
  filter_icebergs_fesom_rule.py <mesh_dir> <icb_dir>
"""
import os
import sys

import numpy as np

RHO_ICE = 850.0  # kg m-3, the generator's value

FIELDS = ["longitude", "latitude", "length", "height", "scaling", "felem",
          "calving_day"]


def _unit_vectors(lon_deg, lat_deg):
    lon = np.radians(lon_deg)
    lat = np.radians(lat_deg)
    cl = np.cos(lat)
    return np.stack([cl * np.cos(lon), cl * np.sin(lon), np.sin(lat)], axis=-1)


def _containing_element(p, verts, tree, elems, k=48):
    """Index of the element containing p, or -1.

    Done on the sphere with signed volumes rather than in lon/lat, so the
    dateline and the high southern latitudes need no special casing.
    """
    _, cand = tree.query(p, k=min(k, len(elems)))
    for e in np.atleast_1d(cand):
        a, b, c = verts[elems[e]]
        d1 = np.dot(np.cross(a, b), p)
        d2 = np.dot(np.cross(b, c), p)
        d3 = np.dot(np.cross(c, a), p)
        if (d1 >= 0 and d2 >= 0 and d3 >= 0) or (d1 <= 0 and d2 <= 0 and d3 <= 0):
            return e
    return -1


def main(argv=None):
    argv = argv if argv is not None else sys.argv[1:]
    if len(argv) < 2:
        raise SystemExit(__doc__)
    mesh_dir, icb_dir = argv[0], argv[1]

    lon_file = os.path.join(icb_dir, "icb_longitude.dat")
    if not os.path.isfile(lon_file):
        print(" *   filter_icebergs: no icb_longitude.dat, nothing to filter")
        return
    berg_lon = np.loadtxt(lon_file, ndmin=1)
    berg_lat = np.loadtxt(os.path.join(icb_dir, "icb_latitude.dat"), ndmin=1)
    n_in = berg_lon.size
    if n_in == 0:
        print(" *   filter_icebergs: empty set, nothing to filter")
        return

    nod = np.loadtxt(os.path.join(mesh_dir, "nod2d.out"), skiprows=1)
    elems = np.loadtxt(os.path.join(mesh_dir, "elem2d.out"), skiprows=1,
                       dtype=np.int64) - 1
    cav_file = os.path.join(mesh_dir, "cavity_depth@node.out")
    if os.path.isfile(cav_file):
        cavity = np.loadtxt(cav_file, ndmin=1) != 0.0
    else:
        print(f" *   filter_icebergs: no {os.path.basename(cav_file)}, "
              "treating every node as open ocean")
        cavity = np.zeros(nod.shape[0], dtype=bool)
    boundary = nod[:, 3] != 0.0

    # FESOM's test, per element, evaluated once for the whole mesh.
    bad_node = cavity | boundary
    elem_rejected = bad_node[elems].all(axis=1)

    verts = _unit_vectors(nod[:, 1], nod[:, 2])
    centroids = verts[elems].mean(axis=1)
    centroids /= np.linalg.norm(centroids, axis=1, keepdims=True)

    from scipy.spatial import cKDTree
    tree = cKDTree(centroids)

    pts = _unit_vectors(berg_lon, berg_lat)
    keep = np.ones(n_in, dtype=bool)
    n_nohit = 0
    for i in range(n_in):
        e = _containing_element(pts[i], verts, tree, elems)
        if e < 0:
            # FESOM's own search would come up empty here too, and an empty
            # search is exactly the case that deadlocks.
            keep[i] = False
            n_nohit += 1
        elif elem_rejected[e]:
            keep[i] = False

    n_out = int(keep.sum())

    # Dropped bergs take their discharge with them, so weigh them before the
    # files are rewritten. This is a real loss from the ice sheet's mass budget,
    # not a bookkeeping detail, and it has to be visible in the log.
    mass = None
    try:
        L = np.loadtxt(os.path.join(icb_dir, "icb_length.dat"), ndmin=1)
        H = np.loadtxt(os.path.join(icb_dir, "icb_height.dat"), ndmin=1)
        S = np.loadtxt(os.path.join(icb_dir, "icb_scaling.dat"), ndmin=1)
        if L.size == H.size == S.size == n_in:
            mass = L * L * H * S * RHO_ICE / 1e12   # Gt
    except OSError:
        pass

    if n_out == n_in:
        print(f" *   filter_icebergs: all {n_in} placeable, nothing dropped")
        return

    for name in FIELDS:
        path = os.path.join(icb_dir, f"icb_{name}.dat")
        if not os.path.isfile(path):
            continue
        with open(path) as fh:
            lines = [l for l in fh if l.strip()]
        if len(lines) != n_in:
            raise SystemExit(
                f"filter_icebergs: icb_{name}.dat has {len(lines)} rows, "
                f"icb_longitude.dat has {n_in}")
        with open(path, "w") as fh:
            fh.writelines(l for l, k in zip(lines, keep) if k)

    dropped = n_in - n_out
    print(f" *   filter_icebergs: {n_out} of {n_in} kept, {dropped} dropped "
          f"({n_nohit} outside the mesh, {dropped - n_nohit} in cavity or "
          f"boundary-only elements)")
    if mass is not None:
        lost = float(mass[~keep].sum())
        tot = float(mass.sum())
        pct = 100.0 * lost / tot if tot > 0 else 0.0
        print(f" *   filter_icebergs: {tot - lost:.1f} Gt kept, {lost:.1f} Gt "
              f"dropped ({pct:.1f}% of this leg's calved mass)")


if __name__ == "__main__":
    main()
