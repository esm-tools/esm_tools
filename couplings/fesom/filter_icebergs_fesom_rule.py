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

# The per-berg files this filter trims when bergs are dropped.
#
# calving_day is deliberately NOT here. make_icebergs.py calls this filter and
# only afterwards writes icb_calving_day.dat, sizing it from the already-filtered
# icb_longitude.dat -- so its length is right by construction and it needs no
# trimming. Listing it made the loop below compare THIS cycle's berg count against
# the PREVIOUS cycle's leftover file and abort. That stayed hidden for 19 coupling
# cycles because the early return above fires whenever nothing is dropped; the
# first cycle to drop a berg (repro_ism37 cycle 20, 1094 fresh vs 1013 stale) hit it.
FIELDS = ["longitude", "latitude", "length", "height", "scaling", "felem"]


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


# Draft and grounding, as FESOM computes them. The draft is
# height * rho_icb / rho_h2o (icb_thermo.F90:281, defaults in icb_allocate.F90),
# and a berg is grounded when that draft exceeds the bottom interpolated from
# zbar_n_bot at its element's three nodes (icb_step.F90:556, depth_bathy).
# Under l_allowgrounding = 2 a grounded berg stops for good and melts in place,
# so a berg seated where the water is shallower than its draft never leaves:
# antg065 1913 put 21 bergs (scaling 484) from the Amery front on one coastal
# element with a 190 m bottom and 192-202 m drafts, and their melt drove the
# surface layer below -5 degC within 34 days.
RHO_WATER = 1027.5
# Head-room under the keel, so a berg seated at the threshold does not ground
# on the first interpolated bottom that is a little shallower than the nodes.
DEPTH_MARGIN = 10.0    # m
# How far a berg may be moved to find deep enough water. Beyond this it is left
# where it is: moving meltwater a basin away is worse than letting it ground.
# antg065 1913 needed at most 45 km (336 carried bergs, median 23 km); 100 km
# is about twice that, short of the next embayment.
MAX_SHIFT_KM = 100.0
EARTH_RADIUS_KM = 6371.0


def _node_bottom_depth(mesh_dir, n_nodes):
    """FESOM's zbar_n_bot per node, positive down, without partial cells.

    zbar is the level list at the top of aux3d.out, nlvls.out the number of
    levels per node, and zbar_n_bot is zbar at that level (use_partial_cell is
    false in these setups, so the raw depth in aux3d.out is not what FESOM uses).
    """
    with open(os.path.join(mesh_dir, "aux3d.out")) as fh:
        nl = int(fh.readline())
        zbar = np.array([float(fh.readline()) for _ in range(nl)])
    nlvls = np.loadtxt(os.path.join(mesh_dir, "nlvls.out"), dtype=np.int64,
                       ndmin=1)
    if nlvls.size != n_nodes:
        raise SystemExit(f"nlvls.out has {nlvls.size} rows, nod2d.out has "
                         f"{n_nodes} nodes")
    return np.abs(zbar[nlvls - 1])


class DeepWater:
    """Finds, for a berg too deep for where it is, the nearest element it fits.

    A target element has no cavity node and no boundary node, so the berg sits
    in open water away from the coast edge, and every node's bottom is at least
    the draft plus DEPTH_MARGIN, so FESOM's interpolated bottom is too.
    """

    def __init__(self, mesh_dir, nod, elems, cavity, boundary, verts):
        bottom = _node_bottom_depth(mesh_dir, nod.shape[0])
        self.elem_bottom = bottom[elems].min(axis=1)
        idx = np.flatnonzero(~(cavity | boundary)[elems].any(axis=1))
        self._idx = idx
        cen = verts[elems[idx]].mean(axis=1)
        cen /= np.linalg.norm(cen, axis=1, keepdims=True)
        self._cen = cen
        from scipy.spatial import cKDTree
        self._tree = cKDTree(cen)
        lon = np.degrees(np.arctan2(cen[:, 1], cen[:, 0]))
        lat = np.degrees(np.arcsin(np.clip(cen[:, 2], -1.0, 1.0)))
        self._lonlat = np.stack([lon, lat], axis=1)

    @staticmethod
    def draft(height, rho_icb=850.0, rho_h2o=RHO_WATER):
        return height * rho_icb / rho_h2o

    def fits(self, e, draft):
        return self.elem_bottom[e] >= draft + DEPTH_MARGIN

    def nearest(self, p, draft):
        """(element, lon, lat, km) of the nearest fitting element, or None."""
        chord = 2.0 * np.sin(MAX_SHIFT_KM / EARTH_RADIUS_KM / 2.0)
        cand = self._tree.query_ball_point(p, chord)
        if not cand:
            return None
        cand = np.asarray(cand)
        cand = cand[self.elem_bottom[self._idx[cand]] >= draft + DEPTH_MARGIN]
        if cand.size == 0:
            return None
        d = np.linalg.norm(self._cen[cand] - p, axis=1)
        j = cand[np.argmin(d)]
        km = 2.0 * EARTH_RADIUS_KM * np.arcsin(min(1.0, d.min() / 2.0))
        return int(self._idx[j]), self._lonlat[j, 0], self._lonlat[j, 1], km


def _shift_summary(tag, moved_km, stuck):
    if moved_km:
        km = np.asarray(moved_km)
        print(f" *   {tag}: {km.size} moved to water deeper than their draft "
              f"(+{DEPTH_MARGIN:.0f} m), median {np.median(km):.0f} km, "
              f"max {km.max():.0f} km")
    if stuck:
        print(f" *   {tag}: {stuck} too deep for their element but no deep "
              f"enough water within {MAX_SHIFT_KM:.0f} km, left in place")


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

    # Heights for the draft test. Without them the depth test cannot be made,
    # and the set is filtered on FESOM's placement rule alone, as before.
    height_file = os.path.join(icb_dir, "icb_height.dat")
    berg_h = (np.loadtxt(height_file, ndmin=1) if os.path.isfile(height_file)
              else None)
    if berg_h is not None and berg_h.size != n_in:
        raise SystemExit(f"filter_icebergs: icb_height.dat has {berg_h.size} "
                         f"rows, icb_longitude.dat has {n_in}")
    deep = (DeepWater(mesh_dir, nod, elems, cavity, boundary, verts)
            if berg_h is not None else None)
    if deep is None:
        print(" *   filter_icebergs: no icb_height.dat, water depth not checked")

    pts = _unit_vectors(berg_lon, berg_lat)
    keep = np.ones(n_in, dtype=bool)
    n_nohit = 0
    # Row index -> replacement line, per file, for the bergs moved to deeper
    # water. icb_felem.dat is 0-based, as the generator writes it.
    moved = {"longitude": {}, "latitude": {}, "felem": {}}
    moved_km, stuck = [], 0
    for i in range(n_in):
        e = _containing_element(pts[i], verts, tree, elems)
        if e < 0:
            # FESOM's own search would come up empty here too, and an empty
            # search is exactly the case that deadlocks.
            keep[i] = False
            n_nohit += 1
        elif elem_rejected[e]:
            keep[i] = False
        elif deep is not None:
            draft = DeepWater.draft(berg_h[i])
            if deep.fits(e, draft):
                continue
            hit = deep.nearest(pts[i], draft)
            if hit is None:
                stuck += 1
                continue
            e_new, lon_new, lat_new, km = hit
            moved["longitude"][i] = "%.18e\n" % lon_new
            moved["latitude"][i] = "%.18e\n" % lat_new
            moved["felem"][i] = "%d\n" % e_new
            moved_km.append(km)

    n_out = int(keep.sum())
    _shift_summary("filter_icebergs", moved_km, stuck)

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

    if n_out == n_in and not moved_km:
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
        new = moved.get(name, {})
        lines = [new.get(i, l) for i, l in enumerate(lines)]
        with open(path, "w") as fh:
            fh.writelines(l for l, k in zip(lines, keep) if k)

    if n_out == n_in:
        print(f" *   filter_icebergs: all {n_in} placeable, nothing dropped")
        return
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
