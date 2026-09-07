#!/usr/bin/env python3
"""Re-seat carried icebergs after the ocean mesh has changed.

`iceberg_elem` in the restart is a global element index on the mesh that wrote
it. A mesh-change leg renumbers the elements, so every carried berg then points
at whichever element happens to hold its old index. Nothing crashes, because the
indices are still in range, but the berg delivers its meltwater to the wrong
place, and the flux is divided by that element's node area
(`icb_coupling.F90`), so a berg reseated into a small element also delivers far
too much of it per unit area.

FESOM already knows how to place a berg from its lon/lat: that is what it does
for a newly calved one. It only skips the search because the restart says
`find_iceberg_elem = .false.`. So set that back to true and let FESOM re-derive
the element. The stale index is then never read.

A berg whose position is no longer ocean has to go, for the same reason a new
one does: the search returns 0 and FESOM's handling of that hangs every rank.
The test is FESOM's own, shared with filter_icebergs_fesom_rule.

The record is fixed-width, `'(18e15.7,I8,L,3e15.7,L,I5,L)'`, so
`find_iceberg_elem` is the single character at column 279.

Usage:
  reseat_iceberg_restart.py <mesh_dir> <restart_file> [<restart_file> ...]
"""
import os
import sys

import numpy as np

N_REALS = 18
W_REAL = 15
W_INT = 8
COL_FIE = N_REALS * W_REAL + W_INT + 1   # 279, the find_iceberg_elem character
COL_LON, COL_LAT = 3, 4                  # 0-based field index in the record

# iceberg.restart stores ROTATED coordinates: FESOM converts lon_deg/lat_deg in
# place with g2r on the first step (icb_step.F90:403) and writes them back that
# way. nod2d.out is GEOGRAPHIC. Testing one against the other rejected 562 of
# 2238 carried bergs in new_ism38 1925; only 4 fail once converted, so ~558 bergs
# (~1900 Gt) were being deleted every leg for no physical reason.
# Euler angles from namelist.config (alphaEuler/betaEuler/gammaEuler defaults).
ALPHA, BETA, GAMMA = 50.0, 15.0, -90.0


def r2g(lon, lat):
    """Rotated -> geographic, as gen_modules_rotate_grid.F90 does it."""
    al, be, ga = np.radians([ALPHA, BETA, GAMMA])
    M = np.array([
        [np.cos(ga)*np.cos(al) - np.sin(ga)*np.cos(be)*np.sin(al),
         np.cos(ga)*np.sin(al) + np.sin(ga)*np.cos(be)*np.cos(al),
         np.sin(ga)*np.sin(be)],
        [-np.sin(ga)*np.cos(al) - np.cos(ga)*np.cos(be)*np.sin(al),
         -np.sin(ga)*np.sin(al) + np.cos(ga)*np.cos(be)*np.cos(al),
         np.cos(ga)*np.sin(be)],
        [np.sin(be)*np.sin(al), -np.sin(be)*np.cos(al), np.cos(be)]])
    lo, la = np.radians(float(lon)), np.radians(float(lat))
    x, y, z = np.cos(la)*np.cos(lo), np.cos(la)*np.sin(lo), np.sin(la)
    xg = M[0, 0]*x + M[1, 0]*y + M[2, 0]*z
    yg = M[0, 1]*x + M[1, 1]*y + M[2, 1]*z
    zg = M[0, 2]*x + M[1, 2]*y + M[2, 2]*z
    return (np.degrees(np.arctan2(yg, xg)),
            np.degrees(np.arcsin(np.clip(zg, -1.0, 1.0))))


def main(argv=None):
    argv = argv if argv is not None else sys.argv[1:]
    if len(argv) < 2:
        raise SystemExit(__doc__)
    mesh_dir, targets = argv[0], argv[1:]

    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from filter_icebergs_fesom_rule import _unit_vectors, _containing_element
    from scipy.spatial import cKDTree

    nod = np.loadtxt(os.path.join(mesh_dir, "nod2d.out"), skiprows=1)
    elems = np.loadtxt(os.path.join(mesh_dir, "elem2d.out"), skiprows=1,
                       dtype=np.int64) - 1
    cav_file = os.path.join(mesh_dir, "cavity_depth@node.out")
    cavity = (np.loadtxt(cav_file, ndmin=1) != 0.0 if os.path.isfile(cav_file)
              else np.zeros(nod.shape[0], dtype=bool))
    rejected = (cavity | (nod[:, 3] != 0.0))[elems].all(axis=1)

    verts = _unit_vectors(nod[:, 1], nod[:, 2])
    centroids = verts[elems].mean(axis=1)
    centroids /= np.linalg.norm(centroids, axis=1, keepdims=True)
    tree = cKDTree(centroids)

    for path in targets:
        if not os.path.isfile(path):
            print(f" *   reseat_icebergs: {path} absent, skipping")
            continue
        with open(path) as fh:
            lines = [l.rstrip("\n") for l in fh if l.strip()]

        kept, dropped = [], 0
        for line in lines:
            fields = line.split()
            try:
                lon = float(fields[COL_LON])
                lat = float(fields[COL_LAT])
            except (IndexError, ValueError):
                # A record we cannot read is a record FESOM cannot read either.
                dropped += 1
                continue
            # The record is rotated; the mesh is geographic. Convert before the
            # containment test. The record itself is written back UNCHANGED in
            # rotated coordinates, because that is what FESOM reads.
            glon, glat = r2g(lon, lat)
            p = _unit_vectors(np.array([glon]), np.array([glat]))[0]
            e = _containing_element(p, verts, tree, elems)
            if e < 0 or rejected[e]:
                dropped += 1
                continue
            if len(line) > COL_FIE:
                line = line[:COL_FIE] + "T" + line[COL_FIE + 1:]
            kept.append(line)

        with open(path, "w") as fh:
            fh.write("\n".join(kept) + ("\n" if kept else ""))
        print(f" *   reseat_icebergs: {os.path.basename(path)}: {len(kept)} "
              f"re-seated, {dropped} dropped as unplaceable on the new mesh")

    # num_non_melted_icb_file has to agree with the ISM restart it counts.
    ism = [t for t in targets if t.endswith("_ISM")]
    if ism:
        n = sum(1 for l in open(ism[0]) if l.strip())
        cnt = os.path.join(os.path.dirname(ism[0]), "num_non_melted_icb_file")
        with open(cnt, "w") as fh:
            fh.write(f"{n}\n")
        print(f" *   reseat_icebergs: num_non_melted_icb_file -> {n}")


if __name__ == "__main__":
    main()
