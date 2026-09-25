#!/usr/bin/env python3
"""Re-seat carried icebergs after the ocean mesh has changed.

`iceberg_elem` in the restart is a global element index on the mesh that wrote
it. A mesh-change leg renumbers the elements, so every carried berg then points
at whichever element happens to hold its old index. Nothing crashes, because the
indices are still in range, but the berg delivers its meltwater to the wrong
place, and the flux is divided by that element's node area
(`icb_coupling.F90`), so a berg reseated into a small element also delivers far
too much of it per unit area.

So give the berg the index it is actually in on the new mesh, and change nothing
else about it.

The tempting shortcut is to set `find_iceberg_elem` back to true and let FESOM
re-derive the element the way it does for a newly calved berg. That is wrong
twice over. The flag is also the frame marker: the block it opens
(`icb_step.F90:402`) *begins* by calling g2r, so to FESOM true means "these
coordinates are geographic". That holds for a newly calved berg --
`init_icebergs_with_icesheet` reads PISM's geographic positions straight out of
buoys_xlon_file and leaves the flag true -- and is the opposite of a carried
berg, whose coordinates line 405 overwrote with rotated ones the moment it was
first placed and which `iceberg_out` writes back that way (only the buoy
*output* is un-rotated, via r2g at line 882). Setting the flag therefore hands
FESOM rotated coordinates labelled geographic and it rotates them a second time:
in new_ism39 1901 that displaced the carried bergs by a median 20 deg, put 1571
of 3504 outside the mesh, and FESOM stopped on the lowest one, ib=900. Because
`par_ex` finalizes only MPI_COMM_FESOM the job did not fail -- it hung until the
wall-clock limit. The flag also runs `initialize_velo`, which discards the
berg's velocity and re-seeds it from the ambient ocean.

Writing the index avoids both. The coordinates are never converted, so there is
no frame to get wrong and no round-trip through the restart's 7 significant
digits, and the berg keeps the momentum it had. A berg still carrying the flag
has not been placed yet, so its stale index is never read and it is left exactly
as it is.

The index is the 1-based row of elem2d.out, which is FESOM's global element
numbering. Checked against a restart FESOM wrote itself, repro_ism37 1927 on
submesh_1927-12-31: 1293 of 1295 bergs land on the element FESOM recorded, and
the two that differ are adjacent elements 0.107 deg apart, a berg on a shared
edge where point_in_triangle breaks the tie the other way.

A berg whose position is no longer ocean has to go: with the index written its
meltwater would otherwise land on a cavity or an element that no longer exists.
The test is FESOM's own, shared with filter_icebergs_fesom_rule.

The record is fixed-width, `'(18e15.7,I8,L,3e15.7,L,I5,L)'`, and FESOM reads it
with that format, so fields are addressed by byte offset: lon_deg at column 45,
lat_deg at 60, `iceberg_elem` the I8 at 270, `find_iceberg_elem` the single
character at 279.

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
COL_ELEM = N_REALS * W_REAL              # 270, the iceberg_elem I8
COL_LON, COL_LAT = 3 * W_REAL, 4 * W_REAL  # 45, 60: byte offsets of lon/lat

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
    # The index goes back into an I8. Overflowing it would silently shift every
    # field after it, so refuse rather than corrupt the record.
    if len(elems) >= 10 ** W_INT:
        raise SystemExit(f"reseat_icebergs: {len(elems)} elements does not fit "
                         f"the restart's I{W_INT} iceberg_elem field")
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

        kept, dropped, reindexed = [], 0, 0
        for line in lines:
            # A record we cannot read is a record FESOM cannot read either.
            if len(line) <= COL_FIE:
                dropped += 1
                continue
            try:
                lon = float(line[COL_LON:COL_LON + W_REAL])
                lat = float(line[COL_LAT:COL_LAT + W_REAL])
            except ValueError:
                dropped += 1
                continue
            # find_iceberg_elem is the frame marker: false means the berg has
            # been placed and its coordinates were rotated in place, true means
            # they are still geographic. The mesh is geographic (FESOM rotates
            # it itself under force_rotation), so only a placed berg needs
            # un-rotating before the containment test.
            placed = line[COL_FIE] != "T"
            glon, glat = r2g(lon, lat) if placed else (lon, lat)
            p = _unit_vectors(np.array([glon]), np.array([glat]))[0]
            e = _containing_element(p, verts, tree, elems)
            if e < 0 or rejected[e]:
                dropped += 1
                continue
            if placed:
                # The element it is in on the new mesh, and nothing else: same
                # coordinates in the same frame, same velocity, flag still false
                # so FESOM neither re-derives nor re-rotates anything.
                line = (line[:COL_ELEM] + "%*d" % (W_INT, e + 1)
                        + line[COL_ELEM + W_INT:])
                reindexed += 1
            kept.append(line)

        with open(path, "w") as fh:
            fh.write("\n".join(kept) + ("\n" if kept else ""))
        print(f" *   reseat_icebergs: {os.path.basename(path)}: {len(kept)} "
              f"kept ({reindexed} re-indexed), {dropped} dropped as unplaceable "
              f"on the new mesh")

    # num_non_melted_icb_file has to agree with the ISM restart it counts.
    # The file is iceberg.restart.ISM; matching only "_ISM" never fired, so the
    # count was left to whatever ran next. It agrees today, but a dropped record
    # with a stale count makes FESOM read past EOF into uninitialised bergs.
    ism = [t for t in targets if t.endswith((".ISM", "_ISM"))]
    if ism:
        n = sum(1 for l in open(ism[0]) if l.strip())
        cnt = os.path.join(os.path.dirname(ism[0]), "num_non_melted_icb_file")
        with open(cnt, "w") as fh:
            fh.write(f"{n}\n")
        print(f" *   reseat_icebergs: num_non_melted_icb_file -> {n}")


if __name__ == "__main__":
    main()
