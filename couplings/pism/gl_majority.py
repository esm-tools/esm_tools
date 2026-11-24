#!/usr/bin/env python
"""Grounding-line nodes by control-volume majority, per carve.  No tunable number.

FESOM keeps tracers on nodes; each node stands for its control volume (about a third of
each surrounding triangle).  The node-regridded mask decides a node from the single
nearest PISM cell, and fesom_submesh.x then drops every element touching a grounded
node -- the whole element row straddling the grounding line.  Here a marine node that
PISM calls grounded or floating is re-decided by which kind of ice covers more of its
control volume:

  floating area > grounded area  -> floating; if it was grounded, its ice base is put
                                    on the (undug) bed, where ice meets bed at the GL
  grounded area > floating area  -> grounded (mask 2, no draft)

Ties and nodes whose control volume holds no PISM cell centre keep their mask.  Ice-free
ocean and land are left alone, so the ice front is untouched.  Each PISM cell centre is
given to the node of its containing triangle with the largest barycentric weight.
Needs the static pre-dig under grounded ice (rule v5g) so a node that turns floating
already has room below its ice base.

  usage: gl_majority.py <node geometry.nc, edited in place> <mother_dir/> <undug aux3d.out>
                        <PISM geometry on its own grid>
"""
import sys
import numpy as np
import netCDF4
from matplotlib.tri import Triangulation

GEOM, MOTHER, BED, PISM = sys.argv[1:5]


def polar(lon, lat):
    r = (90.0 + lat) * 111.195
    lo = np.radians(lon)
    return r * np.sin(lo), r * np.cos(lo)


nod = np.loadtxt(MOTHER + "nod2d.out", skiprows=1)
nn = len(nod)
el = np.loadtxt(MOTHER + "elem2d.out", skiprows=1, dtype=int) - 1
a = open(BED).read().split(); nl = int(a[0]); bed = np.array(a[1 + nl:1 + nl + nn], float)

# PISM cells -> containing mother triangle -> dominant vertex
p = netCDF4.Dataset(PISM)
pm = np.round(np.ma.filled(p["mask"][-1], 0)).ravel()
plon = np.ma.filled(p["lon"][:], 0).ravel(); plat = np.ma.filled(p["lat"][:], 0).ravel()
p.close()
ice = (pm == 2) | (pm == 3)
px, py = polar(plon[ice], plat[ice]); kind = pm[ice]
x, y = polar(nod[:, 1], nod[:, 2])
ant = (nod[:, 2][el] < -55).all(1)
sub = el[ant]
tri = Triangulation(x, y, sub)
h = tri.get_trifinder()(px, py)
ok = h >= 0
t = sub[h[ok]]
X = np.stack([x[t], y[t]], -1)                      # (n,3,2) triangle vertices
P = np.stack([px[ok], py[ok]], -1)
v0, v1 = X[:, 1] - X[:, 0], X[:, 2] - X[:, 0]; v2 = P - X[:, 0]
d00 = (v0 * v0).sum(1); d01 = (v0 * v1).sum(1); d11 = (v1 * v1).sum(1)
d20 = (v2 * v0).sum(1); d21 = (v2 * v1).sum(1)
den = d00 * d11 - d01 * d01
wb = (d11 * d20 - d01 * d21) / den; wc = (d00 * d21 - d01 * d20) / den; wa = 1 - wb - wc
owner = t[np.arange(len(t)), np.argmax(np.stack([wa, wb, wc], 1), 1)]
nfl = np.bincount(owner[kind[ok] == 3], minlength=nn)
ngr = np.bincount(owner[kind[ok] == 2], minlength=nn)

import os
if os.environ.get("GL_MAJORITY_MODE", "node") == "element":
    # ELEMENT mode: an element straddling the grounding line stays in the cavity when
    # more of its area is floating than grounded.  Area from PISM cell centres
    # supersampled 3x3 per 8 km cell; the element's grounded marine nodes then turn
    # floating (ice base on the bed) so fesom_submesh.x keeps it.
    offs = np.array([-8.0 / 3, 0.0, 8.0 / 3])
    OX, OY = np.meshgrid(offs, offs)
    sx = (px[:, None] + OX.ravel()[None, :]).ravel(); sy = (py[:, None] + OY.ravel()[None, :]).ravel()
    sk = np.repeat(kind, 9)
    hs = tri.get_trifinder()(sx, sy)
    oks = hs >= 0
    efl = np.bincount(hs[oks][sk[oks] == 3], minlength=len(sub))
    egr = np.bincount(hs[oks][sk[oks] == 2], minlength=len(sub))
    ELEMENT_MAJ = (efl > egr)
else:
    ELEMENT_MAJ = None

g = netCDF4.Dataset(GEOM, "r+")
mv, dv = g.variables["mask"], g.variables["ice_subNN"]
shape = mv.shape
mask = np.round(np.ma.filled(mv[:], 0.0)).ravel()
draft = np.ma.filled(dv[:], 0.0).ravel()
m = mask[:nn]
marine = bed < 0
if ELEMENT_MAJ is None:
    to_fl = marine & (m == 2) & (nfl > ngr)
    to_gr = marine & (m == 3) & (ngr > nfl)
else:
    straddle = ELEMENT_MAJ & (m[sub] == 2).any(1) & (m[sub] == 3).any(1)
    to_fl = np.zeros(nn, bool)
    to_fl[sub[straddle].ravel()] = True
    to_fl &= marine & (m == 2)
    to_gr = np.zeros(nn, bool)
    print(f"element majority: {int(straddle.sum())} straddling elements are mostly floating")
m[to_fl] = 3; draft[:nn][to_fl] = bed[to_fl]
m[to_gr] = 2; draft[:nn][to_gr] = 0.0
mask[:nn] = m
mv[:] = mask.reshape(shape); dv[:] = draft.reshape(shape)
for name, val_fl, val_gr in (("shelf_msk", 1, 0), ("grounded_ice_mask", 0, 1)):
    if name in g.variables:
        s = np.ma.filled(g.variables[name][:], 0.0).ravel()
        s[:nn][to_fl] = val_fl; s[:nn][to_gr] = val_gr
        g.variables[name][:] = s.reshape(shape)
g.close()
print(f"majority ({os.environ.get('GL_MAJORITY_MODE', 'node')}): grounded -> floating {int(to_fl.sum())}, floating -> grounded {int(to_gr.sum())} "
      f"(PISM cell centres placed {int(ok.sum())} of {int(ice.sum())} ice cells)")
