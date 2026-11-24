"""Generate node-flag inputs for fesom_submesh.x from a regridded PISM geometry.

Reads the PISM ice geometry (already remapped onto the FESOM max-mesh nodes)
and the FESOM max-mesh aux files, then writes topo_raw.txt, cavity_raw.txt
and mask.txt that the Fortran mesh-reduction tool consumes.
"""

import os
import numpy as np
import xarray as xr

print("Starting mesh flag generation from PISM ice geometry")

pathname = os.getenv("PATH2COUPLE")
filename = os.getenv("GEOMETRY_FILE")
Mmesh_folder = os.getenv("MAX_MESH")

print(f"  Coupling directory:     {pathname}")
print(f"  Geometry file:          {filename}")
print(f"  Maximum mesh directory: {Mmesh_folder}")

fname = os.path.join(pathname, filename)
Mnode_fname = os.path.join(Mmesh_folder, "nod2d.out")
Mbed_fname = os.path.join(Mmesh_folder, "aux3d.out")

sea_level = 0.0

print(f"Reading PISM geometry file: {fname}")
ds = xr.open_dataset(fname, decode_times=False)
# build_submesh remaps the PISM ice-type mask (categorical: 2=grounded, 3=floating
# cavity, 4=ocean, 5=outside-domain) bilinearly, which averages categories into
# fractional values (e.g. 2.99, 4.0000005). The classification below uses exact
# integer equality, so round back to the nearest category first -- otherwise
# shelf-edge nodes land in fractional limbo and the cavity is silently dropped.
mask = np.round(ds["mask"].values.flatten())
print(f"  Loaded PISM mask: {mask.shape[0]} nodes, values {np.unique(mask)}")

cavity_depth = np.minimum(ds["ice_subNN"].values.flatten(), 0)
print(f"  Cavity depth range: [{cavity_depth.min():.1f}, {cavity_depth.max():.1f}] m")


print(f"Reading FESOM max-mesh node info: {Mnode_fname}")
with open(Mnode_fname, "r") as f:
    Mnode_num = int(f.readline().strip())
print(f"  Total nodes in max mesh: {Mnode_num}")

print(f"Reading bedrock elevation: {Mbed_fname}")
Mbed_info0 = np.loadtxt(Mbed_fname)
Mvert_num = int(Mbed_info0[0])
Mvert_lev = Mbed_info0[1:Mvert_num + 1]
Mbed_info = Mbed_info0[Mvert_num + 1:]
Mbed_info = -np.abs(Mbed_info)

# Exclude grounded ice shelves even when PISM reports them as floating.
condition = (Mbed_info - cavity_depth) > sea_level

topo_raw = Mbed_info.copy()
topo_raw[condition] = 9999
topo_raw[mask == 2] = 9999  # grounded ice in PISM
topo_raw[mask == 0] = 9999  # ice-free bedrock in PISM (MASK_ICE_FREE_BEDROCK)

# Node classification (PISM Mask.hh: 0=ice-free bedrock, 2=grounded ice,
# 3=floating ice, 4=ice-free ocean; 5=outside-domain marker set upstream):
#   1  ice shelf / open ocean
#   0  grounded ice / ice-free land
#   2  special (mask == 5)
#  -1  above sea level
# mask==0 MUST be land: it is bare bedrock above sea level (e.g. an island
# whose ice cap has melted away). Defaulting it to ocean deletes the island.
Mnode_pism = np.ones(Mnode_num)
Mnode_pism[mask == 3] = 1
Mnode_pism[mask == 4] = 1
Mnode_pism[mask == 2] = 0
Mnode_pism[mask == 1] = 0
Mnode_pism[mask == 0] = 0
Mnode_pism[mask == 5] = 2
Mnode_pism[condition] = -1

print("Node classification:")
print(f"  Ice shelf  (mask=3):        {np.sum(mask == 3)}")
print(f"  Open ocean (mask=4):        {np.sum(mask == 4)}")
print(f"  Grounded   (mask=2):        {np.sum(mask == 2)}")
print(f"  Ice-free bedrock (mask=0):  {np.sum(mask == 0)}")
print(f"  Ice-free   (mask=1):        {np.sum(mask == 1)}")
print(f"  Special    (mask=5):        {np.sum(mask == 5)}")
print(f"  Excluded above SL:          {np.sum(condition)}")

np.savetxt("topo_raw.txt", topo_raw, fmt="%10f")
np.savetxt("cavity_raw.txt", cavity_depth, fmt="%10f")
np.savetxt("mask.txt", Mnode_pism, fmt="% d")

print(f"Wrote topo_raw.txt   ({len(topo_raw)} values)")
print(f"Wrote cavity_raw.txt ({len(cavity_depth)} values)")
print(f"Wrote mask.txt       ({len(Mnode_pism)} values)")
print("Mesh flag generation done.")
