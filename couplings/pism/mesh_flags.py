import numpy as np
import os
import xarray as xr
import cftime
from netCDF4 import Dataset

print("====================================================================")
print("[MESH_FLAGS] Starting mesh flag generation from PISM ice geometry")
print("====================================================================")

pathname = os.getenv('PATH2COUPLE')
filename = os.getenv('GEOMETRY_FILE')
#Mmesh_folder = os.getenv('MESHES')
Mmesh_folder = os.getenv('MAX_MESH')

print(f"[MESH_FLAGS] Configuration:")
print(f"[MESH_FLAGS]   Coupling directory: {pathname}")
print(f"[MESH_FLAGS]   Geometry file: {filename}")
print(f"[MESH_FLAGS]   Maximum mesh directory: {Mmesh_folder}")
# Read netCDF data
fname = os.path.join(pathname,filename)
sea_level=0.0
print(f"[MESH_FLAGS] Reading PISM geometry file: {fname}")
# Read mesh data (assuming text files)
Mnode_fname = os.path.join(Mmesh_folder, 'nod2d.out')
Mbed_fname = os.path.join(Mmesh_folder, 'aux3d.out')
print(f"[MESH_FLAGS] Reading FESOM mesh files:")
print(f"[MESH_FLAGS]   Nodes: {Mnode_fname}")
print(f"[MESH_FLAGS]   Bedrock: {Mbed_fname}")
ds = xr.open_dataset(fname, decode_times=False) 
#print("BLABLA")
#ds = ds.convert_calendar('standard', align_on='year')
#print("BLABLA")
#mask = ds['mask'].isel(ncells=0).values
#with Dataset(fname, 'r') as nc:
#    mask = nc.variables['mask'][:,:]
#    topg = nc.variables['topg'][0,:]
#    thk = nc.variables['thk'][0,:]
#decode_times=False
#ds = ds.convert_calendar('standard', align_on='year')
#time_values = ds['time'].values
mask = ds['mask'].values.flatten()
print(f"[MESH_FLAGS] Loaded PISM mask data: {mask.shape[0]} nodes")
print(f"[MESH_FLAGS]   Mask values present: {np.unique(mask)}")

print(f"[MESH_FLAGS] Computing ice shelf cavity depth...")
cavity_depth = np.minimum(ds['ice_subNN'].values.flatten(),0)
print(f"[MESH_FLAGS]   Cavity depth range: [{cavity_depth.min():.1f}, {cavity_depth.max():.1f}] m")           

# Read Mnode_info
#Mnode_info0 = np.loadtxt(f"{Mmesh_folder}{Mnode_fname}")
# Load the ASCII file as a flat array
print(f"[MESH_FLAGS] Reading FESOM maximum mesh node information...")
with open(Mnode_fname, 'r') as f:
# Read the number from the first line
    Mnode_num = int(f.readline().strip())
print(f"[MESH_FLAGS]   Total nodes in maximum mesh: {Mnode_num}")
# Read Mbed_info
print(f"[MESH_FLAGS] Reading bedrock elevation data...")
Mbed_info0 = np.loadtxt(f"{Mbed_fname}")
Mvert_num = int(Mbed_info0[0])
Mvert_lev = Mbed_info0[1:Mvert_num+1]
Mbed_info = Mbed_info0[Mvert_num+1:]

idx = np.argmin(np.abs(cavity_depth[:, np.newaxis] - Mvert_lev), axis=1)
#Mnode_lon = Mnode_info[:, 1]    
#Mnode_lat = Mnode_info[:, 2]
Mbed_info = -np.abs(Mbed_info)

print(f"[MESH_FLAGS] Processing topography and masks...")
# Prepare arrays to write out
topo_raw = Mbed_info.copy()
#condition=((Mbed_info > sea_level) & (mask < 4)) # not ocean and bedrock above sea level  
print(f"[MESH_FLAGS] Applying mask conditions:")
condition=((Mbed_info - cavity_depth) > sea_level)  # excl grounded ice shelf even if it is floating in PISM. 
print(f"[MESH_FLAGS]   Nodes above sea level: {np.sum(condition)}")
topo_raw[condition] = 9999
print(f"[MESH_FLAGS]   Grounded ice nodes (mask==2): {np.sum(mask == 2)}")
topo_raw[mask == 2] = 9999     # grounded ice in PISM

print(f"[MESH_FLAGS] Creating node classification flags:")
Mnode_pism = np.ones(Mnode_num)
Mnode_pism[mask == 3] = 1   # Ice shelf
Mnode_pism[mask == 4] = 1   # Open ocean
Mnode_pism[mask == 2] = 0   # Grounded ice
Mnode_pism[mask == 1] = 0   # Ice-free land
Mnode_pism[mask == 5] = 2   # Special case
Mnode_pism[condition] = -1  # Above sea level

print(f"[MESH_FLAGS]   Ice shelf nodes (flag=1, mask=3): {np.sum(mask == 3)}")
print(f"[MESH_FLAGS]   Open ocean nodes (flag=1, mask=4): {np.sum(mask == 4)}")
print(f"[MESH_FLAGS]   Grounded ice nodes (flag=0, mask=2): {np.sum(mask == 2)}")
print(f"[MESH_FLAGS]   Ice-free land nodes (flag=0, mask=1): {np.sum(mask == 1)}")
print(f"[MESH_FLAGS]   Special nodes (flag=2, mask=5): {np.sum(mask == 5)}")
print(f"[MESH_FLAGS]   Excluded nodes (flag=-1): {np.sum(condition)}")

#cavity_raw = np.zeros(Mnode_num)

# Write out
print(f"[MESH_FLAGS] Writing output files:")
np.savetxt('topo_raw.txt', topo_raw, fmt='%10f')
print(f"[MESH_FLAGS]   ✓ topo_raw.txt ({len(topo_raw)} values)")
np.savetxt('cavity_raw.txt', cavity_depth, fmt='%10f')
print(f"[MESH_FLAGS]   ✓ cavity_raw.txt ({len(cavity_depth)} values)")
np.savetxt('mask.txt', Mnode_pism, fmt='% d')
print(f"[MESH_FLAGS]   ✓ mask.txt ({len(Mnode_pism)} values)")

print("[MESH_FLAGS] Mesh flag generation completed successfully!")
print("====================================================================")

