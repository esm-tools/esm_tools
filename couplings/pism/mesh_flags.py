import numpy as np
import os
import xarray as xr
import cftime
from netCDF4 import Dataset
print("BLABLA")
pathname = os.getenv('PATH2COUPLE')
filename = os.getenv('GEOMETRY_FILE')
#Mmesh_folder = os.getenv('MESHES')
Mmesh_folder = os.getenv('MAX_MESH')

print(f"Pathname: {pathname}")
print(f"Filename: {filename}")
print(f"Meshes: {Mmesh_folder}")
# Read netCDF data
fname = os.path.join(pathname,filename)
sea_level=0.0
# Read mesh data (assuming text files)
Mnode_fname = os.path.join(Mmesh_folder, 'nod2d.out')
Mbed_fname = os.path.join(Mmesh_folder, 'aux3d.out')
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
print("MASK")

cavity_depth = np.minimum(ds['ice_subNN'].values.flatten(),0)           

# Read Mnode_info
#Mnode_info0 = np.loadtxt(f"{Mmesh_folder}{Mnode_fname}")
# Load the ASCII file as a flat array
with open(Mnode_fname, 'r') as f:
# Read the number from the first line
    Mnode_num = int(f.readline().strip())
# Read Mbed_info
Mbed_info0 = np.loadtxt(f"{Mbed_fname}")
Mvert_num = int(Mbed_info0[0])
Mvert_lev = Mbed_info0[1:Mvert_num+1]
Mbed_info = Mbed_info0[Mvert_num+1:]

idx = np.argmin(np.abs(cavity_depth[:, np.newaxis] - Mvert_lev), axis=1)
#Mnode_lon = Mnode_info[:, 1]    
#Mnode_lat = Mnode_info[:, 2]
Mbed_info = -np.abs(Mbed_info)

# Prepare arrays to write out
topo_raw = Mbed_info.copy()
#condition=((Mbed_info > sea_level) & (mask < 4)) # not ocean and bedrock above sea level  
condition=((Mbed_info - cavity_depth) > sea_level)  # excl grounded ice shelf even if it is floating in PISM. 
topo_raw[condition] = 9999
topo_raw[mask == 2] = 9999     # grounded ice in PISM

Mnode_pism = np.ones(Mnode_num)
Mnode_pism[mask == 3] = 1
Mnode_pism[mask == 4] = 1
Mnode_pism[mask == 2] = 0
Mnode_pism[mask == 1] = 0
Mnode_pism[mask == 5] = 2
Mnode_pism[condition] = -1

#cavity_raw = np.zeros(Mnode_num)
print([Mbed_info, cavity_depth,Mnode_pism])

# Write out
np.savetxt('topo_raw.txt', topo_raw, fmt='%10f')
np.savetxt('cavity_raw.txt', cavity_depth, fmt='%10f')
np.savetxt('mask.txt', Mnode_pism, fmt='% d')

print("Generate new bathymetry and iceshelf topography on maxmesh. Done!")

