# -*- coding: utf-8 -*-
"""
Convert FESOM hydrographic output to array at each column point incl lon, lat.

For the coupling of the FESOM ocean model, the original FESOM output of the 
hydrographic fields temp and salt are modified. First values are computed for
all grid point location from the surface layer down to the deepest depth. In
addition for each location the corresponding longitude and latitude information
are provided.

Dependence:
It depends on the pyfesom code (https://github.com/FESOM/pyfesom)


(C) Christian Rodehacke, AWI, 2017-07-12

Additional code for command line parsing:
    Paul Gierz, AWI, May 7, 2018
"""

#
# Command Line Parser
#
import argparse
def parse_arguments():
    parser = argparse.ArgumentParser()
    parser.add_argument("--FESOM_PATH", nargs="*", required=True)
    parser.add_argument("--FESOM_VARIABLE", nargs="+")
    parser.add_argument("--FESOM_MESH", nargs="+")
    parser.add_argument("--FESOM_YEARS", nargs="+")
    parser.add_argument("--FESOM_OUTPUT", nargs="*", default='fesom_output.nc4')
    parser.add_argument("--FESOM_MESH_ALPHA", type=int, default=50)
    parser.add_argument("--FESOM_MESH_BETA",  type=int, default=15)
    parser.add_argument("--FESOM_MESH_GAMMA", type=int, default=-90)
    return parser.parse_args()

args = parse_arguments()

FESOMDATA_OUTPUT = 'fesom_output.nc4'
NAN_REPLACE = 9.96921e36

# ----------------------------------------------------------------
#
# Import external libaries ...
#

import sys
sys.path.append("./")
sys.path.append("./pyfesom")

import pyfesom2 as pf
import numpy as np
from netCDF4 import Dataset, MFDataset
import time as time
import xarray as xr
import os

# ----------------------------------------------------------------
#
# Some information
#

print('* Start at '+time.ctime(time.time()))

# ----------------------------------------------------------------
#
# Read input data
#
# mesh
print('* Read the mesh MESHPATH=' + " ".join(args.FESOM_MESH))

euler_angle = [args.FESOM_MESH_ALPHA, args.FESOM_MESH_BETA, args.FESOM_MESH_GAMMA]

mesh = pf.load_mesh(*args.FESOM_MESH, abg=euler_angle, usepickle=False)
print("*** mesh.zlev = ", mesh.zlev)

#
# Oceanographic data
#
#print('* Read the data file args.FESOM_FILE=' + " ".join(args.FESOM_FILE))
#FID = MFDataset(args.FESOM_PATH + "".join("/temp.fesom.*.01.nc"))
FID = xr.open_dataset(str(args.FESOM_PATH[0]) + str(args.FESOM_VARIABLE[0]) + ".fesom." + str(args.FESOM_YEARS[0]) + ".nc")

# ----------------------------------------------------------------
#
# Initialize size and Preinitialize fields
#
# mesh.n2d = number of 2d nodes, mesh.e2d = number of 2d elements
no_hori_elemnt = mesh.n2d
#no_zlevels = mesh.nlev - 1
no_zlevels = np.size(mesh.zlev)
#no_zlevels = np.size(mesh_diag.zbar)

timedim = 'time' if 'time' in FID.dims else 'T'
no_timesteps = FID.dims[timedim]
print("no_timesteps: ", no_timesteps)
sizeVert = no_zlevels
sizeHori = no_hori_elemnt

sizeFull = (no_timesteps, no_zlevels, no_hori_elemnt)
print("*** sizeFull = ", sizeFull)

# Fields to write: allocate memory upfront
time_out = np.zeros(no_timesteps, dtype=np.float64)
depth_out = np.zeros(sizeVert, dtype=np.float64)
latitude_out = np.zeros(sizeHori, dtype=np.float64)
longitude_out = np.zeros(sizeHori, dtype=np.float64)
TempFields_out = np.zeros(sizeFull, dtype=np.float64)

# ----------------------------------------------------------------
#
# Loop through all depth levels
#
print('* Loop for each time step through all depth levels')

#
# Depth axis
#
ilevel = -1
#for depth in mesh_diag.zbar.squeeze().values:
for depth in mesh.zlev:
    # Some information
    idepth = int(depth)
    #
    # Prepare data for final netcdf output
    #
    ilevel = ilevel + 1
    print('*   ilevel='+str(ilevel)+' depth='+str(idepth)+\
    ' ('+str(depth)+')')
    depth_out[ilevel] = depth

#
# Time axis
#
time_out = FID.variables['time'][0:no_timesteps]

#
# Full hydrographic fields
#

#
# Check shape of input file to process old (2D array) and new (3D array) FESOM files
#
for itime in np.arange(0, no_timesteps, 1, dtype=np.int32):
    ##time_out[itime] = FID.variables['time'][itime]
    time_read = time_out[itime]
    # Some information
    print('*   TIME('+str(itime)+') = '+str(time_read))

    ilevel = -1
    #for depth in mesh_diag.zbar.squeeze().values:
    for depth in mesh.zlev[:-1]:
        # Some information
        idepth = int(depth)
        print('*   depth='+str(idepth)+' ('+str(depth)+\
        ')  ++ time('+str(itime)+') = '+str(time_read))

        #
        # Prepare data for final netcdf output
        #
        ilevel = ilevel + 1

        flag_verbose=False

        if args.FESOM_YEARS[0] == args.FESOM_YEARS[1]:
            level_data = \
            pf.get_data(args.FESOM_PATH[0], args.FESOM_VARIABLE[0], int(args.FESOM_YEARS[0]), mesh, depth=idepth, how="mean") #, flag_verbose)
        else:
            level_data = \
            pf.get_data(args.FESOM_PATH[0], args.FESOM_VARIABLE[0], [int(args.FESOM_YEARS[0]), int(args.FESOM_YEARS[1])], mesh, depth=idepth, how="mean") #, flag_verbose)
        level_data[np.where(np.isnan(level_data))] = NAN_REPLACE
        TempFields_out[itime, ilevel, :] = level_data


# ----------------------------------------------------------------
#
# Output file
#
print('*   Create file "'+args.FESOM_OUTPUT[0]+'"')
#OK: OCE_OUT = Dataset("".join(args.FESOM_OUTPUT), "w", format="NETCDF3_64BIT_OFFSET")
OCE_OUT = Dataset(args.FESOM_OUTPUT[0], "w", format="NETCDF4") #format="NETCDF3_64BIT_OFFSET")

print('*     Create dimensions and define variables')
#
# Create dimensions
#
OCE_OUT.createDimension("time", None)
OCE_OUT.createDimension("level", sizeVert)
OCE_OUT.createDimension("horizontal", no_hori_elemnt)
#
# Create global attributes
#
OCE_OUT.type = "FESOM_ocean_data"
OCE_OUT.title = "FESOM hydrographic ocean data"
OCE_OUT.model_domain = "Generic"
#OCE_OUT.data_input_file = args.FESOM_FILE
OCE_OUT.insitution = 'Alfred Wegener Institute for Polar and Marine Research (AWI)'
OCE_OUT.address = 'Handelshafen, Bremerhaven, Germany'
OCE_OUT.department = "Climate"
OCE_OUT.contact = 'Christian Rodehacke'
OCE_OUT.web = 'http://www.awi.de'
OCE_OUT.acknowledgment = 'ZUWEISS, BMWF, Germany'
OCE_OUT.history = "Created "+time.ctime(time.time())

#
# Create variables
#
# -- axis-variables
time_var = OCE_OUT.createVariable("time", "f4", ("time",))
level_var = OCE_OUT.createVariable("level", "i4", ("level",))
hori_var = OCE_OUT.createVariable("horizontal", "i4", ("horizontal",))

# -- common variables
depth_var = OCE_OUT.createVariable("depth", "f4", ("level",))
lon_var = OCE_OUT.createVariable("longitude", "f4", ("horizontal",))
lat_var = OCE_OUT.createVariable("latitude", "f4", ("horizontal",))

temp_var = OCE_OUT.createVariable(args.FESOM_VARIABLE[0], "f4", \
    ("time", "level", "horizontal",), fill_value=NAN_REPLACE)
#
# Attributes
#
time_var.long_name = "time"
time_var.axis = "T"
time_var.units = "" #FID.variables['time'].units #"hours since 0001-0101 0:0:0.0"
if hasattr(FID.variables['time'], 'calendar'):
    time_var.calendar = FID.variables['time'].calendar #"standard"

level_var.long_name = "model_level"
level_var.description = "Level number"
level_var.axis = "Z"

hori_var.long_name = "horizontal_grid_id"
hori_var.description = "Number of horizontal grid element"
hori_var.axis = "X"

depth_var.long_name = "depth"
depth_var.units = "m"
depth_var.description = "depth below sea level"
depth_var.positive = 'down'

lon_var.long_name = "longitude"
lon_var.units = "degrees east"
lat_var.long_name = "latitude"
lat_var.units = "degrees north"

temp_var.long_name = "temperature"
temp_var.units = "degree celcius" #FID.variables[args.FESOM_VARIABLE[0]].units
temp_var.coordinates = "longitude latitude"
temp_var.description = "" #FID.variables[args.FESOM_VARIABLE[0]].description
#temp_var.missing_value = NAN_REPLACE

#
# Write the data fields
#
print('*     Write the small fields')
# -- axis variables
time_var[:] = time_out
level_var[:] = np.arange(0, sizeVert, dtype=np.int32)
hori_var[:] = np.arange(0, sizeHori, dtype=np.int32)

# -- common variables
depth_var[:] = mesh.zlev
#depth_var[:] = mesh_diag.zbar
lon_var[:] = mesh.x2
lat_var[:] = mesh.y2

print('*     Write the larger fields')
temp_var[:] = TempFields_out

#
# close the files
#
OCE_OUT.close()

#
# Bye bye
#
print('End   at '+time.ctime(time.time()))
print("    ... Bye")
