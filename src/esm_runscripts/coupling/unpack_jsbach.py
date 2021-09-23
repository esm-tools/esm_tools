import numpy as np
import xarray as xr
import sys
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='unpack jsbach')
    parser.add_argument("var_name")
    parser.add_argument("var_file")
    parser.add_argument("--lsm_file")
    parser.add_argument("-v", "--verbose", action="store_true")
    return parser.parse_args()


#jsbach_lsm_file="/work/ba0989/a270077/MPIESM/test_paul006/work/restart_test_paul006_jsbach.nc"
#jsbach_ini_file="/pool/data//JSBACH/input/r0009/T63/jsbach_T63GR15_11tiles_5layers_1850_dynveg.nc"

#jsbach_lsm_DataArray=xr.open_dataset(jsbach_lsm_file)
#jsbach_ini_DataArray=xr.open_dataset(jsbach_ini_file)

def unpack_jsbach_var(var_name, var_file, lsm_name, lsm_file, out_name=None, out_file=None, verbosity=False):
    # Assign the output variable name and file name to be the same as var_name
    # and var_file, unless otherwise specified:
    if out_name == None:
        out_name = var_name
    if out_file == None:
        out_file = var_name+"_lonlat_grid.nc"
    # Load the variable and land-sea mask from the files
    if verbosity: print("Loading var_file...")
    var_DataArray = xr.open_dataset(var_file) 
    if verbosity: print("Loading lsm_file...")
    lsm_DataArray = xr.open_dataset(lsm_file)
    var, lsm = var_DataArray[var_name], lsm_DataArray[lsm_name]
    # Make sure that the landpoint dimension is the last one, otherwise crash,
    # since unpacking won't work correctly:
    assert var.dims[-1] == 'landpoint'
    # Make an empty array with the land-sea shape, plus any "extra" dimensions
    # For example: tiles, soil layers, canopy layers...)\
    # print("var is: ", var)
    # print("var.ndim:", var.ndim)
    # print("var.shape:", var.shape)
    # print("type(var.shape):", type(var.shape))
    # print("lsm.data.shape:", lsm.data.shape)
    if var.ndim == 1:
        var_unpacked = np.empty((lsm.data.shape))
    elif var.ndim == 2:
        var_unpacked = np.empty((var.shape[0],)+lsm.data.shape)
    elif var.ndim == 3:
        var_unpacked = np.empty((var.shape[0], var.shape[1],)+lsm.data.shape)
    else:
        # The rerun_jsbach.nc file appears to only have at most 3 dimensions,
        # so crash if somehow there are 4!
        print("Opps, your array has more dimensions than unpack_jsbach.py knows how to handle! Goodbye!")
        sys.exit()
    # Fill the empty array with nan:
    var_unpacked[:] = np.nan
    # Turn the land-sea mask into a boolean array
    mask = lsm.data.astype(bool)
    # Fill up the variable into the 2D field. If np.place is unfamiliar to you,
    # see the example here:
    # https://docs.scipy.org/doc/numpy-1.13.0/reference/generated/numpy.place.html
    if var.ndim == 1:
        np.place(var_unpacked[:, :], mask, var[:])
    elif var.ndim == 2:
        for d1 in range(var.shape[0]):
            np.place(var_unpacked[d1, :, :], mask, var[d1, :])
    elif var.ndim == 3:
        for d1 in range(var.shape[0]):
            for d2 in range(var.shape[1]):
                np.place(var_unpacked[d1, d2, :, :], mask, var[d1, d2, :])
    # Define dimension and coordinates to save in netcdf file
    dims = (var_DataArray[var_name].dims[:-1]+('lat', 'lon'))
    if verbosity: print(dims)
    coords = lsm.coords
    # Construct a labeled output DataArray from the unpacked array
    var_unpacked_DataArray=xr.DataArray(var_unpacked, coords=coords, dims=dims, name=out_name)
    # Save to disk
    var_unpacked_DataArray.to_netcdf(out_file)
    # Return the DataArray (for interactive use only...)
    return out_file

#unpack_jsbach_var("veg_height", jsbach_lsm_file, "landseamask", jsbach_lsm_file)
if __name__ == "__main__":
    args = parse_arguments()
    if args.verbose:
            print(args)
            print(args.var_name)
            print(args.var_file)
            print(args.lsm_file)
    lsm_file = args.lsm_file if args.lsm_file else args.var_file
    if args.verbose: print(lsm_file)
    unpack_jsbach_var(args.var_name, args.var_file, "landseamask", lsm_file, verbosity=args.verbose)
