import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
import sys
import argparse


def parse_arguments():
    parser = argparse.ArgumentParser(description="pack jsbach")
    parser.add_argument("var_name")
    parser.add_argument("var_file")
    parser.add_argument("lsm_file")
    return parser.parse_args()


def pack_jsbach_var(
    var_name, var_file, lsm_name, lsm_file, out_name=None, out_file=None
):
    # Assign the output variable name and file name to be the same as var_name
    # and var_file, unless otherwise specified:
    if out_name == None:
        out_name = var_name
    if out_file == None:
        out_file = var_name + "_landpoint_grid.nc"
    # Load the variable and land-sea mask from the files
    var_DataArray, lsm_DataArray = xr.open_dataset(var_file), xr.open_dataset(lsm_file)
    var, lsm = var_DataArray[var_name], lsm_DataArray[lsm_name]
    # Make sure that the landpoint dimension is the last one, otherwise crash,
    # since unpacking won't work correctly:
    assert var.dims[-2] == "lat"
    assert var.dims[-1] == "lon"
    # Make an empty array with the land-sea shape, plus any "extra" dimensions
    # For example: tiles, soil layers, canopy layers...)
    if var.ndim == 1:
        print("I cannot reduce a 1D array!")
        sys.exit()
    elif var.ndim == 2:
        # The lsm.data.sum() is needed, because we need to find out the number of land-sea points.
        # Thre is probably a better way of doing this
        var_packed = np.empty(int(lsm.data.sum()))
    elif var.ndim == 3:
        var_packed = np.empty((var.shape[0], int(lsm.data.sum())))
    elif var.ndim == 4:
        var_packed = np.empty((var.shape[0], var.shape[1], int(lsm.data.sum())))
    else:
        # The rerun_jsbach.nc file appears to only have at most 3 dimensions,
        # so crash if somehow there are 4!
        print(
            "Opps, your array has more dimensions than unpack_jsbach.py knows how to handle! Goodbye!"
        )
        sys.exit()
    # Fill the empty array with nan:
    var_packed[:] = np.nan
    # Turn the land-sea mask into a boolean array
    mask = lsm.data.astype(bool)
    # Fill up the variable into the 2D field. If np.ma.masked_where is unfamiliar to you,
    # see the example here:
    # https://docs.scipy.org/doc/numpy-1.13.0/reference/generated/numpy.ma.masked_where.html

    if var.ndim == 2:
        var_to_be_packed = np.ma.masked_where(~mask, var)
        var_packed = pack_up_with_loop(var_to_be_packed)
    elif var.ndim == 3:
        for d0 in range(var.shape[0]):
            var_to_be_packed = np.ma.masked_where(~mask, var[d0, :, :])
            var_packed[d0, :] = pack_up_with_loop(var_to_be_packed)
    elif var.ndim == 4:
        for d0 in range(var.shape[0]):
            for d1 in range(var.shape[1]):
                var_to_be_packed = np.ma.masked_where(~mask, var[d0, d1, :, :])
                var_packed[d0, d1, :] = pack_up_with_loop(var_to_be_packed)
    return var_packed

    # Define dimension and coordinates to save in netcdf file
    dims = (*var_DataArray[var_name].dims[:-2], "landpoint")
    coords = "landpoint"
    # Construct a labeled output DataArray from the packed array
    var_packed_DataArray = xr.DataArray(var_packed, dims=dims, name=out_name)
    # Save to disk
    var_packed_DataArray.to_netcdf(out_file)
    # Return the DataArray (for interactive use only...)
    return out_file


def pack_up_with_loop(arr):
    output_arr = np.array([])
    assert len(arr.shape) == 2
    for j in range(arr.shape[0]):
        for i in range(arr.shape[1]):
            if not arr.mask[j, i]:
                output_arr = np.append(output_arr, arr[j, i])
    return output_arr


if __name__ == "__main__":
    args = parse_arguments()
    pack_jsbach_var(args.var_name, args.var_file, "landseamask", args.lsm_file)
