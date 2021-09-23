#!/usr/bin/env python
import sys
sys.path.insert(0, '/pf/a/a270077/.local/lib/python3.5/site-packages')

import numpy as np
import xarray as xr     # TODO: find a more lightweight library for the netcdf read/write
import argparse

test_mode = False
# Temporary imports
if test_mode:
    import matplotlib.pyplot as plt
    import cartopy.crs as ccrs


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("jsbach_restart_veg_file")
    parser.add_argument("mask_change_file")
    return parser.parse_args()

def unpack_jsbach_var(var_name, var_file, lsm_name="landseamask", lsm_file=None): 
    if lsm_file is None: lsm_file = var_file

    var_DataArray = xr.open_dataset(var_file) 
    lsm_DataArray = xr.open_dataset(lsm_file)

    var, lsm = var_DataArray[var_name], lsm_DataArray[lsm_name]
    assert var.dims[-1] == 'landpoint'

    if var.ndim == 1:
        var_unpacked = np.empty((lsm.data.shape))
    elif var.ndim == 2:
        var_unpacked = np.empty((var.shape[0],)+lsm.data.shape)
    elif var.ndim == 3:
        var_unpacked = np.empty((var.shape[0], var.shape[1],)+lsm.data.shape)
    else:
        print("Opps, your array has more dimensions than unpack_jsbach.py knows how to handle! Goodbye!")
        sys.exit()

    var_unpacked[:] = np.nan
    mask = lsm.data.astype(bool)

    if var.ndim == 1:
        np.place(var_unpacked[:, :], mask, var[:])
    elif var.ndim == 2:
        for d1 in range(var.shape[0]):
            np.place(var_unpacked[d1, :, :], mask, var[d1, :])
    elif var.ndim == 3:
        for d1 in range(var.shape[0]):
            for d2 in range(var.shape[1]):
                np.place(var_unpacked[d1, d2, :, :], mask, var[d1, d2, :])

    return var_unpacked


def pack_jsbach_var(arr, lsm_file, lsm_name="landseamask"):
    lsm_DataArray = xr.open_dataset(lsm_file)
    lsm = lsm_DataArray[lsm_name]
    if arr.ndim == 1:
        print("I cannot reduce a 1D array!")
        sys.exit()
    elif arr.ndim == 2:
        arr_packed = np.empty(int(lsm.data.sum()))
    elif arr.ndim == 3:
        arr_packed = np.empty((arr.shape[0], int(lsm.data.sum())))
    elif arr.ndim == 4:
        arr_packed = np.empty((arr.shape[0], arr.shape[1], int(lsm.data.sum())))
    else:
        print("Opps, your array has more dimensions than pack_jsbach.py knows how to handle! Goodbye!")
        sys.exit()

    arr_packed[:] = np.nan
    mask = lsm.data.astype(bool)

    if arr.ndim == 2:
        arr_to_be_packed = np.ma.masked_where(~mask, arr)
        arr_packed = pack_up_with_loop(arr_to_be_packed)
    elif arr.ndim == 3:
        for d0 in range(arr.shape[0]):
            arr_to_be_packed = np.ma.masked_where(~mask, arr[d0, :, :])
            arr_packed[d0, :] = pack_up_with_loop(arr_to_be_packed)
    elif arr.ndim == 4:
        for d0 in range(arr.shape[0]):
            for d1 in range(arr.shape[1]):
                arr_to_be_packed = np.ma.masked_where(~mask, arr[d0, d1, :, :])
                arr_packed[d0, d1, :] = pack_up_with_loop(arr_to_be_packed)
    return arr_packed


def pack_up_with_loop(arr):
    output_arr = np.array([])
    assert len(arr.shape) == 2
    for j in range(arr.shape[0]):
        for i in range(arr.shape[1]):
            if not arr.mask[j, i]:
                output_arr = np.append(output_arr, arr[j, i])
    return output_arr

if __name__ == "__main__":
    args = parse_args()
    jsbach_restart_veg_filepath = args.jsbach_restart_veg_file
    mask_change_filepath = args.mask_change_file

    veg_ratio_variable = 'veg_ratio'
    veg_ratio_max_variable = 'veg_ratio_max'

    print("\n\t\t-   Modifying restart file %s variables %s and %s" %
            (jsbach_restart_veg_filepath, veg_ratio_variable, veg_ratio_max_variable))

    veg_ratio = unpack_jsbach_var(
            veg_ratio_variable,
            jsbach_restart_veg_filepath,
            )
    veg_ratio_max = unpack_jsbach_var(
            veg_ratio_max_variable,
            jsbach_restart_veg_filepath,
            )

    with xr.open_dataset(mask_change_filepath) as mask_change_file:
        # In this file; 1 represents newly glaciated cells
        #               0 represents no change in glacier mask
        #              -1 represents shrinking glacier extent
        glacial_mask_dataarray_change = mask_change_file.glac.data
        new_glacier_cells = np.isin(glacial_mask_dataarray_change, [1])
        non_changed_glacier_cells = np.isin(glacial_mask_dataarray_change, [0])
        new_deglacial_cells = np.isin(glacial_mask_dataarray_change, [-1])

    print("There are %s new glacial cells, %s unchanged cells, and %s new deglacial cells" \
            % (sum(new_glacier_cells), sum(non_changed_glacier_cells),
                sum(new_deglacial_cells)))

    # Where the glacial cells have advanced, we need veg_ratio and
    # veg_ratio_max to be exactly 0, otherwise use the old values:
    veg_ratio_with_new_glacier = np.where(
            new_glacier_cells,
            0.0,
            veg_ratio
            )
    veg_ratio_max_with_new_glacier = np.where(
            new_glacier_cells,
            0.0,
            veg_ratio_max
            )

    # Where the glacial cells have retreated there should be no vegetation; so
    # we use the minimum allowed value
    number_of_tiles = 11
    minimum_value_allowed_on_tile = 1e-10

    veg_ratio_new = np.where(
            new_deglacial_cells,
            minimum_value_allowed_on_tile,
            veg_ratio_with_new_glacier
            )

    veg_ratio_max_new = np.where(
            new_deglacial_cells,
            minimum_value_allowed_on_tile,
            veg_ratio_max_with_new_glacier
            )

    # Write the replaced files to a new file before packing; to examine it.
    ds = xr.Dataset(
                {
                'veg_ratio_new': (['tile', 'lat', 'lon'], veg_ratio_new),
                'veg_ratio_max_new': (['lat', 'lon'], veg_ratio_max_new),
                }
            )
    ds.to_netcdf("veg_output_after_modification.nc")
    print("\t\t-    Wrote 'vwg_output_after_modification.nc' for examination")
    # Write the output to a test_output.nc file; this will be moved to replace
    # the real veg restart file. The move command does not occur here; but in
    # the driving infrastructure (i.e.
    # esm-runscripts/functions/iterative_coupling/coupling_ice2echam.functions)

    fname="test_output.nc"
    with xr.open_dataset(jsbach_restart_veg_filepath) as jsbach_restart_veg_file:
        print("\t\t-   Writing results to a temporary file %s; which will be used to replace %s" %
                (fname, jsbach_restart_veg_filepath))
        # Open the file
        veg_ratio_in_restart_file = jsbach_restart_veg_file.veg_ratio.data
        # Replace the data:
        veg_ratio_in_restart_file[:] = pack_jsbach_var(veg_ratio_new, jsbach_restart_veg_filepath)
        # Open the file
        veg_ratio_max_in_restart_file = jsbach_restart_veg_file.veg_ratio_max.data
        # Replace the data:
        veg_ratio_max_in_restart_file[:] = pack_jsbach_var(veg_ratio_max_new, jsbach_restart_veg_filepath)
        jsbach_restart_veg_file.to_netcdf(path=fname)
