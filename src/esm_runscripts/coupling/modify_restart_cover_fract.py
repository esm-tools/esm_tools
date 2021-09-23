#!/usr/bin/env python
import sys
sys.path.insert(0, '/pf/a/a270077/.local/lib/python3.5/site-packages')

import numpy as np
import xarray as xr 	# TODO: find a more lightweight library for the netcdf read/write
import argparse

test_mode = False
# Temporary imports
if test_mode:
    import matplotlib.pyplot as plt
    import cartopy.crs as ccrs


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("jsbach_init_file")
    parser.add_argument("jsbach_restart_file")
    parser.add_argument("mask_change_file")
    parser.add_argument("lsm_file")
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
            np.place(var_unpacked[d1, :, :], mask, var[d1,:])
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
                # print("Placing value at %s, %s = %s" % (j, i, arr[j, i]))
                output_arr = np.append(output_arr, arr[j, i])
    return output_arr

def plot_test():
    f, axs = plt.subplots(3, 4, subplot_kw={"projection": ccrs.Robinson()})
    for ax in axs.flatten(): ax.coastlines()
    axs[0,0].set_title("Mask")
    for tile_num, ax in enumerate(axs.flatten()[1:]): ax.set_title("Tile "+str(tile_num))
    axs[0,0].pcolormesh(jsbach_init_file.lon, jsbach_init_file.lat, glacial_mask_dataarray,
                        transform=ccrs.PlateCarree())
    for lev, ax in enumerate(axs.flatten()[1:]):
        ax.pcolormesh(jsbach_init_file.lon, jsbach_init_file.lat, cover_fract[lev, :, :], 
                      transform=ccrs.PlateCarree())


if __name__ == "__main__":
    args = parse_args()
    jsbach_init_filepath = args.jsbach_init_file 
    jsbach_restart_filepath = args.jsbach_restart_file
    mask_change_filepath = args.mask_change_file
    lsm_filepath = args.lsm_file

    for cover_fract_variable in ('cover_fract', 'cover_fract_pot'):
        print("\nModifying restart file %s variable %s" %
                (jsbach_restart_filepath, cover_fract_variable))
        with xr.open_dataset(jsbach_init_filepath) as jsbach_init_file:
            glacial_mask_dataarray = jsbach_init_file.glac.data
        cover_fract = unpack_jsbach_var(cover_fract_variable, jsbach_restart_filepath, lsm_file=lsm_filepath) 
        with xr.open_dataset(mask_change_filepath) as mask_change_file:
            # In this file; 1 represents newly glaciated cells
            #               0 represents no change in glacier mask
            #              -1 represents shrinking glacier extent
            # TODO: Double check sign +/-
            glacial_mask_dataarray_change = mask_change_file.glac.data
            new_glacier_cells = glacial_mask_dataarray_change == 1.0
            non_changed_glacier_cells = glacial_mask_dataarray_change == 0.0
            new_deglacial_cells = glacial_mask_dataarray_change == -1.0
            all_glacial_cells = glacial_mask_dataarray == 1.0

        if test_mode: plot_test()

        # Turn ON advancing glaciers in the first tile:
        print("-   Turning ON advancing glaciers in the first tile (total number of new glacial cells: %s)" % np.sum(new_glacier_cells))
        cover_fract_advancing = \
                np.where(new_glacier_cells, 1., cover_fract[0, :, :])

        # Turn OFF retreating glaciers in the first tile:
        print("-   Turning OFF retreating glaciers in the first tile (total number of deglacial cells: %s)" % np.sum(new_deglacial_cells))
        print("-   Rather than setting first tile to direct 0; we set it to a very small number, since JSBACH requires a minimum of 1e-10")
        cover_fract_advancing_and_retreating = \
            np.where(new_deglacial_cells, 1.e-10, cover_fract_advancing)
        # NOTE: The first layer needs to be handeled seperately since it also
        # contains the ice sheets; which need to have 1 directly for ice
        # sheets; thus is cannot easily be included in the loop below.
        cover_fract[0, :, :] = cover_fract_advancing_and_retreating
        # Turn OFF glaciated cells in all other tiles and to a very small value
        # for any newly deglaciated cells
        for lev in range(cover_fract.shape[0]-1):
            lev += 1
            print("-   Modifying cover fract in restart for layer %s" % lev)
            print("-   Setting cover fract amount to 0 under the glacial mask")
            cover_fract_lev = np.where(all_glacial_cells, 0, cover_fract[lev, :, :])
            cover_fract[lev, :, :] = cover_fract_lev
            print("-   Setting to 1e-10 under new deglacial cells")
            cover_fract_lev = np.where(new_deglacial_cells, 1.e-10, cover_fract[lev, :, :])
            cover_fract[lev, :, :] = cover_fract_lev
        if cover_fract_variable == 'cover_fract_pot':
            print("-   For cover_fract_pot, setting option to grow tundra under new deglacial cells")
            cover_fract_lev = np.where(new_deglacial_cells, 1.0-(10.*1.e-10),
                    cover_fract[6, :, :])
            cover_fract[6, :, :] = cover_fract_lev
        unpacked_dataset = xr.Dataset({cover_fract_variable+'_new': (['tile', 'lon', 'lat'], cover_fract)})
        unpacked_dataset.to_netcdf(cover_fract_variable+'_new.nc')
        packed_array = pack_jsbach_var(cover_fract, lsm_filepath)
        packed_dataset = xr.Dataset({cover_fract_variable: (['tile', 'landpoint'], packed_array)})
        packed_dataset.to_netcdf(cover_fract_variable+"_new_packed.nc")
        print("-   Writing results to %s" % cover_fract_variable+"_new_packed.nc")

    if test_mode: plot_test(); plt.show()
