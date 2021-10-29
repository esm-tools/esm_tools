#!/usr/bin/env python
import numpy as np
import xarray as xr  # TODO: find a more lightweight library for the netcdf read/write
import argparse
import os
import shutil

test_mode = True
# Temporary imports
if test_mode:
    import matplotlib.pyplot as plt
    import cartopy.crs as ccrs


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("jsbach_restart_file")
    parser.add_argument("echam_restart_file")
    parser.add_argument("--soil_moisture_budget_file")
    parser.add_argument("--lsm_file")
    parser.add_argument("--layer_moisture", action="store_true")
    return parser.parse_args()


def unpack_jsbach_var(var_name, var_file, lsm_name="landseamask", lsm_file=None):
    if lsm_file is None:
        lsm_file = var_file

    var_DataArray = xr.open_dataset(var_file)
    lsm_DataArray = xr.open_dataset(lsm_file)

    var, lsm = var_DataArray[var_name], lsm_DataArray[lsm_name]
    assert var.dims[-1] == "landpoint"

    if var.ndim == 1:
        var_unpacked = np.empty((lsm.data.shape))
    elif var.ndim == 2:
        var_unpacked = np.empty((var.shape[0],) + lsm.data.shape)
    elif var.ndim == 3:
        var_unpacked = np.empty(
            (
                var.shape[0],
                var.shape[1],
            )
            + lsm.data.shape
        )
    else:
        print(
            "Opps, your array has more dimensions than unpack_jsbach.py knows how to handle! Goodbye!"
        )
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
        print(
            "Opps, your array has more dimensions than pack_jsbach.py knows how to handle! Goodbye!"
        )
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


def plot_test():
    pass


if __name__ == "__main__":
    args = parse_args()
    jsbach_restart_filepath = args.jsbach_restart_file
    echam_restart_filepath = args.echam_restart_file
    soil_moisture_budget_filepath = args.soil_moisture_budget_file
    layer_moisture_only = args.layer_moisture
    lsm_file = args.lsm_file

    if layer_moisture_only:
        original_layer_moisture = unpack_jsbach_var(
            "layer_moisture", jsbach_restart_filepath, lsm_file=lsm_file
        )
    else:
        original_layer_moisture = unpack_jsbach_var(
            "layer_moisture", jsbach_restart_filepath
        )
        original_soil_moisture = unpack_jsbach_var(
            "soil_moisture", jsbach_restart_filepath
        )
        original_rel_soil_moisture = unpack_jsbach_var(
            "rel_soil_moisture", jsbach_restart_filepath
        )

    if soil_moisture_budget_filepath is None:
        if layer_moisture_only:
            budget_layer_moisture = np.empty(original_layer_moisture.shape)
        else:
            budget_layer_moisture = np.empty(original_layer_moisture.shape)
            budget_soil_moisture = np.empty(original_soil_moisture.shape)
            budget_rel_soil_moisture = np.empty(original_rel_soil_moisture.shape)
    else:
        if layer_moisture_only:
            budget_layer_moisture = unpack_jsbach_var(
                "layer_moisture", soil_moisture_budget_filepath, lsm_file=lsm_file
            )
        else:
            budget_layer_moisture = unpack_jsbach_var(
                "layer_moisture", soil_moisture_budget_filepath
            )
            budget_soil_moisture = unpack_jsbach_var(
                "soil_moisture", soil_moisture_budget_filepath
            )
            budget_rel_soil_moisture = unpack_jsbach_var(
                "rel_soil_moisture", soil_moisture_budget_filepath
            )

    with xr.open_dataset(echam_restart_filepath) as echam_restart_file:
        glac = echam_restart_file.glac.data.astype(bool)
    if layer_moisture_only:
        new_layer_moisture = np.empty(original_layer_moisture.shape)
        new_budget_layer_moisture = np.empty(original_layer_moisture.shape)

        for arr, original_arr, budget_arr, new_budget_arr in zip(
            [new_layer_moisture],
            [original_layer_moisture],
            [budget_layer_moisture],
            [new_budget_layer_moisture],
        ):
            for l in range(arr.shape[0]):
                new_budget_arr[l, :, :] = np.where(glac, original_arr[l, :, :], 0)
                # Restore old moisture where the mask has receeded
                if soil_moisture_budget_filepath is not None:
                    arr[l, :, :] = np.where(
                        np.logical_and(~glac, arr[l, :, :] == 0),
                        budget_arr[l, :, :],
                        original_arr[l, :, :],
                    )
                # Mask everything to 0 where glac is defined
                arr[l, :, :] = np.where(glac, 0, original_arr[l, :, :])
    else:
        new_soil_moisture = np.empty(original_soil_moisture.shape)
        new_rel_soil_moisture = np.empty(original_rel_soil_moisture.shape)
        new_layer_moisture = np.empty(original_layer_moisture.shape)
        new_budget_soil_moisture = np.empty(original_soil_moisture.shape)
        new_budget_rel_soil_moisture = np.empty(original_rel_soil_moisture.shape)
        new_budget_layer_moisture = np.empty(original_layer_moisture.shape)

        for arr, original_arr, budget_arr, new_budget_arr in zip(
            [new_soil_moisture, new_rel_soil_moisture, new_layer_moisture],
            [
                original_soil_moisture,
                original_rel_soil_moisture,
                original_layer_moisture,
            ],
            [budget_soil_moisture, budget_rel_soil_moisture, budget_layer_moisture],
            [
                new_budget_soil_moisture,
                new_budget_rel_soil_moisture,
                new_budget_layer_moisture,
            ],
        ):
            for l in range(arr.shape[0]):
                new_budget_arr[l, :, :] = np.where(glac, original_arr[l, :, :], 0)
                if soil_moisture_budget_filepath is not None:
                    arr[l, :, :] = np.where(
                        np.logical_and(~glac, arr[l, :, :] == 0),
                        budget_arr[l, :, :],
                        original_arr[l, :, :],
                    )
                arr[l, :, :] = np.where(glac, 0, original_arr[l, :, :])

    new_Dataset = xr.open_dataset(jsbach_restart_filepath)
    if layer_moisture_only:
        new_layer_moisture = pack_jsbach_var(new_layer_moisture, lsm_file)
        new_Dataset.layer_moisture.data[:] = new_layer_moisture
    else:
        new_soil_moisture = pack_jsbach_var(new_soil_moisture, jsbach_restart_filepath)
        new_rel_soil_moisture = pack_jsbach_var(
            new_budget_rel_soil_moisture, jsbach_restart_filepath
        )
        new_layer_moisture = pack_jsbach_var(
            new_layer_moisture, jsbach_restart_filepath
        )

        new_Dataset.soil_moisture.data[:] = new_soil_moisture
        new_Dataset.rel_soil_moisture.data[:] = new_rel_soil_moisture
        new_Dataset.layer_moisture.data[:] = new_layer_moisture
    new_Dataset.to_netcdf("new_jsbach_restart.nc")

    if soil_moisture_budget_filepath is None:
        soil_moisture_budget_filepath = shutil.copyfile(
            jsbach_restart_filepath, "soil_budget_init.nc"
        )
    new_budget_Dataset = xr.open_dataset(soil_moisture_budget_filepath)
    if layer_moisture_only:
        new_budget_layer_moisture = pack_jsbach_var(new_budget_layer_moisture, lsm_file)
        new_budget_Dataset.layer_moisture.data[:] = new_budget_layer_moisture
    else:
        new_budget_soil_moisture = pack_jsbach_var(
            new_budget_soil_moisture, jsbach_restart_filepath
        )
        new_budget_rel_soil_moisture = pack_jsbach_var(
            new_budget_rel_soil_moisture, jsbach_restart_filepath
        )
        new_budget_layer_moisture = pack_jsbach_var(
            new_budget_layer_moisture, jsbach_restart_filepath
        )

        new_budget_Dataset.soil_moisture.data[:] = new_budget_soil_moisture
        new_budget_Dataset.rel_soil_moisture.data[:] = new_budget_rel_soil_moisture
        new_budget_Dataset.layer_moisture.data[:] = new_budget_layer_moisture
    new_budget_Dataset.to_netcdf("new_soil_budget.nc")
    if os.path.isfile("soil_budget_init.nc"):
        os.remove("soil_budget_init.nc")
    os.rename("new_soil_budget.nc", "soil_budget.nc")
