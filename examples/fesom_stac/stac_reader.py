import xarray as xr
from loguru import logger


def _get_xarray_params(asset):
    extra = asset.extra_fields or {}
    return {
        "engine": extra.get("xarray:engine", "netcdf4"),
        "open_kwargs": extra.get("xarray:open_kwargs", {}),
    }


def load_collection(
    stac_collection,
    asset_name="data",
    concat_dim="time",
    chunks={"time": 1},
):
    hrefs = []
    engine = None
    open_kwargs = {}

    for item_id, item in stac_collection.items():
        stac_obj = getattr(item, "_stac_obj", item)
        asset = stac_obj.assets[asset_name]

        hrefs.append(asset.href)

        params = _get_xarray_params(asset)

        # sanity: all files must be opened the same way
        engine = engine or params["engine"]
        open_kwargs = open_kwargs or params["open_kwargs"]

    logger.info(f"Opening {len(hrefs)} files with open_mfdataset")

    ds = xr.open_mfdataset(
        hrefs,
        engine=engine,
        concat_dim=concat_dim,
        combine="nested",
        chunks=chunks,
        coords="minimal",
        compat="override",
        join="override",
        parallel=True,
        **open_kwargs,
    )

    return ds
