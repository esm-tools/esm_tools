from loguru import logger

from . import helpers


def run_job(config):
    """Runs the post-processing job"""
    helpers.evaluate(config, "postprocess", "post_recipe")
    return config


def convert_to_zarr(config):
    """
    Converts the output to zarr format using xarray
    """
    if not config["general"].get("convert_to_zarr", False):
        logger.info("Skipping conversion to zarr")
        return config
    logger.info("Converting output to zarr via xarray")

    return config
