from loguru import logger

from . import helpers


def run_job(config):
    logger.error("Hello from the viz job!")
    helpers.evaluate(config, "viz", "viz_recipe")
    return config
