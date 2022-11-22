from . import helpers


def run_job(config):
    helpers.evaluate(config, "assemble", "assemble_recipe")
    return config
