import os
import sys
import psutil
import time

from . import helpers
from . import database_actions
from . import logfiles


def run_job(config):
    helpers.evaluate(config, "assemble", "assemble_recipe")
    return config
