#!/usr/bin/env python3
import sys
from urllib.parse import quote_plus

import mongoengine
import questionary
from loguru import logger
from mongoengine import connect, disconnect

from . import auth_commands, document_models


def ls():
    connect("esm_tools", host=auth_commands.HOST)
    scenarios = document_models.Simulation.objects
    return scenarios

def mk(field_attrs={}, interactive=False):
    connect("esm_tools", host=auth_commands.HOST)
    known_scenarios = [scen.name for scen in document_models.Simulation.objects]
    available_scenarios = known_scenarios + ["None of these, please help me to make a new one"] # Fix with db hookup
    if interactive:
        chosen = questionary.select("The following simulations already exist, do you maybe want to use one of these?", choices=available_scenarios).ask()
        if chosen != "None of these, please help me to make a new one":
            return
    logger.info("You will need to log into the database first!")
    disconnect()
    try:
        # connect("esm_tools", username=auth_commands.USER, host=auth_commands.HOST, password=auth_commands.check_auth())
        connect("esm_tools", host=auth_commands.HOST)
    except mongoengine.connection.ConnectionFailure as e:
        logger.error("Could not connect to the database!")
        raise e
    logger.debug("Creating object")
    if interactive:
        new_scenario = document_models.Simulation.mk_interactively()
    else:
        new_scenario = document_models.Simulation(**field_attrs)

    new_scenario.save()
    

def show(name):
    connect("esm_tools", host=auth_commands.HOST)
    scens = document_models.Simulation.objects(name=name)
    for scen in scens:
        for field in scen._fields:
            print(f"{field}: {getattr(scen, field)}")
