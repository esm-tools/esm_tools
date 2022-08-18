#!.usr/bin/env python3
"""
Command line interface for MongoDB Interaction with ESM-Tools
"""
import sys

import click

from . import auth_commands, db_commands, scenario_commands, sims_commands


@click.group()
def main():
    """ESM MongoDB: The interface for the NoSQL Database"""
    return 0


@main.group()
def db():
    """Interaction with the database layer of AWI Mongo"""
    return 0

@db.command()
def ls():
    """Lists databases available"""
    dbs = db_commands.ls()
    for db in dbs:
        click.echo(db)
    return 0

@db.command()
@click.argument("name")
def rm(name):
    """Removes the database with NAME"""
    db_commands.rm(name)
    return 0

@db.command()
@click.argument("db_name")
@click.argument("col_name")
def rm_collection(db_name, col_name):
    """Removes the database with NAME"""
    db_commands.rm_collection(db_name, col_name)
    return 0

@db.command()
@click.argument("name")
def ls_collections(name):
    """Prints all collections in the database NAME"""
    cols = db_commands.ls_collections(name)
    for col in cols:
        click.echo(col)
    return 0

@main.group()
def scenario():
    """Interact with time slices"""
    return 0

@scenario.command()
def ls():
    scenarios = scenario_commands.ls()
    for scenario in scenarios:
        click.echo(scenario.name)
    return 0

@scenario.command()
def mk():
    scenario_commands.mk(interactive=True)
    return 0

@scenario.command()
@click.argument("name")
def show(name):
    scenario_commands.show(name)
    return 0


@main.group()
def sims():
    """Interaction with the simulation collection"""
    return 0

@sims.command()
def ls():
    """List all simulations in the database"""
    sims = sims_commands.ls()
    for sim in sims:
        click.echo(sim.name)
    return 0

@sims.command()
def mk():
    """Makes a new simulation from a USER_CONFIG and a FINISHED_TOTAL_CONFIG"""
    sims_commands.mk(interactive=True)

@sims.command()
@click.argument("name")
def show(name):
    sims_commands.show(name)
    return 0

@main.group()
def auth():
    """Authentication with the database"""
    return 0

@auth.command()
def generate():
    try:
        auth_commands.gen_auth()
        return 0
    except Exception as err:
        print(err)
        return 1

if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
