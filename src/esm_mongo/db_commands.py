"""Implementations for db commands

Kept seperate from actual CLI to maintain the option of using these without click
"""

from urllib.parse import quote_plus

from loguru import logger
from pymongo import MongoClient

from . import auth_commands


def ls():
    """Lists collections available"""
    uri = "mongodb://%s:%s@%s" % (quote_plus(auth_commands.USER), quote_plus(auth_commands.check_auth()), auth_commands.HOST)
    with MongoClient(uri) as client:
        client.admin.command("ping")
        logger.debug("Good ping to the database :-)")
        db_names = client.list_database_names()
        logger.debug(f"Got db_names: {db_names}")
        return db_names

def rm(name):
    """Removes a database"""
    uri = "mongodb://%s:%s@%s" % (quote_plus(auth_commands.USER), quote_plus(auth_commands.check_auth()), auth_commands.HOST)
    with MongoClient(uri) as client:
        client.admin.command("ping")
        logger.debug("Good ping to the database :-)")
        client.drop_database(name)
        logger.success(f"Removed from database: {name}")

def ls_collections(name):
    uri = "mongodb://%s:%s@%s" % (quote_plus(auth_commands.USER), quote_plus(auth_commands.check_auth()), auth_commands.HOST)
    with MongoClient(uri) as client:
        client.admin.command("ping")
        logger.debug("Good ping to the database :-)")
        db = client[name]
        collections = db.list_collection_names()
        return collections

def rm_collection(db_name, col_name):
    uri = "mongodb://%s:%s@%s" % (quote_plus(auth_commands.USER), quote_plus(auth_commands.check_auth()), auth_commands.HOST)
    with MongoClient(uri) as client:
        client.admin.command("ping")
        logger.debug("Good ping to the database :-)")
        db = client[db_name]
        collection = db[col_name]
        collection.drop()
