from . import database
from datetime import datetime
import sqlalchemy


def database_entry(config):
    if config.get("general", {}).get("use_database", True):
        if config["general"]["check"]:
            database_entry_check(config)
        elif config["general"]["jobtype"] == "compute":
            database_entry_start(config)
    return config


def database_basic_entry(config):
    thisrun = (
        database.session.query(database.experiment)
        .filter_by(expid=config["general"]["expid"])
        .filter_by(run_timestamp=config["general"]["run_datestamp"])
        .all()
    )

    if thisrun == []:
        thisrun = database.experiment(
            expid=config["general"]["expid"],
            setup_name=config["general"]["setup_name"],
            run_timestamp=config["general"]["run_datestamp"],
            timestamp=datetime.now(),
            outcome="None",
            exp_folder=f"{config['general']['base_dir']}/{config['general']['expid']}/",
        )
        database.session.add(thisrun)
    else:
        thisrun = thisrun[-1]
        thisrun.timestamp = datetime.now()
    return thisrun


def try_to_commit():
    try:
        database.session.commit()
    except sqlalchemy.exc.OperationalError as e:
        print("Sorry, there was some SQL Error!")
        print(e)


def database_entry_check(config):
    thisrun = database_basic_entry(config)
    thisrun.outcome = "check"
    try_to_commit()


def database_entry_start(config):
    thisrun = database_basic_entry(config)
    thisrun.outcome = "started"
    try_to_commit()


def database_entry_success(config):
    thisrun = database_basic_entry(config)
    thisrun.outcome = "success"
    try_to_commit()


def database_entry_crashed(config):
    thisrun = database_basic_entry(config)
    thisrun.outcome = "crashed"
    try_to_commit()
