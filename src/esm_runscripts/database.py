from sqlalchemy import create_engine, Column, Integer, String, Sequence, DateTime
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

import os

# database_file = os.path.dirname(os.path.abspath(__file__)) + "/../database/esm_runscripts.db"
database_file = os.path.expanduser("~") + "/.esm_tools/esm_runscripts.db"
if not os.path.isdir(os.path.expanduser("~") + "/.esm_tools"):
    os.mkdir(os.path.expanduser("~") + "/.esm_tools")

from esm_database import location_database

engine = create_engine("sqlite:///" + database_file, echo=False)
base = declarative_base()


class experiment(base):
    __tablename__ = "experiment"

    id = Column(Integer, Sequence("user_id_seq"), primary_key=True)
    expid = Column(String)
    timestamp = Column(DateTime)
    runtime = Column(String, default="00:15:00")
    run_timestamp = Column(String, default="00000000-00000000")
    setup_name = Column(String)
    outcome = Column(String, default="crashed")
    cpuh = Column(Integer, default=0)
    gb = Column(Integer, default=0)
    exp_folder = Column(String, default="none yet")
    archive_folder = Column(String, default="none yet")

    location_database.register("experiment", database_file, "esm_runscripts")

    @staticmethod
    def topline():
        print()
        print("EXPERIMENTS:")
        print(
            "{0: >4}".format("ID")
            + "   "
            + "{0: >17}".format("timestamp")
            + "   "
            + "{0: >15}".format("runtime")
            + "   "
            + "{0: >15}".format("expid")
            + "   "
            + "{0: >17}".format("run_time_stamp")
            + "   "
            + "{0: >10}".format("setup")
            + "   "
            + "{0: >5}".format("outcome")
            + "   "
            + "{0: >7}".format("CPUh")
            + "   "
            + "{0: >7}".format("GB")
            + "   "
        )
        print("{0: >126}".format("(this run)   (total)"))

    @staticmethod
    def nicer_output(run):
        print("ID: " + str(run.id) + ", EXPID: " + str(run.expid) + ":")
        print("     Timestamp: " + run.timestamp.strftime("%x %X"))
        print("     Runtime: " + run.runtime)
        print("     Setup: " + run.setup_name)
        print("     Model Run Time: " + str(run.run_timestamp))
        print("     Outcome of run: " + run.outcome)
        print("     Used CPUh of this run: " + str(run.cpuh))
        print("     Used disk space of the whole experiment: " + str(run.gb) + " GB")
        print("     Results in folder: " + run.exp_folder)
        print("     Archived results in folder: " + run.archive_folder)

    def __repr__(self):
        setup = (self.setup_name[:10]) if len(self.setup_name) > 10 else self.setup_name
        return "%4s   %17s   %15s   %15s   %9s   %10s   %7s   %7s   %7s" % (
            str(self.id),
            str(self.timestamp.strftime("%x-%X")),
            str(self.runtime),
            str(self.expid),
            str(self.run_timestamp),
            setup,
            self.outcome,
            str(self.cpuh),
            str(self.gb),
        )


base.metadata.create_all(engine)
connection = engine.connect()
Session = sessionmaker(bind=engine)
Session.configure(bind=engine)
session = Session()
