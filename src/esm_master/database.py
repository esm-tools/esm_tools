from sqlalchemy import create_engine, Column, Integer, String, Sequence, DateTime
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import QueuePool

import os

database_file = os.path.expanduser("~") + "/.esm_tools/esm_master.db"
if not os.path.isdir(os.path.expanduser("~") + "/.esm_tools"):
    os.mkdir(os.path.expanduser("~") + "/.esm_tools")

from esm_database import location_database

# engine = create_engine('sqlite:///' + database_file, echo = False, poolclass = NullPool)
engine = create_engine("sqlite:///" + database_file, echo=False, poolclass=QueuePool)
base = declarative_base()


class installation(base):
    __tablename__ = "installation"

    id = Column(Integer, Sequence("user_id_seq"), primary_key=True)
    timestamp = Column(DateTime)

    setup_name = Column(String)
    action = Column(String, default="download")
    folder = Column(String, default="none yet")

    location_database.register("installation", database_file, "esm_master")

    @staticmethod
    def topline():
        print()
        print("INSTALLATIONS:")
        print(
            "{0: >4}".format("ID")
            + "   "
            + "{0: >17}".format("timestamp")
            + "   "
            + "{0: >10}".format("setup")
            + "   "
            + "{0: >10}".format("action")
            + "   "
            + "{0: >45}".format("folder")
            + "   "
        )

    @staticmethod
    def nicer_output(run):
        print("ID: " + str(run.id) + ":")
        print("     Timestamp: " + run.timestamp.strftime("%x %X"))
        print("     Setup: " + run.setup_name)
        print("     Action performed: " + run.action)
        print("     Installation in folder: " + run.folder)

    def __repr__(self):
        setup = (self.setup_name[:10]) if len(self.setup_name) > 10 else self.setup_name
        return "%4s   %17s   %10s   %10s   %45s" % (
            str(self.id),
            str(self.timestamp.strftime("%x-%X")),
            setup,
            self.action,
            self.folder,
        )


base.metadata.create_all(engine)
connection = engine.connect()
Session = sessionmaker(bind=engine)
Session.configure(bind=engine)
session = Session()
