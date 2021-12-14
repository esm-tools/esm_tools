from sqlalchemy import create_engine, Column, Integer, String, Sequence, DateTime
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

import os

main_database_file = os.path.expanduser("~") + "/.esm_tools/esmtools.db"
if not os.path.isdir(os.path.expanduser("~") + "/.esm_tools"):
    os.mkdir(os.path.expanduser("~") + "/.esm_tools")

engine = create_engine("sqlite:///" + main_database_file, echo=False)
base = declarative_base()


class database_location(base):
    __tablename__ = "locations"

    id = Column(Integer, Sequence("user_id_seq"), primary_key=True)
    table_name = Column(String)
    location = Column(String)
    class_in = Column(String)

    @staticmethod
    def topline():
        print()
        print("Databases:")
        print(
            "{0: >4}".format("ID")
            + "   "
            + "{0: >17}".format("table_name")
            + "   "
            + "{0: >60}".format("location")
            + "   "
            + "{0: >17}".format("class_in")
        )

    def __repr__(self):
        return "%4s   %17s   %85s   %17s" % (
            str(self.id),
            str(self.table_name),
            str(self.location),
            str(self.class_in),
        )


base.metadata.create_all(engine)
connection = engine.connect()
Session = sessionmaker(bind=engine)
Session.configure(bind=engine)
session = Session()


def register(table_name, given_location, class_in):
    loc_id = 0
    action = "new"
    if os.path.isfile(main_database_file):
        all_locations = session.query(database_location).all()

        for location in all_locations:
            if location.table_name == table_name:
                if location.location == given_location:
                    action = "skip"
                    break
                else:
                    loc_id = location.id
                    action = "update"
                    break

    if action == "new":
        new_loc = database_location(
            table_name=table_name, location=given_location, class_in=class_in
        )
        session.add(new_loc)
        session.commit()
    elif action == "update":
        loc = session.query(database_location).filter_by(id=loc_id).first()
        loc.location = given_location
        session.commit()
