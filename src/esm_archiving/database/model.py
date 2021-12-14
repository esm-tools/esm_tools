"""
The database module for archiving.

The database extension allows you keep track of which experiments are on the
tape, which files are in which tarball, along with some experiment meta-data.
"""

from datetime import datetime

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, String, DateTime, ForeignKey, Boolean
from sqlalchemy.orm import relationship

Base = declarative_base()


class Experiments(Base):
    __tablename__ = "experiments"

    id = Column(Integer, primary_key=True)
    expid = Column(String, unique=True)
    # One to One relationship with archive:
    archive = relationship("Archive", uselist=False, back_populates="exp_ref")
    created_at = Column(DateTime, default=datetime.utcnow)

    def __repr__(self):
        str_created_at = self.created_at.strftime("%Y-%m-%d %H:%M:%S")
        return "<Image (id='%s', expid='%s', archive='%s', created_at=%s)>" % (
            self.id,
            self.expid,
            self.archive,
            str_created_at,
        )


class Archive(Base):
    __tablename__ = "archives"
    id = Column(Integer, primary_key=True)
    # One to One relationship with expid:
    expid_id = Column(Integer, ForeignKey("experiments.id"))
    exp_ref = relationship("Experiments", back_populates="archive")

    # One to Many relationship with Tarball:
    tarballs = relationship("Tarball", back_populates="archive")


class Tarball(Base):
    __tablename__ = "tarballs"
    id = Column(Integer, primary_key=True)
    fname = Column(String, unique=True)

    # Many to One relationship with Archive
    archive_id = Column(Integer, ForeignKey("archives.id"))
    archive = relationship("Archive", back_populates="tarballs")

    # One to Many relationship with Files
    files = relationship("ArchivedFile", back_populates="tarball")


class ArchivedFile(Base):
    __tablename__ = "files"
    id = Column(Integer, primary_key=True)
    tarball_id = Column(Integer, ForeignKey("tarballs.id"))
    tarball = relationship("Tarball", back_populates="files")
    fname = Column(String, unique=True)
    on_tape = Column(Boolean, default=False)
    on_disk = Column(Boolean, default=True)
