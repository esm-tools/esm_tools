#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
After installation, you have a new command in your path::

    esm_archive

Passing in the argument ``--help`` will show available subcommands::

    Usage: esm_archive [OPTIONS] COMMAND [ARGS]...

      Console script for esm_archiving.

    Options:
      --version             Show the version and exit.
      --write_local_config  Write a local configuration YAML file in the current
                            working directory
      --write_config        Write a global configuration YAML file in
                            ~/.config/esm_archiving/
      --help                Show this message and exit.

    Commands:
      create
      upload

To use the tool, you can first ``create`` a tar archive and then use ``upload``
to put it onto the tape server.


Creating tarballs
~~~~~~~~~~~~~~~~~

Use ``esm_archive create`` to generate tar files from an experiment::

    esm_archive create /path/to/top/of/experiment start_date end_date

The arguments ``start_date`` and ``end_date`` should take the form
``YYYY-MM-DD``. A complete example would be::

    esm_archive create /work/ab0246/a270077/from_ba0989/AWICM/LGM_6hours 1850-01-01 1851-01-01

The archiving tool will automatically pack up all files it finds matching these
dates in the ``outdata`` and ``restart`` directories and generate logs in the
top of the experiment folder. Note that the final date (1851-01-1 in this
example) is **not included**. During packing, you get a progress bar indicating
when the tarball is finished.

Please be aware that are size limits in place on DKRZ's tape server. Any tar
files **larger than 500 Gb will be trucated**. For more information, see:
https://www.dkrz.de/up/systems/hpss/hpss


Uploading tarballs
~~~~~~~~~~~~~~~~~~

A second command ``esm_archive upload`` allows you to put tarballs onto to tape server at DKRZ::

    esm_archive upload /path/to/top/of/experiment start_date end_date

The signature is the same as for the ``create`` subcommand. Note that for this
to work; you need to have a properly configured ``.netrc`` file in your home
directory::

    $ cat ~/.netrc
    machine tape.dkrz.de login a270077 password OMITTED

This file needs to be readable/writable **only** for you, e.g. ``chmod 600``.
The archiving program will then be able to automatically log into the tape
server and upload the tarballs. Again, more information about logging onto the
tape server without password authentication can be found here:
https://www.dkrz.de/up/help/faq/hpss/how-can-i-use-the-hpss-tape-archive-without-typing-my-password-every-time-e-g-in-scripts-or-jobs
"""

import sys
import os
import pprint

import click
import emoji
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from .esm_archiving import (
    archive_mistral,
    check_tar_lists,
    group_files,
    pack_tarfile,
    sort_files_to_tarlists,
    stamp_files,
    sum_tar_lists_human_readable,
)

from .database.model import Base, Experiments, Archive, Tarball, ArchivedFile

from .config import load_config, write_config_yaml

pp = pprint.PrettyPrinter(width=41, compact=True)
config = load_config()

if __name__ == "__main__":
    # DB Initializations (only when actually running):
    engine = create_engine("sqlite:///" + config["general"]["database_file"])
    Base.metadata.create_all(engine)

    Session = sessionmaker(bind=engine)


@click.group(invoke_without_command=True)
@click.version_option()
@click.pass_context
@click.option(
    "--write_local_config",
    is_flag=True,
    help="Write a local configuration YAML file in the current working directory",
)
@click.option(
    "--write_config",
    is_flag=True,
    help="Write a global configuration YAML file in ~/.config/esm_archiving/",
)
def main(ctx, write_local_config=False, write_config=False):
    """Console script for esm_archiving."""
    if ctx.invoked_subcommand is None:
        if write_config:
            click.secho("Writing global (user) configuration...")
            write_config_yaml()
        if write_local_config:
            click.secho("Writing local (experiment) configuration...")
            write_config_yaml(path=os.getcwd())
    return 0


@main.command()
@click.argument("base_dir")
@click.argument("start_date")
@click.argument("end_date")
@click.option("--force", is_flag=True)
@click.option("--interactive", is_flag=True)
def create(base_dir, start_date, end_date, force, interactive):
    session = Session()
    click.secho(
        emoji.emojize(":file_cabinet:") + " Creating archives for:", color="green"
    )
    click.secho(base_dir, color="green")
    click.secho("From: %s" % start_date, color="green")
    click.secho("To: %s" % end_date, color="green")

    exp_db = Experiments(expid=base_dir.split("/")[-1])
    if not session.query(Experiments).filter_by(expid=exp_db.expid).all():
        session.add(exp_db)
    else:
        exp_db = session.query(Experiments).filter_by(expid=exp_db.expid).all()[0]

    archive_db = Archive(exp_ref=exp_db)
    if not session.query(Archive).filter_by(exp_ref=exp_db):
        session.add(archive_db)

    for filetype in ["outdata", "restart"]:
        files = group_files(base_dir, filetype)
        files = stamp_files(files)

        files = sort_files_to_tarlists(files, start_date, end_date, config)
        existing, missing = check_tar_lists(files)
        if interactive:
            click.secho("The following files were requested and found:")
            pp.pprint(existing)
            pp.pprint(sum_tar_lists_human_readable(existing))
        if missing:
            if interactive:
                click.secho("The following files were requested but missing:")
                pp.pprint(missing)
        for model in files:
            click.secho(
                emoji.emojize(":open_file_folder: --> :package:", use_aliases=True)
                + f" Packing up files for {model} ({filetype})"
            )
            archive_name = os.path.join(
                base_dir, f"{model}_{filetype}_{start_date}_{end_date}.tgz"
            )
            tarball_db = Tarball(fname=archive_name, archive=archive_db)
            session.add(tarball_db)
            click.secho(archive_name)
            pack_tarfile(existing[model], base_dir, archive_name)
            for file in existing[model]:
                file_db = ArchivedFile(fname=file, tarball=tarball_db)
                session.add(file_db)
    session.commit()


@main.command()
@click.argument("base_dir")
@click.argument("start_date")
@click.argument("end_date")
def upload(base_dir, start_date, end_date):
    session = Session()
    click.secho(" Uploading archives for:")
    click.secho(base_dir)

    for filetype in ["outdata", "restart"]:
        files = group_files(base_dir, filetype)
        files = stamp_files(files)
        files = sort_files_to_tarlists(files, start_date, end_date, config)

        for model in files:
            archive_name = os.path.join(
                base_dir, f"{model}_{filetype}_{start_date}_{end_date}.tgz"
            )
            # tarball_db = Tarball(fname=archive_name)

            q = session.query(Tarball).filter_by(fname=archive_name)
            archive_mistral(archive_name)
            for f in q.all().pop().files:
                f.on_tape = True
    session.commit()


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
