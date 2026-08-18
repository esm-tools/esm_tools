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

from .esm_archiving import (
    archive_mistral,
    check_tar_lists,
    group_files,
    pack_tarfile,
    sort_files_to_tarlists,
    stamp_files,
    sum_tar_lists_human_readable,
)
from .spec import collect_tarballs, load_archive_specs


from .config import load_config, write_config_yaml

pp = pprint.PrettyPrinter(width=41, compact=True)
config = load_config()


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
    click.secho(" Creating archives for:", color="green")
    click.secho(base_dir, color="green")
    click.secho("From: %s" % start_date, color="green")
    click.secho("To: %s" % end_date, color="green")

    # Template-driven path: if the config gives per-model lists of ArchiveSpec,
    # find files by rendering their jinja `match` glob and pack them into
    # tarballs named by the rendered `tar_template`.
    templated = load_archive_specs(config)
    if templated:
        expid = os.path.basename(os.path.abspath(base_dir))
        for filetype in ["outdata", "restart"]:
            for model, specs in templated.items():
                tarballs = collect_tarballs(
                    base_dir, filetype, model, specs, start_date, end_date, expid
                )
                for tar_name, flist in tarballs.items():
                    archive_name = os.path.join(base_dir, tar_name + ".tgz")
                    click.secho(
                        f" Packing {tar_name} ({filetype}, {len(flist)} files)"
                    )
                    pack_tarfile(flist, base_dir, archive_name)
        return

    # Legacy heuristic path (no templated specs configured):
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
            if not existing.get(model):
                click.secho(
                    f" Nothing to pack for {model} ({filetype}) in this date"
                    " range, skipping"
                )
                continue
            click.secho(f" Packing up files for {model} ({filetype})")
            archive_name = os.path.join(
                base_dir, f"{model}_{filetype}_{start_date}_{end_date}.tgz"
            )
            click.secho(archive_name)
            pack_tarfile(existing[model], base_dir, archive_name)


@main.command()
@click.argument("base_dir")
@click.argument("start_date")
@click.argument("end_date")
def upload(base_dir, start_date, end_date):
    # NOTE: archive_mistral uploads to DKRZ HPSS (tape.dkrz.de) via pypftp; it is
    # NOT the AWI ScoutFS HSM. For AWI, create locally then push the tarballs to
    # scoutfs://hsm.dmawi.de (see fsspec-scoutfs). This command is kept working
    # for DKRZ but the on-tape bookkeeping DB was removed, so it just uploads.
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
            archive_mistral(archive_name)


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
