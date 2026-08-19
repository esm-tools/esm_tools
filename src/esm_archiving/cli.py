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
        arch_dir = os.path.join(base_dir, config.get("archive_dir", "archive"))
        os.makedirs(arch_dir, exist_ok=True)
        for filetype in ["outdata", "restart"]:
            for model, specs in templated.items():
                tarballs = collect_tarballs(
                    base_dir, filetype, model, specs, start_date, end_date, expid
                )
                for tar_name, flist in tarballs.items():
                    archive_name = os.path.join(arch_dir, tar_name + ".tgz")
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
@click.argument("base_dir", default=".", required=False)
@click.option(
    "--to", "dest", default=None,
    help="HSM target directory; overrides the hsm_target config key.",
)
def upload(base_dir, dest):
    """Push the tarballs in <base_dir>/archive/ to the AWI HSM via ScoutFS.

    Destination resolution: --to  >  config `hsm_target` (a jinja template, e.g.
    "/hs/projects/paleodyn/from_experiments/{{ expid }}"). Files are NOT released
    from the online cache here (they are not archdone until the HSM archiver has
    run) — use hsm-release.sh for that.
    """
    import glob as _glob

    expid = os.path.basename(os.path.abspath(base_dir))
    arch_dir = os.path.join(base_dir, config.get("archive_dir", "archive"))
    tarballs = sorted(_glob.glob(os.path.join(arch_dir, "*.tgz")))
    if not tarballs:
        click.secho(f"No tarballs in {arch_dir} - run `esm_archive create` first.")
        return

    if dest is None:
        target = config.get("hsm_target")
        if not target:
            raise click.ClickException(
                "No destination: pass --to, or set `hsm_target` in the config."
            )
        from jinja2 import Template

        dest = Template(target).render(expid=expid, model=config.get("model"))

    protocol = config.get("hsm_protocol")
    if not protocol:
        raise click.ClickException("Set `hsm_protocol` in the config (e.g. scoutfs).")
    host = config.get("hsm_host")
    if not host:
        raise click.ClickException("Set `hsm_host` in the config (e.g. hsm.dmawi.de).")
    click.secho(f" Uploading {len(tarballs)} tarball(s) to {host}:{dest}", color="green")

    if protocol in ("scoutfs", "sftp", "ssh"):
        # ssh-based: transfer with rsync (paramiko's SFTP tops out ~1-10 MB/s;
        # rsync over ssh is ~100x that and resumes/verifies). Its --info=progress2
        # bar renders straight to the terminal (stdout inherited).
        import shlex
        import subprocess

        ssh_cmd = "ssh"
        key = config.get("hsm_ssh_key")
        if key:
            ssh_cmd += " -i " + shlex.quote(os.path.expanduser(key))
        subprocess.run([*shlex.split(ssh_cmd), host, "mkdir", "-p", dest], check=True)
        result = subprocess.run(
            [
                "rsync", "-a", "--partial", "--append-verify", "-h",
                "--info=progress2,name,stats2",
                "-e", ssh_cmd, *tarballs, f"{host}:{dest.rstrip('/')}/",
            ]
        )
        if result.returncode != 0:
            raise click.ClickException(f"rsync failed (exit {result.returncode})")
        total = sum(os.path.getsize(t) for t in tarballs)
        click.secho(
            f" {len(tarballs)} tarball(s), {total / 2 ** 30:.1f} GiB, now under {dest}",
            color="green",
        )
    else:
        # Non-ssh backend (s3, ...): stream via fsspec with a tqdm bar.
        try:
            import fsspec
            from fsspec.callbacks import TqdmCallback
        except ImportError:
            raise click.ClickException("fsspec is required to upload (pip install it).")
        storage_options = dict(config.get("hsm_storage_options") or {})
        fs = fsspec.filesystem(protocol, **storage_options)
        fs.makedirs(dest, exist_ok=True)
        for tarball in tarballs:
            name = os.path.basename(tarball)
            remote = dest.rstrip("/") + "/" + name
            local_size = os.path.getsize(tarball)
            callback = TqdmCallback(
                tqdm_kwargs={"desc": name, "unit": "B", "unit_scale": True,
                             "unit_divisor": 1024}
            )
            callback.set_size(local_size)
            with open(tarball, "rb") as src, fs.open(remote, "wb") as dst:
                while True:
                    data = src.read(8 * 1024 * 1024)
                    if not data:
                        break
                    dst.write(data)
                    callback.relative_update(len(data))
            callback.close()
            if fs.info(remote).get("size") != local_size:
                raise click.ClickException(f"size mismatch for {name}")
    click.secho(
        " Upload complete. Files sit in the online cache until the HSM archiver "
        "copies them to tape; release with hsm-release.sh.",
        color="green",
    )


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
