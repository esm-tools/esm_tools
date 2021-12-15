#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

# -*- coding: utf-8 -*-

# ppftp.py

"""
python interface to HPSS's pftp

"""

import os
import re
import sys
import time
import netrc
import ftplib
import getpass
import fnmatch
import posixpath
import subprocess as sp

from collections import defaultdict
from concurrent.futures import ProcessPoolExecutor


HOST = "tape.dkrz.de"
PORT = 4021


__all__ = ["upload", "download", "Pftp"]


def connect(username=None, password=None):

    ftp_obj = ftplib.FTP()
    ftp_obj.connect(HOST, PORT)

    username = username or getpass.getuser()
    if password is None:
        try:
            n = netrc.netrc()
            username, account, password = n.authenticators(HOST)
        except netrc.NetrcParseError as e:
            print("Could not parse .netrc file: %s", e)
            exit(1)
        except IOError as e:
            print("I/O error: {0}".format(e))
            exit(1)

    ftp_obj.login(username, password)
    return ftp_obj


_msg_re = re.compile("^[0-9]+-?\s?[A-Za-z].*").match


def filter_msg(lines):
    try:
        lines = lines.splitlines()
    except AttributeError:
        pass
    for line in lines:
        if line and not _msg_re(line):
            yield line


def msgs(lines):
    try:
        lines = lines.splitlines()
    except AttributeError:
        pass
    for line in lines:
        if _msg_re(line):
            yield line


class Pftp(object):
    HOST = HOST
    PORT = PORT

    def __init__(self, username=None, password=None):
        self.username = username = username or getpass.getuser()
        self.ftp_obj = connect(username, password)
        self._auth = {"username": username, "password": password}

    def __repr__(self):
        is_active = self.is_connected()
        status = "alive" if is_active else "stale"
        s = (
            "<{0.__class__.__name__} connected to "
            "{0.HOST}:{0.PORT} "
            "as {0.username}>"
        )
        s = s.format(self)
        s += "; connection={}".format(status)
        return repr(s)

    def is_connected(self):
        "check if the connection is still active"
        try:
            self.ftp_obj.voidcmd("NOOP")
        except:
            return False
        return True

    def reconnect(self):
        "reconnects to the ftp server"
        try:
            self.ftp_obj.close()
        except:
            pass
        self.ftp_obj = connect(self._auth["username"], self._auth["password"])

    def quit(self):
        self.ftp_obj.quit()

    def close(self):
        self.quit()
        self.ftp_obj.close()

    def pwd(self):
        "present working directory"
        return self.ftp_obj.pwd()

    def cwd(self, path):
        "change working directory"
        try:
            if path.startswith(self.pwd()):
                path = path.replace(self.pwd(), "").lstrip("/")
            self.ftp_obj.cwd(path)
        except:
            raise ValueError("Path does not exists: " + path)

    def exists(self, path):
        "check if a path exists"
        try:
            result = self.stat(path)
        except:
            result = None
        return True if result else False

    def stat(self, pathname):
        "Returns stat of the path"
        try:
            _stat = self.ftp_obj.sendcmd("STAT {}".format(pathname))
        except ftplib.error_perm:
            return None
        return list(filter_msg(_stat))

    def size(self, pathname):
        "Returns size of path in bytes"
        return int(self.stat(pathname)[0].split()[6])

    def isfile(self, pathname):
        "Returns true if pathname refers to an existing file"
        _stat = self.stat(pathname)
        if _stat is None:
            raise ValueError("path does not exists %s" % pathname)
        line = _stat.pop(0)
        if line[0] == "-":
            return True
        if line[0] == "l":
            pwd = self.pwd()
            name = line.split()[-1]
            _flag = False
            try:
                self.cwd(name)
            except:
                _flag = True
            finally:
                self.cwd(pwd)
            return _flag
        return False

    def isdir(self, pathname):
        "Returns true if pathname refers to an existing directory"
        _stat = self.stat(pathname)
        if _stat is None:
            raise ValueError("path does not exists %s" % pathname)
        line = _stat.pop(0)
        if line[0] == "d":
            return True
        if line[0] == "l":
            pwd = self.pwd()
            name = line.split()[-1]
            try:
                self.cwd(name)
                _flag = True
            except ftplib.error_perm:
                _flag = False
            finally:
                self.cwd(pwd)
            return _flag
        return False

    def islink(self, pathname):
        _stat = self.stat(pathname)
        if _stat is None:
            raise ValueError("path does not exists %s" % pathname)
        line = _stat.pop(0)
        if line[0] == "l":
            return True
        return False

    def listing(self, path=None):
        "list directory contents"
        path = path or self.pwd()
        return self.ftp_obj.nlst(path)

    listdir = listing

    def listing2(self, path=None):
        'directory listing in long form. similar to "ls -l"'
        path = path or self.pwd()
        tmp = []
        self.ftp_obj.dir(path, tmp.append)
        return tmp

    def mlsd(self, path):
        cmd = "MLSD %s" % path
        lines = []
        self.ftp_obj.retrlines(cmd, lines.append)
        CLRF = self.ftp_obj
        for line in lines:
            facts, _, name = line.rstrip().partition(" ")
            entry = {}
            for fact in facts[:-1].split(";"):
                key, _, value = fact.partition("=")
                entry[key.lower()] = value
            yield (name, entry)

    def files(self, path=None):
        "gather files at the given path"
        path = path or self.pwd()
        result = []
        for entry in self.listing2(path):
            name = entry.split()[-1]
            if entry.startswith("-"):
                result.append(name)
            if entry.startswith("l"):
                if self.isfile(name):
                    result.append(name)
        return result

    def directories(self, path=None):
        "gather directories at the given path"
        path = path or self.pwd()
        result = []
        for entry in self.listing2(path):
            name = entry.split()[-1]
            if entry.startswith("d"):
                result.append(name)
            if entry.startswith("l"):
                if self.isdir(name):
                    result.append(name)
        return result

    def walk(self, path=None):
        "recursively walk the directory tree from the given path. Similar to os.walk"
        path = path or self.pwd()
        _files = self.files(path)
        _subdirs = self.directories(path)
        yield (path, _subdirs, _files)
        for subdir in _subdirs:
            for x in self.walk(subdir):
                yield x

    def walk_for_files(self, path=None):
        "recursively gather files"
        for root, _dirs, _files in self.walk(path):
            for f in _files:
                yield f

    def walk_for_directories(self, path=None):
        "recursively gather directories"
        for root, _dirs, _files in self.walk(path):
            for d in _dirs:
                yield d

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    # ========== MANIPULATIONS ==========

    def mkdir(self, path):
        if not self.exists(path):
            print("Created directory: " + path)
            self.ftp_obj.mkd(path)

    def makedirs(self, path):
        """Recursively create dirs as required walking up to an existing
        parent dir"""
        try:
            _isdir = self.isdir(path)
        except ValueError:
            pass
        else:
            if _isdir:
                return
        dirname = posixpath.dirname(path)
        if dirname:
            self.makedirs(dirname)
        self.mkdir(path)
        return

    def rmdir(self, path):
        "remove directory"
        if self.exists(path):
            self.ftp_obj.rmd(path)
            print("Removed directory: " + path)
        return

    def remove(self, filename):
        if self.exists(filename):
            self.ftp_obj.delete(filename)
            print("Deleted file: " + filename)
        return

    def removedirs(self, path):
        remove = self.remove
        rmdir = self.rmdir
        path = posixpath.normpath(path)
        dirs = set()
        for topdir, subdirs, files in self.walk(path):
            for _file in files:
                remove(_file)
            if subdirs:
                dirs.update(subdirs)
            dirs.add(topdir)
        dirs = sorted(dirs, key=lambda x: len(x.split(posixpath.sep)), reverse=True)
        for d in dirs:
            rmdir(d)
        return None

    def rename(self, from_name, to_name):
        if os.path.dirname(to_name) and (
            os.path.dirname(from_name) != os.path.dirname(to_name)
        ):
            raise ValueError("Must rename a file within the same directory")
        self.ftp_obj.rename(from_name, to_name)
        return to_name

    # ========== FILE-TRANSFERS ==========

    @staticmethod
    def upload(source, destination):
        "uses pftp binary for transfering the file"
        return upload(source, destination)

    @staticmethod
    def download(source, destination):
        "uses pftp binary for transfering the file"
        return download(source, destination)


def printProgressBar(
    iteration, total, prefix="", suffix="", decimals=1, length=80, fill=u"+"
):
    "https://stackoverflow.com/questions/3173320/text-progress-bar-in-the-console"
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + "-" * (length - filledLength)
    print("\r%s |%s| %s%% %s" % (prefix, bar, percent, suffix), end="\r")
    # Print New Line on Complete
    # if iteration == total:
    #    print()
    return


def _transfer(cmdlines):
    client = sp.Popen("pftp", stdin=sp.PIPE, stdout=sp.PIPE, stderr=sp.PIPE)
    o, e = client.communicate(cmdlines.encode("utf-8"))
    # for line in o:
    #    print(line)
    return


def _file_stripe_count(filename):
    cmd = ["lfs", "getstripe", "-c", filename]
    return int(sp.check_output(cmd))


def _pftp_upload_commands(source, destination):
    source_filename = os.path.basename(source)
    source_dirname = os.path.dirname(source) or os.curdir
    cmd = ["prompt"]
    stripe_count = _file_stripe_count(source)
    if stripe_count > 1:
        if stripe_count > 8:
            stripe_count = 8  # limit to 8 parallel tasks
        cmd.append("setpwidth {}".format(stripe_count))
        cmd.append("setpblocksize {}".format(1024 * 1024))  # 1MB
    if source_dirname != os.curdir:
        cmd.append("lcd {}".format(source_dirname))
    cmd.append("cd {}".format(destination))
    cmd.append("put {}".format(source_filename))
    cmd.append("bye")
    cmd = "\n".join(cmd)
    return cmd


def upload(source, destination):
    source = source.rstrip("/")
    destination = destination.rstrip("/")
    source_filename = os.path.basename(source)
    destination_maybe_filename = os.path.basename(destination)
    destination_dirname = os.path.dirname(destination)

    p = Pftp()

    requires_renaming = False
    if not p.exists(destination):
        if source_filename == destination_maybe_filename:
            destination = destination_dirname
        elif os.path.splitext(destination_maybe_filename)[-1]:
            destination = destination_dirname
            requires_renaming = True

    destination_filepath = os.path.join(destination, source_filename)

    if not p.exists(destination):
        p.makedirs(destination)

    source_filesize = os.stat(source).st_size

    def get_filesize(filepath):
        if not p.is_connected():
            p.reconnect()
        if not p.exists(filepath):
            return 0
        return p.size(filepath)

    cmdlines = _pftp_upload_commands(source, destination)
    pool = ProcessPoolExecutor(1)
    fut = pool.submit(_transfer, cmdlines)

    # print('Uploading file: {}'.format(source_filename))
    while fut.running():
        time.sleep(1)
        current_size = get_filesize(destination_filepath)
        # if not current_size:
        #    print('\rWaiting for tape to respond...', end='\r')
        # else:
        #    printProgressBar(current_size, source_filesize, source_filename)
        printProgressBar(current_size, source_filesize, source_filename)

    fut.result()
    if requires_renaming:
        p.cwd(destination_dirname)
        p.rename(source_filename, destination_maybe_filename)

    try:
        p.close()
    except:
        pass

    if requires_renaming:
        return os.path.join(destination, destination_maybe_filename)
    return os.path.join(destination, source_filename)


def _pftp_download_commands(source, destination):
    source_filename = os.path.basename(source)
    source_dirname = os.path.dirname(source) or os.curdir
    cmd = ["prompt"]
    cmd.append("cd {}".format(source_dirname))
    if source_dirname != os.curdir:
        cmd.append("lcd {}".format(destination))
    cmd.append("get {}".format(source_filename))
    cmd.append("bye")
    cmd = "\n".join(cmd)
    return cmd


def download(source, destination):
    source = source.rstrip("/")
    destination = destination.rstrip("/")
    source_filename = os.path.basename(source)
    destination_maybe_filename = os.path.basename(destination)
    destination_dirname = os.path.dirname(destination)

    requires_renaming = False
    if not os.path.exists(destination):
        if source_filename == destination_maybe_filename:
            destination = destination_dirname
        elif os.path.splitext(destination_maybe_filename)[-1]:
            destination = destination_dirname
            requires_renaming = True

    destination_filepath = os.path.join(destination, source_filename)

    if not os.path.exists(destination):
        try:
            os.makedirs(destination)
        except:
            pass

    with Pftp() as p:
        source_filesize = p.size(source)

    def get_filesize(filepath):
        if not os.path.exists(filepath):
            return 0
        return os.path.getsize(filepath)

    cmdlines = _pftp_download_commands(source, destination)
    pool = ProcessPoolExecutor(1)
    fut = pool.submit(_transfer, cmdlines)

    # print('Downloading file: {}'.format(source_filename))
    while fut.running():
        time.sleep(1)
        current_size = get_filesize(destination_filepath)
        printProgressBar(
            current_size, source_filesize, os.path.basename(destination_filepath)
        )

    fut.result()
    if requires_renaming:
        curdir = os.path.abspath(os.curdir)
        os.chdir(destination_dirname)
        os.rename(source_filename, destination_maybe_filename)
        os.chdir(curdir)

    if requires_renaming:
        return os.path.join(destination, destination_maybe_filename)
    return os.path.join(destination, source_filename)


def command_line_interface():
    import click

    wildcard_check = re.compile("[*?[]")

    def has_wildcard(path):
        return wildcard_check.search(path) is not None

    class WildcardInDirectoryPath(click.ParamType):
        name = "PathWildcard"

        def convert(self, value, param, ctx):
            try:
                dirname = os.path.dirname(value)
                if has_wildcard(dirname):
                    raise ValueError
                return value
            except ValueError:
                msg = "Wildcard only in file names are allowed {}".format(value)
                self.fail(msg, param, ctx)

    WILDCARD_PATH = WildcardInDirectoryPath()

    def sizeof_fmt(num, suffix="B"):
        for unit in ("", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"):
            if abs(num) < 1024.0:
                return "%3.1f%s%s" % (num, unit, suffix)
            num /= 1024.0
        return "%.1f%s%s" % (num, "Yi", suffix)

    @click.group()
    def main():
        """A command line interface for HPSS (tape archive)."""

    @main.command("download", short_help="Download files from tape")
    @click.argument("src", nargs=-1, type=WILDCARD_PATH)
    @click.argument("dst", nargs=1, type=click.Path())
    def _download(src, dst):
        """Download files from tape.

        \b
        SRC: file on tape
        DST: path on mistral
        """
        p = Pftp()
        files = []
        for entry in src:
            basename = os.path.basename(entry)
            if has_wildcard(basename):
                dirname = os.path.dirname(entry)
                contents = p.listdir(dirname)
                for fullname in contents:
                    if fnmatch.fnmatch(os.path.basename(fullname), basename):
                        files.append(fullname)
            else:
                if p.exists(entry):
                    files.append(entry)
        total_size = sum(map(p.size, files))
        nfiles = len(files)
        p.close()

        click.echo(
            "{} files to download. Total size: {}".format(
                nfiles, sizeof_fmt(total_size)
            )
        )
        width = str(len(str(nfiles)))
        msg = "[{:>" + width + "}/{:>" + width + "}]"
        for index, fname in enumerate(files, 1):
            downloaded_file = download(fname, dst)
            count = msg.format(index, nfiles)
            click.echo(count + " Downloaded: " + downloaded_file + "  ")

    @main.command("upload", short_help="Upload files to tape")
    @click.argument("src", nargs=-1, type=click.Path())
    @click.argument("dst", nargs=1, type=click.Path())
    def _upload(src, dst):
        """Upload files to tape.

        \b
        SRC: file on mistral
        DST: path on tape
        """
        nfiles = len(src)
        total_size = sum(map(os.path.getsize, src))
        width = str(len(str(nfiles)))
        msg = "[{:>" + width + "}/{:>" + width + "}]"
        click.echo(
            "{} files to upload. Total size: {}".format(nfiles, sizeof_fmt(total_size))
        )
        for index, fname in enumerate(src, 1):
            uploaded_file = upload(fname, dst)
            count = msg.format(index, nfiles)
            click.echo(count + " Uploaded: " + uploaded_file + "  ")

    '''
    @main.command()
    @click.option('-l', default=False, is_flag=True, help="long listing")
    @click.argument('path', type=WILDCARD_PATH)
    def ls(path, l):
        """List directory contents on tape.

        \b
        PATH: path on tape
        """
        p = Pftp()
        if has_wildcard(path):
            listings = p.listdir(os.path.dirname(path))
            files = fnmatch.filter(listings, path)
            if l:
                listings2 = p.listing2(os.path.dirname(path))
                files = [long_path for short_path, long_path in zip(listings, listings2)
                         if short_path in files]
            for fname in files:
                click.echo(fname)
        else:
            if not p.exists(path):
                raise ValueError('Path does not exists')
                # click.echo('Path not found...')
            if l:
                contents = p.listing2(path)
            else:
                contents = p.listdir(path)
            for line in contents:
                click.echo(line)
        p.close()
        return
    '''

    @main.command()
    @click.option("-l", default=False, is_flag=True, help="long listing")
    @click.option("-r", default=False, is_flag=True, help="reverse listing")
    @click.option("-t", default=False, is_flag=True, help="sort by mtime")
    @click.option("-h", default=False, is_flag=True, help="human readable size")
    @click.argument("path", type=click.Path())
    def ls(path, l, r, t, h):
        """List directory contents on tape.

        \b
        PATH: path on tape
        """

        def sanitize_path(path):
            return path.split(None, 8)[-1].split(None)[0]

        p = Pftp()
        result = p.listing2(path)
        aux = {}
        names = dict(zip(list(map(sanitize_path, result)), result))
        paths = defaultdict(list)
        for name in names:
            paths[os.path.dirname(name)].append(name)
        for dname, _files in paths.items():
            mlist = dict(list(p.mlsd(dname)))
            for ff in _files:
                entry = mlist[ff]
                aux[names[ff]] = int(entry.get("size", 0)), entry.get("modify", "")
        if t:
            result = sorted(result, key=lambda row: aux[row][-1])
        if not r:
            result = list(reversed(result))
        if h:
            tmp = []
            for line in result:
                size = aux[line][0]
                parts = line.split(None, 8)
                parts[4] = sizeof_fmt(size)
                tmp.append("  ".join(parts))
            result = tmp[:]
        if not l:
            result = [row.split(None, 8)[-1] for row in result]
        for row in result:
            click.echo(row)
        p.close()
        return

    @main.command()
    @click.argument("path")
    def exists(path):
        """check if PATH exists on tape.

        \b
        PATH: path on tape
        """
        with Pftp() as p:
            if not p.exists(path):
                click.echo("False")
            else:
                click.echo("True")

    @main.command()
    @click.option("-h", is_flag=True, help="human readable format")
    @click.argument("path")
    def size(path, h):
        """Prints file size in bytes. Use -h option for human readable format.

        \b
        PATH: path on tape
        """

        with Pftp() as p:
            if not p.exists(path):
                click.echo("File not found")
            else:
                fsize = p.size(path)
                if h:
                    fsize = sizeof_fmt(fsize)
                click.echo("{}".format(fsize))

    @main.command()
    @click.argument("path")
    def mkdir(path):
        """creates PATH on tape.

        \b
        PATH: path on tape
        """
        with Pftp() as p:
            if p.exists(path):
                click.echo("Already exists")
            else:
                p.makedirs(path)

    @main.command()
    @click.argument("path")
    def isdir(path):
        """checks if PATH is a directory.

        \b
        PATH: path on tape
        """
        with Pftp() as p:
            if not p.exists(path):
                click.echo("path not found")
            elif p.isdir(path):
                click.echo("True")
            else:
                click.echo("False")

    @main.command()
    @click.argument("path")
    def isfile(path):
        """checks if PATH is a file.

        \b
        PATH: path on tape
        """
        res = 1
        with Pftp() as p:
            if not p.exists(path):
                click.echo("path not found")
            elif p.isfile(path):
                click.echo("True")
                res = 0
            else:
                click.echo("False")
        sys.exit(res)

    @main.command()
    @click.argument("path")
    def islink(path):
        """checks if PATH is a link.

        \b
        PATH: path on tape
        """
        with Pftp() as p:
            if not p.exists(path):
                click.echo("path not found")
            elif p.islink(path):
                click.echo("True")
            else:
                click.echo("False")

    def pyversion_check():
        import os

        supported_versions = ("python/3.5.2", "python/2.7.12")
        modules = os.environ.get("LOADEDMODULES", "")
        for version in supported_versions:
            if version in modules:
                break
        else:
            pyversion = "".join([v for v in modules.split(":") if "python" in v])
            msg = """\nLOADED VERSION: {}
SUPPORTED VERSIONS: {}
USE "module load" COMMAND TO LOAD A SUPPORTED PYTHON VERSION.
""".format(
                pyversion, " or ".join(supported_versions)
            )
            import warnings

            warnings.warn(msg, RuntimeWarning, stacklevel=2)

    # pyversion_check()
    main()


if __name__ == "__main__":
    command_line_interface()
