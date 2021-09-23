#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""This is the ``esm_archiving`` module."""

# Standard Library:
import logging
import os
import re
import subprocess
import sys
import tarfile

# Third-Party Libraries:
import pandas as pd

# This Library
from esm_archiving.external.pypftp import Pftp


class DatestampLocationError(Exception):
    pass


def query_yes_no(question, default="yes"):  # pragma: no cover
    """Ask a yes/no question via ``input()`` and return their answer.

    "question" is a string that is presented to the user.
    "default" is the presumed answer if the user just hits <Enter>.

    It must be "yes" (the default), "no" or None (meaning an answer is
    required of the user).

    The "answer" return value is True for "yes" or False for "no".

    Note: Shamelessly stolen from StackOverflow It's not hard to implement, but
    Paul is lazy...

    Parameters
    ----------
    question : str
        The question you'd like to ask the user
    default : str
        The presumed answer for ``question``. Defaults to "yes".

    Returns
    -------
    bool :
        True if the user said yes, False if the use said no.
    """
    valid = {"yes": True, "y": True, "ye": True, "no": False, "n": False}
    if default is None:
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        sys.stdout.write(question + prompt)
        choice = input().lower()
        if default is not None and choice == "":
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes' or 'no' " "(or 'y' or 'n').\n")


def find_indices_of(char, in_string):
    """
    Finds indicies of a specific character in a string

    Parameters
    ----------
    char : str
        The character to look for
    in_string : str
        The string to look in

    Yields
    ------
    int
        Each round of the generator gives you the next index for the desired
        character.
    """
    index = -1
    while True:
        index = in_string.find(char, index + 1)
        if index == -1:
            break
        yield index


def group_indexes(index_list):
    """
    Splits indexes into tuples of monotonically ascending values.

    Parameters
    ----------
    list :
        The list to split up

    Returns
    -------
    list :
        A list of tuples, so that you can get only one group of ascending
        tuples.

    Example
    -------
    >>> indexes = [0, 1, 2, 3, 12, 13, 15, 16]
    >>> group_indexes(indexes)
    [(0, 1, 2, 3), (12, 13), (15, 16)]
    """
    rlist, group = [], []
    for idx, i in enumerate(index_list):
        group.append(i)
        try:
            if i + 1 != index_list[idx + 1]:
                rlist.append(tuple(group))
                group = []
        except IndexError:
            if idx == len(index_list) - 1:
                if i - 1 == index_list[idx - 1]:
                    rlist.append(tuple(group))
                else:
                    rlist.append((i,))
    return rlist


# PLAN
# Go through the experiment and determine the filetypes
# For each filetype, build file groups
def group_files(top, filetype):
    """
    Generates quasi-regexes for a specific filetype, replacing all numbers with
    #.

    Parameters
    ----------
    top : str
        Where to start looking (this should normally be top of the experiment)
    filetype : str
        Which files to go through (e.g. outdata, restart, etc...)

    Returns
    -------
    dict
        A dictonary containing keys for each folder found in ``filetype``, and
        values as lists of files with strings where numbers are replaced by #.
    """
    model_dirs = _generate_model_dirs(top, filetype)
    model_files = {model: None for model in model_dirs}
    for model_dir in model_dirs:
        cleaned_filepatterns = _walk_through_model_dir_clean_numbers(
            os.path.join(top, filetype, model_dir)
        )
        model_files[model_dir] = cleaned_filepatterns
    return model_files


def _generate_model_dirs(top, filetype):
    model_dirs = [
        model
        for model in os.listdir(os.path.join(top, filetype))
        if os.path.isdir(os.path.join(top, filetype, model))
    ]
    return model_dirs


def _clean_numbers_nonlonely(s):
    characters = list(s)
    number_replacements = []
    for idx, char in enumerate(characters):
        if idx == 0:
            if char.isdigit() and characters[1].isdigit():
                number_replacements.append(idx)
            continue
        while idx < len(characters) - 1:
            prev_char = characters[idx - 1]
            next_char = characters[idx + 1]
            if char.isdigit() and (prev_char.isdigit() or next_char.isdigit()):
                number_replacements.append(idx)
            break
        if idx == len(characters) - 1:
            prev_char = characters[idx - 1]
            if prev_char.isdigit() and char.isdigit():
                number_replacements.append(idx)
    rlist = []
    for idx, char in enumerate(characters):
        if idx in number_replacements:
            rlist.append("#")
        else:
            rlist.append(char)
    return "".join(rlist)


def _walk_through_model_dir_clean_numbers(top, allow_single_numbers=False):
    cleaned_filepatterns = []
    for root, _, files in os.walk(top):
        for file in files:
            if allow_single_numbers:
                filepattern = "".join([r"#" if c.isdigit() else c for c in file])
            else:
                filepattern = _clean_numbers_nonlonely(file)
            filepattern = os.path.join(root, filepattern)
            if filepattern not in cleaned_filepatterns:
                cleaned_filepatterns.append(filepattern)
    return cleaned_filepatterns


def purify_expid_in(model_files, expid, restore=False):
    """
    Puts or restores >>>EXPID<<< marker in filepatterns

    Parameters
    ----------
    model_files : dict
        The model files for archiving
    expid : str
        The experiment ID to purify or restore
    restore : bool
        Set experiment ID back from the temporary marker

    Returns
    -------
    dict :
        Dictionary containing keys for each model, values for file patterns
    """
    for model in model_files:
        for idx, filepattern in enumerate(model_files[model]):
            try:
                if restore:
                    new_filepattern = filepattern.replace(">>>EXPID<<<", expid)
                else:
                    new_filepattern = filepattern.replace(expid, ">>>EXPID<<<")
                model_files[model][idx] = new_filepattern
            except KeyError:
                logging.debug("Can't replace experiment id for: %s", filepattern)
    return model_files


def stamp_files(model_files):
    """
    Given a sttandard file dictioanry (keys: model names, values: filepattern);
    figures out where the date probably is, and replaces the ``#`` sequence
    with a >>>DATE<<< stamp.

    Parameters
    ----------
    model_files : dict
        Dictionary of keys (model names) where values are lists of files for
        each model.

    Returns
    -------
    dict :
        As the input, but replaces the filepatterns with the >>>DATE<<< stamp.
    """
    for model in model_files:
        idx_modifier = 0
        for idx, filepattern in enumerate(model_files[model]):
            idx += idx_modifier
            try:
                stamped_filepattern = stamp_filepattern(filepattern)
                logging.debug(f"Adding [{model}][{idx}] = {stamped_filepattern}")
                model_files[model][idx] = stamped_filepattern
            except AssertionError:  # List was longer than 1
                stamped_filepatterns = stamp_filepattern(filepattern, force_return=True)
                logging.debug(stamped_filepatterns)
                idx_modifier += len(stamped_filepatterns)
                for idx_modifier, stamped_filepattern in enumerate(
                    stamped_filepatterns
                ):
                    idx += idx_modifier
                    logging.debug(f"Adding [{model}][{idx}] = {stamped_filepattern}")
                    model_files[model][idx] = stamped_filepattern
                continue  # needed??
    return model_files


def get_list_from_filepattern(filepattern):
    dirname = os.path.dirname(filepattern)
    regex_files = re.compile(os.path.basename(filepattern).replace("#", "\d"))  # noqa
    matching_files = sorted(
        [
            os.path.join(dirname, file)
            for file in os.listdir(dirname)
            if re.match(regex_files, file)
        ]
    )
    return matching_files


def stamp_filepattern(filepattern, force_return=False):
    """
    Transforms # in filepatterns to >>>DATE<<< and replaces other numbers back
    to original

    Parameters
    ----------
    filepattern : str
        Filepattern to get date stamps for

    force_return : bool
        Returns the list of filepatterns even if it is longer than 1.

    Returns
    -------
    str :
        New filepattern, with >>>DATE<<<
    """
    logging.debug("Incoming filepattern: ", filepattern)
    # If >>>DATE<<< is already in the filepattern, this should be a no-op:
    if ">>>DATE<<<" in filepattern:
        return filepattern
    # No number in this filepattern, nothing to do:
    if "#" not in filepattern:
        return filepattern
    # Get full file list:
    matching_files = get_list_from_filepattern(filepattern)
    try:
        datestamp_location = determine_datestamp_location(matching_files)
    except DatestampLocationError:
        logging.debug("Couldn't determine a unique datestamp for %s" % filepattern)
        return filepattern  # PG: This might be a horrible idea...
    files = [f.replace(f[datestamp_location], ">>>DATE<<<") for f in matching_files]
    files = set(files)
    try:
        assert len(files) == 1
    except AssertionError:
        if force_return:
            return list(files)
        else:
            raise
    filepattern = files.pop()
    return filepattern


# Figure out where the datestamp is in the file pattern
def determine_potential_datestamp_locations(filepattern):
    """
    For a filepattern, gives back index of potential date locations

    Parameters
    ----------
    filepattern : str
        The filepattern to check.

    Returns
    -------
    list :
        A list of slice object which you can use to cut out dates from the
        filepattern
    """
    indexes = list(
        find_indices_of("#", "".join([r"#" if c.isdigit() else c for c in filepattern]))
    )

    aligned_indexes = []
    if indexes[0] + 1 == indexes[1]:
        aligned_indexes.append(0)
    for idx, i in enumerate(indexes[1:-1]):
        idx += 1
        expected_previous = i - 1
        expected_next = i + 1
        if (indexes[idx + 1] == expected_next) or (
            indexes[idx - 1] == expected_previous
        ):
            aligned_indexes.append(i)
    if indexes[-1] - 1 == indexes[-2]:
        aligned_indexes.append(indexes[-1])

    grouped_indexes = group_indexes(aligned_indexes)

    slices = []
    for group in grouped_indexes:
        slices.append(slice(group[0], group[-1] + 1))
    return slices


def determine_datestamp_location(files):
    """
    Given a list of files; figures where the datestamp is by checking if it
    varies.

    Parameters
    ----------
    files : list
        A list (longer than 1!) of files to check

    Returns
    -------
    slice :
        A slice object giving the location of the datestamp

    Raises
    ------
    DatestampLocationError :
        Raised if there is more than one slice found where the numbers vary over
        different files -or- if the length of the file list is not longer than 1.
    """
    try:
        assert len(files) > 1
    except AssertionError:
        raise DatestampLocationError(
            "Unable to determine a datestamp from just 1 file!"
        )
    # Use the first file as a template (Probably a bad idea):
    filepattern = files[0]
    slices = determine_potential_datestamp_locations(filepattern)
    valid_slices = []
    for slice_ in slices:
        if files[0][slice_] == files[1][slice_]:
            continue
        else:
            valid_slices.append(slice_)
    if len(valid_slices) > 1:
        raise DatestampLocationError("Unable to determine a unique datestamp!")
    return valid_slices[0]


# Build a regex to go through a certain number of dates (e.g. 1 decade)
def get_files_for_date_range(
    filepattern, start_date, stop_date, frequency, date_format=r"%Y%m%d"
):
    """
    Creates a list of files for specified start/stop dates

    Parameters
    ----------
    filepattern : str
        A filepattern to replace dates in
    start_date : str
        The starting date, in a pandas-friendly date format
    stop_date : str
        Ending date, pandas friendly. Note that for end dates, you need to
        **add one month** to assure that you get the last step in your list!
    frequency : str
        Frequency of dates, pandas friendly
    date_format : str
        How dates should be formatted, defaults to %Y%m%d

    Returns
    -------
    list
        A list of strings for the filepattern with correct date stamps.

    Example
    -------
    >>> filepattern =  "LGM_24hourly_PMIP4_echam6_BOT_mm_>>>DATE<<<.nc"
    >>> get_files_for_date_range(filepattern, "1890-07", "1891-11", "1M", date_format="%Y%m")
    [
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189007.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189008.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189009.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189010.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189011.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189012.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189101.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189102.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189103.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189104.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189105.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189106.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189107.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189108.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189109.nc",
        "LGM_24hourly_PMIP4_echam6_BOT_mm_189110.nc",
    ]
    """
    # I had initially wanted to do a regex for this; but I get the impression
    # that's a horrible idea. Or at least, a difficult one. If we know the date
    # format, we can just build a list of the desired dates.

    # BUG: determine_datestamp_location takes a list, not a string!!!
    date_stamp = ">>>DATE<<<"
    dates = [
        date.to_pydatetime().strftime(date_format)
        for date in pd.date_range(start=start_date, end=stop_date, freq=frequency)
    ]
    files = [filepattern.replace(date_stamp, str(date)) for date in dates]
    return files


# Sort the files into lists dependening on the dates
def sort_files_to_tarlists(model_files, start_date, end_date, config):
    out_lists = {}
    for model in model_files:
        frequency = config.get(model, {}).get("archive", {}).get("frequency")
        date_format = config.get(model, {}).get("archive", {}).get("date_format")
        out_lists[model] = []
        for filepattern in model_files[model]:
            out_lists[model].append(
                get_files_for_date_range(
                    filepattern,
                    start_date,
                    end_date,
                    frequency,
                    date_format=date_format,
                )
            )
        out_lists[model] = sorted(
            list(set([item for sublist in out_lists[model] for item in sublist]))
        )
    # Remove still empty lists:
    out_lists = {k: v for k, v in out_lists.items() if v}
    return out_lists


# Perform an integrity check
def check_tar_lists(tar_lists):
    existing_tar_lists = {}
    missing_tar_lists = {}
    for model in tar_lists:
        existing_tar_lists[model] = [f for f in tar_lists[model] if os.path.isfile(f)]
        missing_tar_lists[model] = [
            f for f in tar_lists[model] if not os.path.isfile(f)
        ]
    return existing_tar_lists, missing_tar_lists


# Check size of all files in each list
def sum_tar_lists(tar_lists):
    """
    Sums up the amount of space in the tar lists dictionary

    Given ``tar_lists``, which is generally a dicitonary consisting of keys (model
    names) and values (files to be tarred), figures out how much space the
    **raw, uncompressed** files would use. Generally the compressed tarball
    will take up less space.

    Parameters
    ----------
    tar_lists : dict
        Dictionary of file lists to be summed up. Reports every sum as a value
        for the key of that particular list.

    Returns
    -------
    dict :
        Keys are the same as in the input, values are the sums (in bytes) of
        all files present within the list.
    """
    sizes = {}
    for model in tar_lists:
        sizes[model] = sum(os.path.getsize(f) for f in tar_lists[model])
    return sizes


def sum_tar_lists_human_readable(tar_lists):
    """
    As ``sum_tar_lists`` but gives back strings with human-readable sizes.
    """

    def human_readable_size(size, decimal_places=3):
        for unit in ["B", "KiB", "MiB", "GiB", "TiB"]:
            if size < 1024.0:
                break
            size /= 1024.0
        return f"{size:.{decimal_places}f}{unit}"

    sizes = {k: human_readable_size(v) for k, v in sum_tar_lists(tar_lists).items()}
    return sizes


# Split into multiple lists based on size:
def split_list_due_to_size_limit(in_list, slimit):
    flist = in_list.copy()
    current_size = 0
    total_list = []
    current_list = []
    while flist:
        current_file = flist.pop(0)
        current_size += os.path.getsize(current_file)
        current_list.append(current_file)
        logging.debug(current_size)
        if current_size >= slimit * 0.8:
            logging.debug("Critical size exceeded! Starting new list!")
            total_list.append(current_list)
            current_size = 0
            current_list = []
    if current_list:
        total_list.append(current_list)
    return total_list


# Utility function to make running commands on the shell easier:
def run_command(command):
    """
    Runs ``command`` and directly prints output to screen.

    Parameters
    ----------
    command : str
        The command to run, with pipes, redirects, whatever

    Returns
    -------
    rc : int
        The return code of the subprocess.
    """
    subprocess.check_call(
        command, stdout=sys.stdout, stderr=subprocess.STDOUT, shell=True
    )


# Pack the files into tarball(s), depending on the size of the list
def pack_tarfile(flist, wdir, outname):
    """
    Creates a compressed tarball (``outname``) with all files found in ``flist``.


    Parameters
    ----------
    flist : list
        A list of files to include in this tarball
    wdir : str
        The directory to "change" to when packing up the tar file. This will
        (essentially) be used in the tar command as the -C option by stripping
        off the beginning of the flist
    outname : str
        The output file name

    Returns
    -------
    str :
        The output file name
    """
    # TODO(pgierz): Would it be more sensible to return the tarball object?
    if not wdir.endswith("/"):
        wdir += "/"
    flist = [item.replace(wdir, "") for item in flist]
    tar_part = (
        f"tar --use-compress-program=pigz -cvf {outname} -C {wdir} {' '.join(flist)}"
    )
    tqdm_part = f"tqdm --total {len(flist)} --unit files"
    output_part = f"{outname}.log"
    run_command(tar_part + "|" + tqdm_part + ">>" + output_part)
    # with tarfile.open(outname, "w:gz") as tar:
    #    for f in tqdm.tqdm(flist):
    #        tar.add(f, arcname=os.path.basename(f))
    return outname


# Write a small log of what is in that tarball
def log_tarfile_contents(tfile):
    """
    Generates a log of the tarball contents

    Parameters
    ----------
    tfile : str
        The path for the tar file to generate a log for

    Returns
    -------
    None

    Warning
    -------
        Note that for this function to work, you need to have write permission in
        the directory where the tarball is located. If not, this will probably
        raise an OSError.  I can imagine giving the location of the log path as
        an argument; but would like to see if that is actually needed before
        implementing it...

    """
    with tarfile.open(tfile) as tar:
        with open(os.path.splitext(tfile)[0] + ".tar_contents", "w") as contents:
            for member in tar.getmembers():
                # INFO: For which attributes you can write, see here:
                # https://docs.python.org/3/library/tarfile.html#tarfile.TarInfo
                for attr in ["name", "size"]:
                    contents.write(str(getattr(member, attr)))
                    contents.write("\t")
                contents.write("\n")


# Upload the tarball to the tape
def archive_mistral(tfile, rtfile=None):
    """
    Puts the ``tfile`` to the tape archive using ``tape_command``

    Parameters
    ----------
    tfile : str
        The full path of the file to put to tape

    rtfile : str
        The filename on the remote tape server. Defaults to None, in which case
        a replacement is performed to keep as much of the filename the same as
        possible. Example: /work/ab0246/a270077/experiment.tgz -->
        /hpss/arch/ab0246/a270077/experiment.tgz


    Returns
    -------
    None
    """
    if not rtfile:
        rtfile = tfile.replace("/work", "/hpss/arch")

    remote_base_dir = os.path.dirname(rtfile)

    tape_server = Pftp()

    if not tape_server.exists(remote_base_dir):
        tape_server.makedirs(remote_base_dir)

    if os.path.isfile(tfile) and os.stat(tfile).st_size > 0:
        # print(archive_name, "will be uploaded to", remote_archive_name)
        tape_server.upload(tfile, rtfile)
    else:
        print(f"{rtfile} doesn't exist or is an empty file, skipping...")


# If requested, delete the original data
def delete_original_data(tfile, force=False):
    """
    Erases data which is found in the tar file.

    Parameters
    ----------
    tfile : str
        Path to the tarfille whose data should be erased.
    force : bool
        If False, asks the user if they really want to delete their files.
        Otherwise just does this silently. Default is ``False``

    Returns
    -------
    None
    """
    with tarfile.open(tfile) as tar:
        if not force:
            print("WARNING! You are about to delete these files:")
            for f in tar.getnames():
                print(f)
            response = query_yes_no("Do you want to continue?", default="no")
            if not response:
                return
        for f in tar.getnames():
            os.remove(f)
