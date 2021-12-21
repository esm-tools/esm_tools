# -*- coding: utf-8 -*-

"""Top-level package for ESM Archiving."""

__author__ = """Paul Gierz"""
__email__ = "pgierz@awi.de"
__version__ = "6.0.3"

from .esm_archiving import (
    archive_mistral,
    check_tar_lists,
    delete_original_data,
    determine_datestamp_location,
    determine_potential_datestamp_locations,
    find_indices_of,
    get_files_for_date_range,
    get_list_from_filepattern,
    group_files,
    group_indexes,
    log_tarfile_contents,
    pack_tarfile,
    purify_expid_in,
    sort_files_to_tarlists,
    split_list_due_to_size_limit,
    stamp_filepattern,
    stamp_files,
    sum_tar_lists,
    sum_tar_lists_human_readable,
)

__all__ = (
    "archive_mistral",
    "check_tar_lists",
    "delete_original_data",
    "determine_datestamp_location",
    "determine_potential_datestamp_locations",
    "find_indices_of",
    "get_files_for_date_range",
    "get_list_from_filepattern",
    "group_files",
    "group_indexes",
    "log_tarfile_contents",
    "pack_tarfile",
    "purify_expid_in",
    "sort_files_to_tarlists",
    "split_list_due_to_size_limit",
    "stamp_filepattern",
    "stamp_files",
    "sum_tar_lists",
    "sum_tar_lists_human_readable",
)
