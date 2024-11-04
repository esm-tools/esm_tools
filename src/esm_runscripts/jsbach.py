"""
Functionality specific for the JSBACH sub-component.
"""

import f90nml
from loguru import logger

from .echam import _get_mvstream_tags_from_namelist


def _get_comments_for_streams(namelist, mvstream_tags, flag="ESM_TOOLS_JSBACH_STREAM"):
    """
        Extracts tags from a namelist file that have a specific comment flag.

        Parameters
        ----------
        namelist : str
            Path to the namelist file.
        mvstream_tags : list of str
            List of tags to search for in the namelist file.
    flag : str, optional
            The flag to search for in the comments (default is "ESM_TOOLS_JSBACH_STREAM").

        Returns
        -------
        list of str
            List of tags that have the specified comment flag.

        Examples
        --------
        Assuming you have a namelist file with the following contents:

        .. code-block:: fortran

        &mvstreamctl
            filetag = 'plants' ! ESM_TOOLS_JSBACH_STREAM
            source = 'g3b'
            variables = 'soilwet', 'soiltemp', 'lai', 'gpp', 'npp'
            interval = 1, 'months', 'last', 0
        /
        &mvstreamctl
            filetag = 'soil' ! ESM_TOOLS_JSBACH_STREAM
            source = 'g3b'
            variables = 'soilwet', 'soiltemp'
            interval = 1, 'months', 'last', 0
        /
        &mvstreamctl
            filetag = 'something'
            source = 'g3b'
            variables = 'temp2'
            interval = 1, 'months', 'last', 0
        /
        The following will extract ['plants', 'soil']::

        >>> namelist = "tests/namelists/echam/jsbach_tags_namelist.echam"
        >>> mvstream_tags = ["plants", "soil", "something"]
        >>> _get_comments_for_streams(namelist, mvstream_tags)
        ['plants', 'soil']
    """
    with open(namelist, "r") as f:
        lines = f.readlines()
    jsbach_tags = []
    for tag in mvstream_tags:
        # Find the tag in the namlist
        matching_lines = [line for line in lines if tag in line]
        if not matching_lines:
            continue
        # Find the comment
        for line in matching_lines:
            if "!" not in line:
                continue
            comment = line.split("!")[1].strip()
            if flag in comment:
                jsbach_tags.append(tag)
    return jsbach_tags


def append_namelist_dependent_sources(config):
    """
    Append namelist dependent sources of JSBACH configuration.

    Parameters
    ----------
    config : dict

    Notes
    -----
    - The function reads the namelist and will filter mvstream tags that have
      the comment "ESM_TOOLS_JSBACH_STREAM".
    """
    expid = config["general"]["expid"]
    jconfig = config["jsbach"]
    namelist_file = f"{jconfig['namelist_dir']}/namelist.echam"
    try:
        namelist = jconfig["namelist_objs"]
    except KeyError:
        # NOTE(PG): This is one of the reasons ECHAM/JSBACH is unfriendly...
        #           JSBACH is controlled by the ECHAM namelist, this makes
        #           it difficult to separate the two components.
        namelist = f90nml.read(namelist_file)
    mvstream_tags = _get_mvstream_tags_from_namelist(namelist)
    # Check if any of the JSBACH streams identified in the namelist
    # have a comment attached to them:
    flagged_tags = _get_comments_for_streams(namelist.file, mvstream_tags)
    mvstream_dict = {tag: f"{expid}*{tag}" for tag in flagged_tags}
    if namelist["runctl"].get("out_filetype") == 2:
        # Using NetCDF Outputs:
        mvstream_dict = {k: v + ".nc" for k, v in mvstream_dict.items()}
    logger.info("Updating outdata_sources...")
    for k, v in mvstream_dict.items():
        logger.info(f"{k}: {v}")
    jconfig["outdata_sources"].update(mvstream_dict)
    logger.info("...done!")

    return config
