"""
This module provides functionality specific to the ECHAM component.

The functions included in this module are:

- ``append_namelist_dependent_sources``: Appends namelist dependent sources to the ECHAM
  configuration.

These functions are used to manage and update the configuration settings for ECHAM,
particularly focusing on handling namelist files and updating output data sources
based on the configuration parameters.
"""

import f90nml
from loguru import logger


def _get_mvstream_tags_from_namelist(namelist):
    """
    Extracts mvstream tags from a given namelist.

    Parameters
    ----------
    namelist : str or f90nml.namelist.Namelist
        The path to the namelist file or an already parsed namelist object.

    Returns
    -------
    list of str
        A list of mvstream tags found in the namelist.

    Raises
    ------
    FileNotFoundError
        If the namelist file specified by the path cannot be found.
    TypeError
        If the provided namelist cannot be converted to an f90nml.namelist.Namelist
        object.

    Examples
    --------
    Assuming you have a namelist file at ``/tmp/example/namelist.echam`` with the
    following contents:

    .. code-block:: fortran

    ! This is the "no output" variant of namelist.echam. It contains absolutely no
    ! output, and can be used as a template for testing.
    !
    ! Extended by mvstreamctl namelist block, as an example for the mvstream tags.
    !
    ! P. Gierz
    ! Alfred Wegener Institute, Helmholtz Centre for Polar and Marine Research
    ! July 2021
    !
    ! P. Gierz
    ! October 2024

    &runctl
        dt_start = 2285, 12, 31, 23, 52, 30
        dt_stop = 6699, 12, 31, 23, 52, 30
        putrerun = 12, 'months', 'last', 0
        lfractional_mask = .false.
        lresume = .true.
        out_datapath = './'
        out_expname = 'E280'
        rerun_filetype = 4
        delta_time = 450
        putdata = 1, 'months', 'last', 0
        nproma = 8
        lcouple = .true.
        getocean = 1, 'days', 'last', 0
        putocean = 1, 'days', 'last', 0
        lcouple_co2 = .true.
        default_output = .false.
    /

    &parctl
        nproca = 24
        nprocb = 24
    /

    &submodelctl
        lmethox = .true.
    /

    &submdiagctl
        vphysc_lpost = .false.
    /

    &radctl
        iaero = 3
        io3 = 4
        isolrad = 6
        ich4 = 3
        in2o = 3
        co2vmr = 284.3169860840e-06
        ch4vmr = 808.2490234375e-09
        n2ovmr = 273.0210571289e-09
        yr_perp = 1850
    /
    &mvstreamctl
        filetag = 'paul_custom'
        source = 'g3b'
        variables = 'temp2:mean>temp2=167'
        interval = 1, 'months', 'last', 0
    /

    The following code will extract the mvstream tags from the namelist:

    >>> namelist_path = "/tmp/example/namelist.echam"
    >>> tags = _get_mvstream_tags_from_namelist(namelist_path)
    >>> print(tags)
    ['paul_custom']
    """
    mvstream_tags = []
    if not isinstance(namelist, f90nml.namelist.Namelist):
        try:
            namelist = f90nml.read(namelist)
        except FileNotFoundError:
            logger.error(f"Namelist specified by {namelist} could not be found")
        except TypeError as e:
            logger.error(
                f"Could not convert {namelist} to f90nml.namelist.Namelist object."
            )
            raise e

    for chapter, contents in namelist.items():
        if chapter == "mvstreamctl":
            tag = contents.get("filetag")
            if tag is not None:
                mvstream_tags.append(tag)
    return mvstream_tags


def append_namelist_dependent_sources(config):
    """
    Append namelist dependent sources to the ECHAM configuration.

    This function updates the `outdata_sources` in the ECHAM configuration
    based on the namelist objects and other configuration parameters.

    Parameters
    ----------
    config : dict
        The configuration dictionary containing general, ECHAM, and JSBACH settings.

    Notes
    -----
    - The function reads the namelist from the specified directory if not
      already loaded.
    - It filters out tags that are to be ignored based on the JSBACH streams
      or specified ignore tags.
    - The output file type is checked, and if it is NetCDF (indicated by
      ``out_filetype`` == 2), the file extension `.nc` is appended to the tags.
    - The function logs the updates made to the ``outdata_sources``.
    """
    expid = config["general"]["expid"]
    econfig = config["echam"]
    try:
        namelist = econfig["namelist_objs"]
    except KeyError:  # Namelists not yet loaded...
        namelist = f90nml.read(f"{econfig['namelist_dir']}/namelist.echam")
    mvstream_tags = _get_mvstream_tags_from_namelist(namelist)
    jsbach_streams = config["jsbach"].get("streams", [])
    ignore_these_tags = econfig.get("ignore_tags", []])
    if econfig.get("ignore_tags_include_jsbach_tags", True):
        ignore_these_tags.extend(jsbach_streams)
    mvstream_tags = [tag for tag in mvstream_tags if tag not in ignore_these_tags]
    mvstream_dict = {tag: f"{expid}*{tag}" for tag in mvstream_tags}
    if namelist["runctl"].get("out_filetype") == 2:
        # Using NetCDF Outputs:
        mvstream_dict = {k: v + ".nc" for k, v in mvstream_dict.items()}
    logger.info("Updating outdata_sources...")
    for k, v in mvstream_dict.items():
        logger.info(f"{k}: {v}")
    econfig["outdata_sources"].update(mvstream_dict)
    logger.info("...done!")
