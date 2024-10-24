import f90nml
from loguru import logger


def _get_mvstream_tags_from_namelist(namelist):
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
    expid = config["general"]["expid"]
    econfig = config["echam"]
    try:
        namelist = econfig["namelist_objs"]
    except KeyError:  # Namelists not yet loaded...
        namelist = f90nml.read(econfig["namelist_dir"] + "/namelist.echam")
    mvstream_tags = _get_mvstream_tags_from_namelist(namelist)
    jsbach_streams = config["jsbach"]["streams"]
    ignore_these_tags = econfig.get("ignore_tags", jsbach_streams)
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
