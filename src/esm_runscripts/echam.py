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
            mvstream_tags.append(tag)
    return mvstream_tags


def append_namelist_dependent_sources(config):
    expid = config["general"]["expid"]
    work_dir = config["general"]["work_dir"]
    econfig = config["echam"]
    namelist = econfig["namelist_objs"]
    mvstream_tags = _get_mvstream_tags_from_namelist(namelist)
    mvstream_dict = {tag: f"{work_dir}/{expid}*_{tag}" for tag in mvstream_tags}
    config["outdata_sources"].update(mvstream_dict)
