import copy
import sys
from io import StringIO

import f90nml
import yaml
from loguru import logger
from ruamel.yaml import YAML


def yaml_dump(config, config_file_path=None):
    """
    Dump the config dictionary to a YAML file. The YAML file will contain comments
    with provenance information.

    Parameters
    ----------
    config : dict
        Dictionary containing the simulation/compilation information
    config_file_path : str, optional
        Path to the YAML file where the config will be saved. If not provided, the
        config will be printed to the standard output.
    """
    # This is here to avoid circular imports
    import esm_calendar
    import esm_parser
    import esm_runscripts

    my_yaml = YAML()
    my_yaml.width = 10000

    # first define the representers for the non-built-in types, as recommended
    # here: https://pyyaml.org/wiki/PyYAMLDocumentation
    def date_representer(dumper, date):
        return dumper.represent_str(f"{date.output()}")

    def calendar_representer(dumper, calendar):
        # Calendar has a __str__ method
        return dumper.represent_str(str(calendar))

    def batch_system_representer(dumper, batch_system):
        return dumper.represent_str(f"{batch_system.name}")

    def coupler_representer(dumper, coupler):
        # prevent dumping of whole namcouple
        return dumper.represent_str(f"{coupler.name}")

    def oasis_representer(dumper, oasis):
        return dumper.represent_str(f"{oasis.name}")

    def namelist_representer(dumper, f90nml):
        return dumper.represent_str(f"{f90nml.name}")

    def namelist_key_representer(dumper, f90nml):
        return dumper.represent_str(f"{f90nml._key}")

    def listwithprov_representer(dumper, listwithprov):
        return dumper.represent_sequence("tag:yaml.org,2002:seq", listwithprov)

    def dictwithprov_representer(dumper, dictwithprov):
        return dumper.represent_mapping("tag:yaml.org,2002:map", dictwithprov)

    # pyyaml does not support tuple and prints !!python/tuple
    my_yaml.representer.add_representer(
        tuple, yaml.representer.SafeRepresenter.represent_list
    )

    # Determine how non-built-in types will be printed be the YAML dumper
    my_yaml.representer.add_representer(esm_calendar.Date, date_representer)

    my_yaml.representer.add_representer(
        esm_calendar.esm_calendar.Calendar, calendar_representer
    )
    # yaml.representer.SafeRepresenter.represent_str)

    my_yaml.representer.add_representer(
        esm_parser.esm_parser.ConfigSetup,
        yaml.representer.SafeRepresenter.represent_dict,
    )

    my_yaml.representer.add_representer(
        esm_runscripts.batch_system, batch_system_representer
    )

    # format for the other ESM data structures
    my_yaml.representer.add_representer(
        esm_runscripts.coupler.coupler_class, coupler_representer
    )

    my_yaml.representer.add_representer(f90nml.namelist.Namelist, namelist_representer)
    my_yaml.representer.add_representer(
        f90nml.namelist.NmlKey, namelist_key_representer
    )

    # Provenance representers
    my_yaml.representer.add_representer(
        esm_parser.provenance.ListWithProvenance, listwithprov_representer
    )
    my_yaml.representer.add_representer(
        esm_parser.provenance.DictWithProvenance, dictwithprov_representer
    )

    if "oasis3mct" in config:
        my_yaml.representer.add_representer(
            esm_runscripts.oasis.oasis, oasis_representer
        )

    # Avoid saving ``prev_run`` information in the config file
    config_final = copy.deepcopy(config)  # PrevRunInfo
    delete_prev_objects(config_final)  # PrevRunInfo

    # Get the original values without provenance
    config_final = esm_parser.provenance.clean_provenance(config_final)

    # Attach provenance comments to the values:
    stream = StringIO()
    my_yaml.dump(config_final, stream)

    # Make a ruamel load of the config
    stream.seek(0)
    config_with_comments = my_yaml.load(stream)
    # Add comments to the ruamel.yaml dict instance
    add_eol_comments_with_provenance(config_with_comments, config)

    if config_file_path:
        with open(config_file_path, "w") as config_file:
            # Write the finished_config.yaml file
            out = my_yaml.dump(config_with_comments, config_file)
    else:
        my_yaml.dump(config_with_comments, sys.stdout)


def add_eol_comments_with_provenance(commented_config, config):
    """
    Add end-of-line comments to the ``commented_config`` with provenance information
    from the ``config``.

    Parameters
    ----------
    commented_config : dict
        Dictionary with the config values and ruamel.yaml comments.
    config : dict
        Dictionary with the config values and provenance information.
    """
    if isinstance(commented_config, dict):
        for key, cvalue in commented_config.items():
            pvalue = config[key]
            if isinstance(cvalue, (list, dict)):
                add_eol_comments_with_provenance(cvalue, pvalue)
            else:
                provenance = getattr(pvalue, "provenance", [None])[-1]
                if provenance:
                    provenance_comment = f"{provenance['yaml_file']},line:{provenance['line']},col:{provenance['col']}"
                else:
                    provenance_comment = f"no provenance info"
                commented_config.yaml_add_eol_comment(provenance_comment, key)
    elif isinstance(commented_config, list):
        for indx, value in enumerate(commented_config):
            if isinstance(value, (list, dict)):
                add_eol_comments_with_provenance(value, config[indx])
            else:
                provenance = getattr(config[indx], "provenance", [None])[-1]
                if provenance:
                    provenance_comment = f"{provenance['yaml_file']},line:{provenance['line']},col:{provenance['col']}"
                else:
                    provenance_comment = f"no provenance info"
                commented_config.yaml_add_eol_comment(provenance_comment, indx)


def delete_prev_objects(config):
    """
    Delete key-values in the ``config`` which values correspond to ``prev_`` objects,
    for example, ``prev_run`` (contains values of the config of the previous run) and
    ``prev_chunk_<model>`` (that contains values of the config of a previous chunk
    in a offline coupled simulation). This deletion is necessary because otherwise
    the ``finished_config.yaml`` gets a lot of nested information from the previous
    runs that keeps growing each new run/chunk.

    Parameters
    ----------
    config : dict
        Dictionary containing the simulation/compilation information
    """
    for prev_object in getattr(config, "prev_objects", []):
        del config[prev_object]
