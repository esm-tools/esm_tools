import os
import re
import sys

import yaml
from loguru import logger

import esm_parser

YAML_AUTO_EXTENSIONS = ["", ".yml", ".yaml", ".YML", ".YAML"]


class EsmConfigFileError(Exception):
    """
    Exception for yaml file containing tabs or other syntax issues.

    An exception used when yaml.load() throws a yaml.scanner.ScannerError.
    This error occurs mainly when there are tabs inside a yaml file or
    when the syntax is incorrect. If tabs are found, this exception returns
    a user-friendly message indicating where the tabs are located in the
    yaml file.

    Parameters
    ----------
    fpath : str
        Path to the yaml file
    """

    def __init__(self, fpath, yaml_error):
        report = ""
        # Loop through the lines inside the yaml file searching for tabs
        with open(fpath) as yaml_file:
            for n, line in enumerate(yaml_file):
                # Save lines and line numbers with tabs
                if "\t" in line:
                    report += str(n) + ":" + line.replace("\t", "____") + "\n"

        # Message to return
        if len(report) == 0:
            # If no tabs are found print the original error message
            print("\n\n\n" + yaml_error)
        else:
            # If tabs are found print the report
            self.message = (
                "\n\n\n"
                f"Your file {fpath} has tabs, please use ONLY spaces!\n"
                "Tabs are in following lines:\n" + report
            )
        super().__init__(self.message)


# This next part is stolen here:
# https://medium.com/swlh/python-yaml-configuration-with-environment-variables-parsing-77930f4273ac
# Deniz: unfortunately this example as well as the other examples and even
# PyYaml itself is bit buggy. Basically everything with ${VAR} passes through
# the pipeline and this ends up replacing anything with ${VAR} with VAR and
# export them as environmental variables. That propagates even further and
# messes up the rest of the esm_tools (eg. master, runscripts, ...). The fix
# below corrects that issue.
def create_env_loader(tag="!ENV", loader=yaml.SafeLoader):
    # pattern for ${VAR} and extract VAR
    pattern_envvar = re.compile("\${(\w+)}")

    # This pattern matches the valid (uncommented) !ENV lines. Eg.
    # - !ENV ${VAR}
    # - !ENV "${VAR}" or !ENV '${VAR}'
    # Note: '!ENV ...' or "!ENV ..." does not work since PyYaml requires the tag outside of the quotes
    pattern_envtag = re.compile("""^[^\#]*\!ENV[ \t]+['|"]?\$\{\w+\}['|"]?""")

    # the tag will be used to mark where to start searching for the pattern
    # e.g. somekey: !ENV somestring${MYENVVAR}blah blah blah
    loader.add_implicit_resolver(tag, pattern_envvar, None)
    loader.env_variables = []

    def constructor_env_variables(loader, node):
        """
        Extracts the environment variable from the node's value
        :param yaml.Loader loader: the yaml loader
        :param node: the current node in the yaml
        :return: the parsed string that contains the value of the environment
        variable
        """
        # file name (including the full path if not in PWD)
        fname = node.start_mark.name

        # line number (starts from 0) where match is found
        line_num = node.start_mark.line

        # start and end of the colums numbers in the line (starts from 0)
        # For character index from the start of the file use node.start_mark.index
        column_start = node.start_mark.column
        column_end = node.end_mark.column

        # read the file into a list of line. This can also be achieved by
        # read() instead but since the YAML files are not large (max few KBs),
        # this is a quicked solution
        yaml_file = open(fname, "r")
        file_lines = yaml_file.readlines()
        yaml_file.close()

        # current line without the newline at the end
        cur_line = file_lines[line_num].rstrip()

        # Print extra debug messages if ESM_PARSER_DEBUG environment variable is set
        if os.getenv("ESM_PARSER_DEBUG"):
            print()
            print("===== esm_parser -> yaml_to_dict.py -> create_env_loader() =====")
            print(f"::: Reading the file: {fname}")
            print("::: Reading the line:")
            print(f">>>{cur_line}<<<")
            print(f"::: found the match: >>>{cur_line[column_start:column_end]}<<<")
            print("==================== END OF ESM_PARSER_DEBUG ===================")
            print()

        value = loader.construct_scalar(node)

        # Check if we have a valid !ENV tag on the line
        envtag_match = re.search(pattern_envtag, cur_line)

        # list of matches for ${VAR}. Each match is VAR
        envvar_matches = pattern_envvar.findall(value)

        # Parse the environmental variables only if the line has a valid !ENV tag
        if envtag_match:
            if envvar_matches:
                full_value = value
                for env_var in envvar_matches:

                    # first check if the variable exists in the shell environment
                    if not os.getenv(env_var):
                        esm_parser.user_error(
                            f"{env_var} is not defined",
                            f"{env_var} is not an environment variable. Exiting",
                        )

                    # replace {env_var} with the value of the env_var
                    full_value = full_value.replace(
                        f"${{{env_var}}}", os.getenv(env_var)
                    )
                    loader.env_variables.append((env_var, full_value))
                return full_value
        return value

    loader.add_constructor(tag, constructor_env_variables)
    return loader


def yaml_file_to_dict(filepath):
    """
    Given a yaml file, returns a corresponding dictionary.

    If you do not give an extension, tries again after appending one.
    It raises an EsmConfigFileError exception if yaml files contain tabs.

    Parameters
    ----------
    filepath : str
        Where to get the YAML file from

    Returns
    -------
    dict
        A dictionary representation of the yaml file.

    Raises
    ------
    EsmConfigFileError
        Raised when YAML file contains tabs or other syntax issues.
    FileNotFoundError
        Raised when the YAML file cannot be found and all extensions have been tried.
    """
    loader = create_env_loader()
    for extension in YAML_AUTO_EXTENSIONS:
        try:
            with open(filepath + extension) as yaml_file:
                # Check for duplicates
                check_duplicates(yaml_file)
                # Back to the beginning of the file
                yaml_file.seek(0, 0)
                # Actually load the file
                yaml_load = yaml.load(yaml_file, Loader=loader)  # yaml.FullLoader)
                # Check for incompatible ``_changes`` (no more than one ``_changes``
                # type should be accessible simultaneously)
                check_changes_duplicates(yaml_load, filepath + extension)
                # Add the file name you loaded from to track it back:
                yaml_load["debug_info"] = {"loaded_from_file": yaml_file.name}
                if loader.env_variables:
                    runtime_env_changes = yaml_load.get("computer", {}).get(
                        "runtime_environment_changes", {}
                    )
                    add_export_vars = runtime_env_changes.get("add_export_vars", {})
                    for env_var_name, env_var_value in loader.env_variables:
                        add_export_vars[env_var_name] = env_var_value
                    # TODO(PG): There is probably a more elegant way of doing this:
                    yaml_load["computer"] = yaml_load.get("computer") or {}
                    yaml_load["computer"]["runtime_environment_changes"] = (
                        yaml_load["computer"].get("runtime_environment_changes") or {}
                    )
                    yaml_load["computer"]["runtime_environment_changes"][
                        "add_export_vars"
                    ] = add_export_vars
                # Check for empty components/models
                # check_for_empty_components(yaml_load, filepath + extension)
                return yaml_load
        except IOError as error:
            logger.debug(
                f"IOError ({error.errno}): File not found with {filepath+extension}, trying another extension pattern."
            )
        except yaml.scanner.ScannerError as yaml_error:
            logger.debug(f"Your file {filepath + extension} has syntax issues!")
            raise EsmConfigFileError(filepath + extension, yaml_error)
        except Exception as error:
            print("Something else went wrong")
            print(f"Serious issue with {filepath}, goodbye...")
            logger.exception(error)
            sys.exit()
    raise FileNotFoundError(
        "All file extensions tried and none worked for %s" % filepath
    )


def check_changes_duplicates(yamldict_all, fpath):
    """
    Checks for duplicates and conflicting ``_changes`` and ``add_``:

    1. Finds variables containing ``_changes`` (but excluding ``add_``) and checks
       if they are compatible with the same ``_changes`` inside the same file. If they
       are not compatible returns an error where the conflicting variable paths are
       specified. More than one ``_changes`` type in a file are allowed but they need
       to be part of the same ``_choose`` and not be accessible simultaneously in any
       situation.

    2. Checks if there is any variable containing ``add_`` in the main sections of
       a file and labels it as incompatible if the same variable is found inside a
       ``choose_`` block. ``add_<variable>``s are compatible as long as they are inside
       ``choose_`` blocks, but if you want to include something as a default, please just
       do it inside the ``<variable>``.

       .. warning:: ``add_<variable>``s are not checked for incompatibility when they
          are included inside ``choose_`` blocks. Merging of these ``add_<variable>``s
          is done using ``deep_update``, meaning that the merge is arbitrary (i.e. if
          two ``choose_`` blocks are modifying the same variable using ``add_``, the
          final value would be decided arbitrarily). It is up to the developer/user to
          make good use of ``add_``s inside ``choose_`` blocks.

    Parameters
    ----------
    yamldict_all : dict
        Dictionary read from the yaml file
    fpath : str
        Path to the yaml file
    """
    changes_note = (
        "Note that if there are more than one ``_changes`` in the "
        "file, they need to be placed inside different cases of the "
        "same ``choose`` and these options need to be compatible "
        "(only one ``_changes`` can be reached at a time).\n"
        "Use ``add_<variable>_changes`` if you want to add/overwrite "
        "variables inside the main ``_changes``."
    )
    add_note = (
        "Note that multiple ``add_<variable>`` in a single file are compatible "
        "as long as they are included inside ``choose_`` blocks. An "
        "``add_<variable>`` out of a ``choose_`` block and the same "
        "``add_<variable>`` inside of a ``choose_`` block are considered "
        "incompatible. If the general ``add_<variable>`` should be added "
        "as a default, please include it to ``<variable>`` instead."
    )

    # If it is a couple setup, check for ``_changes`` duplicates separately for each component
    if "general" not in yamldict_all:
        yamldict_all = {"main": yamldict_all}

    # Loop through the components or main
    for yamldict in yamldict_all.values():
        # Check if any <variable>_changes or add_<variable> exists, if not, return
        # Perform the check only for the dictionary objects
        if isinstance(yamldict, dict):
            changes_list = esm_parser.find_key(
                yamldict, "_changes", "add_", paths2finds=[], sep=","
            )
            add_list = esm_parser.find_key(
                yamldict, ["add_"], "", paths2finds=[], sep=","
            )
            if (len(changes_list) + len(add_list)) == 0:
                continue

        # Find ``_changes`` types
        changes_types = set(
            [y for x in changes_list for y in x.split(",") if "_changes" in y]
        )
        # Find ``add_`` types
        add_types = set([y for x in add_list for y in x.split(",") if "add_" in y])
        # Define ``_changes`` groups
        changes_groups = []
        for change_type in changes_types:
            changes_groups.append(
                [x for x in changes_list if change_type == x.split(",")[-1]]
            )
        # Define ``add_`` groups
        add_groups = []
        for add_type in add_types:
            add_groups.append([x for x in add_list if add_type == x.split(",")[-1]])

        # Loop through the different ``_changes`` groups
        for changes_group in changes_groups:
            # Check for ``_changes`` without ``choose_``, "there can be only one"
            changes_no_choose = [x for x in changes_group if "choose_" not in x]
            # If more than one ``_changes`` without ``choose_`` return error
            if len(changes_no_choose) > 1:
                changes_no_choose = [x.replace(",", ".") for x in changes_no_choose]
                esm_parser.user_error(
                    "YAML syntax",
                    "More than one ``_changes`` out of a ``choose_``in "
                    + fpath
                    + ":\n    - "
                    + "\n    - ".join(changes_no_choose)
                    + "\n"
                    + changes_note
                    + "\n\n",
                )
            # If only one ``_changes`` without ``choose_`` check for ``_changes`` inside
            # ``choose_`` and return error if any is found
            elif len(changes_no_choose) == 1:
                changes_group.remove(changes_no_choose[0])
                if len(changes_group) > 0:
                    changes_group = [x.replace(",", ".") for x in changes_group]
                    esm_parser.user_error(
                        "YAML syntax",
                        "The general ``"
                        + changes_no_choose[0]
                        + "`` and ``_changes`` in ``choose_`` are not compatible in "
                        + fpath
                        + ":\n    - "
                        + "\n    - ".join(changes_group)
                        + "\n"
                        + "\n"
                        + changes_note
                        + "\n\n",
                    )

            # If you reach this point all ``_changes`` are inside
            # some number of ``choose_`` (there are no ``_changes``
            # outside of a ``choose_``)

            # Check for incompatible ``_changes`` inside ``choose_``:
            # Split the path of the variables
            changes_group_split = [x.split(",") for x in changes_group]
            # Loop through the paths of the ``_changes`` in the group
            for count, changes in enumerate(changes_group_split):
                # Find the path of the last ``choose_`` in ``changes`` and
                # its case
                path2choose, case = find_last_choose(changes)
                # Loop through the changes following the current one
                for other_changes in changes_group_split[count + 1 :]:
                    # Find the path of the last ``choose_`` in
                    # ``other_changes`` and its case
                    sub_path2choose, sub_case = find_last_choose(other_changes)
                    # If one ``choose_`` is contained into the other
                    # find the common ``choose_`` and compare the cases.
                    # If the case is the same, duplicates exist and error
                    # is returned (i.e. choose_lresume.True.namelist_changes
                    # and choose_lresume.True.choose_another_switch
                    # False.namelist_changes)
                    if path2choose in sub_path2choose or sub_path2choose in path2choose:
                        if path2choose in sub_path2choose:
                            sub_case = sub_path2choose.replace(
                                path2choose + ",", ""
                            ).split(",")[0]
                        elif sub_path2choose in path2choose:
                            case = path2choose.replace(sub_path2choose + ",", "").split(
                                ","
                            )[0]
                        if case == sub_case:
                            esm_parser.user_error(
                                "YAML syntax",
                                "The following ``_changes`` can be accessed "
                                + "simultaneously in "
                                + fpath
                                + ":\n"
                                + "    - "
                                + ".".join(changes)
                                + "\n"
                                + "    - "
                                + ".".join(other_changes)
                                + "\n"
                                + "\n"
                                + changes_note
                                + "\n\n",
                            )
                    else:
                        # If these ``choose_`` are different they can be accessed
                        # simultaneously, then it returns an error
                        esm_parser.user_error(
                            "YAML syntax",
                            "The following ``_changes`` can be accessed "
                            + "simultaneously in "
                            + fpath
                            + ":\n"
                            + "    - "
                            + ".".join(changes)
                            + "\n"
                            + "    - "
                            + ".".join(other_changes)
                            + "\n"
                            + "\n"
                            + changes_note
                            + "\n\n",
                        )

        # Loop through the different ``add_`` groups
        for add_group in add_groups:
            # Count ``add_`` occurrences out of a ``choose_``
            add_no_choose = [x for x in add_group if "choose_" not in x]
            # Check if the heading of the path is the same. If not is not a
            # repetition
            for anc in add_no_choose:
                add_no_choose = []
                heading = anc.split(",")[0]
                counter = 0
                for ag in add_group:
                    if heading in ag:
                        counter += 1
                if counter > 1:
                    add_no_choose.append(anc)

            # If one ``add_`` without ``choose_`` check for ``add_`` inside
            # ``choose_`` and return error if any is found (incompatible ``add_``s)
            if len(add_no_choose) >= 1:
                add_group.remove(add_no_choose[0])
                if len(add_group) > 0:
                    add_group = [x.replace(",", ".") for x in add_group]
                    esm_parser.user_error(
                        "YAML syntax",
                        "The general ``"
                        + add_no_choose[0]
                        + "`` and ``add_`` in ``choose_`` are not compatible in "
                        + fpath
                        + ":\n    - "
                        + "\n    - ".join(add_group)
                        + "\n\n"
                        + add_note
                        + "\n\n",
                    )


def check_for_empty_components(yaml_load, fpath):
    for key, value in yaml_load.items():
        if not value:
            esm_parser.user_error(
                "YAML syntax",
                f"The component ``{key}`` is empty in the file ``{fpath}``. ESM-Tools does"
                + " not support empty components, either add some variables to the "
                + f"``{key}`` section, or remove it from this file.",
            )


def find_last_choose(var_path):
    """
    Locates the last ``choose_`` on a string containing the path to a
    variable separated by ",", and returns the path to the ``choose_``
    (also separated by ",") and the case that follows the ``choose_``.

    Parameters
    ----------
    var_path : str
        String containing the path to the last ``choose_`` separated by
        ",".

    Returns
    -------
    path2choose : str
        Path to the last ``choose_``.
    case : str
        Case after the choose.
    """
    # Find the last ``choose_``
    last_choose = [x for x in var_path if "choose_" in x][-1]
    # Find the last ``choose_`` index
    choose_index = var_path.index(last_choose)
    # Defines the path to the last ``choose_``
    path2choose = ",".join(var_path[: var_path.index(last_choose) + 1])
    # Defines the case of the last ``choose_``
    case = var_path[choose_index + 1]
    return path2choose, case


def check_duplicates(src):
    """
    Checks that there are no duplicates in a yaml file, and if there are
    returns an error stating which key is repeated and in which file the
    duplication occurs.

    Parameters
    ----------
    src : object
        Source file object

    Raises
    ------
    ConstructorError
        If duplicated keys are found, returns an error
    """

    class PreserveDuplicatesLoader(yaml.loader.Loader):
        # We deliberately define a fresh class inside the function,
        # because add_constructor is a class method and we don't want to
        # mutate pyyaml classes.
        pass

    def map_constructor(loader, node, deep=False):
        """
        Mapping, finds any duplicate keys.
        """
        mapping = {}
        for key_node, value_node in node.value:
            key = loader.construct_object(key_node, deep=deep)
            value = loader.construct_object(value_node, deep=deep)

            if key in mapping:
                esm_parser.user_error(
                    "Duplicated variables",
                    "Key ``{0}`` is duplicated {1}\n\n".format(
                        key, str(key_node.start_mark).replace("  ", "").split(",")[0]
                    ),
                )

            mapping[key] = value

        return loader.construct_mapping(node, deep)

    PreserveDuplicatesLoader.add_constructor(
        yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, map_constructor
    )
    new_loader = create_env_loader(loader=PreserveDuplicatesLoader)
    return yaml.load(src, Loader=new_loader)


class EsmConfigFileError(Exception):
    """
    Exception for yaml file containing tabs or other syntax issues.

    An exception used when yaml.load() throws a yaml.scanner.ScannerError.
    This error occurs mainly when there are tabs inside a yaml file or
    when the syntax is incorrect. If tabs are found, this exception returns
    a user-friendly message indicating where the tabs are located in the
    yaml file.

    Parameters
    ----------
    fpath : str
        Path to the yaml file
    """

    def __init__(self, fpath, yaml_error):
        report = ""
        # Loop through the lines inside the yaml file searching for tabs
        with open(fpath) as yaml_file:
            for n, line in enumerate(yaml_file):
                # Save lines and line numbers with tabs
                if "\t" in line:
                    report += str(n) + ":" + line.replace("\t", "____") + "\n"

        # Message to return
        if len(report) == 0:
            # If no tabs are found print the original error message
            print("\n\n\n" + yaml_error)
        else:
            # If tabs are found print the report
            self.message = (
                "\n\n\n"
                f"Your file {fpath} has tabs, please use ONLY spaces!\n"
                "Tabs are in following lines:\n" + report
            )
        super().__init__(self.message)
