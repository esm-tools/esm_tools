"""
=====
Usage
=====

This package contains functions to set, get, and use entries stored in the
esmtoolsrc file.

To use ESM RCFile in a project::

    import esm_rcfile

You can set specific values in the ``~/.esmtoolsrc`` with::

    set_rc_entry(key, value)

For example::

    >>> set_rc_entry("SCOPE_CONFIG", "/pf/a/a270077/Code/scope/configs/")

Retriving an entry::

    >>> fpath = get_rc_entry("FUNCTION_PATH")
    >>> print(fpath)
    /pf/a/a270077/Code/esm_tools/esm_tools/configs

With a default value for a non-existing key::

    >>> scope_config = get_rc_entry("SCOPE_CONFIG", "/dev/null")
    >>> print(scope_config)
    /dev/null

Without a default value, you get ``EsmRcfileError``::

    >>> echam_namelist = get_rc_entry("ECHAM_NMLDIR")
    EsmRcFileError: No value for ECHAM_NMLDIR found in esmtoolsrc file!!

This error is also raised if there is no ``~/.esmtoolsrc`` file, and no default
is provided.

You can also get the entire rcfile as a dict::

    >>> rcdict = import_rc_file()


API Documentation
-----------------
"""
import os
import esm_tools

# FIXME(PG): Deprecate this soon. Globals should be in all-caps.
rcfile = RCFILE = os.path.expanduser("~") + "/.esmtoolsrc"


class EsmRcfileError(Exception):
    """Raise this error when there is a problem with ``.esmtoolsrc``."""


class EsmToolsDir(str):
    """
    A string subclass whose instances provide the paths to `esm_tools` folders
    (`configs`, `namelists` and `runscripts`) when evaluated as a string (i.e.
    `str(<your_instance>)` or `<your_instance> + <a_string>`).

    This class is thought as a generalised solution for the problem of removing the
    `FUNCTION_PATH`, `NAMELIST_PATH` and `RUNSCRIPT_PATH` from the `.esmtoolsrc` file,
    and intends to provide those paths correctly, both for virtual environment and open
    runs, right at the time where the variable containing the instance is evaluated
    by the `esm_parser`, as a string.

    This should solve issues with, for example, preprocessing and postprocessing scripts
    called during runtime with a `NONE_YET/<path_to_the_script>`.
    """

    def __init__(self, path_type):
        """
        Initializes the instance. To do that, use `<your_instance_name> =
        EsmToolsDir(<path_type>)` where `path_type` can be::
          - "FUNCTION_PATH"
          - "NAMELIST_PATH"
          - "RUNSCRIPT_PATH"

        Parameters
        ----------
        path_type : str
            ESM-Tools path type to be loaded by the object.
        """
        self.path_type = path_type

    def __str__(self):
        """
        When the instance is evaluated as a string, it returns the correct path to its
        corresponding `esm_tools` folder.

        Returns
        -------
        <esm_tools_folder_type_PATH> : str
            The path to the required folder.
        """
        return self.find_path()

    def __add__(self, add_string):
        """
        When the instance is used together with a sum operation, it returns the correct
        path to its corresponding `esm_tools` folder, plus the operation input.

        Parameters
        ----------
        add_string : str
            String to be added to the path.

        Returns
        -------
        <PATH+add_string> : str
            The path to the required folder plus the string that follows (`add_string`).
        """
        return self.find_path() + add_string

    def find_path(self):
        """
        This method returns the path of the `esm_tools` folder required.

        Returns
        -------
        <esm_tools_folder_type_PATH> : str
            The path to the required folder.
        """
        if self.path_type == "FUNCTION_PATH":
            cpath = esm_tools.get_config_filepath(".")
            if cpath:
                return f"{cpath}/"
            else:
                return "/dev/null"
        if self.path_type == "NAMELIST_PATH":
            npath = esm_tools.get_namelist_filepath(".")
            if npath:
                return f"{npath}/"
            else:
                return "/dev/null"
        if self.path_type == "RUNSCRIPT_PATH":
            rpath = esm_tools.get_runscript_filepath(".")
            if rpath:
                return f"{rpath}/"
            else:
                return "/dev/null"
        raise Exception("Incorrect path type!")


def set_rc_entry(key, value):
    """
    Sets values in ``esmtoolsrc``

    Parameters
    ----------
    key : str
    value : str

    Note
    ----
    Using this functions modifies the ``rcfile``; which is stored in the
    current user's home directory.
    """
    all_lines = [key + "=" + value]

    if os.path.isfile(RCFILE):
        with open(RCFILE) as rc:
            for line in rc.readlines():
                line = line.strip()
                if not key == line.split("=", 1)[0]:
                    all_lines.append(line)
        os.remove(RCFILE)

    with open(RCFILE, "w") as rc:
        for line in all_lines:
            rc.write(line + "\n")


def get_rc_entry(key, default=None):
    """
    Gets a specific entry

    Parameters
    ----------
    key : str
    default : str

    Returns
    -------
    str
        Value for key, or default if provided

    Raises
    ------
    EsmRcfileError
        * Raised if key cannot be found in the rcfile and no default is
          provided
        * Raised if the esmtoolsrc file cannot be found and no default is
          provided.
    """
    if os.path.isfile(RCFILE):
        with open(RCFILE) as rc:
            for line in rc.readlines():
                line = line.strip()
                if line.split("=", 1)[0] == key.upper():
                    return line.split("=", 1)[1]
            if default:
                return default
            else:
                raise EsmRcfileError("No value for %s found in esmtoolsrc file!" % key)
    if default:
        return default
    else:
        raise EsmRcfileError("The file esmtoolsrc file was not found!")


def import_rc_file():
    """
    Gets current values of the esmtoolsrc file

    Returns
    -------
    dict
        A dictionary representation of the rcfile
    """
    if os.path.isfile(RCFILE):
        rcdict = {}
        with open(RCFILE) as rc:
            for line in rc.readlines():
                line = line.strip()
                rcdict[line.split("=", 1)[0]] = line.split("=", 1)[1]
        return rcdict
    raise EsmRcfileError("The file esmtoolsrc file was not found!")


# PG: Should this be in a if __name__ == "__main__" ?
# MA: The following lines are most likely never reached since the work
# from PG in implementing the venv. It is probably wise to remove them
# in the next cleanup.
if os.path.isfile(RCFILE):
    FUNCTION_PATH = get_rc_entry("FUNCTION_PATH", "NONE_YET")
else:
    FUNCTION_PATH = "NONE_YET"
