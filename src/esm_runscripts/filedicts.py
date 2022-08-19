"""
The file-dictionary implementation

Developer Notes
---------------
* Internal functions, decorators, and methods are prefixed with _. They should
  only be used inside of this file.
* Decorators should have names that map to an attribute of the object. See the
  example in ``_allowed_to_be_missing``.
"""
import copy
import functools
import os
import pathlib
import shutil
import sys
from enum import Enum, auto
from typing import AnyStr, Tuple, Type, Union

import dpath.util
import yaml
from loguru import logger

from esm_parser import ConfigSetup, user_error

logger.remove()
LEVEL = "ERROR" # "WARNING"  # "INFO"  # "DEBUG"
LOGGING_FORMAT = "[{time:HH:mm:ss  DD/MM/YYYY}]  <level>|{level}|  [{file} -> {function}() line:{line: >3}] >> </level>{message}"
logger.add(sys.stderr, level=LEVEL, format=LOGGING_FORMAT)

# Enumeration of file types
class FileTypes(Enum):
    FILE = auto()  # ordinary file
    DIR = auto()  # directory
    LINK = auto()  # symbolic link
    EXISTS = auto()  # object exists in the system
    NOT_EXISTS = auto()  # file does not exist
    BROKEN_LINK = auto()  # target of the symbolic link does not exist


# NOTE(PG): Comment can be removed later. Here I prefix with an underscore as
# this decorator should **only** be used inside of this file.
def _allowed_to_be_missing(method):
    """Allows to decorate a method with ``_allowed_to_be_missing``, causing it
    to always return ``None``.

    If a method is decorated with ``@_allowed_to_be_missing``, it will return
    ``None`` instead of executing if the file has a attribute of
    ``allowed_to_be_missing`` set to ``True. You get a warning via the logger
    giving the full method name that was decorated and a representation of the
    file that was trying to be moved, linked, or copied.

    Usage Example
    -------------
    Given you have an instanciated simulation file under ``sim_file`` with the following property::
        >>> sim_file.allowed_to_be_missing  # doctest: +SKIP
        True

    And given that you have a decorated method foo, that would act on the file::
        >>> rvalue = sim_file.foo(*args, **kwargs)  # doctest: +SKIP
        >>> rvalue is None  # doctest: +SKIP
        True
        >>> print(rvalue)  # doctest: +SKIP
        None

    Programming Example
    -------------------
    class MyCoolClass:
        def __init__(self):
            self.allowed_to_be_missing = True

        @_allowed_to_be_missing
        def foo(self, *args, **kwargs):
            # This method will always return None, the return below is never
            # reached:
            return 123
    """

    @functools.wraps(method)
    def inner_method(self, *args, **kwargs):
        if self.allowed_to_be_missing:
            try:
                return method(self, *args, **kwargs)
            except (FileNotFoundError, IOError):
                logger.warning(
                    f"Skipping {method.__qualname__} as this file ({self}) is allowed to be missing!"
                )
                return None  # None is the default return, but let us be explicit here, as it is a bit confusing
        else:
            return method(self, *args, **kwargs)

    return inner_method


class SimulationFile(dict):
    """
    Describes a file used within a ESM Simulation.

    Given a config, you should be able to use this in YAML::

        $ cat dummy_config.yaml

        echam:
            files:
                jan_surf:
                    name_in_computer: T63CORE2_jan_surf.nc
                    name_in_work: unit.24
                    filetype: NetCDF
                    allowed_to_be_missing: True
                    description: >
                        Initial values used for the simulation, including
                        properties such as geopotential, temperature, pressure

    And, assuming config is as described above::

        >>> sim_file = SimulationFile(config, ['echam']['files']['jan_surf'])  # doctest: +SKIP

    You could then copy the file to the experiment folder::

        >>> sim_file.cp_to_exp_tree()  # doctest: +SKIP
    """

    def __init__(self, full_config: dict, attrs_address: dict):
        """
        - Initiates the properties of the object
        - Triggers basic checks

        Parameters
        ----------
        full_config : dict
            The full simulation configuration
        attrs_address : str
            The address of this specific file in the full config, separated by dots.

        Note
        ----
        A file can be located in one of these categories (``LOCATION_KEYS``):
        - computer: pool/source directory (for input files)
        - exp_tree: file in the category directory in experiment directory (eg. input, output, ...)
        - run_tree: file in the experiment/run_<DATE>/<CATEGORY>/ directory
        - work:     file in the current work directory. Eg. experiment/run_<DATE>/work/

        LOCATION_KEY is one of the strings defined in LOCATION_KEY list
        - name_in<LOCATION_KEY> : file name (without path) in the LOCATION_KEY
          - eg. name_in_computer: T63CORE2_jan_surf.nc
          - eg. name_in_work: unit.24
        - absolute_path_in_<LOCATION_KEY> : absolute path in the LOCATION_KEY
          - eg. absolute_path_in_run_tree:
          - /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam/T63CORE2_jan_surf.nc
        """
        attrs_dict = dpath.util.get(
            full_config, attrs_address, separator=".", default={}
        )
        super().__init__(attrs_dict)
        self._original_filedict = copy.deepcopy(attrs_dict)
        self._config = full_config
        self._attrs_address = attrs_address
        self.name = attrs_address.split(".")[-1]
        self.component = component = attrs_address.split(".")[0]
        self.all_model_filetypes = full_config["general"]["all_model_filetypes"]
        self.path_in_computer = self.get("path_in_computer")
        if self.path_in_computer:
            self.path_in_computer = pathlib.Path(self.path_in_computer)

        self._check_file_syntax()

        # Complete tree names if not defined by the user
        self._complete_file_names()

        # possible paths for files:
        location_keys = ["computer", "exp_tree", "run_tree", "work"]
        # initialize the locations and complete paths for all possible locations
        self.locations = dict.fromkeys(location_keys, None)
        self._resolve_abs_paths()

        # Verbose set to true by default, for now at least
        self._verbose = full_config.get("general", {}).get("verbose", True)

        # Checks
        self._check_path_in_computer_is_abs()

    def __str__(self):
        address = " -> ".join(self._attrs_address.split("."))
        return address

    def _complete_file_names(self):
        """
        Complete missing names in the file with the default name, depending whether
        the file is of type ``input`` or ``output``.
        """
        if self["type"] in self.input_file_types:
            default_name = self["name_in_computer"]
        elif self["type"] in self.output_file_types:
            default_name = self["name_in_work"]
        self["name_in_computer"] = self.get("name_in_computer", default_name)
        self["name_in_run_tree"] = self.get("name_in_run_tree", default_name)
        self["name_in_exp_tree"] = self.get("name_in_exp_tree", default_name)
        self["name_in_work"] = self.get("name_in_work", default_name)

    # This part allows for dot-access to allowed_to_be_missing:
    @property
    def allowed_to_be_missing(self):
        """
        Example
        -------
            >>> sim_file = SimulationFile(config, 'echam.files.jan_surf')  # doctest: +SKIP
            >>> sim_file.allowed_to_be_missing  # doctest: +SKIP
            True
        """
        return self.get("allowed_to_be_missing", False)

    @_allowed_to_be_missing
    def cp(self, source: str, target: str) -> None:
        """
        Copies the source file or folder to the target path. It changes the name of the
        target if ``self["name_in_<target>"]`` differs from ``self["name_in_<source>"].

        Parameters
        ----------
        source : str
            String specifying one of the following options: ``"computer"``, ``"work"``,
            ``"exp_tree"``, ``run_tree``
        target : str
            String specifying one of the following options: ``"computer"``, ``"work"``,
            ``"exp_tree"``, ``run_tree``
        """
        if source not in self.locations:
            raise ValueError(
                f"Source is incorrectly defined, and needs to be in {self.locations}"
            )
        source_path = self[f"absolute_path_in_{source}"]
        target_path = self[f"absolute_path_in_{target}"]

        # general checks
        # TODO (deniz): need to add higher level exception handler (eg. user_error)
        self._check_source_and_target(source_path, target_path)

        # Actual copy
        source_path_type = self._path_type(source_path)
        if source_path_type == FileTypes.DIR:
            copy_func = shutil.copytree
        else:
            copy_func = shutil.copy2
        try:
            copy_func(source_path, target_path)
            logger.debug(f"Copied {source_path} --> {target_path}")
        except Exception as error:
            raise Exception(
                f"Unable to copy {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    @_allowed_to_be_missing
    def ln(self, source: AnyStr, target: AnyStr) -> None:
        """creates symbolic links from the path retrieved by ``source`` to the one by ``target``.

        Parameters
        ----------
        source : str
            key to retrieve the source from the file dictionary. Possible options: ``computer``, ``work``, ``exp_tree``, ``run_tree``

        target : str
            key to retrieve the target from the file dictionary. Possible options: ``computer``, ``work``, ``exp_tree``, ``run_tree``

        Returns
        -------
        None

        Raises
        ------
        FileNotFoundError
            - Source path does not exist
        OSError
            - Target path is a directory
            - Symbolic link is trying to link to itself
            - Target path does not exist
        FileExistsError
            - Target path already exists
        """
        # full paths: directory path / file name
        source_path = self[f"absolute_path_in_{source}"]
        target_path = self[f"absolute_path_in_{target}"]

        # general checks
        # TODO (deniz): need to add higher level exception handler (eg. user_error)
        self._check_source_and_target(source_path, target_path)

        if os.path.isdir(target_path):
            err_msg = f"Unable to create symbolic link: `{target_path}` is a directory"
            raise OSError(err_msg)

        os.symlink(source_path, target_path)
        logger.debug(f"Linked {source_path} --> {target_path}")

    @_allowed_to_be_missing
    def mv(self, source: str, target: str) -> None:
        """
        Moves (renames) the SimulationFile from it's location in ``source`` to
        it's location in ``target``.

        Parameters
        ----------
        source : str
            One of ``"computer"``, ``"work"``, ``"exp_tree"``, "``run_tree``"
        target : str
            One of ``"computer"``, ``"work"``, ``"exp_tree"``, "``run_tree``"
        """
        if source not in self.locations:
            raise ValueError(
                f"Source is incorrectly defined, and needs to be in {self.locations}"
            )
        source_path = self[f"absolute_path_in_{source}"]
        target_path = self[f"absolute_path_in_{target}"]

        # general checks
        # TODO (deniz): need to add higher level exception handler (eg. user_error)
        self._check_source_and_target(source_path, target_path)

        # Perform the movement:
        try:
            source_path.rename(target_path)
            logger.debug(f"Moved {source_path} --> {target_path}")
        except IOError as error:
            # NOTE(PG): Re-raise IOError with our own message:
            raise IOError(
                f"Unable to move {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    def _resolve_abs_paths(self) -> None:
        """
        Builds the absolute paths of the file for the different locations
        (``computer``, ``work``, ``exp_tree``, ``run_tree``) using the information
        about the experiment paths in ``self._config`` and the
        ``self["path_in_computer"]``.

        It defines these new variables in the ``SimulationFile`` dictionary:
        - ``self["absolute_path_in_work"]``
        - ``self["absolute_path_in_computer"]``
        - ``self["absolute_path_in_run_tree"]``
        - ``self["absolute_path_in_exp_tree"]``
        """
        self.locations = {
            "work": pathlib.Path(self._config["general"]["thisrun_work_dir"]),
            "computer": self.path_in_computer,  # Already Path type from _init_
            "exp_tree": pathlib.Path(
                self._config[self.component][f"experiment_{self['type']}_dir"]
            ),
            "run_tree": pathlib.Path(
                self._config[self.component][f"thisrun_{self['type']}_dir"]
            ),
        }

        for key, path in self.locations.items():
            if key == "computer" and path == None:
                self[f"absolute_path_in_{key}"] = None
            else:
                self[f"absolute_path_in_{key}"] = path.joinpath(self[f"name_in_{key}"])

    def _path_type(self, path: pathlib.Path) -> int:
        """
        Checks if the given ``path`` exists. If it does returns it's type, if it
        doesn't, returns ``None``.

        Parameters
        ----------
        path : pathlib.Path
            Path to be checked.

        Returns
        -------
        Enum value
            One of the values from FileType enumeration

        Raises
        ------
        TypeError
          - when ``path`` has incompatible type
          - when ``path`` is not identified
        """
        if not isinstance(path, (str, pathlib.Path)):
            datatype = type(path).__name__
            raise TypeError(
                f"Path ``{path}`` has an incompatible datatype ``{datatype}``. str or pathlib.Path is expected"
            )

        if isinstance(path, str):
            path = pathlib.Path(path)

        # NOTE: is_symlink() needs to come first because it is also a is_file()
        # NOTE: pathlib.Path().exists() also checks is the target of a symbolic link exists or not
        if path.is_symlink() and not path.exists():
            logger.warning(f"Broken link detected: {path}")
            return FileTypes.BROKEN_LINK
        elif not path.exists():
            logger.warning(f"File does not exist: {path}")
            return FileTypes.NOT_EXISTS
        elif path.is_symlink():
            return FileTypes.LINK
        elif path.is_file():
            return FileTypes.FILE
        elif path.is_dir():
            return FileTypes.DIR
        else:
            # probably, this will not happen
            raise TypeError(f"{path} can not be identified")

    def _check_file_syntax(self):
        """
        Checks for missing variables:
        - ``type``
        - ``path_in_computer`` if the file it an input for the experiment
        - ``name_in_computer`` if the file it an input for the experiment
        - ``name_in_work`` if the file it an output of the experiment

        It also checks whether ``type``'s value is correct.

        It notifies the user about this errors in the syntacm using
        ``esm_parser.error``.
        """
        error_text = ""
        missing_vars = ""
        types_text = ", ".join(self.all_model_filetypes)
        this_filedict = copy.deepcopy(self._original_filedict)
        self.input_file_types = input_file_types = ["config", "forcing", "input"]
        self.output_file_types = output_file_types = [
            "analysis",
            "couple",
            "log",
            "mon",
            "outdata",
            "restart",
            "viz",
            "ignore",
        ]

        if "type" not in self.keys():
            error_text = (
                f"{error_text}"
                f"- the ``type`` variable is missing. Please define a ``type`` "
                f"({types_text})\n"
            )
            missing_vars = (
                f"{missing_vars}    ``type``: forcing/input/restart/outdata/...\n"
            )
        elif self["type"] not in self.all_model_filetypes:
            error_text = (
                f"{error_text}"
                f"- ``{self['type']}`` is not a supported ``type`` "
                f"(``files.{self.name}.type``), please choose one of the following "
                f"types: {types_text}\n"
            )
            this_filedict["type"] = f"``{this_filedict['type']}``"

        if (
            "path_in_computer" not in self.keys()
            and self.get("type") in input_file_types
        ):
            error_text = (
                f"{error_text}"
                f"- the ``path_in_computer`` variable is missing. Please define a "
                f"``path_in_computer`` (i.e. the path to the file excluding its name)."
                f" NOTE: this is only required for {', '.join(input_file_types)} file "
                f"types\n"
            )
            missing_vars = (
                f"{missing_vars}    ``path_in_computer``: <path_to_file_dir>\n"
            )

        if (
            "name_in_computer" not in self.keys()
            and self.get("type") in input_file_types
        ):
            error_text = (
                f"{error_text}"
                f"- the ``name_in_computer`` variable is missing. Please define a ``name_in_computer`` "
                f"(i.e. name of the file in the work folder). NOTE: this is only required for "
                f"{', '.join(input_file_types)} file types\n"
            )
            missing_vars = f"{missing_vars}    ``name_in_computer``: <name_of_file_in_computer_dir>\n"

        if "name_in_work" not in self.keys() and self.get("type") in output_file_types:
            error_text = (
                f"{error_text}"
                f"- the ``name_in_work`` variable is missing. Please define a ``name_in_work`` "
                f"(i.e. name of the file in the work folder). NOTE: this is only required for "
                f"{', '.join(output_file_types)} file types\n"
            )
            missing_vars = (
                f"{missing_vars}    ``name_in_work``: <name_of_file_in_work_dir>\n"
            )

        missing_vars = (
            f"Please, complete/correct the following vars for your file:\n\n"
            f"{self.pretty_filedict(this_filedict)}"
            f"{missing_vars}"
        )

        if error_text:
            error_text = (
                f"The file dictionary ``{self.name}`` is missing relevant information "
                f"or is incorrect:\n{error_text}"
            )
            user_error("File Dictionaries", f"{error_text}\n{missing_vars}")

    def _check_path_in_computer_is_abs(self):
        if (
            self.path_in_computer is not None
            and not self.path_in_computer.is_absolute()
        ):
            user_error(
                "File Dictionaries",
                "The path defined for "
                f"``{self.component}.files.{self.name}.path_in_computer`` is not "
                f"absolute (``{self.path_in_computer}``). Please, always define an "
                "absolute path for the ``path_in_computer`` variable.",
            )

    def _check_source_and_target(
        self, source_path: pathlib.Path, target_path: pathlib.Path
    ) -> None:
        """
        Performs common checks for file movements

        Parameters
        ----------
        source_path : pathlib.Path
            path of the file to be copied / linked / moved

        target_path : pathlib.Path
            path of the file to be generated

        Returns
        -------
        True

        Raises
        ------
        Exception
            - If the ``source_path`` does not exist
            - If the ``target_path`` exists
            - If the parent dir of the ``target_path`` does not exist
        """
        # Types. Eg. file, dir, link, or None
        source_path_type = self._path_type(source_path)
        target_path_type = self._path_type(target_path)

        # Checks
        # ------
        # Source does not exist
        if source_path_type == FileTypes.NOT_EXISTS:
            err_msg = f"Unable to perform file operation. Source ``{source_path}`` does not exist!"
            raise FileNotFoundError(err_msg)

        # Target already exists
        target_exists = (
            os.path.exists(target_path) or target_path_type == FileTypes.LINK
        )
        if target_exists:
            err_msg = f"Unable to perform file operation. Target ``{target_path}`` already exists"
            raise FileExistsError(err_msg)

        # Target parent directory does not exist
        if not target_path.parent.exists():
            # TODO: we might consider creating it (Miguel)
            err_msg = f"Unable to perform file operation. Parent directory of the target ``{target_path}`` does not exist"
            raise FileNotFoundError(err_msg)

        # if source is a broken link. Ie. pointing to a non-existing file
        if source_path_type == FileTypes.BROKEN_LINK:
            err_msg = f"Unable to create symbolic link: ``{source_path}`` points to a broken path: {source_path.resolve()}"
            raise FileNotFoundError(err_msg)

        return True

    def pretty_filedict(self, filedict):
        """
        Returns a string in yaml format of the given file dictionary.

        Parameters
        ----------
        dict
            A file dictionary

        Returns
        -------
        str
            A string in yaml format of the given file dictionary
        """
        return yaml.dump({"files": {self.name: filedict}})


def copy_files(config):
    """Copies files"""
    # PG: No. We do not want this kind of general function. This is just to
    # demonstrate how the test would work
    return config


def resolve_file_movements(config: ConfigSetup) -> ConfigSetup:
    """Replaces former assemble() function"""
    # TODO: to be filled with functions
    # DONE: type annotation
    # DONE: basic unit test: test_resolve_file_movements
    return config
