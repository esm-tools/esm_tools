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
from typing import Any, AnyStr, Tuple, Type, Union

import dpath.util
import yaml
from esm_parser import ConfigSetup, user_error
from loguru import logger


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
            logger.warning(
                f"Skipping {method.__qualname__} as this file ({self}) is allowed to be missing!"
            )
            return None  # None is the default return, but let us be explicit here, as it is a bit confusing
        else:
            return method(self, *args, **kwargs)

    return inner_method


def _fname_has_date_stamp_info(fname, date, reqs=["%Y", "%m", "%d"]):
    date_attrs = {
        "%Y": "syear",
        "%m": "smonth",
        "%d": "sday",
        "%H": "shour",
        "%M": "sminute",
        "%S": "ssecond",
    }
    required_attrs = [getattr(date, v) for k, v in date_attrs.items() if k in reqs]
    # all(attr in fname for attr in required_attrs)
    for attr in required_attrs:
        if attr in fname:
            fname = fname.replace(attr, "checked", 1)
    return fname.count("checked") == len(reqs)


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
        """
        attrs_dict = dpath.util.get(
            full_config, attrs_address, separator=".", default={}
        )
        super().__init__(attrs_dict)
        self._original_filedict = copy.deepcopy(attrs_dict)
        self._config = full_config
        self._sim_date = full_config["general"]["current_date"]
        self.name = attrs_address.split(".")[-1]
        self.component = component = attrs_address.split(".")[0]
        self.all_model_filetypes = full_config["general"]["all_model_filetypes"]

        self._check_file_syntax()

        self.path_in_computer = pathlib.Path(self["path_in_computer"])

        # Complete tree names if not defined by the user
        self["name_in_run_tree"] = self.get(
            "name_in_run_tree", self["name_in_computer"]
        )
        self["name_in_exp_tree"] = self.get(
            "name_in_exp_tree", self["name_in_computer"]
        )
        if self["type"] not in ["restart", "outdata"]:
            self["name_in_work"] = self.get("name_in_work", self["name_in_computer"])

        # Complete paths for all possible locations
        self._resolve_paths()

        # Verbose set to true by default, for now at least
        self._verbose = full_config.get("general", {}).get("verbose", True)

        # Checks
        self._check_path_in_computer_is_abs()

    def __setattr__(self, name: str, value: Any) -> None:
        """Checks when changing dot attributes for disallowed values"""
        if name == "datestamp_format":
            self._check_datestamp_format_is_allowed(value)
        if name == "datestamp_method":
            self._check_datestamp_method_is_allowed(value)
        return super().__setattr__(name, value)

    def __setitem__(self, key: Any, value: Any) -> None:
        """Checks for changing with sim_file["my_key"] = "new_value""""
        if key == "datestamp_format":
            self._check_datestamp_format_is_allowed(value)
        if key == "datestamp_method":
            self._check_datestamp_method_is_allowed(value)
        return super().__setitem__(key, value)

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

    @property
    def datestamp_method(self):
        """
        Defines which datestamp_method shall be used when possibly including
        date stamps to the file. Valid choices are "never", "always",
        "avoid_overwrite".
        """
        datestamp_method = self.get(
            "datestamp_method", "avoid_overwrite"
        )  # This is the old default behaviour
        return datestamp_method

    @property
    def datestamp_format(self):
        """
        Defines which datestamp_format shall be used when possibly including
        date stamps to the file. Valid choices are "check_from_filename" and
        "append".
        """
        datestamp_format = self.get(
            "datestamp_format", "append"
        )  # This is the old default behaviour
        return datestamp_format

    def update(self, *args, **kwargs):
        """
        Standard dictionary update method, enhanced by additional safe-guards
        for particular values.
        """
        for k, v in dict(*args, **kwargs).items():
            if k == "datestamp_format":
                self._check_datestamp_format_is_allowed(v)
            if k == "datestamp_method":
                self._check_datestamp_method_is_allowed(v)
            self[k] = v

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

        # Datestamps
        if self.datestamp_method == "always":
            target_path = self._always_datestamp(target_path)
        if self.datestamp_method == "avoid_overwrite":
            target_path = self._avoid_override_datestamp(target_path)

        # Checks
        self._check_source_and_target(source_path, target_path)

        # Actual copy
        source_path_type = self._path_type(source_path)
        if source_path_type == "dir":
            copy_func = shutil.copytree
        else:
            copy_func = shutil.copy2
        try:
            copy_func(source_path, target_path)
            logger.success(f"Copied {source_path} --> {target_path}")
        except Exception as error:
            raise Exception(
                f"Unable to copy {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    @_allowed_to_be_missing
    def ln(self, source: AnyStr, target: AnyStr) -> None:
        """creates symbolic links from the path retrieved by ``source`` to the one by ``target``

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

        # This will need to be moved once Deniz implements this function to be
        # in the same style as the others.
        #
        # Datestamps
        if self.datestamp_method == "always":
            target_path = self._always_datestamp(target_path)
        if self.datestamp_method == "avoid_overwrite":
            target_path = self._avoid_override_datestamp(target_path)

        points_to_itself = source_path == target_path
        if points_to_itself:
            err_msg = (
                f"Unable to create symbolic link: `{source_path}` is linking to itself"
            )
            raise OSError(err_msg)

        if not os.path.exists(source_path):
            err_msg = f"Unable to create symbolic link: source file `{source_path}` does not exist"
            raise FileNotFoundError(err_msg)

        if os.path.isdir(target_path):
            err_msg = f"Unable to create symbolic link: `{target_path}` is a directory"
            raise OSError(err_msg)

        target_exists = os.path.exists(target_path) or os.path.islink(target_path)
        if target_exists:
            err_msg = (
                f"Unable to create symbolic link: `{target_path}`. File already exists"
            )
            raise FileExistsError(err_msg)

        target_parent = target_path.parent
        if not target_parent.exists():
            err_msg = (
                f"Unable to create symbolic link: `{target_parent}` does not exist"
            )
            raise FileNotFoundError(err_msg)

        os.symlink(source_path, target_path)

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
        if target not in self.locations:
            raise ValueError(
                f"Target is incorrectly defined, and needs to be in {self.locations}"
            )
        source_path = self[f"absolute_path_in_{source}"]
        target_path = self[f"absolute_path_in_{target}"]

        # Datestamps
        if self.datestamp_method == "always":
            target_path = self._always_datestamp(target_path)
        if self.datestamp_method == "avoid_overwrite":
            target_path = self._avoid_override_datestamp(target_path)
        # Checks
        self._check_source_and_target(source_path, target_path)

        # Perform the movement:
        try:
            source_path.rename(target_path)
            logger.success(f"Moved {source_path} --> {target_path}")
        except IOError as error:
            # NOTE(PG): Re-raise IOError with our own message:
            raise IOError(
                f"Unable to move {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    _allowed_datestamp_methods = {"never", "always", "avoid_overwrite"}
    """
    Set containing the allowed datestamp methods which can be chosen from.

    Notes on possible datestamp methods
    -----------------------------------
    never : str
        This will never add a datestamp to a file. **WARNING** this will
        cause you to possibly overwrite files.
    always : str
        This will always add a datestamp to a file, even if the canonical
        target name would not suggest one.
    avoid_overwrite : str
        This will add a datestamp at the end of the file, if the during the
        mv/cp/ln operation the file would be identically named.
    """

    _allowed_datestamp_formats = {"check_from_filename", "append"}
    """
    Set containing the allowed datestamp formats which can be chosen from.

    Notes on possible datestamp formats
    -----------------------------------
    check_from_filename : str
        This option will add a datestamp to a file, if the year, month, and day
        cannot be extracted from the standard declared filename.
    append : str
        This will add a datestamp at the end of the file, regardless of if it
        can be extracted from the file or not.
    """

    def _check_datestamp_method_is_allowed(self, datestamp_method):
        """
        Ensures that the datestamp method is in the defined valid set.
        """
        if datestamp_method not in self._allowed_datestamp_methods:
            raise ValueError(
                "The datestamp_method must be defined as one of never, always, or avoid_overwrite"
            )

    def _check_datestamp_format_is_allowed(self, datestamp_format):
        """
        Ensures that the datestamp format is in the defined valid set.
        """
        if datestamp_format not in self._allowed_datestamp_formats:
            raise ValueError(
                "The datestamp_format must be defined as one of check_from_filename or append"
            )

    def _resolve_paths(self) -> None:
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
            "computer": pathlib.Path(self["path_in_computer"]),
            "exp_tree": pathlib.Path(
                self._config[self.component][f"experiment_{self['type']}_dir"]
            ),
            "run_tree": pathlib.Path(
                self._config[self.component][f"thisrun_{self['type']}_dir"]
            ),
        }

        for key, path in self.locations.items():
            self[f"absolute_path_in_{key}"] = path.joinpath(self[f"name_in_{key}"])

    def _path_type(self, path: pathlib.Path) -> Union[str, bool]:
        """
        Checks if the given ``path`` exists. If it does returns it's type, if it
        doesn't, returns ``False``.

        Parameters
        ----------
        path : pathlib.Path
            Path to be checked.

        Returns
        -------
        str or bool
            If the path exists it returns its type as a string (``file``, ``dir``,
            ``link``). If it doesn't exist returns ``False``.
        """
        if path.is_file():
            return "file"
        elif path.is_dir():
            return "dir"
        elif path.is_symlink():
            return "link"
        elif not path.exists():
            return False
        else:
            raise Exception(f"Cannot identify the path's type of {path}")

    def _always_datestamp(self, fname):
        if self.datestamp_format == "append":
            return pathlib.Path(f"{fname}_{self._sim_date}")
        if self.datestamp_format == "check_from_filename":
            if _fname_has_date_stamp_info(fname, self._sim_date):
                return fname
            else:
                return pathlib.Path(f"{fname}_{self._sim_date}")

    def _avoid_override_datestamp(self, target: pathlib.Path) -> pathlib.Path:
        """
        If source and target are identical, adds the date stamp to the target.

        This method is used in the case that the object's attribute
        ``datestamp_method`` is set to ``avoid_overwrite``, and is called
        before the checks of each of ln, cp, and mv.

        Parameters
        ----------
        target : pathlib.Path

        Returns
        -------
        pathlib.Path :
            The new target that can be used
        """
        if target.exists():
            if self.datestamp_format == "append":
                target = pathlib.Path(f"{target}_{self._sim_date}")
            # The other case ("check_from_filename") is meaningless?
        return target

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
        input_file_types = ["config", "forcing", "input"]
        output_file_types = [
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
        if not self.path_in_computer.is_absolute():
            user_error(
                "File Dictionaries",
                "The path defined for "
                f"``{self.component}.files.{self.name}.path_in_computer`` is not "
                f"absolute (``{self.path_in_computer}``). Please, always define an "
                "absolute path for the ``path_in_computer`` variable.",
            )

    def _check_source_and_target(self, source_path, target_path):
        """
        Performs checks for file movements

        Raises
        ------
        Exception
            - If the ``source_path`` does not exist
            - If the ``target_path`` exists
            - If the parent dir of the ``target_path`` does not exist
        """

        # Types
        source_path_type = self._path_type(source_path)
        target_path_type = self._path_type(target_path)
        target_path_parent_type = self._path_type(target_path.parent)

        # Checks
        # ------
        # Source exists
        if not source_path_type:
            raise Exception(f"Source file ``{source_path}`` does not exist!")
        # Target exist
        if target_path_type:
            # TODO: Change this behavior
            raise Exception(f"File ``{target_path_type}`` already exists!")
        # Target dir exists
        if not target_path_parent_type:
            # TODO: we might consider creating it
            raise Exception(
                f"Target directory ``{target_path_parent_type}`` does not exist!"
            )

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
