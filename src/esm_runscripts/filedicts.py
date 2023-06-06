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
import glob
import inspect
import os
import pathlib
import shutil
import sys
from enum import Enum, auto
from typing import Any, AnyStr, Dict, Iterator

import dpath.util
import yaml
from loguru import logger

from esm_calendar import Date
# These should be relative
from esm_parser import ConfigSetup, user_error

# Set up the logger:
logger.remove()
LEVEL = "ERROR"
LOGGING_FORMAT = "[{time:HH:mm:ss  DD/MM/YYYY}]  <level>|{level}|  [{file} -> {function}() line:{line: >3}] >> </level>{message}"
logger.add(sys.stderr, level=LEVEL, format=LOGGING_FORMAT)


class DatestampFormatError(Exception):
    """Raise this error when the Datestamp formatter is incorrectly used"""


class DotDict(dict):
    """
    A dictionary subclass that allows accessing data via dot-attributes and keeps changes between dictionary
    keys and dot-attributes in sync.

    This class inherits from the built-in `dict` class and overrides the `__getattr__` and `__setattr__` methods
    to provide dot-attribute access to dictionary items. When an attribute is accessed using dot notation, the
    corresponding dictionary key is returned. Similarly, when an attribute is set using dot notation, the corresponding
    dictionary key is updated. Changes made using dictionary keys are also reflected in the dot-attributes.

    Note that this implementation assumes that the keys in the dictionary are strings, since dot-attributes can
    only be strings in Python.
    """

    def __getattr__(self, attr):
        try:
            return self[attr]
        except KeyError:
            raise AttributeError(
                f"'{self.__class__.__name__}' object has no attribute '{attr}'"
            )

    def __setattr__(self, attr, value):
        self[attr] = value


class NameIterEnum(Enum):
    def __iter__(self) -> Iterator[str]:
        """Returns list of names of the iteration, guarentted to be lower-case"""
        return iter(str(name).lower() for name in self.__members__)


class FileTypes(NameIterEnum):
    """Describes which type a file might belong to, e.g. input, outdata, forcing"""

    ANALYSIS = auto()
    CONFIG = auto()
    COUPLE = auto()
    FORCING = auto()
    IGNORE = auto()
    INPUT = auto()
    LOG = auto()
    MON = auto()
    OUTDATA = auto()
    RESTART = auto()
    VIZ = auto()


class FileLocations(NameIterEnum):
    """Posibile locations for a file"""

    COMPUTER = auto()
    EXP_TREE = auto()
    RUN_TREE = auto()
    WORK = auto()


class FileStatus(NameIterEnum):
    """Describes which status a particular file might have, e.g. ``FILE``, ``NOT_EXISTS``, ``BROKEN_LINK``."""

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
    ``_allowed_to_be_missing`` set to ``True``. You get a warning via the logger
    giving the full method name that was decorated and a representation of the
    file that was attempted to be moved, linked, or copied.

    Usage Example
    -------------
    Given you have an instantiated simulation file under ``sim_file`` with
    the following property in YAML::

        echam:
            files:
                sim_file:
                    allowed_to_be_missing: True
                    ...other properties...

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

    Notes
    -----
    Why does this thing have an underscore and the attribute does not?

    Because this is a decorator to enable the functionality, and I do not want
    anyone to use this decorator outside of this file, so, we start with ``_``
    to denote that ("Private", even though Python does not formally have that).

    And, the attribute might be interesting for the end-user, not the programmer.

    That's why.
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
                # None is the default return, but let us be explicit here, as it is a bit confusing
                return None
        else:
            return method(self, *args, **kwargs)

    return inner_method


def _fname_has_date_stamp_info(fname, date, reqs=["%Y", "%m", "%d"]):
    """
    Checks if a particular file has all elements of a particular date in its name.

    Parameters
    ----------
    fname : str
        The name of the file to check
    date : esm_calendar.Date
        The date to be checked against
    reqs : list of str
        A list of ``strftime`` compliant strings to determine which elements of
        the date to check. Compatible with %Y %m %d %H %M %S (year, month, day,
        hour, minute, second)

    Returns
    -------
    bool :
        True if all elements appear in the filename, False otherwise.

    """
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


def _globbing(method):
    """
    Decorator method for ``SimulationFile``'s methods ``cp``, ``mv``, ``ln``, that
    enables globbing. If a ``*`` is found on the ``source`` or ``target`` the globbing
    logic is activated, and consist of:
    - run checks for globbing syntax
    - check if any file matches the globbing pattern
    - construct one instance of ``SimulationFile`` for each file matching the globbing
    - run the ``method`` for that particular file

    Parameters
    ----------
    method : method
        The decorated method (``cp``, ``mv``, ``ln``)

    Returns
    -------
    method : method
        If no globbing is needed, returns the method as it was given originally.
    """

    @functools.wraps(method)
    def inner_method(self, source, target, *args, **kwargs):
        method_name = method.__name__
        source_name = self[f"name_in_{source}"]
        target_name = self[f"name_in_{target}"]

        if "*" in source_name or "*" in target_name:
            # Get wildcard patterns
            source_pattern = source_name.split("*")
            target_pattern = target_name.split("*")

            # Check wild cards syntax
            self._wild_card_check(source_pattern, target_pattern)

            # Obtain source files
            glob_source_paths = self._find_globbing_files(source)

            # Extract globbing source names
            glob_source_names = [
                pathlib.Path(glob_source_path).name
                for glob_source_path in glob_source_paths
            ]

            # Solve the globbing target names
            glob_target_names = []
            for glob_source_name in glob_source_names:
                glob_target_name = glob_source_name
                for sp, tp in zip(source_pattern, target_pattern):
                    glob_target_name = glob_target_name.replace(sp, tp)
                glob_target_names.append(glob_target_name)

            # Loop through source files
            for glob_source_name, glob_target_name in zip(
                glob_source_names, glob_target_names
            ):
                # Create a new simulation file object for this specific glob file's config
                glob_dict = dict(self)
                glob_dict[f"name_in_{source}"] = glob_source_name
                glob_dict[f"name_in_{target}"] = glob_target_name
                glob_file = SimulationFile(**glob_dict)
                # Use method
                this_method = getattr(glob_file, method_name)
                return this_method(source, target, *args, **kwargs)
        else:
            return method(self, source, target, *args, **kwargs)

    return inner_method


class SimulationFile(DotDict):
    """
    Describes a file used within a ESM Simulation.

    A ``SimulationFile`` object describes one particular file used within an
    ``esm-tools`` run. This description is similar to a standard Python
    dictionary. Beyond the standard dictionary methods and attributes, there
    are a variety of attributes that describe how the file should behave, as
    well as a few additional methods you can use to relocate the file around on
    the system. Please see the detailed documentation on each of the methods
    for more specifics, but in summary, a ``SimulationFile`` has the following
    additional functions::

        >>> sim_file = SimulationFile(...)  # doctest: +SKIP
        >>> sim_file.mv("computer", "work")  # doctest: +SKIP
        >>> sim_file.ln("work", "run_tree")  # doctest: +SKIP
        >>> sim_file.cp("run_tree", "exp_tree")  # doctest: +SKIP

    You get extra functions for moving, copying, or linking a file from one
    location to another. Location keys are desccribed in detail in the Notes
    section.

    Furthermore, there are a few attributes that you should be aware of. These
    include:

    * ``name`` : A human readable name for the file.
    * ``allowed_to_be_missing`` : A ``bool`` value to set a certain file as
      allowed to be missing or not. In case it is, the cp/ln/mv command will not
      fail if the original file is not found.
    * ``datestamp_method`` : Sets how a datestamp should be added. See
      ``_allowed_datestamp_methods`` for more information.
    * ``datestamp_format`` : Sets how a datestamp should be formatted. See
      ``_allowed_datestamp_methods`` for more information.

    Example
    -------
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

        >>> sim_file = SimulationFile.from_config(config, 'echam.files.jan_surf')  # doctest: +SKIP

    You could then copy the file to the experiment folder::

        >>> sim_file.cp("pool", "work")  # doctest: +SKIP

    Notes
    -----
    A file can be located in one of these categories (``LOCATION_KEYS``):
    - computer: pool/source directory (for input files)
    - exp_tree: file in the category directory in experiment directory (eg. input, output, ...)
    - run_tree: file in the experiment/run_<DATE>/<CATEGORY>/ directory
    - work:     file in the current work directory. Eg. experiment/run_<DATE>/work/

    LOCATION_KEY is one of the strings defined in LOCATION_KEY list
    - name_in_<LOCATION_KEY> : file name (without path) in the LOCATION_KEY
      - eg. name_in_computer: T63CORE2_jan_surf.nc
      - eg. name_in_work: unit.24
    - absolute_path_in_<LOCATION_KEY> : absolute path in the LOCATION_KEY
      - eg. absolute_path_in_run_tree: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam/T63CORE2_jan_surf.nc
    """

    # Should all be replaced by Enums:
    input_file_kinds = [
        "config",
        "forcing",
        "input",
    ]
    output_file_kinds = [
        "analysis",
        "couple",
        "log",
        "mon",
        "outdata",
        "restart",
        "viz",
        "ignore",
    ]
    all_model_filekinds = (
        input_file_kinds + output_file_kinds + ["src"]
    )  # FIXME: In review, someone should check this

    def __init__(
        self,
        name="",
        component="",
        paths={},
        kind=None,
        allowed_to_be_missing=False,
        description="",
        filetype="",
        datestamp_method="avoid_overwrite",
        **kwargs,
    ):
        # self.name = name
        # self.paths = paths
        # self.kind = kind
        # self.allowed_to_be_missing = allowed_to_be_missing
        # self.description = description
        # self.filetype = filetype
        # self._datestamp_method = datestamp_method
        # self.locations = {k: v.parent for k, v in self.paths.items()}

        super().__init__(
            name=name,
            component=component,
            paths={k: pathlib.Path(v) for k, v in paths.items()},
            kind=kind,
            allowed_to_be_missing=allowed_to_be_missing,
            description=description,
            filetype=filetype,
            datestamp_method=datestamp_method,
            locations={k: pathlib.Path(v).parent for k, v in paths.items()},
            **kwargs,
        )

        for location, path in paths.items():
            for attr_name, attr_value in {
                f"absolute_path_in_{location}": path.resolve(),
                f"name_in_{location}": path.name,
            }.items():
                if attr_name not in self:
                    self[attr_name] = attr_value

        # possible paths for files:

        # location_keys = ["computer", "exp_tree", "run_tree", "work"]
        # initialize the locations and complete paths for all possible locations
        # self.locations = dict.fromkeys(location_keys, None)

    # Current Attributes:
    # {'absolute_path_in_computer': PosixPath('/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc'),
    #  'absolute_path_in_exp_tree': PosixPath('/work/ollie/pgierz/some_exp/input/echam/T63CORE2_jan_surf.nc'),
    #  'absolute_path_in_run_tree': PosixPath('/work/ollie/pgierz/some_exp/run_20000101-20000101/input/echam/T63CORE2_jan_surf.nc'),
    #  'absolute_path_in_work': PosixPath('/work/ollie/pgierz/some_exp/run_20010101-20010101/work/unit.24'),
    #  'allowed_to_be_missing': False,
    #  'description': 'Initial values used for the simulation, including properties such as geopotential, temperature, pressure\n',
    #  'filetype': 'NetCDF',
    #  'name_in_computer': 'T63CORE2_jan_surf.nc',
    #  'name_in_exp_tree': 'T63CORE2_jan_surf.nc',
    #  'name_in_run_tree': 'T63CORE2_jan_surf.nc',
    #  'name_in_work': 'unit.24',
    #  'path_in_computer': '/work/ollie/pool/ECHAM/T63',
    #  'type': 'input'}

    ##############################################################################################
    # Initialize from esm-tools config
    ##############################################################################################

    @classmethod
    def from_config(cls, full_config: dict, attrs_address: str):
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
        # _original_filedict = copy.deepcopy(attrs_dict)

        name = attrs_address.split(".")[-1]
        component = attrs_address.split(".")[0]
        # Check if attr dict gives a sufficient representation of a file
        cls._check_config_syntax(attrs_dict, name)
        kind = attrs_dict.get("kind")
        # Complete tree names if not defined by the user
        names = cls._complete_file_names(attrs_dict)
        paths = cls._resolve_abs_paths(full_config, component, attrs_dict, names, kind)
        obj = cls(name=name, paths=paths, **attrs_dict)

        # Verbose set to true by default, for now at least
        obj._verbose = full_config.get("general", {}).get("verbose", True)

        # Checks
        obj._check_path_in_computer_is_abs(paths, component, name)
        return obj

    @classmethod
    def _check_config_syntax(cls, cfg, name) -> None:
        """
        Checks for missing variables:
        - ``kind``
        - ``path_in_computer`` if the file it an input for the experiment
        - ``name_in_computer`` if the file it an input for the experiment
        - ``name_in_work`` if the file it an output of the experiment

        It also checks whether ``kind``'s value is correct.

        It notifies the user about these errors in the syntax using
        ``esm_parser.error``.
        """
        error_text = ""
        missing_vars = ""
        kinds_text = ", ".join(cls.all_model_filekinds)
        this_filedict = copy.deepcopy(cfg)

        if "kind" not in cfg.keys():
            error_text = (
                f"{error_text}"
                f"- the ``kind`` variable is missing. Please define a ``kind`` "
                f"({kinds_text})\n"
            )
            missing_vars = (
                f"{missing_vars}    ``kind``: forcing/input/restart/outdata/...\n"
            )
        elif cfg["kind"] not in cls.all_model_filekinds:
            error_text = (
                f"{error_text}"
                f"- ``{cfg['kind']}`` is not a supported ``kind`` "
                f"(``files.{name}.kind``), please choose one of the following "
                f"kinds: {kinds_text}\n"
            )
            this_filedict["kind"] = f"``{this_filedict['kind']}``"

        if (
            "path_in_computer" not in cfg.keys()
            and cfg.get("kind") in cls.input_file_kinds
        ):
            error_text = (
                f"{error_text}"
                f"- the ``path_in_computer`` variable is missing. Please define a "
                f"``path_in_computer`` (i.e. the path to the file excluding its name)."
                f" NOTE: this is only required for {', '.join(cls.input_file_kinds)} file "
                f"kinds\n"
            )
            missing_vars = (
                f"{missing_vars}    ``path_in_computer``: <path_to_file_dir>\n"
            )

        if (
            "name_in_computer" not in cfg.keys()
            and cfg.get("kind") in cls.input_file_kinds
        ):
            error_text = (
                f"{error_text}"
                f"- the ``name_in_computer`` variable is missing. Please define a ``name_in_computer`` "
                f"(i.e. name of the file in the work folder). NOTE: this is only required for "
                f"{', '.join(cls.input_file_kinds)} file kinds\n"
            )
            missing_vars = f"{missing_vars}    ``name_in_computer``: <name_of_file_in_computer_dir>\n"

        if (
            "name_in_work" not in cfg.keys()
            and cfg.get("kind") in cls.output_file_kinds
        ):
            error_text = (
                f"{error_text}"
                f"- the ``name_in_work`` variable is missing. Please define a ``name_in_work`` "
                f"(i.e. name of the file in the work folder). NOTE: this is only required for "
                f"{', '.join(cls.output_file_kinds)} file kinds\n"
            )
            missing_vars = (
                f"{missing_vars}    ``name_in_work``: <name_of_file_in_work_dir>\n"
            )

        missing_vars = (
            f"Please, complete/correct the following vars for your file:\n\n"
            f"{_pretty_filedict(name, this_filedict)}"
            f"{missing_vars}"
        )

        if error_text:
            error_text = (
                f"The file dictionary ``{name}`` is missing relevant information "
                f"or is incorrect:\n{error_text}"
            )
            user_error("File Dictionaries", f"{error_text}\n{missing_vars}")

    @classmethod
    def _complete_file_names(cls, cfg):
        """
        Complete missing names in the file with the default name, depending whether
        the file is of kind ``input`` or ``output``.
        """
        if cfg["kind"] in cls.input_file_kinds:
            default_name = cfg["name_in_computer"]
        elif cfg["kind"] in cls.output_file_kinds:
            default_name = cfg["name_in_work"]
        else:
            raise TypeError(f"Unknown file kind: {cfg['kind']}")
        names = {}
        names["computer"] = cfg.get("name_in_computer", default_name)
        names["run_tree"] = cfg.get("name_in_run_tree", default_name)
        names["exp_tree"] = cfg.get("name_in_exp_tree", default_name)
        names["work"] = cfg.get("name_in_work", default_name)
        return names

    @staticmethod
    def _resolve_abs_paths(config, component, attrs_dict, names, kind) -> Dict:
        # NOTE(PG): I....hate this! :-(
        """
        Builds the absolute paths of the file for the different locations
        (``computer``, ``work``, ``exp_tree``, ``run_tree``) using the information
        about the experiment paths in ``config`` and the
        ``self["path_in_computer"]``.

        It defines these new variables in the ``SimulationFile`` dictionary:
        - ``self["absolute_path_in_work"]``
        - ``self["absolute_path_in_computer"]``
        - ``self["absolute_path_in_run_tree"]``
        - ``self["absolute_path_in_exp_tree"]``
        """
        locations = {
            "work": pathlib.Path(config["general"]["thisrun_work_dir"]),
            "computer": pathlib.Path(attrs_dict.get("path_in_computer", "/dev/null")),
            "exp_tree": pathlib.Path(config[component][f"experiment_{kind}_dir"]),
            "run_tree": pathlib.Path(config[component][f"thisrun_{kind}_dir"]),
        }

        return {key: path.joinpath(names[key]) for key, path in locations.items()}

    @staticmethod
    def _check_path_in_computer_is_abs(paths, component, name):
        if paths["computer"] is not None and not paths["computer"].is_absolute():
            user_error(
                "File Dictionaries",
                "The path defined for "
                f"``{component}.files.{name}.path_in_computer`` is not "
                f"absolute (``{paths['computer']}``). Please, always define an "
                "absolute path for the ``path_in_computer`` variable.",
            )

    ##############################################################################################
    # Overrides of standard dict methods
    ##############################################################################################

    def __setattr__(self, name: str, value: Any) -> None:
        """Checks when changing dot attributes for disallowed values"""
        if name == "datestamp_format":
            self._check_datestamp_format_is_allowed(value)
        if name == "datestamp_method":
            self._check_datestamp_method_is_allowed(value)
        return super().__setattr__(name, value)

    def __setitem__(self, key: Any, value: Any) -> None:
        """Checks for changing with sim_file['my_key'] = 'new_value'"""
        if key == "datestamp_format":
            self._check_datestamp_format_is_allowed(value)
        if key == "datestamp_method":
            self._check_datestamp_method_is_allowed(value)
        return super().__setitem__(key, value)

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

    ##############################################################################################

    ##############################################################################################
    # Object Properties
    ##############################################################################################

    @property
    def datestamp_method(self):
        """
        Defines which datestamp_method shall be used when possibly including
        date stamps to the file. Valid choices are "never", "always",
        "avoid_overwrite".
        """
        return self._datestamp_method

    @datestamp_method.setter
    def datestamp_method(self, new_attr_value):
        """
        Sets a new value for datestamp method.
        """
        # NOTE(PG): The checks could go here
        self._datestamp_method = new_attr_value

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

    ##############################################################################################
    # Main Methods
    ##############################################################################################
    @_globbing
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

        # General Checks
        # TODO (deniz): need to add higher level exception handler (eg. user_error)
        self._check_source_and_target(source_path, target_path)

        # Actual copy
        source_path_type = self._path_type(source_path)
        if source_path_type == FileStatus.DIR:
            copy_func = shutil.copytree
        else:
            copy_func = shutil.copy2
        try:
            copy_func(source_path, target_path)
            logger.success(f"Copied {source_path} --> {target_path}")
        except IOError as error:
            raise IOError(
                f"Unable to copy {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    @_globbing
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
        if source not in self.locations:
            raise ValueError(
                f"Source is incorrectly defined, and needs to be in {self.locations}"
            )
        if target not in self.locations:
            raise ValueError(
                f"Target is incorrectly defined, and needs to be in {self.locations}"
            )
        # full paths: directory path / file name
        source_path = self[f"absolute_path_in_{source}"]
        target_path = self[f"absolute_path_in_{target}"]

        # Datestamps
        if self.datestamp_method == "always":
            target_path = self._always_datestamp(target_path)
        if self.datestamp_method == "avoid_overwrite":
            target_path = self._avoid_override_datestamp(target_path)
        # General Checks
        # TODO (deniz): need to add higher level exception handler (eg. user_error)
        self._check_source_and_target(source_path, target_path)

        try:
            os.symlink(source_path, target_path)
        except IOError as error:
            raise IOError(
                f"Unable to link {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    @_globbing
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
        # General Checks
        # TODO (deniz): need to add higher level exception handler (eg. user_error)
        self._check_source_and_target(source_path, target_path)

        # Perform the movement:
        try:
            source_path.rename(target_path)
            logger.success(f"Moved {source_path} --> {target_path}")
        except IOError as error:
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

    def _path_type(self, path: pathlib.Path) -> FileStatus:
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

        path = pathlib.Path(path)

        # NOTE: is_symlink() needs to come first because it is also a is_file()
        # NOTE: pathlib.Path().exists() also checks is the target of a symbolic link exists or not
        if path.is_symlink() and not path.exists():
            return FileStatus.BROKEN_LINK
        elif not path.exists():
            return FileStatus.NOT_EXISTS
        elif path.is_symlink():
            return FileStatus.LINK
        elif path.is_file():
            return FileStatus.FILE
        elif path.is_dir():
            return FileStatus.DIR
        else:
            # probably, this will not happen
            raise TypeError(f"{path} can not be identified")

    def _always_datestamp(self, fname) -> pathlib.Path:
        """
        Method called when ``always`` is the ``datestamp_method.

        Appends the datestamp in any case if ``datestamp_format`` is
        ``append``. Appends the datestamp only if it is not obviously in the
        filename if the ``datestamp_format`` is ``check_from_filename``. Only
        appends to files or links, not directories.

        Parameters
        ----------
        fname : pathlib.Path
            The file who's name should be modified.

        Returns
        -------
        pathlib.Path
            A modified file with an added date stamp.
        """
        if fname.is_dir():
            return fname
        if self.datestamp_format == "append":
            return pathlib.Path(f"{fname}_{self._sim_date}")
        if self.datestamp_format == "check_from_filename":
            if _fname_has_date_stamp_info(fname, self._sim_date):
                return fname
            else:
                return pathlib.Path(f"{fname}_{self._sim_date}")
        raise DatestampFormatError(
            "Unknown Datestamp formatting type, please use `append` or `check_from_filename`"
        )

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
        if target.exists() and not target.is_dir():
            if self.datestamp_format == "append":
                target = pathlib.Path(f"{target}_{self._sim_date}")
            # The other case ("check_from_filename") is meaningless?
        return target

    @staticmethod
    def _wild_card_check(source_pattern: list, target_pattern: list) -> bool:
        """
        Checks for syntax mistakes. If any were found, it notifies the user about these
        errors in the syntax using ``esm_parser.error``.

        Parameters
        ----------
        source_pattern : list
            A list including the different pieces of the source name pattern
        target_pattern : list
            A list including the different pieces of the target name pattern

        Returns
        -------
        bool :
            If no issues were found
        """
        target_and_source_patterns_match = len(target_pattern) == len(source_pattern)
        if not target_and_source_patterns_match:
            user_error(
                "Wild card",
                (
                    "The wild card pattern of the source "
                    + f"``{source_pattern}`` does not match with the "
                    + f"target ``{target_pattern}``. Make sure the "
                    + f"that the number of ``*`` are the same in both "
                    + f"sources and targets."
                ),
            )

        return target_and_source_patterns_match

    def _find_globbing_files(self, location: str) -> list:
        """
        Lists the files matching the globbing path of the given ``location``, and
        notifies the user if none were found, via ``esm_parser.user_error``.

        Parameters
        ----------
        location : str
            The location string (``work``, ``computer``, ``exp_tree``, ``run_tree``)

        Returns
        -------
        glob_paths : list
            List of paths found matching the globbing case for the ``location`` pattern
        """
        absolute_path_in_location = str(self[f"absolute_path_in_{location}"])
        glob_paths = glob.glob(absolute_path_in_location)

        # Check that there are any source files available
        if len(glob_paths) == 0:
            user_error(
                "Globbing",
                f"No files found for the globbing pattern "
                f"``{absolute_path_in_location}``.",
            )

        return glob_paths

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
        if source_path_type == FileStatus.NOT_EXISTS:
            err_msg = f"Unable to perform file operation. Source ``{source_path}`` does not exist!"
            raise FileNotFoundError(err_msg)

        # Target already exists
        target_exists = (
            os.path.exists(target_path) or target_path_type == FileStatus.LINK
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
        if source_path_type == FileStatus.BROKEN_LINK:
            err_msg = f"Unable to create symbolic link: ``{source_path}`` points to a broken path: {source_path.resolve()}"
            raise FileNotFoundError(err_msg)


class DatedSimulationFile(SimulationFile):
    """A SimultionFile which also needs to know about dates"""

    def __init__(
        self,
        date=Date("2000-01-01"),
        **kwargs,
    ):
        super().__init__(**kwargs)
        self._sim_date = date

    @classmethod
    def from_config(cls, full_config: dict, attrs_address: str, date: Date):
        obj = super().from_config(full_config, attrs_address)
        obj._sim_date = date
        return obj


def _pretty_filedict(name, filedict):
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
    return yaml.dump({"files": {name: filedict}})


def copy_files(config):
    """Copies files"""
    # PG: No. We do not want this kind of general function. This is just to
    # demonstrate how the test would work
    return config


class SimulationFileCollection(dict):
    """
    Once instanciated, searches in the ``config`` dictionary for the ``files`` keys.
    This class contains the methods to: 1) instanciate each of the files defined in
    ``files`` as ``SimulationFile`` objects and 2) loop through these objects
    triggering the desire file movement.
    """

    def __init__(self):
        pass

    # PG: Not sure I need this...
    @property
    def _defined_from(self):
        stack = inspect.stack()
        caller_frame = stack[1]  # Get the frame of the caller
        caller_name = caller_frame.function
        return caller_name

    @classmethod
    def from_config(cls, config: dict):
        sim_files = cls()
        for component in config["general"]["valid_model_names"]:
            config_address = f"{component}.files"
            for file_key in dpath.util.get(
                config, config_address, separator="."
            ).keys():
                sim_files[file_key] = SimulationFile.from_config(
                    config, f"{config_address}.{file_key}"
                )
        return sim_files

    def _gather_file_movements(self) -> None:
        """Puts the methods for each file movement into the dictionary as callable values behind the `_filesystem_op` key"""
        for sim_file_id, sim_file_obj in self.items():
            movement_type = sim_file_obj.get("movement_type", "cp")
            if movement_type == "mv":
                self[sim_file_id]["_filesystem_op"] = getattr(sim_file_obj, "mv")
            elif movement_type == "cp":
                self[sim_file_id]["_filesystem_op"] = getattr(sim_file_obj, "cp")
            elif movement_type == "ln":
                self[sim_file_id]["_filesystem_op"] = getattr(sim_file_obj, "ln")
            else:
                raise ValueError(
                    f"Movement Type is not defined correctly, please use `mv`, `cp` or `ln` for {sim_file_id}"
                )

    def execute_filesystem_operation(
        self, config: ConfigSetup
    ) -> ConfigSetup:  # , from: pathlib.Path | str, to: pathlib.Path | str) -> None:
        self._gather_file_movements()
        for sim_file_id, sim_file_obj in self.items():
            logger.info(f"Processing {sim_file_id}")
            if config["general"]["jobtype"] == "prepcompute":
                src, dest = "pool", "work"
            elif config["general"]["jobtype"] == "tidy":
                src, dest = "work", "exp_tree"
            else:
                raise ValueError(f"Incorrect jobtype specified for {sim_file_obj}")
            sim_file_obj["_filesystem_op"](src, dest)
        return config


def resolve_file_movements(config: ConfigSetup) -> ConfigSetup:
    """
    Runs all methods required to get files into their correct locations. This will
    instantiate the ``SimulationFiles`` class. It's called by the recipe manager.

    Parameters
    ----------
    config : ConfigSetup
        The complete simulation configuration.

    Returns
    -------
    config : ConfigSetup
        The complete simulation configuration, potentially modified.
    """
    sim_file_collection = SimulationFileCollection.from_config(config)
    config = sim_file_collection.execute_filesystem_operation(config)
    return config
