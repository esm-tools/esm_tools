"""
The file-dictionary implementation
"""
from typing import Type, Tuple
import pathlib
import shutil

import dpath.util
from loguru import logger

from esm_parser import ConfigSetup, user_error

class SimulationFile(dict):
    """
    Desribes a file used within a ESM Simulation.

    Given a config, you should be able to use this in YAML::

        $ cat dummy_config.yaml

        echam:
            files:
                jan_surf:
                    name_in_computer: T63CORE2_jan_surf.nc
                    name_in_work: unit.24
                    filetype: NetCDF
                    description: >
                        Initial values used for the simulation, including
                        properties such as geopotential, temperature, pressure

    And, assuming config is as described above::

        >>> sim_file = SimulationFile(config, ['echam']['files']['jan_surf'])

    You could then copy the file to the experiment folder::

        >>> sim_file.cp_to_exp_tree()
    """

    def __init__(self, full_config, attrs_address):
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
        self._config = full_config
        self.name = name = attrs_address.split(".")[-1]
        self.component = component = attrs_address.split(".")[0]
        self.locations = {
            "work": pathlib.Path(full_config[component]["thisrun_work_dir"]),
            "computer": pathlib.Path(self["path_in_computer"]),
#            "exp_tree": pathlib.Path(full_config[component]["exp_dir"]), # This name is incorrect and depends on the type of file (to be resolved somewhere else before feeding it here)
#            "run_tree": pathlib.Path(full_config[component]["thisrun_dir"]), # This name is incorrect and depends on the type of file (to be resolved somewhere else before feeding it here)

        }
        self.names = {
            "work": pathlib.Path(self["name_in_work"]),
            "computer": pathlib.Path(self["name_in_computer"]),
        }
        # Allow dot access:
        self.path_in_work = self.locations["work"]
        self.path_in_computer = self.locations["computer"]
#        self.path_exp_tree = self.locations["exp_tree"] # TODO: uncomment when lines above are fixed
#        self.path_run_tree = self.locations["run_tree"] # TODO: uncomment when lines above are fixed

        # Verbose set to true by default, for now at least
        self.verbose = full_config.get("general", {}).get("verbose", True)

        # Checks
        self._check_path_in_computer_is_abs()

    def cp(self, source, target) -> None:
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
        # Build target and source paths
        source_path = self.locations[source].joinpath(self.names[source])
        target_path = self.locations[target].joinpath(self.names[target])

        # Checks
        self.check_source_and_target(source_path, target_path)
        source_path_type = self.path_type(source_path)

        # Actual copy
        if source_path_type == "dir":
            copy_func = shutil.copytree
        else:
            copy_func = shutil.copy2
        try:
            copy_func(source_path, target_path)
        except Exception as error:
            raise Exception(
                f"Unable to copy {source_path} to {target_path}\n\n"
                f"Exception details:\n{error}"
            )

    def ln(self) -> None:
        pass

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
                f"source is incorrectly defined, and needs to be in {self.locations}"
            )
        source_path, target_path = self._determine_names(source, target)
        # Perform the movement:
        try:
            source_path.rename(target_path)
            logger.success(f"Moved {source_path} --> {target_path}")
        except IOError:
            # NOTE(PG): Re-raise IOError with our own message:
            raise IOError(f"Unable to move {source_path} to {target_path}")

    def _determine_names(
        self, source: str, target: str
    ) -> Tuple[pathlib.Path, pathlib.Path]:
        """
        Determines names for source and target, depending on name and path

        Source and target should be on of work, computer, exp_tree, or run_tree.
        You need to specify name_in_`source` and name_in_`target` in the
        object's attrs_dict.

        Parameters
        ----------
        source : str
            One of ``"computer"``, ``"work"``, ``"exp_tree"``, "``run_tree``"
        target : str
            One of ``"computer"``, ``"work"``, ``"exp_tree"``, "``run_tree``"

        Returns
        -------
        tuple of pathlib.Path, pathlib.Path :
           The calculated source path and target path.

        """
        # Figure out names in source and target:
        source_name = self[f"name_in_{source}"]
        target_name = self[f"name_in_{target}"]
        # Relative path in source and target
        source_relative_path = self.get(f"path_in_{source}", ".")
        target_relative_path = self.get(f"path_in_{target}", ".")
        # Build target and source paths:
        source_path = self.locations[source].joinpath(source_relative_path, source_name)
        target_path = self.locations[target].joinpath(target_relative_path, target_name)
        return source_path, target_path

    def path_type(self, path: pathlib.Path) -> str or bool:
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

    def _check_path_in_computer_is_abs(self):
        if not self.path_in_computer.is_absolute():
            user_error(
                "File Dictionaries",
                "The path defined for "
                f"``{self.component}.files.{self.name}.path_in_computer`` is not "
                "abosulte. Please, always define an absolute path for the "
                "``path_in_computer`` variable."
            )

    def check_source_and_target(self, source_path, target_path):
        """
        Performs checks for file movements

        Raises
        ------
        Exception
            - If the ``target_path`` exists
            - If the parent dir of the ``target_path`` does not exist

        Note
        ----
            - If the ``source_path`` does not exist adds it to a list of missing files
              in the ``self._config`` (TODO)
        """

        # Types
        source_path_type = self.path_type(source_path)
        target_path_type = self.path_type(target_path)
        target_path_parent_type = self.path_type(target_path.parent)

        # Checks
        # ------
        # Source exists
        if not source_path_type:
            if self.verbose:
                print(f"Source file ``{source_path}`` does not exist!") # I'll change this when we have loguru available
            # TODO: Add the missing file to the config["<model>"]["missing_files"]
        # Target exist
        if target_path_type:
            # TODO: Change this behavior
            raise Exception("File already exists!")
        # Target dir exists
        if not target_path_parent_type:
            # TODO: we might consider creating it
            raise Exception("Target directory does not exist!")

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
