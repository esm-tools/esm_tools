"""
The file-dictionary implementation
"""
from typing import Type, Tuple, AnyStr, Union
import pathlib
import os

import dpath.util
from loguru import logger

from esm_parser import ConfigSetup


class SimulationFile(dict):
    """
    Desribes a file used within a ESM Simulation.

    Given a config, you should be able to use this in YAML::

        $ cat dummy_config.yaml

        echam:
            files:
                jan_surf:
                    name_in_pool: T63CORE2_jan_surf.nc
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
        self.locations = {
            "work": pathlib.Path(full_config["general"]["thisrun_work_dir"]),
            "pool": pathlib.Path(full_config["computer"]["pool_dir"]),
            "exp_tree": pathlib.Path(full_config["general"]["exp_dir"]),
            "run_tree": pathlib.Path(full_config["general"]["thisrun_dir"]),
        }
        # Allow dot access:
        self.work = self.locations["work"]
        self.pool = self.locations["pool"]
        self.exp_tree = self.locations["exp_tree"]
        self.run_tree = self.locations["run_tree"]

    def cp(self) -> None:
        pass

    def ln(
        self, source_path: Union[AnyStr, os.PathLike], destination_path: str
    ) -> None:
        """creates symbolic links. Wrapper around os.symlink

        Parameters
        ----------
        source_path : str, PathLike
            path of the file to be linked

        destination_path : str, PathLike
            path of the symbolic link

        Returns
        -------
        None

        Raises
        ------
        FileNotFoundError
            - Source path does not exist
        OSError
            - Destination path is a directory
            - Symbolic link is trying to link to itself
            - Destination path does not exist
        FileExistsError
            - Destination path already exists
        """
        # Retrieve the absolute path by expanding ~
        source_path = os.path.abspath(os.path.expanduser(source_path))
        destination_path = os.path.abspath(os.path.expanduser(destination_path))

        if not os.path.exists(source_path):
            err_msg = f"Unable to create symbolic link: `{source_path}` does not exist"
            raise FileNotFoundError(err_msg)

        if os.path.isdir(destination_path):
            err_msg = (
                f"Unable to create symbolic link: `{destination_path}` is a directory"
            )
            raise OSError(err_msg)

        destination_exists = os.path.exists(destination_path) or os.path.islink(
            destination_path
        )
        if destination_exists:
            err_msg = f"Unable to create symbolic link: `{destination_path}`. File already exists"
            raise FileExistsError(err_msg)

        points_to_itself = source_path == destination_path
        if points_to_itself:
            err_msg = (
                f"Unable to create symbolic link: `{source_path}` is linking to itself"
            )
            raise OSError(err_msg)

        destination_parent = pathlib.Path(destination_path).parents[0]
        if not destination_parent.exists():
            err_msg = (
                f"Unable to create symbolic link: `{destination_parent}` does not exist"
            )
            raise FileNotFoundError(err_msg)

        os.symlink(source_path, destination_path)

    def mv(self, source: str, target: str) -> None:
        """
        Moves (renames) the SimulationFile from it's location in ``source`` to
        it's location in ``target``.

        Parameters
        ----------
        source : str
            One of ``"pool"``, ``"work"``, ``"exp_tree"``, "``run_tree``"
        target : str
            One of ``"pool"``, ``"work"``, ``"exp_tree"``, "``run_tree``"
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

        Source and target should be on of work, pool, exp_tree, or run_tree.
        You need to specify name_in_`source` and name_in_`target` in the
        object's attrs_dict.

        Parameters
        ----------
        source : str
            One of ``"pool"``, ``"work"``, ``"exp_tree"``, "``run_tree``"
        target : str
            One of ``"pool"``, ``"work"``, ``"exp_tree"``, "``run_tree``"

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
