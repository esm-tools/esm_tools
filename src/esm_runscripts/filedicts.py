"""
The file-dictionary implementation
"""
import functools
import pathlib

import dpath.util
from loguru import logger


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
                    allowed_to_be_missing: True
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
        attrs_dict = dpath.util.get(full_config, attrs_address, separator=".")
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

    # This part allows for dot-access to allowed_to_be_missing:
    @property
    def allowed_to_be_missing(self):
        """
        Example
        -------
            >>> sim_file = SimulationFile(config['echam']['files']['jan_surf'])
            >>> sim_file.allowed_to_be_missing
            True
        """
        return self.get("allowed_to_be_missing", False)

    # NOTE(PG): I **do not** understand this syntax. Suddenly no more self, and
    # then yes, and then no, and what?? But hey, it seems to work...
    # NOTE(PG): https://www.geeksforgeeks.org/creating-decorator-inside-a-class-in-python/
    def skip_missing(method):
        """Allows to decorate a method with skip_missing

        If a method is decorated with skip_missing, it will return None instead
        of executing. Used for cp, ln, mv.
        """

        @functools.wraps(method)
        def inner_method(self, *args, **kwargs):
            if self.allowed_to_be_missing:
                logger.warning(
                    f"Skipping {method.__qualname__} as this file ({self}) is allowed to be missing!"
                )
                return None  # None is the default return, but let us be explicit here, as it is a bit confusing
            else:
                return method(*args, **kwargs)

        return inner_method

    @skip_missing
    def cp(self) -> None:
        pass

    @skip_missing
    def ln(self) -> None:
        pass

    @skip_missing
    def mv(self) -> None:
        pass


def copy_files(config):
    """Copies files"""
    # PG: No. We do not want this kind of general function. This is just to
    # demonstrate how the test would work
    return config
