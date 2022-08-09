"""
The file-dictionary implementation
"""
import pathlib

import dpath.util


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
        super().__init__()
        attrs_dict = dpath.util.get(full_config, attrs_address, separator=".")
        self.update(attrs_dict)
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

    def ln(self) -> None:
        pass

    def mv(self) -> None:
        pass
        
        
def copy_files(config):
    """Copies files"""
    # PG: No. We do not want this kind of general function. This is just to
    # demonstrate how the test would work
    return config
