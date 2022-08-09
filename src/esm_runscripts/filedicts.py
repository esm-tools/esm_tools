"""
The file-dictionary implementation
"""
import pathlib
import shutil

import dpath.util
from esm_parser import user_error


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
        attrs_dict = dpath.util.get(full_config, attrs_address, separator=".")
        super().__init__(attrs_dict)
        self._config = full_config
        self.component = component = attrs_address.split(".")[0]
        self.locations = {
            "work": pathlib.Path(full_config[component]["thisrun_work_dir"]),
            "pool": pathlib.Path(self["path_in_pool"]),
#            "exp_tree": pathlib.Path(full_config[component]["exp_dir"]), # This name is incorrect and depends on the type of file (to be resolved somewhere else before feeding it here)
#            "run_tree": pathlib.Path(full_config[component]["thisrun_dir"]), # This name is incorrect and depends on the type of file (to be resolved somewhere else before feeding it here)

        }
        self.names = {
            "work": pathlib.Path(self["name_in_work"]),
            "pool": pathlib.Path(self["name_in_pool"]),
        }
        # Allow dot access:
        self.work = self.locations["work"]
        self.pool = self.locations["pool"]
#        self.exp_tree = self.locations["exp_tree"] # TODO: uncomment when lines above are fixed
#        self.run_tree = self.locations["run_tree"] # TODO: uncomment when lines above are fixed

        # Verbose set to true by default, for now at least
        self.verbose = full_config.get("general", {}).get("verbose", True)

    def cp(self, source, target) -> None:
        """
        Copies the source file or folder to the target path. It changes the name of the
        target if ``self["name_in_<target>"]`` differs from ``self["name_in_<source>"].

        Parameters
        ----------
        source : str
            String specifying one of the following options: ``"pool"``, ``"work"``,
            ``"exp_tree"``, ``run_tree``
        target : str
            String specifying one of the following options: ``"pool"``, ``"work"``,
            ``"exp_tree"``, ``run_tree``
        """
        # Build target and source paths
        spath = self.locations[source].joinpath(self.names[source])
        tpath = self.locations[target].joinpath(self.names[target])

        # Checks
        spath_type = self.check_source_and_target(spath, tpath)

        # Actual copy
        try:
            shutil.copy2(spath, tpath)
        except Exception:
            user_error("Filedict Error", f"Unable to copy {spath} to {tpath}")

    def ln(self) -> None:
        pass

    def mv(self) -> None:
        pass

    def path_type(self, path):
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

    def check_source_and_target(self, spath, tpath):

        # Types
        spath_type = self.path_type(spath)
        tpath_type = self.path_type(tpath)
        tpath_parent_type = self.path_type(tpath.parent)

        # Checks
        # ------
        # Source exists
        if not spath_type:
            if self.verbose:
                print(f"Source file ``{spath}`` does not exist!") # I'll change this when we have loguru available
            # TODO: Add the missing file to the config["<model>"]["missing_files"]
        # Target exist
        if tpath_type:
            # TODO: Change this behavior
            raise Exception("File already exists!")
        # Target dir exists
        if not tpath_parent_type:
            # TODO: we might consider creating it
            raise Exception("Target directory does not exist!")

        return spath_type
