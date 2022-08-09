"""
The file-dictionary implementation
"""


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

        >>> sim_file = SimulationFile(config['echam']['files']['jan_surf'])

    You could then copy the file to the experiment folder::

        >>> sim_file.cp_to_exp_tree()
    """

    # Do NOT implment an __init__ unless you really think you need to. dict
    # takes care of this in a way is that is considerably smarter than any of us are.
    #
    # Please delete my annoying comments before merging into actual release ;-)

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
        spath = self.location[source].joinpath(self.name[source])
        tpath = self.location[target].joinpath(self.name[target])

        # Checks

    def ln(self) -> None:
        pass

    def mv(self) -> None:
        pass

    def path_type(self, path):
        if spath.is_file():
            return "file"
        elif spath.is_dir:
            return "dir"
        elif spath.is_link:
            return "link"
        elif not spath.exist():
            return False
        else:
            raise Exception(f"Cannot identify the path's type of {path}")
        
        
        
def copy_files(config):
    """Copies files"""
    # PG: No. We do not want this kind of general function. This is just to
    # demonstrate how the test would work
    return config
