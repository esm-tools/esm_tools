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

    def cp(self) -> None:
        pass

    def ln(self) -> None:
        pass

    def mv(self) -> None:
        pass
