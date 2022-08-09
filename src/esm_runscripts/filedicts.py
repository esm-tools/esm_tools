"""
The file-dictionary implementation
"""
import functools

from loguru import logger


class SimulationFiles(dict):
    """
    Describes all files used in a ESM Simulation

    Given a config like this::

        $ cat dummy_config.yaml

        model_a:
            files:
                file_a:
                    name_in_pool: blah
                    name_in_work: blah2
                file_b:
                    name_in_pool: woohoo
                    name_in_work: woohoo2

    You can get in Python::

        >>> sim_files = SimulationFiles(config["model_a"]["files"])
        >>> print(sim_files)
        {file_a: <SimulationFile(...)>, file_b: <SimulationFile(...)>}
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
                    allowed_to_be_missing: True
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
