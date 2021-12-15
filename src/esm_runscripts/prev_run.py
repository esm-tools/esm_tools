import os
import sys
import yaml

import questionary

import esm_parser
from esm_calendar import Date, Calendar


class PrevRunInfo(dict):
    """
    A dictionary subclass to access information from the previous run. The object is
    created in the ``SimulationSetup`` class in ``self.config["prev_run"]``. The idea
    behind this class is that variables from the previous run can be called from the
    yaml files with the same syntax as one would do for the current run.

    The syntax is as follows:

    .. code-block:: yaml

       <your_var>: ${prev_run.<path>.<to>.<var>}

    For example, let's assume we want to access the ``time_step`` from the previous run
    of a `FESOM` simulation and store it in a variable called ``prev_time_step``:

    .. code-block:: yaml

       prev_time_step: ${prev_run.fesom.time_step}

    .. Note:: Only the single previous simulation loaded

    .. Warning:: Use this feature only when there is no other way of accessing the
       information needed. Note that, for example, dates of the previous run are
       already available in the current run, under variables such as
       ``last_start_date``, ``parent_start_date``, etc.
    """

    def __init__(self, config={}, prev_config=None):
        """
        Links the current ``config`` and ``prev_config`` to the object.

        Parameters
        ----------
        config : dict, esm_parser.ConfigSetup
            ConfigSetup object containing the information of the current simulation.
            **Note:** this variable needs to remain untouched inside this class because
            it is not a deepCopy, and it cannot be a deepCopy because this class needs
            this config to be updated on the go to properly work.
        prev_config : dict
            Dictionary that contains information loaded from the previous config
            file/run. If not provided, it means that the object is not nested into
            another PrevRunInfo object. When provided, it means it is nested.
        """
        self._config = config
        self._prev_config = prev_config
        # Set default value of the object while the config file has not been read
        self.__setitem__("NONE_YET", {})
        # prev_run_config_file and calendar date
        self._prcf = {}
        # List of components containning variables using the ``prev_run`` feature
        self.components_with_prev_run()
        # Counter for debuggin
        self._prev_config_count = 0

    def components_with_prev_run(self):
        """
        Lists components containning variables using the ``prev_run`` feature. Reading
        of the previous config files will occur only for those components that use
        ``prev_var``.
        """
        # Search for components only if ``self._config`` is not empty
        if len(self._config) > 0:
            components = self._config.keys()
            # Make sure prev_run is not included and also that general is the last of
            # the components
            components = [
                component
                for component in components
                if component not in ["prev_run", "general"]
            ]
            components.append("general")
            # Loop through the components, and find which ones contain at least one
            # ``prev_run.`` value
            c_with_prev_run = []
            for component in components:
                if self.str_value_in_nested_dictionary(
                    "prev_run.", self._config[component]
                ):
                    c_with_prev_run.append(component)

            self._components = c_with_prev_run
        else:
            self._components = []

    def str_value_in_nested_dictionary(self, string_to_search, nested_dict):
        """
        Search recursively inside of a component for a ``string_to_search`` in the
        values of the nested dictionary ``nested_dict``.

        Parameters
        ----------
        string_to_search : str
            A string to be match in any values of the nested dictionary.
        nested_dict : dict
            Maybe, a nested dictionary which keys need to be recursively evaluated, to
            try to find in their values the ``string_to_search``. It really does not need
            to be a dictionary, as it stops the recursive search as soon as it is not a
            dictionary.

        Returns
        -------
        found : bool
            A boolean indicating if the ``string_to_search`` was found in any value.
        """
        found = False
        # If it's a dictionary, call this method recursively for each key
        if isinstance(nested_dict, dict):
            for key in nested_dict.keys():
                found = self.str_value_in_nested_dictionary(
                    string_to_search, nested_dict[key]
                )
                if found:
                    break
        # If it's a string check if ``string_to_searh`` is contained
        elif isinstance(nested_dict, str):
            if string_to_search in nested_dict:
                found = True

        return found

    def __getitem__(self, key):
        """
        Defines the special behaviour for accessing a ``key`` of the object (i.e. when
        the object is called such as ``<object>[key]``). If ``_prev_config`` is already
        loaded returns the value of the ``key``. Otherwise, it tries to load
        ``_prev_config`` and if not possible yet, returns a ``PrevRunInfo`` instance.
        """

        # If the previous config is not loaded yet (no file found), try to load it
        if not self._prev_config and len(self._config) > 0:
            self.prev_run_config()
        # If the previous config was loaded return the key
        if self._prev_config:
            if key == "NONE_YET":
                value = {}
            else:
                value = self._prev_config[key]
        # If the previous config is not loaded yet, return an instance of
        # ``PrevRunInfo``
        else:
            value = PrevRunInfo(prev_config=self._prev_config)

        return value

    def get(self, *args, **kwargs):
        """
        Defines the special behaviour for the ``get`` method of the object (i.e. when
        the object is called such as ``<object>.get(key, <value>)``). If
        ``_prev_config`` is already loaded returns the value of the ``key`` in
        ``_prev_config`` if it exists, if not it returns ``None`` if no second argument
        is defined for the ``get``, or it returns the second argument, just as a
        standard ``<dict>.get`` would do.
        """

        key = args[0]
        # If the previous config is not loaded yet (no file foudn), try to load it
        if not self._prev_config and len(self._config) > 0:
            self.prev_run_config()
        # If the previous config was loaded, use get
        if self._prev_config:
            if key == "NONE_YET":
                value = {}
            else:
                value = self._prev_config.get(*args, **kwargs)
        # If the previous config is not loaded yet, return get of an empty dict
        else:
            value = {}.get(*args, **kwargs)

        return value

    def prev_run_config(self):
        """
        If all the necessary information is available, loads the previous config file
        for each component. The component loop is only run once per model. Once loaded,
        the ``get`` and ``__get__`` methods will not call this method anymore.
        """
        # Logic for interactive questions about the config file
        fromdir = os.path.realpath(self._config["general"].get("started_from", ""))
        scriptsdir = os.path.realpath(
            f"{self._config['general'].get('experiment_dir', '')}/scripts/"
        )
        # This is necessary to display the message only once, instead of twice
        self.warn = (
            fromdir == scriptsdir
            and self._config["general"].get("jobtype", "") == "compute"
        )
        # Check for interactive, or submitted from a computing node, to avoid
        # using ``input()`` or ``questionaries`` in the second case
        self.isinteractive = (
            self._config["general"].get("last_jobtype", "") == "command_line"
        )

        # Loop through components
        components = self._components
        for component in components:
            # Check if the ``prev_run_config_file`` was previously found already. If
            # not, try to find it
            if self._prcf.get(component):
                prev_run_config_file, calc_prev_date = self._prcf[component]
            else:
                prev_run_config_file, calc_prev_date = self.find_config(component)
                self._prcf[component] = (prev_run_config_file, calc_prev_date)

            # If the file exists, load the file content
            if os.path.isfile(prev_run_config_file):
                # For debugging purposes: this line should not be reached more that
                # once per component including ``prev_run`` variables
                self._prev_config_count += 1
                # print(f"PREV CONFIG COUNT: {self._prev_config_count}")

                # Open the file and load the previous run information
                with open(prev_run_config_file, "r") as prev_file:
                    prev_config = yaml.load(prev_file, Loader=yaml.FullLoader)
                # Back-compatibility with old config files
                if "dictitems" in prev_config:
                    prev_config = prev_config["dictitems"]
                # In case a ``prev_run`` info exists inside the file, remove it to
                # avoid config files from getting huge (prev_run nested inside
                # prev_run...)
                if "prev_run" in prev_config:
                    del prev_config["prev_run"]

                # Check that the data really comes from the previous run
                prev_date = prev_config["general"]["end_date"]
                prev_date_stamp = Date(prev_date).format(
                    form=9, givenph=False, givenpm=False, givenps=False
                )
                calc_prev_date_stamp = calc_prev_date.format(
                    form=9, givenph=False, givenpm=False, givenps=False
                )
                # Dates don't match
                if calc_prev_date_stamp != prev_date_stamp and self.warn:
                    esm_parser.user_note(
                        f"End date of the previous configuration file for '{component}'"
                        + " not coinciding:",
                        (
                            f"    File loaded: {prev_run_config_file}\n"
                            + f"    This previous date: {calc_prev_date}\n"
                            + f"    Previous date in prev config: {prev_date}\n"
                        ),
                    )
                    # Only ask the user about a mismatch when manually restarted
                    if self.isinteractive:
                        no_input = True
                        while no_input:
                            answer = input(f"Do you want to proceed anyway?[y/n]: ")
                            if answer == "y":
                                no_input = False
                            elif answer == "n":
                                sys.exit(0)
                            else:
                                print("Incorrect answer.")

                # Load the component info into the self._prev_config dictionary
                if not self._prev_config:
                    self._prev_config = {}
                self._prev_config[component] = prev_config[component]
                # Load the general value of this component's prev_run
                # MA: this is potentially dangerous if all the following conditions are
                # met: 1) more than one component uses the prev_run feature, 2) both
                # components are branched off and come from different spinups, meaning
                # that they don't share the same general configuration, and 3) some of
                # the models need the general configuration from the previous run.
                if component != "general":
                    self._prev_config["general"] = prev_config["general"]

    def find_config(self, component):
        """
        Finds the config file path
        (``<prev_exp_path>/config/*_finished_config.yaml_<DATE>``) required for the
        accessing the information of the previous experiment. It contemplates the
        following cases:

        a. **first spinup run:** no need for providing any path

        b. **automatic continuation of a run in the same directory:** get the path to
           the previous config file by matching with the end date. If multiple files
           match the end date, throw an error indicating the user how to proceed.

        c. **manual continuation of a run in the same directory:** get the path to
           the previous config file by matching with the end date. If multiple files
           match the end date, ask the user which one to use.

        d. **branching off experiment:** if this experiment branches off from a parent
           experiment, return the ``prev_run_config_file`` variable of the evaluated
           ``component`` as the path. If ``prev_run_config_file`` is not defined in the
           runscript, throw an error indicating how to proceed.

        Parameters
        ----------
        component : str
            Component for which the previous config file needs to be located.

        Returns
        -------
        prev_run_config_path : str
            Absolute path to the previous config file.
        prev_date : esm_calendar.Date
            Previous date calculated by substracting the time step of the ``component``
            to the current date.
        """
        prev_run_config_file = ""
        # This experiment ``config_dir``
        config_dir = (
            self._config.get("general", {}).get("experiment_dir", "") + "/config/"
        )
        # Find ``lresume`` and ``run_number`` for this component
        lresume = self._config.get(component, {}).get("lresume", False)
        run_number = self._config.get("general", {}).get("run_number", 1)
        # It's run 1
        if run_number == 1:
            # It's a branchoff experiment
            if lresume:
                # The user needs to provide the path to the config file of the previous
                # run for branchoff experiments that use the prev_run feature
                user_prev_run_config_full_path = self._config[component].get(
                    "prev_run_config_file"
                )
                if not user_prev_run_config_full_path:
                    esm_parser.user_error(
                        "'prev_run_config_file' not defined",
                        "You are trying to run a branchoff experiment that uses the "
                        + f"'prev_run' functionality for '{component}' without "
                        + "specifying the path to the previous config file. "
                        + "Please, add to your runscript the following:\n\n"
                        + f"{component}:\n"
                        + "\tprev_run_config_file: <path_to_config_file>\n\n"
                        + "Note: the path to the config file from the parent "
                        + "is '<path_to_parent_exp>/configs/*_finished_*.yaml_<DATE>'.",
                    )
                # Resolve for variables in the provided path
                if "${" in user_prev_run_config_full_path:
                    user_prev_run_config_full_path = esm_parser.find_variable(
                        [component, "prev_run_config_file"],
                        user_prev_run_config_full_path,
                        self._config,
                        [],
                        True,
                    )
                # Separate the base name from the path to the file
                user_prev_run_config_file = os.path.basename(
                    user_prev_run_config_full_path
                )
                config_dir = os.path.dirname(user_prev_run_config_full_path)

            # It's a cold start
            else:
                # There is no need of prev_run for cold starts. Do nothing
                return prev_run_config_file, ""

        # Resolve config path
        config_dir = esm_parser.find_variable(
            ["general"], config_dir, self._config, [], True
        )

        # Check for errors
        if not os.path.isdir(config_dir):
            esm_parser.user_error(
                "Config folder not existing",
                (
                    f"The config folder {config_dir} does not exist. "
                    + "The existance of this folder is a requirement for the use of the "
                    + "prev_run feature."
                ),
            )

        # Calculate previous date. This point is reached some times before it is
        # calculated in prepare.py, that's why we need the calculation here. It's only
        # use is to search for the config file time stamps and do some checks for the
        # PrevRunInfo.
        current_date = Date(self._config["general"]["current_date"])
        time_step = self._config[component].get("time_step", 1)
        try:
            time_step = int(time_step)
        except ValueError:
            time_step = 1
        prev_date = current_date - (0, 0, 0, 0, 0, time_step)

        # Calculate end date for the previous run
        prev_datestamp = prev_date.format(
            form=9, givenph=False, givenpm=False, givenps=False
        )

        # List all the config files in the config folder
        config_files = [
            cf for cf in os.listdir(config_dir) if "_finished_config.yaml" in cf
        ]
        # Select the ones ending with the correct timestamp
        potential_prev_configs = []
        for cf in config_files:
            if cf.endswith(prev_datestamp):
                potential_prev_configs.append(cf)

        # CASES FOR FINDING THE CONFIG FILE
        # ---------------------------------
        # Continuing run, not branched off, but no timestamped config files. Select the
        # one without timestamp (the case for run 1 in spinup).
        if len(potential_prev_configs) == 0 and run_number > 1:
            prev_run_config_file = (
                self._config["general"]["expid"] + "_finished_config.yaml"
            )
        # Continuing run, not branched off, and one potential file. That's our file!
        elif len(potential_prev_configs) == 1 and run_number > 1:
            prev_run_config_file = potential_prev_configs[0]
        # Continuing run, too many possibilities, if interactive, ask the user,
        # otherwise, crash the simulation
        elif len(potential_prev_configs) > 1 and run_number > 1:
            if self.warn:
                if self.isinteractive:
                    text = (
                        "Using the 'prev_run' feature several valid config files were"
                        + f" found for the component '{component}'."
                    )
                    # Ask the user
                    prev_run_config_file = self.ask_about_files(
                        potential_prev_configs, component, config_dir, text
                    )
                else:
                    # Error
                    esm_parser.user_error(
                        "Too many possible config files",
                        "There is more than one config file with the same final date "
                        + "as the one required for the continuation of this experiment."
                        + " Please, resubmit the simulation, then you'll be ask about "
                        + "which file you'd like to use. This error comes from the "
                        + f"PrevRunInfo class, for the '{component}' component.",
                    )
            else:
                # If started by the user in a directory different than the experiment
                # directory, scripts, then load the first option. The resubmission will
                # ask about which file really use
                prev_run_config_file = potential_prev_configs[0]
        # Branch off, load what the user specifies in the runscript (only branchoffs
        # reach this point as run_number=1 and spinup have returned already up in this
        # method)
        elif run_number == 1:
            prev_run_config_file = user_prev_run_config_file
            prev_run_config_path = f"{config_dir}/{prev_run_config_file}"
            if not os.path.isfile(prev_run_config_path):
                esm_parser.user_error(
                    "'prev_run_config_file' incorrectly defined",
                    f"The file defined in the '{component}.prev_run_config_path' "
                    + f"({prev_run_config_path}) does not exist.",
                )

        prev_run_config_path = f"{config_dir}/{prev_run_config_file}"
        return prev_run_config_path, prev_date

    def ask_about_files(self, potential_prev_configs, component, config_dir, text):
        """
        Questionary to interactively decide which file to use as the previous config
        file, when more than one option.

        Parameters
        ----------
        potential_prev_configs : list
            List of file names to ask the user about.
        component : str
            Component to be asked about.
        config_dir : dict, esm_parser.ConfigSetup
            Dictionary containing the information about the current run configuration.
        text : str
            Text to be displayed in the questionary.
        """
        questionary.print(100 * "=")
        questionary.print(text)
        questionary.print(100 * "=")

        user_confirmed = False
        while not user_confirmed:
            response = questionary.select(
                f"Which one do you want to use?",
                choices=(
                    potential_prev_configs
                    + ["[Quit] None of the files, stop simulation now"]
                ),
            ).ask()  # returns value of selection
            if "[Quit]" in response:
                if questionary.confirm("Are you sure?").ask():
                    sys.exit(0)
            user_confirmed = questionary.confirm("Are you sure?").ask()
        return response
