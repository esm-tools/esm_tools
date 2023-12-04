import copy
import esm_parser

import pdb


class Workflow:
    """A workflow class."""

    def __init__(self, workflow_yaml):
        """
        Create a new workflow.

        Parameters
        ----------
        workflow_yaml : dict
            Dictionary from defaults.yaml to initialize workflow
            for default phases.

        Returns
        -------
        none
        """
        # TODO: check if key is in workflow_yaml dict
        self.phases = []
        self.user_phases = []
        self.clusters = {}
        self.first_task_in_queue = workflow_yaml["first_task_in_queue"]
        self.last_task_in_queue = workflow_yaml["last_task_in_queue"]
        self.next_run_triggered_by = workflow_yaml["next_run_triggered_by"]
        # TODO: Call here the phase object ???

    def get_workflow_phase_by_name(self, phase_name):
        """
        Returns phase of phase_name

        Arguments
        ---------
            self : class Workflow
            phase_name : str (name of the phase to be returned

        Returns
        -------
            phase : class phase or user_phase
        """
        for phase in self.phases + self.user_phases:
            if phase["name"] == phase_name:
                return phase

    def get_phases_attribs_list(self, phase_type, attrib):
        """
        Returns a certain attribute for all phases as a list.

        Parameters
        ----------
            phase_type : str
                ``default`` or ``user``
            attrib : str

        Returns
        -------
            phases_attribs : list
        """
        if phase_type == 'user':
            phases_attribs = [phase[attrib] for phase in self.user_phases]
        else:
            phases_attribs = [phase[attrib] for phase in self.phases]

        return phases_attribs


    def set_default_nproc(self, config):
        """
        Calculating the number of mpi tasks for each component/model/script

        Parameters
        ----------
            config : dict

        Returns
        -------
            self : Workflow object
        """

        tasks = calc_number_of_tasks(config)

        for ind, phase in enumerate(self.phases):
            if phase["submit_to_batch_system"]:
                phase["nproc"] = tasks

        return self

    def set_workflow_attrib(self, attrib, value):
        """
        Sets a workflow attribute.

        Parameters
        ----------
            attrib : str
            value :

        Returns
        -------
            None
        """

        if type(getattr(self, attrib)).__name__ == "list":
            self.__dict__[attrib].append(value)
        else:
            self.__setattr__(attrib, value)

    def check_if_keyword_is_valid(self, keyword):
        """
        Checks if the key given for a user workflow is valid.
        Only keywords are allowed, that are already set during
        initialization.

        Parameters
        ----------
            keyword : str

        Returns
        -------
            true or false
        """

        return hasattr(self, keyword)

    def collect_all_user_phases(self, config):
        """
        Collect all workflows defined in config files.

        Parameters
        ----------
            self : Workflow object
            config : dict

        Returns
        -------
            self : Workflow object
        """

        user_workflow_phases = []
        user_workflow_phases_names = []
        user_workflow_next_run_triggered_by = []
        for model in config:
            if "workflow" in config[model]:
                w_config = config[model]["workflow"]
                if "phases" in w_config:
                    # check if still tries to set workflow keywords
                    for key, value in w_config.items():
                        if not key == "phases":
                            err_msg = f"``{key}`` is not allowed to be set for a workflow."
                            esm_parser.user_error("ERROR", err_msg)
                    for phase in w_config["phases"]:
                        # each phase (of a model/setup) needs to have an unique name
                        # same phases of the same model/setup defined in different config files
                        # are overwritten by the usual config file hierarchy
                        # user phases are not alowed to have the same name as default phases (e.g. compute)

                        # check if ``new_phase`` is already defined as a default phase
                        # look for the name of the current phase in the list of default phase names
                        # if found, raise exception
                        if phase in self.get_phases_attribs_list("default", "name"):
                            err_msg = (
                                f"The user phase ``{phase}`` "
                                f"has the same name as a default workflow phase. "
                                f"This is not allowed."
                            )
                            esm_parser.user_error("ERROR", err_msg)

                        # check if the name of the new user phase (for a model/setup) does not already exist
                        # (for another model/setup).
                        if phase in user_workflow_phases_names:
                            err_msg = (
                                f"Two workflow phases have the same name "
                                f"``{phase}``."
                            )
                            esm_parser.user_error("ERROR", err_msg)

                        # if user phase (for each setup/model) has a new and unique name
                        else:
                            phase_config = copy.deepcopy(w_config["phases"][phase])
                            # add phase name
                            phase_config["name"] = phase
                            # Make sure that batch_or_shell is set to batch if submit_to_batch is true
                            # Should not be set by user. TODO: Remove from documentation.
                            if phase_config.get("submit_to_batch_system", False):
                                phase_config["batch_or_shell"] = "batch"
                                # Check if run_on_queue is given if submit_to_sbatch is true
                                if not phase_config.get("run_on_queue", False):
                                    err_msg = f"No value for target queue given by ``run_on_queue`` for phase ``{phase}``."
                                    esm_parser.user_error("ERROR", err_msg)
                            else:
                                phase_config["batch_or_shell"] = "shell"
                            # create a new user phase object for ``phase``
                            new_phase = WorkflowPhase(phase_config)
                            # append it to the list of user phases of the workflow
                            user_workflow_phases.append(new_phase)
                            user_workflow_phases_names.append(phase)
                            if phase_config.get("trigger_next_run", False):
                                user_workflow_next_run_triggered_by.append(phase)
        # check if more than one user phase has set trigger_next_run to true
        if len(user_workflow_next_run_triggered_by) > 1:
            err_msg = (
                f"More than one phase is set to "
                f"trigger the next run: ``{user_workflow_next_run_triggered_by}``. "
                f"Only set ``trigger_next_run: True`` for one phase."
            )
            esm_parser.user_error("ERROR", err_msg)
        elif user_workflow_next_run_triggered_by:
            self.next_run_triggered_by = user_workflow_next_run_triggered_by[0]

        self.user_phases = user_workflow_phases

        # check if there are unknown phases, if yes, will give error exception
        unknown_phases = self.check_unknown_phases()
        if unknown_phases:
            unknowns = ', '.join(unknown_phases)
            err_msg = (
                f"Unknown phase(s) ``{unknowns}`` defined as ``run_before`` "
                f"or ``run_after``."
            )
            esm_parser.user_error("ERROR", err_msg)

        # check if run_after or run_before is set for each user phase
        # if not, run_after will be set to last default phase
        for user_phase in self.user_phases:
            if not user_phase["run_before"] and not user_phase["run_after"]:
                user_phase["run_after"] = self.phases[-1]["name"]
                err_msg = (
                    f"No value given for ``run_after`` or ``run_before`` "
                    f"of user phase ``{user_phase['name']}``. "
                    f"Set it to last default phase in workflow: "
                    f"``{self.phases[-1]['name']}``."
                )
                esm_parser.user_note("NOTE", err_msg)

        return self

    def cluster_phases(self):
        """Merge phases into clusters."""

        clusters = {}
        # create an empty phases list for each cluster
        for cluster in self.get_phases_attribs_list("default", "cluster") + self.get_phases_attribs_list("user", "cluster"):
            clusters[cluster] = {"phases": []}
        # append all phases that are within the same cluster
        for phase in self.phases + self.user_phases:
            clusters[phase["cluster"]]["phases"].append(phase["name"])

        for cluster in clusters:
            nproc = nproc_sum = nproc_max = 0
            # if only one phase in cluster
            if len(clusters[cluster]["phases"]) == 1:
                phase_name = clusters[cluster]["phases"][0]
                phase = self.get_workflow_phase_by_name(phase_name)
                clusters[cluster].update(phase)
            # if more than one phase are within the same cluster
            else:
                # fill in default phase keys for each cluster to cluster dictionary
                clusters[cluster].update(WorkflowPhase({}))
                # create a list of all phases (dicts) that are within the same cluster
                phases_list = []
                for phase_name in clusters[cluster]["phases"]:
                    phases_list.append(self.get_workflow_phase_by_name(phase_name))

                # check for inconsistencies of phase keywords within a cluster
                keywords = {}
                for key in WorkflowPhase({}):
                    keywords[key] = []
                    # append keyword of a phase only if not already in keywords[key]
                    [keywords[key].append(item) for item in [phase[key] for phase in phases_list] if item not in keywords[key]]
                    # if there are no inconsistencies, all phases have the same values for keyword
                    if len(keywords[key]) == 1:
                        clusters[cluster][key] = keywords[key][0]
                    # if different phases have set different values for the same keyword/attrib
                    else:
                        if type(clusters[cluster][key]) is list:
                            clusters[cluster][key] = keywords[key]
                        else:
                            if key not in ["name", "script", "scriptdir", "order_in_cluster", "nproc", "trigger_next_run"]:
                                err_msg = (
                                    f"Mismatch for {key}")
                                esm_parser.user_error("ERROR", err_msg)
                            elif key == "name":
                                clusters[cluster]["name"] = cluster
                            elif key == "trigger_next_run":
                                # set key of cluster to True if key for any (at least one) of the phases is set to True
                                clusters[cluster][key] = any(keywords[key])
                            else:
                                # if key is set different for each phase in same cluster set to fill value (e.g. for script, scriptdir)
                                clusters[cluster][key] = "check phase"

                # calculate nproc if cluster is to be submitted to sbatch system
                for phase in phases_list:
                    nproc_sum += phase["nproc"]
                    nproc_max = max(phase["nproc"], nproc_max)

                    if clusters[cluster].get("submit_to_batch_system", False):
                        if phase["order_in_cluster"] == "concurrent":
                            if clusters[cluster]["order_in_cluster"] is None:
                                clusters[cluster]["order_in_cluster"] = "concurrent"
                            nproc = nproc_sum
                        else:
                            clusters[cluster]["order_in_cluster"] = "sequential"
                            nproc = nproc_max
                clusters[cluster]["nproc"] = nproc
        # write clusters dictionary to workflow object attribute
        self.clusters = clusters
        return self

    def write_to_config(self, config):
        """
        Write to config.
        TODO: Rename ``subjobs`` to ``phases``. But this needs changes also in resubmit.py and other files???
        """
        # 1. Delete unnecessary config workflow entries (e.g. in general)
        if "workflow" in config["general"]:
            del config["general"]["workflow"]

        config["general"]["workflow"] = {}
        config["general"]["workflow"].update(self.__dict__)

        # 3. Write clusters
        config["general"]["workflow"]["subjob_clusters"] = {}
        for cluster in self.clusters:
            config["general"]["workflow"]["subjob_clusters"][cluster] = {}
            config["general"]["workflow"]["subjob_clusters"][cluster]["subjobs"] = []
            for phase_name in self.clusters[cluster]["phases"]:
                config["general"]["workflow"]["subjob_clusters"][cluster]["subjobs"].append(phase_name)
            for att in self.clusters[cluster]:
                config["general"]["workflow"]["subjob_clusters"][cluster][att] = self.clusters[cluster][att]

        # 2. Write subjobs/phases
        config["general"]["workflow"]["subjobs"] = {}
        for phase in self.phases + self.user_phases:
            temp_dict = phase
            config["general"]["workflow"]["subjobs"][phase["name"]] = temp_dict

        # delete phases and user_phases
        del config["general"]["workflow"]["phases"]
        del config["general"]["workflow"]["user_phases"]

        return config

    def check_user_workflow_dependency(self):
        """
        Check whether the user defined workflow phases are independent
        from each other or not.

        Arguments
        ---------
            self : Workflow object

        Returns
        -------
            independent : bool (default: False)
        """
        independent = False
        user_phases_names = self.get_phases_attribs_list('user', 'name')
        run_after_list = self.get_phases_attribs_list('user', 'run_after')
        run_before_list = self.get_phases_attribs_list('user', 'run_before')

        # All user phases are independent from each other, if
        # none of the ``user_phases_names`` are found in the union of
        # ``run_before_list`` and ``run_after_list``
        # That means alls user phases can be run independent from each other.
        if not set(user_phases_names).intersection(set(run_after_list).union(set(run_before_list))):
            independent = True
        else:
            # TODO: What todo in other case?
            independent = False

        return independent

    def check_unknown_phases(self):
        """
        Check if any user phase keyword (run_afteer, run_before) points to an unknown workflow phase.

        Parameters
        ----------
            self : Workflow object

        Returns
        -------
            unknown_phases : set
        """
        unknown_phases = []
        phases_names = self.get_phases_attribs_list('default', 'name')          # list of names of all default phases
        user_phases_names = self.get_phases_attribs_list('user', 'name')        # list of name of all user phases
        run_after = self.get_phases_attribs_list('user', 'run_after')           # list of all run_after values for all user phases
        run_before = self.get_phases_attribs_list('user', 'run_before')         # list of all run_before values for all user phases
        # Filter out all elements that are None
        # ``filter(None, anylist)`` will filter out all items of anylist,
        # for which ``if item`` is false (e.g. [], "", None, {}, '').
        # See also https://docs.python.org/3/library/functions.html#filter
        run_after_list = list(filter(None, run_after))
        run_before_list = list(filter(None, run_before))
        # Get all phases that are defined as run_after or run_before,
        # but do not exist as user or default phase.
        # If unknown_phase is not empty, there is a user_phase that defines run_after
        # or run_before for a not existing phase.
        unknown_phases = set(run_after_list).union(set(run_before_list)).difference(set(user_phases_names).union(set(phases_names)))

        return unknown_phases

    def order_phases_and_clusters(self):
        """
        Put the phases and clusters in the right order.

        Parameters
        ----------
            self : Workflow object

        Returns
        -------
            self : Workflow object
        """

# Correct workflow attributes (``last_task_in_queue``, ``first_task_in_queue``, ``next_run_triggered``)

        # next_run_triggered_by is always the last phase

        # check if next_triggered is set to a default or user phase
        # if user phase
        # get last default phase and correct next_submit and run_before
        # get first default phase and correct run_after, called_from
        # correct last_task_in_queue of workflow

        next_triggered = self.next_run_triggered_by
        triggered_next_run_phase = self.get_workflow_phase_by_name(next_triggered)
        if next_triggered not in self.get_phases_attribs_list("default", "name"):
            first_task_name = self.first_task_in_queue
            first_phase = self.get_workflow_phase_by_name(first_task_name)
            old_last_task_name = self.last_task_in_queue
            old_last_phase = self.get_workflow_phase_by_name(old_last_task_name)

            old_last_phase["next_submit"].remove(first_phase["name"])
            old_last_phase["next_submit"].append(next_triggered)
            old_last_phase["run_before"] = next_triggered
            old_last_phase["trigger_next_run"] = False
            if triggered_next_run_phase["cluster"] not in self.clusters[old_last_phase["cluster"]]["next_submit"]:
                self.clusters[old_last_phase["cluster"]]["next_submit"].append(triggered_next_run_phase["cluster"])
            self.clusters[old_last_phase["cluster"]]["run_before"] = triggered_next_run_phase["cluster"]
            self.clusters[old_last_phase["cluster"]]["trigger_next_run"] = False

            first_phase["run_after"] = next_triggered
            first_phase["called_from"] = next_triggered
            self.clusters[first_phase["cluster"]]["run_after"] = triggered_next_run_phase["cluster"]
            self.clusters[first_phase["cluster"]]["called_from"] = triggered_next_run_phase["cluster"]

            self.clusters[triggered_next_run_phase["cluster"]]["next_submit"].append(first_phase["cluster"])
            self.clusters[triggered_next_run_phase["cluster"]]["run_before"] = first_phase["cluster"]
            self.clusters[triggered_next_run_phase["cluster"]]["run_after"] = old_last_phase["cluster"]

            self.last_task_in_queue = next_triggered


# Intergrate new user phases by correcting next_submit, called_from, run_after, run_before

        # Set "next_submit" and "called_from"
        # "next_submit" which phase/cluster will be called next (run_after of the next phase)
        # "called_from" name of previous phase, run_after of current phase

        # Create a dict of all phases and for all clusters with empty lists
        next_submits_phases = {}
        next_submits_clusters = {}
        for phase in self.phases + self.user_phases:
            next_submits_phases[phase["name"]] = []
            next_submits_clusters[phase["cluster"]] = []

        for phase2 in self.phases + self.user_phases:
            if phase2.get("run_after", None):
                if phase2["name"] not in next_submits_phases[phase2["run_after"]]:
                    next_submits_phases[phase2["run_after"]].append(phase2["name"])
                phase2["called_from"] = phase2["run_after"]
            if self.clusters[phase2["cluster"]].get("run_after", None):
                if phase2["cluster"] not in next_submits_clusters[self.clusters[phase2["cluster"]]["run_after"]]:
                    next_submits_clusters[self.clusters[phase2["cluster"]]["run_after"]].append(phase2["cluster"])
                self.clusters[phase2["cluster"]]["called_from"] = self.clusters[phase2["cluster"]]["run_after"]
            else:
                # if only run_before is set, e.g. to add a phase at the beginning of a run
                if phase2.get("run_before", None):
                    if phase2["run_before"] == self.first_task_in_queue:
                        old_first_phase = self.get_workflow_phase_by_name(self.first_task_in_queue)
                        last_phase = self.get_workflow_phase_by_name(self.last_task_in_queue)
                        next_submits_phases[phase2["name"]].append(self.first_task_in_queue)
                        if self.first_task_in_queue not in next_submits_clusters[phase2["cluster"]]:
                            next_submits_clusters[phase2["cluster"]].append(self.first_task_in_queue)
                        next_submits_clusters[self.last_task_in_queue].append(phase2["cluster"])
                        next_submits_phases[self.last_task_in_queue].append(phase2["name"])
                        next_submits_phases[self.last_task_in_queue].remove(self.first_task_in_queue)
                        next_submits_clusters[last_phase["cluster"]].remove(old_first_phase["cluster"])
                        phase2["run_after"] = self.last_task_in_queue
                        last_phase["run_before"] = phase2["name"]
                        self.clusters[last_phase["cluster"]]["run_before"] = phase2["name"]
                        self.clusters[old_first_phase["cluster"]]["run_after"] = phase2["name"]
                        self.clusters[old_first_phase["cluster"]]["called_from"] = phase2["name"]
                        self.clusters[phase2["cluster"]]["called_from"] = last_phase["cluster"]
                        self.clusters[phase2["cluster"]]["run_after"] = last_phase["cluster"]
                        last_phase["next_submit"].append(phase2["name"])
                        self.first_task_in_queue = phase2["name"]

        for cluster in self.clusters:
            if next_submits_clusters[cluster]:
                self.clusters[cluster]["next_submit"] = next_submits_clusters[cluster]

        for phase3 in self.phases + self.user_phases:
            if next_submits_phases[phase3["name"]]:
                phase3.set_attrib("next_submit", next_submits_phases[phase3["name"]])

        return self

    def prepend_newrun_job(self):
        """
        - Creates a new cluster "newrun" if first_task_in_queue is not of
          type 'SimulationSetup'

        Looks for subjob_cluster that are set by user workflow (not a 'SimulationSetup')
        and are not of type 'SimulationSetup'.

        Parameters
        ----------
            self : Workflow object

        Returns
        -------
            self : Workflow object
        """
        first_task_name = self.first_task_in_queue
        first_phase = self.get_workflow_phase_by_name(first_task_name)

        if not first_phase["batch_or_shell"] == "SimulationSetup":

            last_task_name = self.last_task_in_queue
            last_phase = self.get_workflow_phase_by_name(last_task_name)

            # Create new default phase object
            config_new_first_phase = {
                "name": "newrun",
                "next_submit": [first_phase["cluster"]],
                "called_from": last_phase["cluster"],
                "run_before": first_phase["cluster"],
                "run_after": last_phase["cluster"],
                "cluster": "newrun",
                "batch_or_shell": "SimulationSetup",
                "nproc": 1
            }
            new_first_phase = WorkflowPhase(config_new_first_phase)

            # reset last_task attributes
            last_phase["next_submit"].append("newrun")
            self.clusters[last_phase["cluster"]]["next_submit"] = ["newrun"]
            self.clusters[last_phase["cluster"]]["run_before"] = "newrun"
            self.clusters[new_first_phase["cluster"]] = new_first_phase
            self.clusters[new_first_phase["cluster"]]["phases"] = ["newrun"]
            last_phase["next_submit"].remove(first_phase["cluster"])
            # why does the next line not work???
            # last_phase.set_attrib("next_submit", "newrun")
            # last_phase.remove_attrib("next_submit", first_phase["cluster"])

            # reset first_task attributes
            first_phase.set_attrib("called_from", "newrun")
            first_phase.set_attrib("run_after", "newrun")
            self.clusters[first_phase["cluster"]]["called_from"] = "newrun"
            self.clusters[first_phase["cluster"]]["run_after"] = "newrun"

            # reset workflow attributes
            self.first_task_in_queue = "newrun"

            # Set new phase to beginning of default phase list
            self.phases.insert(0, new_first_phase)

        return self


def skip_cluster(cluster, config):
    """
    Checks if a phase/cluster can be skipped.
    Needed keywords: run_only, skip_chunk_number
    Is called from resubmit.py

    Parameters
    ----------
        self
        config : dict

    Returns
    -------
        True or False
    """
    gw_config = config["general"]["workflow"]
    clusterconf = gw_config["subjob_clusters"][cluster]

    """
    print(f"run_only {clusterconf.get('run_only', 'Error') }")
    print(f"skip_chunk_number {clusterconf.get('skip_chunk_number', -999)}")
    print(f"skip_run_number {clusterconf.get('skip_run_number', -999)}")
    print(f"chunk_number {config['general'].get('chunk_number', -998)}")
    print(f"run_number {config['general'].get('run_number', -998)}")
    print(f"last_run_in_chunk {config['general']['last_run_in_chunk']}")
    print(f"first_run_in_chunk {config['general']['first_run_in_chunk']}")
    """

    if clusterconf.get("run_only", "Error") == "last_run_in_chunk" and not config[
        "general"
    ].get("last_run_in_chunk", False):
        return True
    if clusterconf.get("run_only", "Error") == "first_run_in_chunk" and not config[
        "general"
    ].get("first_run_in_chunk", False):
        return True
    if clusterconf.get("skip_chunk_number", -999) == config["general"].get(
        "chunk_number", -998
    ):
        return True
    if clusterconf.get("skip_run_number", -999) == config["general"].get(
        "run_number", -998
    ):
        return True

    return False


class WorkflowPhase(dict):
    """A workflow phase class."""

    def __init__(self, phase):
        # defaults
        self["name"] = None
        self["script"] = None
        self["script_dir"] = None
        self["nproc"] = 1                              # needed
        self["run_before"] = None
        self["run_after"] = None
        self["trigger_next_run"] = False               # needed
        self["submit_to_batch_system"] = False         # needed
        self["run_on_queue"] = None
        self["cluster"] = None
        self["next_submit"] = []                       # needed
        self["called_from"] = None                     # needed
        self["batch_or_shell"] = "SimulationSetup"     # needed
        self["order_in_cluster"] = None                # needed ???
        self["run_only"] = None
        self["skip_chunk_number"] = None
        self["skip_run_number"] = None
        self["call_function"] = None
        self["env_preparation"] = None

        # check if phase keywords are valid
        for key, value in phase.items():
            if key not in self:
                err_msg = (
                    f"``{key}`` of workflow phase "
                    f"``{phase['name']}`` is not a valid keyword "
                    f"of a workflow phase."
                )
                esm_parser.user_error("ERROR", err_msg)

        super().__init__(phase)

        # make sure batch_or_shell is batch for sbatch jobs
        if self.get("submit_to_batch_system", False):
            self["batch_or_shell"] = "batch"

        # set cluster to phase name, if not given
        if self.get("cluster", None) is None:
            self["cluster"] = self["name"]

    def set_attrib(self, attrib, value):
        if type(self[attrib]) == "list":
            self[attrib].append(value)
        else:
            self[attrib] = value

    def remove_attrib(self, attrib, value):
        if type(self[attrib]) == "list":
            self[attrib].remove(value)
        else:
            self[attrib] = None


def assemble_workflow(config):
    from . import Workflow
    """
    Assembles the workflow tasks.
    Is called from the plugin recipe prepcompute.

    Parameters
    ----------
        config : dict

    Returns
    -------
        config : dict
    """
    # 1. Generate default workflow object and
    # 2. initialize default workflow phases from defaults.yaml
    workflow = init_default_workflow(config)

    # 3. Calc mpi tasks for batch jobs of default phases
    # TODO: Put it into other method???
    workflow = workflow.set_default_nproc(config)

    # 3. Read in phases from runscript and config files
    workflow = workflow.collect_all_user_phases(config)

    # 4. Cluster phases
    workflow = workflow.cluster_phases()

    # 4. Order user workflows into default workflow wrt. phase attributs.
    workflow = workflow.order_phases_and_clusters()

    # 5. create new first phase of type SimulationSetup, if first_task_in_queue is
    #    a user phase (type batch or shell)
    workflow = workflow.prepend_newrun_job()

    # 6. write the workflow to config
    # 7. Remove old worklow from config
    config = workflow.write_to_config(config)

    # Set "jobtype" for the first task???
    # NOTE: This is either first default phase or
    #       newrun??? Can't this not be set in prepend_newrun then?
    if config["general"]["jobtype"] == "unknown":
        config["general"]["command_line_config"]["jobtype"] = config["general"][
            "workflow"
        ]["first_task_in_queue"]
        config["general"]["jobtype"] = config["general"]["workflow"][
            "first_task_in_queue"
        ]

    return config

def init_default_workflow(config):
    """
    Initialize workflow and default phases from defauls.yaml
    """
    # 1. Generate default workflow object
    # initialize the default workflow as Workflow object
    # TODO: Where are these default phases defined? For now I placed it in
    # esm_tools/configs/esm_software/esm_runscripts/defaults.yaml
    if "defaults.yaml" in config["general"]:
        if "workflow" in config["general"]["defaults.yaml"]:
            workflow = config["general"]["defaults.yaml"]["workflow"]
            phases = config["general"]["defaults.yaml"]["workflow"].get("phases", [])
        else:
            esm_parser.user_error("ERROR", "No default workflow defined.")
    else:
        workflow = []
        phases = []

    # 2. Initialize default workflow phases from defaults.yaml
    if phases:
        workflow = Workflow(workflow)
        for phase in phases:
            workflow.phases.append(WorkflowPhase(phases[phase]))
    else:
        esm_parser.user_error("ERROR", "No default workflow phases defined.")
        # Note: Should this work also if no default phases are set in such a config
        # file, but instead all workflow phases are defined in different configs
        # and/or runscripts?
        # Where could a user define a different (default) phase list?
        # Or should this be changed in defaults.yaml as it is now?

    return workflow

def get_phase_attrib(workflow_phases, phase_name, attrib):
    if not type(workflow_phases) is list:
        workflow_phases = [workflow_phases]
    for phase in workflow_phases:
        if phase["name"] == phase_name:
            value = phase[attrib]
            return value


def calc_number_of_tasks(config):
    """
    Calculates the total number of needed tasks
    in phase compute
    TODO: make this phase method??? Or recipe entry???
    """
    tasks = 0
    for model in config["general"]["valid_model_names"]:
        if "nproc" in config[model]:
            tasks += config[model]["nproc"]
        elif "nproca" in config[model] and "nprocb" in config[model]:
            tasks += config[model]["nproca"] * config[model]["nprocb"]
            if "nprocar" in config[model] and "nprocbr" in config[model]:
                if (
                    config[model]["nprocar"] != "remove_from_namelist"
                    and config[model]["nprocbr"] != "remove_from_namelist"
                ):
                    tasks += config[model]["nprocar"] * config[model]["nprocbr"]
    return tasks


def display_workflow(config):
    """
    Displays workflow sequence.

    Parameters
    ----------
        config : dict

    Returns
    -------
        config : dict
    """

    display_nicely(config)
    display_workflow_sequence(config)


def display_workflow_sequence(config, display=True):

    first_phase = config["general"]["workflow"]["first_task_in_queue"]
    subjobs = config["general"]["workflow"]["subjob_clusters"][first_phase]["subjobs"]
    # Note: next_submit points to the next cluster (not phase)
    second_phase = config["general"]["workflow"]["subjobs"][first_phase]["next_submit"]

    workflow_order = f"``{first_phase}`` {subjobs}"

    # While first_phase (first_task_in_queue) is not to be called by the next phase (next_submit).
    # In other words: If not last phase/cluster is reached.
    while first_phase not in second_phase and second_phase:
        sec_phase_str = ""
        for sec_phase in second_phase:
            if config["general"]["workflow"]["subjob_clusters"][sec_phase]["next_submit"]:
                second_phase = config["general"]["workflow"]["subjob_clusters"][sec_phase]["next_submit"]
                subjobs = config["general"]["workflow"]["subjob_clusters"][sec_phase]["subjobs"]
            else:
                subjobs = config["general"]["workflow"]["subjob_clusters"][sec_phase]["subjobs"]
            if sec_phase_str == "":
                sec_phase_str = f"{sec_phase_str} ``{sec_phase}`` {subjobs}"
            else:
                sec_phase_str = f"{sec_phase_str}, ``{sec_phase}`` {subjobs}"
        workflow_order = f"{workflow_order} -> {sec_phase_str}"
    # For last phase that would start the next run
    else:
        sec_phase_str = ""
        # for all cluster in next_submit
        for sec_phase in second_phase:
            second_phase = config["general"]["workflow"]["subjob_clusters"][sec_phase]["next_submit"]
            subjobs = config["general"]["workflow"]["subjob_clusters"][sec_phase]["subjobs"]
            if sec_phase_str == "":
                sec_phase_str = f"{sec_phase_str} ``{sec_phase}`` {subjobs}"
            else:
                sec_phase_str = f"{sec_phase_str} and ``{sec_phase}`` {subjobs}"
        workflow_order = f"{workflow_order} -> {sec_phase_str}"

    if display:
        esm_parser.user_note("Workflow sequence (cluster [phases])", f"{workflow_order}")
    else:
        workflow_order = workflow_order.replace("``", "")
    return workflow_order


def display_nicely(config):
    """
    Pretty prints the workflow configuration assembled in config["general"].
    Is called by e.g. ``esm_runscripts runscript.yaml -e <expid> -i workflow``

    Parameters
    ----------
        config : dict

    Returns
    -------
        config : dict
    """
    esm_parser.pprint_config(config["general"]["workflow"])
    return config
