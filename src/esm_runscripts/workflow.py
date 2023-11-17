import sys
import copy
import esm_parser

# from pprint import pprint

import pdb


class Workflow:
    """A workflow class."""

    def __init__(self, phases, always_run_with=[], phases_to_submit_to_batch_system=[]):
        """
        Create a new workflow.

        Parameters
        ----------
        phases : list
            List of workflow phases names
        always_run_with : list
            List of phases that precedes each phase in phases

        Returns
        -------
        none
        """
        self.user_phases = []
        self.first_task_in_queue = None                     # needed
        self.last_task_in_queue = None
        self.next_run_triggered_by = None
        # TODO: Call here the phase object ???
        self.phases = phases
        self.always_run_with = always_run_with
        self.phases_to_submit_to_batch_system = phases_to_submit_to_batch_system

    @property
    def num_phases(self):
        """
        Return the number of phases in workflow.
        """
        return len(self.phases)

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
            if phase.name == phase_name:
                return phase

    def get_phases_attribs_list(self, phase_type, attrib):
        """
        Returns a certain attribute for all phases as a list.

        Parameters
        ----------
            self: class Workflow
            phase_type: str (default or user)
            attrib: str

        Returns
        -------
            phases_attribs : list
        """
        if phase_type == 'user':
            phases_attribs = [getattr(phase, attrib) for phase in self.user_phases]
        else:
            phases_attribs = [getattr(phase, attrib) for phase in self.phases]

        return phases_attribs

    def init_default_workflow(self, config):
        """
        Add workflow for precompute, compute, and tidy phases
        etc information already here!

        Parameters
        ----------
            self : Workflow object
            config : dict

        Returns
        -------
            self : Workflow object
        """

        workflow_phases = self.phases

        # Calculating the number of tasks for each component/model
        # needed for phase compute
        tasks = calc_number_of_tasks(config)
        # Initiate/create default workflow phase objects
        # and reset/append to Workflow.phases variable
        self.phases = []
        for ind, phase in enumerate(workflow_phases):
            self.phases.append(WorkflowPhase(phase))

        for ind, phase in enumerate(self.phases):
            if ind < self.num_phases - 1:
                # Set run_before attrib of all phases (except last on)
                # to the next phase name
                phase.run_before = self.phases[ind+1].name
                phase.next_submit.append(self.phases[ind+1].name)
                phase.run_after = self.phases[ind-1].name
            else:
                # Set run_after attrib of last phase to previous phase name
                phase.run_before = self.phases[0].name
                phase.next_submit.append(self.phases[0].name)
                phase.run_after = self.phases[ind-1].name

            phase.cluster = phase.name
            if phase.name in self.phases_to_submit_to_batch_system:
                phase.batch_or_shell = 'batch'
                phase.submit_to_batch_system = True
                phase.run_on_queue = config["computer"]["partitions"]["compute"]["name"]
                phase.nproc = tasks

        # Set default workflow values
        self.set_workflow_attrib("first_task_in_queue", self.phases[0].name)
        self.set_workflow_attrib("last_task_in_queue", self.phases[-1].name)
        self.set_workflow_attrib("next_run_triggered_by", self.phases[-1].name)

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

        if hasattr(self, keyword):
            return True
        else:
            return False

    def collect_all_user_workflows(self, config):
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
        for model in config:
            if "workflow" in config[model]:
                w_config = config[model]["workflow"]
                # if "subjobs" in w_config:
                if "phases" in w_config:
                    # Set attributes of workflow
                    # This will be overwritten by all user defined workflows???
                    # Collect them in a list???
                    # check if valid workflow keywords
                    for key, value in w_config.items():
                        if self.check_if_keyword_is_valid(key):
                            # set here only workflow attributes
                            if not key == "phases":
                                self.set_workflow_attrib(key, value)
                        else:
                            err_msg = f"``{key}`` is not a valid keyword of a workflow."
                            esm_parser.user_error("ERROR", err_msg)
                    # for subjob in list(copy.deepcopy(w_config["subjobs"])):
                    for phase in list(copy.deepcopy(w_config["phases"])):
                        new_phase_name = phase
                        # create a new user phase object for ``phase``
                        new_phase = UserWorkflowPhase(new_phase_name)
                        # each subjob needs to have an unique name
                        # 1. check if ``new_phase`` is already defined as a default phase
                        if phase in self.get_phases_attribs_list("default", "name"):
                            err_msg = (
                                f"The user phase ``{new_phase_name}`` "
                                f"has the same name as a default workflow phase. "
                                f"This is not allowed."
                            )
                            esm_parser.user_error("ERROR", err_msg)
                        # 2. check if the name of the new user phase does not already exist
                        if new_phase_name in user_workflow_phases_names:
                            err_msg = (
                                f"Two workflow phases have the same name "
                                f"{new_phase_name}."
                            )
                            esm_parser.user_error("ERROR", err_msg)
                        # 3. if user phase has a new and unique name
                        else:
                            # append it to the list of user phases of the workflow
                            user_workflow_phases_names.append(new_phase_name)
                            # set attributes of user_workflow phases from
                            # config settings
                            # check if valid phase keywords
                            for key, value in w_config["phases"][phase].items():
                                if new_phase.check_if_keyword_is_valid(key):
                                    set_phase_attrib(
                                        new_phase, new_phase_name, key, value
                                    )
                                else:
                                    err_msg = (
                                        f"``{key}`` of workflow phase "
                                        f"``{new_phase_name}`` is not a valid keyword "
                                        f"of a workflow phase."
                                    )
                                    esm_parser.user_error("ERROR", err_msg)
                            # Make sure that batch_or_shell is set to batch if submit_to_batch is true
                            # TODO: remove/replace batch_or_shell by submit_to_batch_system? Is needed
                            # for setting it to SimulationSetup and in other functions (resubmit, etc.)
                            # Should not be set by user. TODO: Remove from dumentation.
                            if new_phase.submit_to_batch_system:
                                new_phase.batch_or_shell == "batch"

                            user_workflow_phases.append(new_phase)

        self.user_phases = user_workflow_phases
        return self

    def write_to_config(self, config):
        """
        Write to config.
        TODO: Rename ``subjobs`` to ``phases``. But this needs changes also in resubmit.py and other files???
        """
        cluster_att = []
        for att in dir(self.phases[0]):
            if (att[:2] != "__"):
                cluster_att.append(att)
        # 1. Delete unnecessary config workflow entries (e.g. in general)
        if "workflow" in config["general"]:
            del config["general"]["workflow"]

        config["general"]["workflow"] = {}
        config["general"]["workflow"].update(self.__dict__)
        # 3. Write clusters
        config["general"]["workflow"]["subjob_clusters"] = {}
        for cluster in self.get_phases_attribs_list("default", "cluster") + self.get_phases_attribs_list("user", "cluster"):
            config["general"]["workflow"]["subjob_clusters"][cluster] = {}
            config["general"]["workflow"]["subjob_clusters"][cluster]["subjobs"] = []
            for phase in self.phases + self.user_phases:
                if phase.cluster == cluster:
                    config["general"]["workflow"]["subjob_clusters"][cluster]["subjobs"].append(phase.name)
                    for att in cluster_att:
                        config["general"]["workflow"]["subjob_clusters"][cluster][att] = getattr(phase, att)
        # 2. Write subjobs/phases
        config["general"]["workflow"]["subjobs"] = {}
        for phase in self.phases+self.user_phases:
            temp_dict = {phase.name: phase.__dict__}
            config["general"]["workflow"]["subjobs"].update(temp_dict)

        # Todo: delete phases and user_phases
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
        Check if any user phase attributes points to any unknown workflow phase.

        Parameters
        ----------
            self : Workflow object

        Returns
        -------
            unknown_phases : set
        """
        unknown_phases = []
        phases_names = self.get_phases_attribs_list('default', 'name')
        user_phases_names = self.get_phases_attribs_list('user', 'name')
        run_after = self.get_phases_attribs_list('user', 'run_after')
        run_before = self.get_phases_attribs_list('user', 'run_before')
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

    def order_phases(self):
        """
        Put the phases in order.

        Parameters
        ----------
            config : dict

        Returns
        -------
            self : Workflow object
        """
        # check if user phases are independent from each other
        # TODO: What if not independent?
        independent = self.check_user_workflow_dependency()
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
            if not user_phase.run_before and not user_phase.run_after:
                user_phase.run_after = self.phases[-1].name
                err_msg = (
                    f"No value given for ``run_after`` or ``run_before`` "
                    f"of user phase ``{user_phase.name}``. "
                    f"Set it to last default phase in workflow: "
                    f"``{self.phases[-1].name}``."
                )
                esm_parser.user_note("NOTE", err_msg)

            # Check if not both run_after and run_before are set at the same
            # time for each user phase
#            if user_phase.run_before and user_phase.run_after:
#                err_msg = (
#                    f"Both run_after and run_before are set. Don't know when "
#                    f"to start {user_phase.name}. Please only set run_after "
#                    f"or run_before."
#                )
#                esm_parser.user_error("ERROR", err_msg)

        # Correct for ``last_task_in_queue`` if necessary
        # Collect all next_run_triggered_by entries
        next_triggered = self.next_run_triggered_by
        # check if next_triggered is default or user phase
        # if user phase
        # get last default phase and correct next_submit
        # get first default phase and correct run_after, called_from
        # correct last_task_in_queue of workflow
        if next_triggered not in self.get_phases_attribs_list("default", "name"):
            self.phases[-1].next_submit.remove(self.phases[0].name)
            self.phases[-1].next_submit.append(next_triggered)
            self.phases[0].run_after = next_triggered
            self.phases[0].called_from = next_triggered
            self.last_task_in_queue = next_triggered

        # Set "next_submit" and "called_from"
        # "next_submit" which phase/cluster will be called next (run_after of the next phase)
        # "called_from" name of previous phase, run_after of current phase
        # Create a dict of all phases with empty lists
        next_submits = {}
        for phase in self.phases + self.user_phases:
            next_submits[phase.name] = []

        for phase4 in self.phases + self.user_phases:
            # if a cluster is not set for a phase set it to the phase name,
            # so that every phase belongs to a cluster
            # default cluster has the same name as the phase itself
            if phase4.cluster is None:
                phase4.cluster = phase4.name

        # set next_submits to the cluster name rather then to the phase name
        for phase2 in self.phases + self.user_phases:
            if phase2.run_after is not None:
                if phase2.cluster not in next_submits[phase2.run_after]:
                    next_submits[phase2.run_after].append(phase2.cluster)
                phase2.called_from = phase2.run_after

        for phase3 in self.phases + self.user_phases:
            phase3.next_submit = next_submits[phase3.name]

        first_cluster_name = self.first_task_in_queue
        last_cluster_name = self.last_task_in_queue

        # if first_cluster_name is not next_submit of last_cluster_name
        # set 'next_submit' of last phase/cluster to first phase/cluster in workflow
        if first_cluster_name not in get_phase_attrib(self.phases+self.user_phases, last_cluster_name, "next_submit"):
            set_phase_attrib(self.phases+self.user_phases, last_cluster_name, "next_submit", first_cluster_name)
        # if last_cluster_name is not called_from of first_cluster_name
        # set 'called_from' of first phase/cluster to last phase/cluster
        if not last_cluster_name == get_phase_attrib(self.phases+self.user_phases, first_cluster_name, "called_from"):
            set_phase_attrib(self.phases+self.user_phases, first_cluster_name, "called_from", last_cluster_name)

        return self

#    def complete_clusters(self, config):
#        # all that are within a next_submit list are in a cluster if:
#        # run concurrently
#        # have the same cluster entry.
#        """
#        Rearanges the subjobs to their subjobs_clusters ???
#
#        Parameters
#        ----------
#            self : Workflow object
#            config : dict
#
#        Returns
#        -------
#            subjob_clusters : dict
#        """
#        # sort into dict subjob_clusters
#        subjob_clusters = {}
#
#        for phase in self.phases + self.user_phases:
#            # Erstellt ein leeres dict im dict subjob_clusters
#            if phase.cluster not in subjob_clusters:
#                subjob_clusters[phase.cluster] = {}
#
#            # Create empty list for each subjob_cluster
#            if "subjobs" not in subjob_clusters[phase.cluster]:
#                subjob_clusters[phase.cluster]["subjobs"] = []
#
#            # Append subjobs to list.
#            subjob_clusters[phase.cluster]["subjobs"].append(phase.name)
#
#        # Then, complete the resource information per cluster
#        # determine whether a cluster is to be submitted to a batch system
#        for subjob_cluster in subjob_clusters:
#            nproc_sum = nproc_max = 0
#            attributes = ["submit_to_batch_system", "order_in_cluster", "run_on_queue", "run_after", "run_before", "run_only", "skip_run_number", "skip_chunk_number", "batch_or_shell"]
#            for attrib in attributes:
#                temp_list = []
#                for subjob in subjob_clusters[subjob_cluster]["subjobs"]:
#                    if not get_phase_attrib(self.phases + self.user_phases, subjob, attrib) in temp_list:
#                        subjob_clusters[subjob_cluster][attrib] = get_phase_attrib(self.phases + self.user_phases, subjob, attrib)
#                    else:
#                        print("Missmatch in attributes")
#                        sys.exit(-1)
#                nproc_sum += get_phase_attrib(self.phases + self.user_phases, subjob, "nproc")
#                nproc_max = max(get_phase_attrib(self.phases + self.user_phases, subjob, "nproc"), nproc_max)
#
#    #        if subjob_clusters[subjob_cluster].get("submit_to_batch_system", False):
#    #            subjob_clusters[subjob_cluster]["batch_or_shell"] = "batch"
#    #        elif subjob_clusters[subjob_cluster].get("script", False):
#    #            subjob_clusters[subjob_cluster]["batch_or_shell"] = "shell"
#    #
#            if "run_on_queue" not in subjob_clusters[subjob_cluster]:
#                print(f"Information on target queue is missing in cluster {subjob_cluster}.")
#                sys.exit(-1)
#    #
#    # TODO: Check in nproc is calculated correctly
#            if subjob_clusters[subjob_cluster]["order_in_cluster"] == "concurrent":
#                nproc = nproc_sum
#            else:
#                nproc = nproc_max
#            subjob_clusters[subjob_cluster]["nproc"] = nproc
#        return subjob_clusters

    def prepend_newrun_job(self, config):
        """
        - Creates a new cluster "newrun" if first_task_in_queue is not of
          type 'SimulationSetup'
        - Why is this needed? So that every first task is a SimulationSetup to init
          a config object???

        Looks for subjob_cluster that are set by user workflow (not a 'SimulationSetup')
        and are not of type 'SimulationSetup'.

        Parameters
        ----------
            self : Workflow object
            config : dict

        Returns
        -------
            self : Workflow object
        """
        first_task_name = self.first_task_in_queue
        first_phase = self.get_workflow_phase_by_name(first_task_name)

        if not get_phase_attrib(first_phase, first_phase.name, "batch_or_shell") == "SimulationSetup":

            last_task_name = self.last_task_in_queue
            last_phase = self.get_workflow_phase_by_name(last_task_name)

            new_first_phase_name = "newrun_general"
            # Create new default phase object
            new_first_phase = WorkflowPhase(new_first_phase_name)
            set_phase_attrib(new_first_phase, new_first_phase_name, "called_from", last_task_name)
            set_phase_attrib(new_first_phase, new_first_phase_name, "run_before", first_task_name)
            set_phase_attrib(new_first_phase, new_first_phase_name, "next_submit", first_task_name)
            set_phase_attrib(new_first_phase, new_first_phase_name, "cluster", "newrun")
            set_phase_attrib(new_first_phase, new_first_phase_name, "batch_or_shell", "SimulationSetup")
            set_phase_attrib(new_first_phase, new_first_phase_name, "nproc", 1)

            # reset last_task attributes
            set_phase_attrib(last_phase, last_phase.name, "next_submit", new_first_phase_name)
            last_phase.next_submit.remove(first_task_name)

            # reset first_task attributes
            first_phase.called_from = new_first_phase_name

            # reset workflow attributes
            self.first_task_in_queue = new_first_phase_name

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


class WorkflowPhase:
    """A workflow phase class."""

    def __init__(self, phase_name):
        self.name = None
        self.nproc = 1                              # needed
        self.run_before = None
        self.run_after = None
        self.submit_to_batch_system = False         # needed
        self.run_on_queue = None
        self.cluster = None
        self.next_submit = []                       # needed
        self.called_from = None                     # needed
        self.batch_or_shell = "SimulationSetup"     # needed
        self.order_in_cluster = "sequential"        # needed ???
        self.run_only = None
        self.skip_chunk_number = None
        self.skip_run_number = None
        self.name = phase_name


class UserWorkflowPhase(WorkflowPhase):
    """A user workflow phase class."""

    def __init__(self, phase_name):

        WorkflowPhase.__init__(self, phase_name)

        self.script = None
        self.script_dir = None
        self.call_function = None
        self.env_preparation = None
        self.batch_or_shell = "shell"
        self.submit_to_batch_system = False

    def check_if_keyword_is_valid(self, keyword):
        """
        Checks if the key given for a user workflow is valie

        Parameters
        ----------
            keyword : str

        Returns
        -------
            true or false
        """

        if hasattr(self, keyword):
            return True
        else:
            return False


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

    # 1. Generate default workflow object
    # initialize the default workflow as Workflow object
    # TODO: Where are these default phases defined? For now I placed it in
    # esm_tools/configs/esm_software/esm_runscripts/defaults.yaml
    if "defaults.yaml" in config["general"]:
        if "workflow" in config["general"]["defaults.yaml"]:
            phases = config["general"]["defaults.yaml"]["workflow"].get("phases", [])
            always_run_with = config["general"]["defaults.yaml"]["workflow"].get("always_run_with", [])
            phases_to_submit_to_batch_system = config["general"]["defaults.yaml"]["workflow"].get("phases_to_submit_to_batch_system", [])

    if phases:
        workflow = Workflow(phases, always_run_with=always_run_with, phases_to_submit_to_batch_system=phases_to_submit_to_batch_system)
    else:
        esm_parser.user_error("ERROR", "No default workflow phases defined.")
        # Note: Should this work also if no default phases are set in such a config
        # file, but instead all workflow phases are defined in different configs
        # and/or runscripts?
        # Where could a user define a different (default) phase list?
        # Or should this be changed in defaults.yaml as it is now?

    # 2. Initialize default workflow phases
    workflow = workflow.init_default_workflow(config)

    # 3. Read in workflows from runscript and config files
    workflow = workflow.collect_all_user_workflows(config)

    # 4. Order user workflows into default workflow wrt. workflow and phase attributs.
    workflow = workflow.order_phases()

    # What is the next functions needed for?
    # subjob_clusters = workflow.complete_clusters(config)

    # 5. create new first phase of type SimulationSetup, if first_task_in_queue is
    #    user phase (type batch or shell)
    workflow = workflow.prepend_newrun_job(config)

    # 6. write the workflow to config
    # 7. Remove old worklow from config
    config = workflow.write_to_config(config)


    # Set "jobtype" for the first task???
    if config["general"]["jobtype"] == "unknown":
        config["general"]["command_line_config"]["jobtype"] = config["general"][
            "workflow"
        ]["first_task_in_queue"]
        config["general"]["jobtype"] = config["general"]["workflow"][
            "first_task_in_queue"
        ]

    return config


def set_phase_attrib(workflow_phases, phase_name, attrib, value):
    if not type(workflow_phases) is list:
        workflow_phases = [workflow_phases]
    for phase in workflow_phases:
        if phase.name == phase_name:
            if type(getattr(phase, attrib)).__name__ == "list":
                phase.__dict__[attrib].append(value)
            else:
                phase.__setattr__(attrib, value)


def get_phase_attrib(workflow_phases, phase_name, attrib):
    if not type(workflow_phases) is list:
        workflow_phases = [workflow_phases]
    for phase in workflow_phases:
        if phase.name == phase_name:
            value = getattr(phase, attrib)
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
    Displays current workflow settings.

    Parameters
    ----------
        config : dict

    Returns
    -------
        config : dict (needed???)
    """

    display_nicely(config)

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
            if config["general"]["workflow"]["subjobs"][sec_phase]["next_submit"]:
                second_phase = config["general"]["workflow"]["subjobs"][sec_phase]["next_submit"]
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

    esm_parser.user_note("Workflow sequence (cluster [phases])", f"{workflow_order}")
    return config


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

# ################## Maybe outdated routines ######################
#
#
# def collect_all_workflow_information(config):
#    """
#    Collects all workflow information for each component entry in config
#    (can be a model/component or a new entry (e.g. 'flows')
#    NOTE: Should it be possible to set a workflow in the model section of the
#          runscript? Why not?
#
#    Checks if there are "workflow" entries in the user runscript and copies or
#    merges them into
#    config["general"]["workflow"]
#
#    Parameters
#    ----------
#        config : dict
#
#    Returns
#    -------
#        config : dict
#    """
#    for model in config:
#        if "workflow" in config[model]:
#            # looks for "workflow" in each entry of config (can be model/component, general, etc.)
#            w_config = config[model]["workflow"]
#            # looks for "workflow" in "general" section of config.
#            gw_config = config["general"]["workflow"]
#
#            # looks for entry 'subjob_clusters' in config of each component that has a "workflow"
#            if "subjob_clusters" in w_config:
#                for cluster in w_config["subjob_clusters"]:
#                    # if a certain cluster is also in the general config, this cluster will be merged together ...
#                    # what cluster could this be?
#                    if cluster in gw_config["subjob_clusters"]:
#                        gw_config["subjob_clusters"][cluster] = merge_if_possible(
#                            w_config["subjob_clusters"][cluster],
#                            gw_config["subjob_clusters"][cluster],
#                        )
#                    # if cluster is not in general config, it will copied into it.
#                    else:
#                        gw_config["subjob_clusters"][cluster] = copy.deepcopy(
#                            w_config["subjob_clusters"][cluster],
#                        )
#
#            # looks for entry 'subjobs' in config of each component
#            if "subjobs" in w_config:
#                # copies component workflow config to new variable ref_config
#                ref_config = copy.deepcopy(w_config)
#                # ??? for every subjob in ???
#                for subjob in list(copy.deepcopy(w_config["subjobs"])):
#
#                    # subjobs (other than clusters) should be model specific
#                    # subjobs that are defined in subjobs of components workflow configs and not in a subjob_cluster are copied to general with suffix of componet entry.
#                    # appends the model name to the subjob name and copy it to config["general"]
#                    gw_config["subjobs"][subjob + "_" + model] = copy.deepcopy(
#                        w_config["subjobs"][subjob]
#                    )
#                    # if this copied subjobs is also n general workflow subjobs it will be deleted there
#                    if subjob in gw_config["subjobs"]:
#                        del gw_config["subjobs"][subjob]
#
#                    # make sure that the run_after and run_before refer to that cluster
#                    # for all subjobs now in general workflow
#                    for other_subjob in gw_config["subjobs"]:
#                        # sets run_after and run_before to correct subjob???
#                        # if a subjob of general workflow has run_after attribute to a user subjob (that has been renamed to subjob_model)
#                        # this run_after will be set to the new subjob name (subjob_model)
#                        if "run_after" in gw_config["subjobs"][other_subjob]:
#                            if (gw_config["subjobs"][other_subjob]["run_after"] == subjob):
#                                gw_config["subjobs"][other_subjob]["run_after"] == subjob + "_" + model
#                        if "run_before" in gw_config["subjobs"][other_subjob]:
#                            if (gw_config["subjobs"][other_subjob]["run_before"] == subjob):
#                                gw_config["subjobs"][other_subjob]["run_before"] == subjob + "_" + model
#
#                    # if not in another cluster, each subjob gets its own
#                    if (not "subjob_cluster" in gw_config["subjobs"][subjob + "_" + model]):
#                        gw_config["subjobs"][subjob + "_" + model]["subjob_cluster"] = subjob  # + "_" + model
#
#            # checks if next_run:triggered_by is tidy or the one in user workflow, or empty?
#            if "next_run_triggered_by" in w_config:
#                if not gw_config["next_run_triggered_by"] in ["tidy", w_config["next_run_triggered_by"], ]:
#                    print("Mismatch found setting next_run_triggered_by for workflow.")
#                    sys.exit(-1)
#                else:
#                    gw_config["next_run_triggered_by"] = w_config["next_run_triggered_by"]
#                    # what if w_config["next_run_triggered_by"] is empty?
#
#    return config
#
# def merge_single_entry_if_possible(entry, sourceconf, targetconf):
#    """
#    Merges a dictionary entry into a target dictionary that has he same key.
#
#    Parameters
#    ----------
#        entry : str
#            dictionary key
#        sourceconf : dict
#        targetconf : dict
#
#    Returns
#    -------
#        targetconf : dict
#    """
#    if entry in sourceconf:
#        # Check if entry is already in targetconf AND different to sourceconf, then exit
#        if entry in targetconf and not sourceconf[entry] == targetconf[entry]:
#            print(f"Mismatch found in {entry} for cluster {targetconf}")
#            sys.exit(-1)
#        # Continues here if entry exists already in targetconf AND the same as sourceconf or
#        # not already in targetconf and set it to sourceconf
#        targetconf[entry] = sourceconf[entry]
#    return targetconf
#
# def merge_if_possible(source, target):
#    """
#    Does the same as above but for a whole dict
#
#    Merges the entries of source dictionary into target dictionary, if not already in.
#    (Will not overwrite entries in target dictionary.)
#
#    Parameters
#    ----------
#        source : dict
#        target : dict
#
#    Returns
#    -------
#        target : dict
#    """
#    for entry in source:
#        if entry in target:
#            if not source[entry] == target[entry]:
#                print(
#                    f"Mismatch while trying to merge subjob_clusters {source} into {target}"
#                )
#                sys.exit(-1)
#        else:
#            target[entry] = source[entry]
#    return target
