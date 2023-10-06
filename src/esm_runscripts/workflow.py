import sys, copy, os
import esm_parser

#import pdb

class Workflow:
    """A workflow class."""
    default_phases = []
    user_phases = []
    always_run_with = []
    first_task_in_queue = ""
    last_task_in_queue = ""
    next_run_triggered_by = ""

    def __init__(self, phases, always_run_with=[]):
        """
        Create a new workflow.

        Arguments:
            phases -- List of workflow phases
            always_run_with -- List of phases that precedes each phase
        """
        # TODO: NW call here the phase object ???
        self.phases = phases
        self.always_run_with = always_run_with

    def num_phases_in_workflow(self):
        """
        Return the number of phases in workflow.
        """
        return len(self.phases)

    def write_to_config(self, config):
        """
        Write to config.
        """
        # NW: It is assumed here, that there are no workflows in config["general"]
        # or that these are removed after collect_...
        config["general"]["workflow"] = {}
        config["general"]["workflow"].update(self.__dict__)
        config["general"]["workflow"]["subjobs"] = {}
        for phase in self.phases:
            temp_dict = {phase.name: phase.__dict__}
            config["general"]["workflow"]["subjobs"].update(temp_dict)

        return config

    def check_user_workflow_dependency(self):
        """
        Check whether the user defined workflow phases are independent from eachother or not.
        """
        independent = False
        user_phases_names = [phase.name for phase in self.user_phases]
        run_after_list = [phase.run_after for phase in self.user_phases]
        run_before_list = [phase.run_before for phase in self.user_phases]
        if not set(user_phases_names).intersection(set(run_after_list).union(set(run_before_list))):
            independent = True
        else:
            independent = False

        return independent

    def check_unknown_phases(self):
        """
        Check if any user phase addresses an unknown workflow phase.
        """
        unknown_user_phase = True
        phases_names = [phase.name for phase in self.phases]
        user_phases_names = [phase.name for phase in self.user_phases]
        # Filter out all falsy items (e.g. [], "", None)
        run_after_list = list(filter(None, [phase.run_after for phase in self.user_phases]))
        run_before_list = list(filter(None, [phase.run_before for phase in self.user_phases]))

        unknown_user_phases = set(run_after_list).union(set(run_before_list)).difference(set(user_phases_names).union(set(phases_names)))
        return unknown_user_phases

    def skip_cluster(self, config):
        """
        Checks if a phase/cluster can be skipped.
        Needed keywords: run_only, skip_chunk_number
        Arguments:
            self
            config
        Returns:
            True or False
        """
        #gw_config = config["general"]["workflow"]
        #clusterconf = gw_config["subjob_clusters"][cluster]

        #"""
        #print(f"run_only {clusterconf.get('run_only', 'Error') }")
        #print(f"skip_chunk_number {clusterconf.get('skip_chunk_number', -999)}")
        #print(f"skip_run_number {clusterconf.get('skip_run_number', -999)}")
        #print(f"chunk_number {config['general'].get('chunk_number', -998)}")
        #print(f"run_number {config['general'].get('run_number', -998)}")
        #print(f"last_run_in_chunk {config['general']['last_run_in_chunk']}")
        #print(f"first_run_in_chunk {config['general']['first_run_in_chunk']}")
        #"""

        #if clusterconf.get("run_only", "Error") == "last_run_in_chunk" and not config[
        #    "general"
        #].get("last_run_in_chunk", False):
        #    return True
        #if clusterconf.get("run_only", "Error") == "first_run_in_chunk" and not config[
        #    "general"
        #].get("first_run_in_chunk", False):
        #    return True
        #if clusterconf.get("skip_chunk_number", -999) == config["general"].get(
        #    "chunk_number", -998
        #):
        #    return True
        #if clusterconf.get("skip_run_number", -999) == config["general"].get(
        #    "run_number", -998
        #):
        #    return True

        return False

class WorkflowPhase:
    """A workflow phase class."""
    name = None
    nproc = 1
    run_before = None
    run_after = None
    submit_to_batch_system = True
    run_on_queue = None
    cluster = None
    next_submit = []
    called_from = None
    batch_or_shell = "SimulationSetup"
    order_in_cluster = "sequential"
    run_only = None
    skip_chunk_number = None
    skip_run_number = None

    def __init__(self, phase_name):
        self.name = phase_name

class UserWorkflowPhase(WorkflowPhase):
    """A user workflow phase class."""
    script = None
    script_dir = None
    call_function = None
    env_preparation = None

    def __init__(self, phase_name):
        self.name = phase_name
        batch_or_shell = "batch"
        submit_to_batch_system = False

def assemble_workflow(config):
    from . import Workflow
    """
    Assembles the workflow tasks.
    Is called from the plugin recipe prepcompute.

    Arguments:
        config -- dictionary
    Returns:
        config
    """

    # 1. Generate default workflow object
    #TODO: preset of default workflow phases should be set in some config file.
    workflow = Workflow(["prepcompute","compute","tidy"],always_run_with=["prepare","prepexp"])
    # initialize the default workflow as Workflow object
    # TODO: NW where are these default phases defined???
    # 2. Initialize default workflow phases
    workflow = init_default_workflow(workflow, config)
    # 3. Read in workflows from runscript and config files
    workflow = collect_all_user_workflows(workflow, config)

    #config = collect_all_workflow_information(config)

# Why do I need to do the following function call?
    # 4. Order user workflows into default workflow wrt. workflow attributs.
    workflow = order_clusters(workflow, config)

    subjob_clusters = complete_clusters(workflow, config)
    subjob_clusters = prepend_newrun_job(config)
    # 5. write the workflow to config
    config = workflow.write_to_config(config)
    breakpoint()
    # 6. Remove old worklow from config

    # Set "jobtype" for the first task???
    if config["general"]["jobtype"] == "unknown":
        config["general"]["command_line_config"]["jobtype"] = config["general"][
            "workflow"
        ]["first_task_in_queue"]
        config["general"]["jobtype"] = config["general"]["workflow"][
            "first_task_in_queue"
        ]

    return config

def write_subjob_clusters_to_config(config, subjob_clusters):
    config["general"]["subjob_clusters"] = subjob_clusters
    return config

def display_nicely(config):
    """
    Pretty prints the workflow configuration assembled in config["general"].

    Arguments:
        config -- dictionary
    Returns:
        config
    """
    esm_parser.pprint_config(config["general"]["workflow"])
    return config


def prepend_newrun_job(workflow, config, subjob_clusters):
    """
    Looks for subjob_cluster that are set by user workflow (not a 'SimulationSetup')
    and do not follow a 'SimulationSetup' subjob_clusters.
    E.g. if two user workflow are the last two subjob_clusters ???
    Any other example cases when this is the case?

    Arguments:
        config -- dictionary
    Returns:
        workflow
        subjob_clusters
    """
    first_cluster_name = workflow.first_task_in_queue
    first_cluster = subjob_clusters[first_cluster_name]
    #esm_parser.pprint_config(first_cluster)

    if not first_cluster.get("batch_or_shell", "Error") == "SimulationSetup":

        last_cluster_name = workflow.last_task_in_queue
        last_cluster = subjob_clusters[last_cluster_name]

        new_first_cluster_name = "newrun"
        new_first_cluster = {
            "newrun": {
                "called_from": last_cluster_name,
                "run_before": first_cluster_name,
                "next_submit": [first_cluster_name],
                "subjobs": ["newrun_general"],
                "batch_or_shell": "SimulationSetup",
            }
        }

        last_cluster["next_submit"].append("newrun")
        last_cluster["next_submit"].remove(first_cluster_name)

        first_cluster["called_from"] = "newrun"

        workflow.first_task_in_queue = "newrun"

        new_subjob = {
            "newrun_general": {
                "nproc": 1,
                "called_from": last_cluster_name,
                "run_before": first_cluster_name,
                "next_submit": [first_cluster_name],
                "subjob_cluster": "newrun",
            }
        }

        subjob_clusters.update(new_first_cluster)

# TODO: add new phase to workflow???
        #gw_config["subjobs"].update(new_subjob)

    return [workflow, subjob_clusters]

def set_phase_attrib(workflow_phases, phase_name, attrib, value):
    for phase in workflow_phases:
        if phase.name == phase_name:
            if type(getattr(phase, attrib)).__name__ == "list":
                phase.__dict__[attrib].append(value)
            else:
                phase.__setattr__(attrib, value)

def get_phase_attrib(workflow_phases, phase_name, attrib):
    for phase in workflow_phases:
        if phase.name == phase_name:
            value = getattr(phase, attrib)
    return value

def order_clusters(workflow, config):
    """
    Put the subjob_clusters in order ???

    Arguments:
        config -- dictionary
    Returns:
        workflow
    """
    independent = workflow.check_user_workflow_dependency()
    unknown_phases = workflow.check_unknown_phases()

    if unknown_phases:
        esm_parser.user_error("ERROR", "Undefined subjob/phase.")

    for user_phase in workflow.user_phases:
# TODO: Check if run_after or run_before is set for each user phase
        if not user_phase.run_before and not user_phase.run_after:
            esm_parser.user_error("ERROR", "Don't know when to start user_phase. Please set run_after or run_before")
# TODO: Check if not both run_after and run_before are set at the same time for each user phase
        if user_phase.run_before and user_phase.run_after:
            esm_parser.user_error("ERROR", "Both run_after and run_before are. Don't know when to start user_phase. Please only set run_after or run_before")

# TODO: Correct for "last_task_in_queue" if necessary
    # Collect all next_run_triggered_by entrie
    next_triggered = []
    run_after = []
    for model in config:
        if "workflow" in config[model]:
            if "next_run_triggered_by" in config[model]["workflow"]:
                next_triggered.append(config[model]["workflow"]["next_run_triggered_by"])
    next_triggered = list(filter((workflow.next_run_triggered_by).__ne__, next_triggered))
    if len(next_triggered) > 1:
        esm_parser.user_error("ERROR", f"Mismatch found setting next_run_triggered_by for workflow.")
    elif len(next_triggered) == 1:
        workflow.next_run_triggered_by = next_triggered[0]
    #else: let default

# Fill up "next_submit" list
    next_submits = {}
    for phase in workflow.phases + workflow.user_phases:
        next_submits[phase.name] = []
    for phase2 in workflow.phases + workflow.user_phases:
        if not phase2.run_after == None:
            next_submits[phase2.run_after].append(phase2.name)
            phase2.called_from = phase2.run_after
    for phase3 in workflow.phases + workflow.user_phases:
        phase3.next_submit = next_submits[phase3.name]

    for phase4 in workflow.phases + workflow.user_phases:
        calling_cluster = phase4.run_after
#
        if calling_cluster == workflow.last_task_in_queue:
            workflow.last_task_in_queue = phase4.name
#
        called_cluster = phase4.run_before
        set_phase_attrib(workflow.phases+workflow.user_phases, called_cluster, "called_from", phase4.name)
        if called_cluster == workflow.first_task_in_queue:
            workflow.first_task_in_queue = phase4.name
        if phase4.cluster == None:
            phase4.cluster = phase4.name
#
    first_cluster_name = workflow.first_task_in_queue
    last_cluster_name = workflow.last_task_in_queue
#
    value = get_phase_attrib(workflow.phases+workflow.user_phases, last_cluster_name, "next_submit")
    if not first_cluster_name in get_phase_attrib(workflow.phases+workflow.user_phases, last_cluster_name, "next_submit"):
        set_phase_attrib(workflow.phases+workflow.user_phases, last_cluster_name, "next_submit", first_cluster_name)
    if not last_cluster_name == get_phase_attrib(workflow.phases+workflow.user_phases, first_cluster_name, "called_from"):
        set_phase_attrib(workflow.phases+workflow.user_phases, first_cluster_name, "called_from", last_cluster_name)
#
    return workflow


def complete_clusters(workflow, config):
    # all that are within a next_submit list are in a cluster if:
    # run concurrently
    # have the same cluster entry.
    """
    Rearanges the subjobs to their subjobs_clusters ???
    Arguments:
        workflow -- obj
        config -- dictionary
    Returns:
        subjob_clusters -- dictionary
    """
    # sort into dict subjob_clusters
    subjob_clusters = {}

    for phase in workflow.phases + workflow.user_phases:
        # Erstellt ein leeres dict im dict subjob_clusters
        if not phase.cluster in subjob_clusters:
            subjob_clusters[phase.cluster] = {}

        # Create empty list for each subjob_cluster
        if not "subjobs" in subjob_clusters[phase.cluster]:
            subjob_clusters[phase.cluster]["subjobs"] = []

        # Append subjobs to list.
        subjob_clusters[phase.cluster]["subjobs"].append(phase.name)

    # Then, complete the resource information per cluster
    # determine whether a cluster is to be submitted to a batch system
    for subjob_cluster in subjob_clusters:
        nproc_sum = nproc_max = 0
        attributes = ["submit_to_batch_system", "order_in_cluster", "run_on_queue", "run_after", "run_before", "run_only", "skip_run_number", "skip_chunk_number", "batch_or_shell"]
        for attrib in attributes:
            temp_list = []
            for subjob in subjob_clusters[subjob_cluster]["subjobs"]:
                if not get_phase_attrib(workflow.phases + workflow.user_phases, subjob, attrib) in temp_list:
                    subjob_clusters[subjob_cluster][attrib] = get_phase_attrib(workflow.phases + workflow.user_phases, subjob, attrib)
                else:
                    print("Missmatch in attributes")
                    sys.exit(-1)
            nproc_sum += get_phase_attrib(workflow.phases + workflow.user_phases, subjob, "nproc")
            nproc_max = max(get_phase_attrib(workflow.phases + workflow.user_phases, subjob, "nproc"), nproc_max)

#        if subjob_clusters[subjob_cluster].get("submit_to_batch_system", False):
#            subjob_clusters[subjob_cluster]["batch_or_shell"] = "batch"
#        elif subjob_clusters[subjob_cluster].get("script", False):
#            subjob_clusters[subjob_cluster]["batch_or_shell"] = "shell"
#
        if not "run_on_queue" in subjob_clusters[subjob_cluster]:
            print(f"Information on target queue is missing in cluster {subjob_cluster}.")
            sys.exit(-1)
#
# TODO: Check in nproc is calculated correctly
        if subjob_clusters[subjob_cluster]["order_in_cluster"] == "concurrent":
            nproc = nproc_sum
        else:
            nproc = nproc_max
        subjob_clusters[subjob_cluster]["nproc"] = nproc
    return subjob_clusters

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

def init_default_workflow(default_workflow, config):
    """
    Add workflow for precompute, compute, and tidy phases
    etc information already here!

    Arguments:
        default_workflow -- workflow object
        config -- dictionary
    Returns:
        default_workflow
    """

    # TODO: make a method of class Workflow

    # For testing only, set in some yaml config
    workflow_phases = default_workflow.phases

    # Calculating the number of tasks for each component/model
    # needed for phase compute
    tasks = calc_number_of_tasks(config)
    # Create default workflow phase objects:
    default_workflow.phases = []
    for ind, phase in enumerate(workflow_phases):
        default_workflow.phases.append(WorkflowPhase(phase))

    for ind, phase in enumerate(default_workflow.phases):
        if ind < default_workflow.num_phases_in_workflow() - 1:
            phase.run_before = default_workflow.phases[ind+1].name
        else:
            phase.run_after = default_workflow.phases[ind-1].name
        # TODO: this needs to be set somewhere else, or different.
        phase.cluster = phase.name
        if phase.name == "compute":
            phase.nproc = tasks
            phase.submit_to_batch_system = config["general"].get("submit_to_batch_system", True)
            phase.run_on_queue = config["computer"]["partitions"]["compute"]["name"]

    default_workflow.first_task_in_queue = default_workflow.phases[0].name      # prepcompute
    default_workflow.last_task_in_queue = default_workflow.phases[-1].name      # tidy
    # next_run_triggered_by only used to set last_task_in_queue
    # TODO: why not set last_task_in_queue directly?
    default_workflow.next_run_triggered_by = default_workflow.phases[-1].name   # tidy

    return default_workflow

def collect_all_user_workflows(user_workflow,config):
    """
    Collect all workflows set by config files.
    """
    user_workflow_phases = []
    user_workflow_phases_names = []
    for model in config:
        if "workflow" in config[model]:
            w_config = config[model]["workflow"]
            if "subjobs" in w_config:
                # copies component workflow config to new variable ref_config
                ref_config = copy.deepcopy(w_config)
                for subjob in list(copy.deepcopy(w_config["subjobs"])):
                    # create a new phase object for subjob
                    # new_phase_name = subjob + "_" + model
                    # each subjob needs to have an unique name
                    new_phase_name = subjob
                    new_phase = UserWorkflowPhase(new_phase_name)
                    if not new_phase_name in user_workflow_phases_names:
                        user_workflow_phases_names.append(new_phase_name)
                        # set attributes of user_workflow phases
                        for key, value in w_config["subjobs"][subjob].items():
                            new_phase.__setattr__(key, value)
                        user_workflow_phases.append(new_phase)
                    else:
                        esm_parser.user_error("ERROR", "Two subjobs of the same name.")

    user_workflow.user_phases = user_workflow_phases
    return user_workflow







################### Maybe outdated routines ######################

def collect_all_workflow_information(config):
    """
    Collects all workflow information for each component entry in config
    (can be a model/component or a new entry (e.g. 'flows')
    NOTE(NW): Should it be possible to set a workflow in the model section of the runscript? Why not?

    Checks if there are "workflow" entries in the user runscript and copies or merges them into
    config["general"]["workflow"]

    Arguments:
        config -- dictionary
    Returns:
        config
    """
    for model in config:
        if "workflow" in config[model]:
            # looks for "workflow" in each entry of config (can be model/component, general, etc.)
            w_config = config[model]["workflow"]
            # looks for "workflow" in "general" section of config.
            gw_config = config["general"]["workflow"]

            # looks for entry 'subjob_clusters' in config of each component that has a "workflow"
            if "subjob_clusters" in w_config:
                for cluster in w_config["subjob_clusters"]:
                    # if a certain cluster is also in the general config, this cluster will be merged together ...
                    # what cluster could this be?
                    if cluster in gw_config["subjob_clusters"]:
                        gw_config["subjob_clusters"][cluster] = merge_if_possible(
                            w_config["subjob_clusters"][cluster],
                            gw_config["subjob_clusters"][cluster],
                        )
                    # if cluster is not in general config, it will copied into it.
                    else:
                        gw_config["subjob_clusters"][cluster] = copy.deepcopy(
                            w_config["subjob_clusters"][cluster],
                        )

            # looks for entry 'subjobs' in config of each component
            if "subjobs" in w_config:
                # copies component workflow config to new variable ref_config
                ref_config = copy.deepcopy(w_config)
                # ??? for every subjob in ???
                for subjob in list(copy.deepcopy(w_config["subjobs"])):

                    # subjobs (other than clusters) should be model specific
                    # subjobs that are defined in subjobs of components workflow configs and not in a subjob_cluster are copied to general with suffix of componet entry.
                    # appends the model name to the subjob name and copy it to config["general"]
                    gw_config["subjobs"][subjob + "_" + model] = copy.deepcopy(
                        w_config["subjobs"][subjob]
                    )
                    # if this copied subjobs is also n general workflow subjobs it will be deleted there
                    if subjob in gw_config["subjobs"]:
                        del gw_config["subjobs"][subjob]

                    # make sure that the run_after and run_before refer to that cluster
                    # for all subjobs now in general workflow
                    for other_subjob in gw_config["subjobs"]:
                        # sets run_after and run_before to correct subjob???
                        # if a subjob of general workflow has run_after attribute to a user subjob (that has been renamed to subjob_model)
                        # this run_after will be set to the new subjob name (subjob_model)
                        if "run_after" in gw_config["subjobs"][other_subjob]:
                            if (gw_config["subjobs"][other_subjob]["run_after"] == subjob):
                                gw_config["subjobs"][other_subjob]["run_after"] == subjob + "_" + model
                        if "run_before" in gw_config["subjobs"][other_subjob]:
                            if (gw_config["subjobs"][other_subjob]["run_before"] == subjob):
                                gw_config["subjobs"][other_subjob]["run_before"] == subjob + "_" + model

                    # if not in another cluster, each subjob gets its own
                    if (not "subjob_cluster" in gw_config["subjobs"][subjob + "_" + model]):
                        gw_config["subjobs"][subjob + "_" + model]["subjob_cluster"] = subjob  # + "_" + model

            # checks if next_run:triggered_by is tidy or the one in user workflow, or empty?
            if "next_run_triggered_by" in w_config:
                if not gw_config["next_run_triggered_by"] in ["tidy", w_config["next_run_triggered_by"], ]:
                    print(f"Mismatch found setting next_run_triggered_by for workflow.")
                    sys.exit(-1)
                else:
                    gw_config["next_run_triggered_by"] = w_config["next_run_triggered_by"]
                    # what if w_config["next_run_triggered_by"] is empty?

    return config

def merge_single_entry_if_possible(entry, sourceconf, targetconf):
    """
    Merges a dictionary entry into a target dictionary that has he same key.

    Arguments:
        entry -- dictionary key
        sourceconf -- dictionary
        targetconf -- dictionary
    Returns:
        targetconf
    """
    if entry in sourceconf:
        # Check if entry is already in targetconf AND different to sourceconf, then exit
        if entry in targetconf and not sourceconf[entry] == targetconf[entry]:
            print(f"Mismatch found in {entry} for cluster {targetconf}")
            sys.exit(-1)
        # Continues here if entry exists already in targetconf AND the same as sourceconf or
        # not already in targetconf and set it to sourceconf
        targetconf[entry] = sourceconf[entry]
    return targetconf

def merge_if_possible(source, target):
    """
    Does the same as above but for a whole dict

    Merges the entries of source dictionary into target dictionary, if not already in.
    (Will not overwrite entries in target dictionary.)

    Arguments:
        source -- dictionary
        target -- dictionary
    Returns:
        target
    """
    for entry in source:
        if entry in target:
            if not source[entry] == target[entry]:
                print(
                    f"Mismatch while trying to merge subjob_clusters {source} into {target}"
                )
                sys.exit(-1)
        else:
            target[entry] = source[entry]
    return target
