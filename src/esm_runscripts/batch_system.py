import os
import textwrap
import sys
import stat
import shutil
import copy

import esm_environment
import six

from esm_parser import user_error
from . import helpers
from . import dataprocess
from .slurm import Slurm
from .pbs import Pbs

known_batch_systems = ["slurm", "pbs"]
reserved_jobtypes = ["prepcompute", "compute", "prepare", "tidy", "inspect"]


class UnknownBatchSystemError(Exception):
    """Raise this exception when an unknown batch system is encountered"""


class batch_system:

    # all wrappers to slurm, pbs and co as esm_runscript
    # should be written independent of actual batch system
    def __init__(self, config, name):
        self.name = name
        if name == "slurm":
            self.bs = Slurm(config)
        elif name == "pbs":
            self.bs = Pbs(config)
        else:
            raise UnknownBatchSystemError(name)

    def check_if_submitted(self):
        return self.bs.check_if_submitted()

    def get_jobid(self):
        return self.bs.get_jobid()

    def get_job_state(self, jobid):
        return self.bs.get_job_state(jobid)

    def job_is_still_running(self, jobid):
        return self.bs.job_is_still_running(jobid)

    def add_pre_launcher_lines(self, config, sadfile):
        return self.bs.add_pre_launcher_lines(config, sadfile)

    def write_het_par_wrappers(self, config):
        return self.bs.write_het_par_wrappers(config)

    # methods that actually do something

    def write_hostfile(self, config):
        self.bs.write_hostfile(config)
        hostfile_in_work = (
            config["general"]["work_dir"] + "/" + os.path.basename(self.bs.path)
        )
        shutil.copyfile(self.bs.path, hostfile_in_work)
        return config

    @staticmethod
    def get_sad_filename(config, cluster):
        folder = config["general"]["thisrun_scripts_dir"]
        expid = config["general"]["expid"]
        startdate = config["general"]["current_date"]
        enddate = config["general"]["end_date"]
        sad_filename = (
            f"{folder}/{expid}_{cluster}" f"_{config['general']['run_datestamp']}.sad"
        )
        return sad_filename

    @staticmethod
    def get_shell_header(config, cluster):
        header = []
        coupling_dir = os.path.dirname(os.path.realpath(__file__))

        this_batch_system = config["computer"]
        if "sh_interpreter" in this_batch_system:
            header.append("#!" + this_batch_system["sh_interpreter"])

        header.append(". " + coupling_dir + "/coupling/coupling_general.functions")
        return header

    @staticmethod
    def get_batch_header(config, cluster):
        header = []
        this_batch_system = config["computer"]
        if "sh_interpreter" in this_batch_system:
            header.append("#!" + this_batch_system["sh_interpreter"])

        tasks = config["general"]["resubmit_tasks"]
        nodes = config["general"]["resubmit_nodes"]

        if config["computer"].get("heterogeneous_parallelization", False):
            tasks_nodes_flag = "nodes_flag"
        elif config["computer"]["batch_system"] in ["pbs"]:
            tasks_nodes_flag = "nodes_flag"
        else:
            tasks_nodes_flag = "tasks_flag"

        if cluster == "compute":
            partition = config["computer"]["partitions"]["compute"]["name"]
        else:
            partition = config["computer"]["partitions"]["pp"]["name"]

        replacement_tags = [
            ("@tasks@", tasks),
            ("@nodes@", nodes),
            ("@partition@", partition),
            ("@jobtype@", cluster),
        ]

        all_flags = [
            "partition_flag",
            "time_flag",
            tasks_nodes_flag,
            "output_flags",
            "name_flag",
        ]
        conditional_flags = [
            "accounting_flag",
            "notification_flag",
            "hyperthreading_flag",
            "additional_flags",
            "overcommit_flag",
        ]
        # ??? Do we need the exclusive flag?
        if config["general"]["jobtype"] in ["compute", "tidy"]:
            conditional_flags.append("exclusive_flag")
        for flag in conditional_flags:
            if flag in this_batch_system:
                values_in_flag = []
                flag_value = this_batch_system[flag]

                if isinstance(flag_value, str):
                    values_in_flag.append(flag_value)
                elif isinstance(flag_value, list):
                    values_in_flag.extend(flag_value)

                # checking whether we have any empty values
                any_empty_values = any([item.strip() == "" for item in values_in_flag])
                if not any_empty_values:
                    all_flags.append(flag)

        # some items in `all_values` list might be lists, so flatten it
        all_values = [this_batch_system[flag] for flag in all_flags]
        all_values_flat = []
        for value in all_values:
            if isinstance(value, str):
                all_values_flat.append(value)
            elif isinstance(value, list):
                all_values_flat.extend(value)

        # loop over all batch flag values and replace the tags
        for value in all_values_flat:
            for (tag, repl) in replacement_tags:
                value = value.replace(tag, str(repl))
            header.append(this_batch_system["header_start"] + " " + value)

        return header

    @staticmethod
    def calculate_requirements(config, cluster=None):
        # get number of tasks for the whole job to be submitted,
        # as well as number of start process and end process for each
        # component (in case a hostfile needs to be written)

        tasks = 0
        nodes = 0
        start_proc = 0
        end_proc = 0
        start_core = 0
        end_core = 0

        # if not explicitly stated for which cluster we need the
        # requirements, calculate them for the job we are already in

        if not cluster:
            cluster = config["general"]["jobtype"]

        if cluster in reserved_jobtypes:
            for model in config["general"]["valid_model_names"]:
                omp_num_threads = int(config[model].get("omp_num_threads", 1))

                if "nproca" in config[model] and "nprocb" in config[model]:
                    config[model]["tasks"] = (
                        config[model]["nproca"] * config[model]["nprocb"]
                    )
                    # end_proc = start_proc + int(config[model]["nproca"])*int(config[model]["nprocb"]) - 1
                    # KH 30.04.20: nprocrad is replaced by more flexible
                    # partitioning using nprocar and nprocbr
                    remove_nprocr_options = ["remove_from_namelist", 0]
                    if (
                        config[model].get("nprocar", 0) not in remove_nprocr_options
                        and config[model].get("nprocbr", 0) not in remove_nprocr_options
                    ):
                        config[model]["tasks"] += (
                            config[model]["nprocar"] * config[model]["nprocbr"]
                        )

                elif "nproc" in config[model]:
                    print(f"nproc: {config[model]['nproc']}")
                    config[model]["tasks"] = config[model]["nproc"]

                    # cores_per_node = config['computer']['cores_per_node']
                else:
                    continue

                nproc = config[model]["tasks"]
                if cluster == "compute":
                    cores_per_node = config["computer"]["partitions"]["compute"][
                        "cores_per_node"
                    ]
                else:
                    cores_per_node = config["computer"]["partitions"]["pp"][
                        "cores_per_node"
                    ]
                nodes += int(nproc * omp_num_threads / cores_per_node) + (
                    (nproc * omp_num_threads) % cores_per_node > 0
                )

                config[model]["threads"] = config[model]["tasks"] * omp_num_threads
                tasks += config[model]["tasks"]
                print(f"tasks: {tasks}")
                # Use the number of tasks and threads to update end_proc/core
                end_proc = start_proc + config[model]["tasks"] - 1
                end_core = start_core + config[model]["threads"] - 1
                # Feed the config with the final start_proc/core and end_proc/core
                config[model]["end_proc"] = end_proc
                config[model]["end_core"] = end_core
                config[model]["start_proc"] = start_proc
                config[model]["start_core"] = start_core
                # Reset the start_proc/core variables for the next component
                start_proc = end_proc + 1
                start_core = end_core + 1

        else:
            # dataprocessing job with user definded name
            # number of tasks are actually already prepared in
            # workflow

            if (
                not cluster
                or not cluster in config["general"]["workflow"]["subjob_clusters"]
            ):
                print(f"Unknown or unset cluster: {cluster}.")
                sys.exit(-1)
            # user defined jobtype doing dataprocessing
            tasks = config["general"]["workflow"]["subjob_clusters"][cluster]["nproc"]
            cores_per_node = config["computer"]["partitions"]["pp"]["cores_per_node"]
            nodes = int(tasks / cores_per_node) + ((tasks % cores_per_node) > 0)

        config["general"]["resubmit_tasks"] = tasks
        print(f"resubmit tasks: {config['general']['resubmit_tasks']}")
        config["general"]["resubmit_nodes"] = nodes

        return config

    @staticmethod
    def get_environment(config, subjob):
        environment = []

        env = esm_environment.environment_infos("runtime", config)
        commands = env.commands
        if not subjob.replace("_general", "") in reserved_jobtypes:  # ??? fishy
            commands += dataprocess.subjob_environment(config, subjob)
        commands += [""]

        return commands

    @staticmethod
    def get_extra(config):
        extras = []
        if config["general"].get("unlimited_stack_size", True):
            extras.append("# Set stack size to unlimited")
            extras.append("ulimit -s unlimited")
        if config["general"].get("use_venv", False):
            extras.append("# Start everything in a venv")
            extras.append(
                "source "
                + config["general"]["experiment_dir"]
                + "/.venv_esmtools/bin/activate"
            )
        if config["general"].get("funny_comment", True):
            extras.append("# 3...2...1...Liftoff!")
        # Search for ``pre_run_commands``s in the components
        for component in config.keys():
            pre_run_commands = config[component].get("pre_run_commands")
            if isinstance(pre_run_commands, list):
                for pr_command in pre_run_commands:
                    if isinstance(pr_command, str):
                        extras.append(pr_command)
                    else:
                        user_error(
                            'Invalid type for "pre_run_commands"',
                            (
                                f'"{type(pr_command)}" type is not supported for '
                                + f'elements of the "pre_run_commands", defined in '
                                + f'"{component}". Please, define '
                                + '"pre_run_commands" as a "list" of "strings" or a "list".'
                            ),
                        )
            elif isinstance(pre_run_commands, str):
                extras.append(pre_run_commands)
            elif pre_run_commands == None:
                continue
            else:
                user_error(
                    'Invalid type for "pre_run_commands"',
                    (
                        f'"{type(pre_run_commands)}" type is not supported for '
                        + f'"pre_run_commands" defined in "{component}". Please, define '
                        + '"pre_run_commands" as a "string" or a "list" of "strings".'
                    ),
                )
        return extras

    @staticmethod
    def append_start_statement(config, subjob):
        line = helpers.assemble_log_message(
            config,
            [
                subjob.replace("_general", ""),
                config["general"]["run_number"],
                config["general"]["current_date"],
                config["general"]["jobid"],
                "- start",
            ],
            timestampStr_from_Unix=True,
        )
        startline = "echo " + line + " >> " + config["general"]["experiment_log_file"]
        return startline

    @staticmethod
    def append_done_statement(config, subjob):
        line = helpers.assemble_log_message(
            config,
            [
                subjob.replace("_general", ""),
                config["general"]["run_number"],
                config["general"]["current_date"],
                config["general"]["jobid"],
                "- done",
            ],
            timestampStr_from_Unix=True,
        )
        doneline = "echo " + line + " >> " + config["general"]["experiment_log_file"]
        return doneline

    @staticmethod
    def get_run_commands(config, subjob):  # here or in compute.py?

        commands = []
        if subjob.startswith("compute"):
            if config["general"].get("submit_to_batch_system", True):
                batch_system = config["computer"]
                if "execution_command" in batch_system:
                    commands.append(
                        "time " + batch_system["execution_command"] + " 2>&1 &"
                    )
                    if config["general"].get("multi_srun"):
                        return self.bs.get_run_commands_multisrun(config, commands)
            else:
                for model in config:
                    if model == "computer":
                        continue
                    if "execution_command" in config[model]:
                        commands.append(
                            "time ./" + config[model]["execution_command"] + " 2>&1 &"
                        )
        else:
            subjob_tasks = dataprocess.subjob_tasks(config, subjob)
            for task in subjob_tasks:
                commands.append(task)

        return commands

    @staticmethod
    def get_submit_command(config, batch_or_shell, sadfilename):
        # in case of slurm e.g. returns:
        # cd SCRIPTDIR; sbatch sadfile
        # in case if shell:
        # cd SCRIPTDIR; ./sadfile

        commands = []
        if batch_or_shell == "batch":
            call = config["computer"]["submit"] + " "
        else:
            call = "./"

        batch_system = config["computer"]

        if "submit" in batch_system:
            commands.append(
                "cd "
                + config["general"]["thisrun_scripts_dir"]
                + "; "
                + call
                + os.path.basename(sadfilename)
            )

        return commands

    @staticmethod
    def write_simple_runscript(config, cluster, batch_or_shell="batch"):

        # if no cluster is specified, work on the one we are in
        # if not cluster:
        #    cluster = config["general"]["jobtype"]

        clusterconf = None
        if "workflow" in config["general"]:
            if "subjob_clusters" in config["general"]["workflow"]:
                if cluster in config["general"]["workflow"]["subjob_clusters"]:
                    clusterconf = config["general"]["workflow"]["subjob_clusters"][
                        cluster
                    ]

        if not clusterconf:
            print(f"No config found for cluster {cluster}.")
            sys.exit(-1)

        self = config["general"]["batch"]
        sadfilename = batch_system.get_sad_filename(config, cluster)

        if config["general"]["verbose"]:
            print("still alive")
            print("jobtype: ", config["general"]["jobtype"])
            print("writing sad file for:", cluster)

        with open(sadfilename, "w") as sadfile:

            # batch header (if any)
            if batch_or_shell == "batch":

                config = batch_system.calculate_requirements(config, cluster)
                if cluster in reserved_jobtypes:
                    config = config["general"]["batch"].write_het_par_wrappers(config)
                header = batch_system.get_batch_header(config, cluster)
                config = add_batch_hostfile(config)

                for line in header:
                    sadfile.write(line + "\n")
                sadfile.write("\n")

            else:
                header = batch_system.get_shell_header(config, cluster)
                for line in header:
                    sadfile.write(line + "\n")
                sadfile.write("\n")

            if clusterconf:
                for subjob in clusterconf["subjobs"]:

                    # environment for each subjob of a cluster
                    environment = batch_system.get_environment(config, subjob)
                    batch_system.write_env(config, environment, sadfilename)
                    for line in environment:
                        sadfile.write(line + "\n")

                    # extra entries for each subjob
                    extra = batch_system.get_extra(config)
                    for line in extra:
                        sadfile.write(line + "\n")

                    # Add actual commands
                    commands = batch_system.get_run_commands(config, subjob)
                    # commands = clusterconf.get("data_task_list", [])
                    sadfile.write("\n")
                    sadfile.write(self.append_start_statement(config, subjob) + "\n")
                    sadfile.write("\n")
                    sadfile.write("cd " + config["general"]["thisrun_work_dir"] + "\n")
                    if cluster in reserved_jobtypes:
                        config["general"]["batch"].add_pre_launcher_lines(
                            config, sadfile
                        )

                    for line in commands:
                        sadfile.write(line + "\n")

            # elif multisrun_stuff: # pauls stuff maybe here? or matching to clusterconf possible?
            #    dummy = 0
            else:  # "normal" case
                dummy = 0

            if submits_another_job(config, cluster):  # and batch_or_shell == "batch":
                # -j ? is that used somewhere? I don't think so, replaced by workflow
                #   " -j "+ config["general"]["jobtype"]

                observe_call = (
                    "esm_runscripts "
                    + config["general"]["scriptname"]
                    + " -e "
                    + config["general"]["expid"]
                    + " -t observe_"
                    + cluster
                    + " -p ${process}"
                    + " -s "
                    + config["general"]["current_date"].format(
                        form=9, givenph=False, givenpm=False, givenps=False
                    )
                    + " -r "
                    + str(config["general"]["run_number"])
                    + " -v "
                )

                if "--open-run" in config["general"]["original_command"] or not config[
                    "general"
                ].get("use_venv"):
                    observe_call += " --open-run"
                elif "--contained-run" in config["general"][
                    "original_command"
                ] or config["general"].get("use_venv"):
                    observe_call += " --contained-run"
                else:
                    print("ERROR -- Not sure if you were in a contained or open run!")
                    print(
                        "ERROR -- See write_simple_runscript for the code causing this."
                    )
                    sys.exit(1)

                if "modify_config_file_abspath" in config["general"]:
                    if config["general"]["modify_config_file_abspath"]:
                        observe_call += (
                            " -m " + config["general"]["modify_config_file_abspath"]
                        )

                subjobs_to_launch = config["general"]["workflow"]["subjob_clusters"][
                    cluster
                ]["next_submit"]

                sadfile.write("\n")
                sadfile.write("# Call to esm_runscript to start subjobs:\n")
                sadfile.write("# " + str(subjobs_to_launch) + "\n")
                sadfile.write("process=$! \n")
                sadfile.write(
                    "# Comment the following line if you don't want esm_runscripts to restart:\n"
                )
                sadfile.write(
                    "cd " + config["general"]["experiment_scripts_dir"] + "\n"
                )
                sadfile.write(observe_call + "\n")
                sadfile.write("\n")
                sadfile.write(self.append_done_statement(config, subjob) + "\n")

            sadfile.write("\n")
            sadfile.write("wait\n")

        config["general"]["submit_command"] = batch_system.get_submit_command(
            config, batch_or_shell, sadfilename
        )

        if batch_or_shell == "shell":
            sadfilestats = os.stat(sadfilename)
            os.chmod(sadfilename, sadfilestats.st_mode | stat.S_IEXEC)

        if config["general"]["verbose"]:
            six.print_("\n", 40 * "+ ")
            six.print_("Contents of ", sadfilename, ":")
            with open(sadfilename, "r") as fin:
                print(fin.read())
            if os.path.isfile(self.bs.filename):
                six.print_("\n", 40 * "+ ")
                six.print_("Contents of ", self.bs.filename, ":")
                with open(self.bs.filename, "r") as fin:
                    print(fin.read())

        return config

    @staticmethod
    def write_env(config, environment=[], sadfilename=""):
        # Ensures that the config is not modified in this method
        local_config = copy.deepcopy(config)
        # Allows to run it from a recipe (i.e. before a preprocessing job)
        if len(environment) == 0 or len(sadfilename) == 0:
            sadfilename = batch_system.get_sad_filename(local_config, "prepcompute")
            environment = batch_system.get_environment(
                local_config, "prepcompute_general"
            )

        folder = local_config["general"]["thisrun_scripts_dir"]
        this_batch_system = local_config["computer"]
        sadfilename_short = sadfilename.split("/")[-1]
        envfilename = folder + "/env.sh"

        with open(envfilename, "w") as envfile:
            if "sh_interpreter" in this_batch_system:
                envfile.write("#!" + this_batch_system["sh_interpreter"] + "\n")
            envfile.write(f"# ENVIRONMENT used in {sadfilename_short}\n")
            envfile.write("# Use this file to source the environment in your\n")
            envfile.write("# preprocessing or postprocessing scripts\n\n")
            for line in environment:
                envfile.write(line + "\n")

        return config


def submits_another_job(config, cluster):
    clusterconf = config["general"]["workflow"]["subjob_clusters"][cluster]
    if clusterconf.get("next_submit", []) == []:
        return False
    return True


def add_batch_hostfile(config):
    config["general"]["batch"].write_hostfile(config)

    # config = all_files_to_copy_append(
    #    config,
    #    "general",
    #    "config",
    #    "batchhostfile",
    #    config["general"]["batch"].bs.path,
    #    None,
    #    None,
    # )
    return config


def find_openmp(config):
    for model in config:
        if "omp_num_threads" in config[model]:
            config["general"]["heterogeneous_parallelization"] = True
            config["computer"]["heterogeneous_parallelization"] = True  # dont like this
            if (
                not config[model].get("nproc", False)
                and not config[model].get("nproca", False)
                and not config[model].get("nprocar", False)
            ):
                config[model]["nproc"] = 1
    return config
