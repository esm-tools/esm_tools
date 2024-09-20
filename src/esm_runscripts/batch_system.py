import copy
import math
import os
import stat
import sys
import textwrap

from loguru import logger

import esm_environment
from esm_parser import find_variable, user_error, user_note

from . import dataprocess, helpers, prepare
from .pbs import Pbs
from .slurm import Slurm

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

    def add_pre_launcher_lines(self, config, cluster, runfile):
        return self.bs.add_pre_launcher_lines(config, cluster, runfile)

    # TODO: remove it once it's not needed anymore (substituted by packjob)
    def write_het_par_wrappers(self, config):
        return self.bs.write_het_par_wrappers(config)

    def het_par_headers(self, config, cluster, all_values_flat):
        return self.bs.het_par_headers(config, cluster, all_values_flat)

    def prepare_launcher(self, config, cluster):
        self.bs.prepare_launcher(config, cluster)
        return config

    @staticmethod
    def get_run_filename(config, cluster):
        folder = config["general"]["thisrun_scripts_dir"]
        expid = config["general"]["expid"]
        startdate = config["general"]["current_date"]
        enddate = config["general"]["end_date"]
        run_filename = (
            f"{folder}/{expid}_{cluster}" f"_{config['general']['run_datestamp']}.run"
        )
        return run_filename

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
        if config["general"]["jobtype"] in ["prepcompute", "tidy"]:
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
        all_values = [this_batch_system.get(flag) for flag in all_flags]
        all_values_flat = []
        for value in all_values:
            if isinstance(value, str):
                all_values_flat.append(value)
            elif isinstance(value, list):
                all_values_flat.extend(value)

        # Call the ``het_par_headers`` method to calculate the heterogeneous
        # parallelization flags, if necessary
        all_values_flat = config["general"]["batch"].het_par_headers(
            config, cluster, all_values_flat
        )

        # loop over all batch flag values and replace the tags
        for value in all_values_flat:
            for tag, repl in replacement_tags:
                value = value.replace(tag, str(repl))
            if this_batch_system.get("header_start") is not None:
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
                # CPUs per MPI-rank (e.g. aprun -d)
                cpus_per_proc = config[model].get("cpus_per_proc", omp_num_threads)

                if "nproc" in config[model]:
                    logger.info(f"nproc: {config[model]['nproc']}")

                    # kh 21.04.22 multi group support added, i.e. using (nproc * mpi_num_groups) MPI processes to start a program multiple times
                    # (used for FESOM-REcoM tracer loop parallelization (MPI based))
                    mpi_num_groups = config[model].get("mpi_num_groups", 1)

                    # kh 22.06.22 adjust total number of MPI processes via mpi_num_groups at lowest level (nproc)
                    config[model]["nproc"] *= mpi_num_groups
                    config[model]["tasks"] = config[model]["nproc"]

                    # cores_per_node = config['computer']['cores_per_node']

                elif "nproca" in config[model] and "nprocb" in config[model]:
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

                else:
                    continue

                # seb-wahl: add support for ECHAM6's parallel I/O feature
                # namelist parctl in namelist.echam
                if "nprocio" in config[model]:
                    config[model]["tasks"] += config[model].get("nprocio", 0)

                nproc = config[model]["tasks"]
                if cluster == "compute":
                    cores_per_node = config["computer"]["partitions"]["compute"][
                        "cores_per_node"
                    ]
                else:
                    cores_per_node = config["computer"]["partitions"]["pp"][
                        "cores_per_node"
                    ]
                nodes += int(nproc * cpus_per_proc / cores_per_node) + (
                    (nproc * cpus_per_proc) % cores_per_node > 0
                )

                config[model]["threads"] = config[model]["tasks"] * omp_num_threads
                tasks += config[model]["tasks"]
                logger.info(f"tasks: {tasks}")
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
                or cluster not in config["general"]["workflow"]["subjob_clusters"]
            ):
                logger.error(f"Unknown or unset cluster: {cluster}.")
                sys.exit(-1)
            # user defined jobtype doing dataprocessing
            tasks = config["general"]["workflow"]["subjob_clusters"][cluster]["nproc"]
            cores_per_node = config["computer"]["partitions"]["pp"]["cores_per_node"]
            nodes = int(tasks / cores_per_node) + ((tasks % cores_per_node) > 0)

        config["general"]["resubmit_tasks"] = tasks
        logger.info(f"resubmit tasks: {config['general']['resubmit_tasks']}")
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
    def get_run_commands(config, subjob, batch_or_shell):  # here or in compute.py?
        commands = []
        if subjob.startswith("compute"):
            if config["general"].get("submit_to_batch_system", True):
                batch_system = config["computer"]
                if "execution_command" in batch_system:
                    commands.append(
                        "time "
                        + batch_system["execution_command"]
                        + f" 2>&1{config['computer'].get('write_execution_log', '')} &"
                    )
                    if config["general"].get("multi_srun"):
                        return self.bs.get_run_commands_multisrun(config, commands)
            else:
                for model in config:
                    if model == "computer":
                        continue
                    if "execution_command" in config[model]:
                        commands.append(
                            "time ./"
                            + config[model]["execution_command"]
                            + f" 2>&1{config['computer'].get('write_execution_log', '')} &"
                        )
        else:
            subjob_tasks = dataprocess.subjob_tasks(config, subjob, batch_or_shell)
            for task in subjob_tasks:
                commands.append(task)

        return commands

    @staticmethod
    def get_submit_command(config, batch_or_shell, runfilename):
        # in case of slurm e.g. returns:
        # cd SCRIPTDIR; sbatch runfile
        # in case if shell:
        # cd SCRIPTDIR; ./runfile

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
                + os.path.basename(runfilename)
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
            logger.error(f"No config found for cluster {cluster}.")
            sys.exit(-1)

        self = config["general"]["batch"]
        runfilename = batch_system.get_run_filename(config, cluster)

        logger.debug("still alive")
        logger.debug(f"jobtype: {config['general']['jobtype']}")
        logger.debug(f"writing run file for: {cluster}")

        with open(runfilename, "w") as runfile:
            # batch header (if any)
            if batch_or_shell == "batch":
                config = batch_system.calculate_requirements(config, cluster)
                # TODO: remove it once it's not needed anymore (substituted by packjob)
                if cluster in reserved_jobtypes and config["computer"].get(
                    "hetpar_type", "standard"
                ) in ["taskset", "hostfile_srun"]:
                    config = config["general"]["batch"].write_het_par_wrappers(config)
                # Prepare launcher
                config = config["general"]["batch"].prepare_launcher(config, cluster)
                # Initiate the header
                header = batch_system.get_batch_header(config, cluster)

                for line in header:
                    runfile.write(line + "\n")
                runfile.write("\n")

            else:
                header = batch_system.get_shell_header(config, cluster)
                for line in header:
                    runfile.write(line + "\n")
                runfile.write("\n")

            if clusterconf:
                for subjob in clusterconf["subjobs"]:
                    # environment for each subjob of a cluster
                    environment = batch_system.get_environment(config, subjob)
                    batch_system.write_env(config, environment, runfilename)
                    for line in environment:
                        runfile.write(line + "\n")

                    # extra entries for each subjob
                    extra = batch_system.get_extra(config)
                    for line in extra:
                        runfile.write(line + "\n")

                    # Add actual commands
                    commands = batch_system.get_run_commands(
                        config, subjob, batch_or_shell
                    )
                    # commands = clusterconf.get("data_task_list", [])
                    runfile.write("\n")
                    runfile.write(self.append_start_statement(config, subjob) + "\n")
                    runfile.write("\n")
                    runfile.write("cd " + config["general"]["thisrun_work_dir"] + "\n")
                    if cluster in reserved_jobtypes:
                        config["general"]["batch"].add_pre_launcher_lines(
                            config, cluster, runfile
                        )

                    for line in commands:
                        runfile.write(line + "\n")

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
                    + " --last-jobtype "
                    + config["general"]["jobtype"]
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
                    logger.error(
                        "ERROR -- Not sure if you were in a contained or open run!"
                    )
                    logger.error(
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

                runfile.write("\n")
                runfile.write("# Call to esm_runscript to start subjobs:\n")
                runfile.write("# " + str(subjobs_to_launch) + "\n")
                runfile.write("process=$! \n")
                runfile.write(
                    "# Comment the following line if you don't want esm_runscripts to restart:\n"
                )
                runfile.write(
                    "cd " + config["general"]["experiment_scripts_dir"] + "\n"
                )
                runfile.write(observe_call + "\n")
                runfile.write("\n")
                runfile.write(self.append_done_statement(config, subjob) + "\n")

            runfile.write("\n")
            runfile.write("wait\n")

        config["general"]["submit_command"] = batch_system.get_submit_command(
            config, batch_or_shell, runfilename
        )

        if batch_or_shell == "shell":
            runfilestats = os.stat(runfilename)
            os.chmod(runfilename, runfilestats.st_mode | stat.S_IEXEC)

        logger.debug("\n" + 40 * "+ ")
        logger.debug(f"Contents of {runfilename}:")
        with open(runfilename, "r") as fin:
            logger.debug(fin.read())
        if os.path.isfile(self.bs.filename):
            logger.debug("\n" + 40 * "+ ")
            logger.debug(f"Contents of {self.bs.filename}:")
            with open(self.bs.filename, "r") as fin:
                logger.debug(fin.read())

        return config

    @staticmethod
    def write_env(config, environment=[], runfilename=""):
        # Ensures that the config is not modified in this method
        local_config = copy.deepcopy(config)
        # Allows to run it from a recipe (i.e. before a preprocessing job)
        if len(environment) == 0 or len(runfilename) == 0:
            runfilename = batch_system.get_run_filename(local_config, "prepcompute")
            environment = batch_system.get_environment(
                local_config, "prepcompute_general"
            )

        folder = local_config["general"]["thisrun_scripts_dir"]
        this_batch_system = local_config["computer"]
        runfilename_short = runfilename.split("/")[-1]
        envfilename = folder + "/env.sh"

        with open(envfilename, "w") as envfile:
            if "sh_interpreter" in this_batch_system:
                envfile.write("#!" + this_batch_system["sh_interpreter"] + "\n")
            envfile.write(f"# ENVIRONMENT used in {runfilename_short}\n")
            envfile.write("# Use this file to source the environment in your\n")
            envfile.write("# preprocessing or postprocessing scripts\n\n")
            for line in environment:
                envfile.write(line + "\n")

        return config

    @staticmethod
    def find_openmp(config):
        """
        Defines the ``heterogeneous_parallelization`` variable based on the
        ``omp_num_threads`` found in the model's sections. If any
        ``omp_num_threads`` > 1, then ``heterogeneous_parallelization`` becomes
        ``True``. Otherwise, is set to ``False``. The user has no control on setting
        this variable, as the user's choice is overridden here. This is because the
        functionality triggered by ``heterogeneous_parallelization`` is entirely
        dependent on the values of ``omp_num_threads``, so for the user, it doesn't
        make sense to define two variables for the same thing. One could think then
        that there is not need for such a variable, however, there are instances
        in which the yaml files need to know whether the simulation is heterogeneously
        parallelize (i.e. in the machine files to define some environment variables
        under a ``choose_computer.heterogeneous_parallelization``), so this is a way
        of not having to check every ``omp_num_threads`` in the yamls to verify such
        condition.

        Parameters
        ----------
        config : dict
            Dictionary containing the information about the experiment.

        Returns
        -------
        config : dict
            Dictionary containing the information about the experiment.
        """
        if config["computer"].get("heterogeneous_parallelization", False):
            user_error(
                "heterogeneous_parallelization variable",
                (
                    "Since version 6.0, ``heterogeneous_parallelization`` variable "
                    "defined by the user is ignored, and instead its value is "
                    "set to true if any ``omp_num_threads`` in the model's sections "
                    "is larger than 1. To get rid of this error, remove "
                    "``heterogeneous_parallelization`` from your yaml files. "
                    "``heterogeneous_parallelization`` can still be used from a "
                    "``choose_`` block to decice the case."
                ),
            )
        # Set ``heterogeneous_parallelization`` false, overriding whatever the user
        # has defined for this variable to be
        config["computer"]["heterogeneous_parallelization"] = False
        # Loop through the components to find ``omp_num_threads``
        for model in config:
            # Set omp_num_threads 1 if it is not defined in the model
            omp_num_threads = str(config[model].get("omp_num_threads", 1))
            # Resolve the variable if necessary
            if "${" in omp_num_threads:
                # Resolve necessary chooses and the ``omp_num_threads`` variable
                config2 = copy.deepcopy(config)
                config2 = prepare.resolve_some_choose_blocks(config2)
                omp_num_threads = find_variable(
                    [model, "omp_num_threads"],
                    config2[model]["omp_num_threads"],
                    config2,
                    [],
                    True,
                )
            # Set ``heterogeneous_parallelization`` true if needed
            if int(omp_num_threads) > 1 and model in config["general"].get(
                "valid_model_names", []
            ):
                config["computer"]["heterogeneous_parallelization"] = True
                if (
                    not config[model].get("nproc", False)
                    and not config[model].get("nproca", False)
                    and not config[model].get("nprocar", False)
                ):
                    config[model]["nproc"] = 1
        return config

    def het_par_launcher_lines(self, config, cluster):
        """
        Loops through the components to generate job launcher flags and execution
        commands, to be appended in substitution to the ``@components@`` tag, in
        ``computer.execution_command``, that would later be used in the writing of the
        ``.run`` file, in ``batch_system.py``.

        Parameters
        ----------
        config : dict
            Configuration dictionary containing information about the experiment and
            experiment directory.
        cluster : str
            Type of job cluster.
        """
        component_lines = []
        # Read in the separator to be used in between component calls in the job
        # launcher
        sep = config["computer"].get("launcher_comp_sep", "\\\n    ") + " "
        # Loop through the components
        for model in config["general"]["valid_model_names"]:
            command = None
            # Read in execution command
            if "execution_command" in config[model]:
                command = config[model]["execution_command"]
            elif "executable" in config[model]:
                command = config[model]["executable"]
            # Prepare the MPMD commands

            # kh 24.06.22 workaround: filter hdmodel
            if command and (command != "NONE"):
                launcher = config["computer"].get("launcher")
                launcher_flags = self.calc_launcher_flags(config, model, cluster)
                component_lines.append(f"{launcher_flags} ./{command} ")

        # Merge each component flags and commands into a single string
        components = sep.join(component_lines)
        # Substitute the ``@components@`` tag for the final launch command
        config["computer"]["execution_command"] = (
            config["computer"]["execution_command"]
            .replace("@components@", components)
            .replace("@jobtype@", cluster)
        )

    @staticmethod
    def calc_launcher_flags(config, model, cluster):
        """
        Calculates the launcher flags for the job luncher based on the ``nproc`` of the
        different components, with the possibility of using heterogeneous
        parallelization (using both MPI and OpenMP).

        Launcher flags will vary from job launcher to job launcher, and therefore, a
        variable ``launcher_flags`` needs to be defined for each different launcher in
        the ``pbs.yaml`` configuration file. This variable should contain the flags
        themselves followed by their respective values to be calculated in this method
        and substituted in their respective tags of the ``launcher_flags`` variable.
        This method supports the following variables:

            - ``@nproc@``: total number of MPI tasks for the component taken from
              ``<component>.nproc``.
            - ``@nproc_per_node@``: number of MPI tasks per node calculated from
              the number of number of CPUs per MPI task (``<component>.cpus_per_proc``),
              and the number of cores per node in the machine
              (``computer.cores_per_node``).
            - ``@cpus_per_proc@``: number of CPUs per MPI task to be used
              (``<component>.cpus_per_proc``). If not defined, it takes the value of
              ``<component>.omp_num_threads``. This variables are not necessarily the
              same, to allow the use of  more cores per MPI task than OpenMP threads,
              if necessary.
            - ``@omp_num_threads@``: number of OpenMP threads per MPI task
              (``<component>.omp_num_threads``). Its default value is ``1``, if not
              defined. The number of OpenMP threads per task cannot be larger than the
              number of CPUs per task.

        Parameters
        ----------
        config : dict
            Configuration dictionary containing information about the experiment and
            experiment directory.
        model : str
            Component for which the flags are to be calculated.
        cluster : str
            Type of job cluster.

        Returns
        -------
        launcher_flags : str
            Launcher flags string with the calculated numbers for the ``model`` already
            substituted in the tags.
        """
        launcher = config["computer"]["launcher"]
        launcher_flags = config["computer"]["launcher_flags_per_component"]
        # Cores per node
        # cores_per_node = config["computer"]["cores_per_node"]
        if cluster == "compute":
            cores_per_node = config["computer"]["partitions"]["compute"][
                "cores_per_node"
            ]
        else:
            cores_per_node = config["computer"]["partitions"]["pp"]["cores_per_node"]
        # Get the OMP number of threads
        omp_num_threads = config[model].get("omp_num_threads", 1)

        if "nproc" in config[model]:
            # aprun flags commented following the conventions in p. 14 of the ALEPH ppt
            # manual day_1.session_2.advanced_use_of_aprun.ppt

            # Total number of PEs (MPI-ranks) (e.g. aprun -n)
            nproc = config[model]["nproc"]
            # CPUs per MPI-rank (e.g. aprun -d)
            cpus_per_proc = config[model].get("cpus_per_proc", omp_num_threads)
            # Check for CPUs and OpenMP threads
            if omp_num_threads > cpus_per_proc:
                esm_parser.user_error(
                    "OpenMP configuration",
                    (
                        "The number of OpenMP threads cannot be larger than the number"
                        + "of CPUs per MPI task requested. Your values:\n"
                        + f"    {model}.omp_num_threads: {omp_num_threads}\n"
                        + f"    {model}.cpus_per_proc: {cpus_per_proc}\n"
                    ),
                )
        elif "nproca" in config[model] and "nprocb" in config[model]:
            # ``nproca``/``nprocb`` not compatible with ``omp_num_threads``
            if omp_num_threads > 1:
                esm_parser.user_note(
                    "nproc",
                    "``nproca``/``nprocb`` not compatible with ``omp_num_threads``",
                )
            nproc = config[model]["nproca"] * config[model]["nprocb"]
            cpus_per_proc = 1
            omp_num_threads = 1
        else:
            # kh 22.06.22 defensive (user_error/user_note could also be added here)
            nproc = 0
            cpus_per_proc = 0
        #           omp_num_threads = 0

        # Number of nodes needed
        nodes = int(nproc * cpus_per_proc / cores_per_node) + (
            (nproc * cpus_per_proc) % cores_per_node > 0
        )
        config[model]["nodes"] = nodes

        # PEs (MPI-ranks) per compute node (e.g. aprun -N)
        nproc_per_node = math.ceil(nproc / nodes)

        # Replace tags in the laucher flags
        replacement_tags = [
            ("@nnodes@", nodes),
            ("@nproc@", nproc),
            ("@nproc_per_node@", nproc_per_node),
            ("@cpus_per_proc@", int(cpus_per_proc)),
            ("@omp_num_threads@", omp_num_threads),
        ]
        # Replace all tags
        for tag, repl in replacement_tags:
            launcher_flags = launcher_flags.replace(tag, str(repl))
        # Substitute @MODEL@ with the model name
        launcher_flags = launcher_flags.replace("@MODEL@", model.upper())

        return launcher_flags


def submits_another_job(config, cluster):
    clusterconf = config["general"]["workflow"]["subjob_clusters"][cluster]
    if clusterconf.get("next_submit", []) == []:
        return False
    return True
