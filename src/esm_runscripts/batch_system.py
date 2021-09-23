import os
import textwrap
import sys
import copy

import esm_environment
import six

from esm_parser import user_error
from . import helpers
from .slurm import Slurm
from .pbs import Pbs

known_batch_systems = ["slurm", "pbs"]


class UnknownBatchSystemError(Exception):
    """Raise this exception when an unknown batch system is encountered"""


class batch_system:
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

    def calc_requirements(self, config):
        return self.bs.calc_requirements(config)

    def get_job_state(self, jobid):
        return self.bs.get_job_state(jobid)

    def job_is_still_running(self, jobid):
        return self.bs.job_is_still_running(jobid)

    def add_pre_launcher_lines(self, config, sadfile):
        self.bs.add_pre_launcher_lines(config, sadfile)

    @staticmethod
    def get_sad_filename(config):
        folder = config["general"]["thisrun_scripts_dir"]
        expid = config["general"]["expid"]
        startdate = config["general"]["current_date"]
        enddate = config["general"]["end_date"]
        sad_filename = \
            f"{folder}/{expid}_{config['general']['jobtype']}" \
            f"_{config['general']['run_datestamp']}.sad" 
        return sad_filename

    @staticmethod
    def get_batch_header(config):
        header = []
        this_batch_system = config["computer"]
        if "sh_interpreter" in this_batch_system:
            header.append("#!" + this_batch_system["sh_interpreter"])
        tasks, nodes = batch_system.calculate_requirements(config)

        replacement_tags = [("@tasks@", tasks), ("@nodes@", nodes)]
        if config["computer"].get("heterogeneous_parallelization", False):
            tasks_nodes_flag = "nodes_flag"
        elif config["computer"]["batch_system"] in ["pbs"]:
            tasks_nodes_flag = "nodes_flag"
        else:
            tasks_nodes_flag = "tasks_flag"

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
            "overcommit_flag"
        ]
        if config["general"]["jobtype"] in ["compute", "tidy_and_resume"]:
            conditional_flags.append("exclusive_flag")

        # avoid using empty conditional flags (strings and list of strings are
        # supported)
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
    def calculate_requirements(config):
        tasks = 0
        nodes = 0
        if config["general"]["jobtype"] == "compute":
            for model in config["general"]["valid_model_names"]:
                if "nproc" in config[model]:
                    nproc = config[model]["nproc"]
                    cores_per_node = config['computer']['cores_per_node']
                    tasks += nproc
                    # If heterogeneous MPI-OMP
                    if config["computer"].get("heterogeneous_parallelization", False):
                        omp_num_threads = config[model].get("omp_num_threads", 1)
                    # If only MPI
                    else:
                        omp_num_threads = 1
                    nodes += (
                        int(nproc * omp_num_threads / cores_per_node)
                        + ((nproc * omp_num_threads) % cores_per_node > 0)
                    )
                elif "nproca" in config[model] and "nprocb" in config[model]:
                    tasks += config[model]["nproca"] * config[model]["nprocb"]

                    # KH 30.04.20: nprocrad is replaced by more flexible
                    # partitioning using nprocar and nprocbr
                    if "nprocar" in config[model] and "nprocbr" in config[model]:
                        if (
                            config[model]["nprocar"] != "remove_from_namelist"
                            and config[model]["nprocbr"] != "remove_from_namelist"
                        ):
                            tasks += config[model]["nprocar"] * config[model]["nprocbr"]

        elif config["general"]["jobtype"] == "post":
            tasks = 1
        return tasks, nodes

    @staticmethod
    def get_environment(config):
        environment = []
        env = esm_environment.environment_infos("runtime", config)
        return env.commands

    @staticmethod
    def determine_nodelist(config):
        setup_name = config['general']['setup_name']
        if config['general'].get('multi_srun'):
            for run_type in config['general']['multi_srun']:
                print(run_type)
                total_tasks = 0
                for model in config['general']['multi_srun'][run_type]['models']:
                    print(total_tasks)
                    # determine how many nodes that component needs
                    if "nproc" in config[model]:
                        print("Adding to total_tasks")
                        total_tasks += int(config[model]["nproc"])
                        print(total_tasks)
                    elif "nproca" in config[model] and "nprocb" in config[model]:
                        print("Adding to total_tasks")
                        total_tasks += int(config[model]["nproca"])*int(config[model]["nprocb"])
                        print(total_tasks)

                        # KH 30.04.20: nprocrad is replaced by more flexible
                        # partitioning using nprocar and nprocbr
                        if "nprocar" in config[model] and "nprocbr" in config[model]:
                            if config[model]["nprocar"] != "remove_from_namelist" and config[model]["nprocbr"] != "remove_from_namelist":
                                print("Adding to total_tasks")
                                total_tasks += config[model]["nprocar"] * config[model]["nprocbr"]
                                print(total_tasks)

                    else:
                        continue
                config['general']['multi_srun'][run_type]['total_tasks'] = total_tasks
            print(config['general']['multi_srun'])


    @staticmethod
    def get_extra(config):
        extras = []
        if config["general"].get("unlimited_stack_size", True):
            extras.append("# Set stack size to unlimited")
            extras.append("ulimit -s unlimited")
        if config['general'].get('use_venv', False):
            extras.append("# Start everything in a venv")
            extras.append("source "+config["general"]["experiment_dir"]+"/.venv_esmtools/bin/activate")
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
                        user_error('Invalid type for "pre_run_commands"', (
                            f'"{type(pr_command)}" type is not supported for ' +
                            f'elements of the "pre_run_commands", defined in ' +
                            f'"{component}". Please, define ' +
                            '"pre_run_commands" as a "list" of "strings" or a "list".'
                        )
                        )
            elif isinstance(pre_run_commands, str):
                extras.append(pre_run_commands)
            elif pre_run_commands==None:
                continue
            else:
                user_error('Invalid type for "pre_run_commands"', (
                    f'"{type(pre_run_commands)}" type is not supported for ' +
                    f'"pre_run_commands" defined in "{component}". Please, define ' +
                    '"pre_run_commands" as a "string" or a "list" of "strings".'
                )
                )
        return extras

    @staticmethod
    def get_run_commands(config):  # here or in compute.py?
        commands = []
        batch_system = config["computer"]
        if "execution_command" in batch_system:
            line = helpers.assemble_log_message(
                config,
                [
                    config["general"]["jobtype"],
                    config["general"]["run_number"],
                    config["general"]["current_date"],
                    config["general"]["jobid"],
                    "- start",
                ],
                timestampStr_from_Unix=True,
            )
            commands.append(
                "echo " + line + " >> " + config["general"]["experiment_log_file"]
            )
            if config['general'].get('multi_srun'):
                return get_run_commands_multisrun(config, commands)
            commands.append("time " + batch_system["execution_command"] + " &")
        return commands



    @staticmethod
    def get_submit_command(config, sadfilename):
        # FIXME(PG): Here we need to include a multi-srun thing
        commands = []
        batch_system = config["computer"]
        if "submit" in batch_system:
            commands.append(
                "cd "
                + config["general"]["thisrun_scripts_dir"]
                + "; "
                + batch_system["submit"]
                + " "
                + sadfilename
            )
        return commands

    @staticmethod
    def write_simple_runscript(config):
        self = config["general"]["batch"]
        sadfilename = batch_system.get_sad_filename(config)
        header = batch_system.get_batch_header(config)
        environment = batch_system.get_environment(config)
        # NOTE(PG): This next line allows for multi-srun simulations:
        batch_system.determine_nodelist(config)
        extra = batch_system.get_extra(config)

        if config["general"]["verbose"]:
            print("still alive")
            print("jobtype: ", config["general"]["jobtype"])

        if config["general"]["jobtype"] == "compute":
            commands = batch_system.get_run_commands(config)
            tidy_call = \
                f"esm_runscripts {config['general']['scriptname']}" \
                f" -e {config['general']['expid']}" \
                f" -t tidy_and_resubmit -p ${{process}}" \
                f" -j {config['general']['jobtype']} -v --no-motd "
            
            if "--open-run" in config["general"]["original_command"] or not config["general"].get("use_venv"):
                tidy_call += " --open-run"
            elif "--contained-run" in config['general']['original_command'] or config["general"].get("use_venv"):
                tidy_call += " --contained-run"
            else:
                print("ERROR -- Not sure if you were in a contained or open run!")
                print("ERROR -- See write_simple_runscript for the code causing this.")
                sys.exit(1)

            if "modify_config_file_abspath" in config["general"]:
                if config["general"]["modify_config_file_abspath"]:
                    tidy_call += " -m " + config["general"]["modify_config_file_abspath"]

        elif config["general"]["jobtype"] == "post":
            tidy_call = ""
            commands = config["general"]["post_task_list"]

        with open(sadfilename, "w") as sadfile:
            # Write heading
            for line in header:
                sadfile.write(line + "\n")
            sadfile.write("\n")
            # Write environment
            for line in environment:
                sadfile.write(line + "\n")
            # Write extras
            for line in extra:
                sadfile.write(line + "\n")
            sadfile.write("\n")
            sadfile.write("cd " + config["general"]["thisrun_work_dir"] + "\n")
            # Write pre-launcher lines
            self.add_pre_launcher_lines(config, sadfile)
            # Launch simulation
            for line in commands:
                sadfile.write(line + "\n")
            sadfile.write("process=$! \n")
            # Run tidy and resubmit
            sadfile.write("cd " + config["general"]["experiment_scripts_dir"] + "\n")
            sadfile.write(tidy_call + "\n")


        config["general"]["submit_command"] = batch_system.get_submit_command(
            config, sadfilename
        )

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

        # Write the environment in a file that can be sourced from preprocessing and
        # postprocessing scripts
        batch_system.write_env(config, environment, sadfilename)

        return config

    @staticmethod
    def write_env(config, environment=[], sadfilename=""):
        # Ensures that the config is not modified in this method
        local_config = copy.deepcopy(config)
        # Allows to run it from a recipe (i.e. before a preprocessing job)
        if len(environment)==0 or len(sadfilename)==0:
            sadfilename = batch_system.get_sad_filename(local_config)
            environment = batch_system.get_environment(local_config)

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

    @staticmethod
    def submit(config):
        if not config["general"]["check"]:
            if config["general"]["verbose"]:
                six.print_("\n", 40 * "+ ")
            print("Submitting jobscript to batch system...")
            print()
            print(f"Output written by {config['computer']['batch_system']}:")
            if config["general"]["verbose"]:
                for command in config["general"]["submit_command"]:
                    print(command)
                six.print_("\n", 40 * "+ ")
            for command in config["general"]["submit_command"]:
                os.system(command)
        else:
            print(
                "Actually not submitting anything, this job preparation was launched in 'check' mode (-c)."
            )
            print()
        return config


def get_run_commands_multisrun(config, commands):
    default_exec_command = config['computer']["execution_command"]
    print("---> This is a multi-srun job.")
    print("The default command:")
    print(default_exec_command)
    print("Will be replaced")
    # Since I am already confused, I need to write comments.
    #
    # The next part is actually a shell script fragment, which will be injected
    # into the "sad" file. sad = Sys Admin Dump. It's sad :-(
    #
    # In this part, we figure out what compute nodes we are using so we can
    # specify nodes for each srun command. That means, ECHAM+FESOM will use one
    # pre-defined set of nodes, PISM another, and so on. That should be general
    # enough to also work for other model combos...
    #
    # Not sure if this is specific to Mistral as a HPC, Slurm as a batch
    # system, or whatever else might pop up...
    # @Dirk, please move this where you see it best (I guess slurm.py)
    job_node_extraction = r"""
    # Job nodes extraction
    nodeslurm=$SLURM_JOB_NODELIST
    echo "nodeslurm = ${nodeslurm}"
    # Get rid of the hostname and surrounding brackets:
    tmp=${nodeslurm#"*["}
    nodes=${tmp%]*}
    # Turn it into an array seperated by newlines:
    myarray=(`echo ${nodes} | sed 's/,/\n/g'`)
    #
    idx=0
    for element in "${myarray[@]}"; do
        if [[ "$element" == *"-"* ]]; then
            array=(`echo $element | sed 's/-/\n/g'`)
            for node in $(seq ${array[0]} ${array[1]}); do
               nodelist[$idx]=${node}
               idx=${idx}+1
            done
        else
            nodelist[$idx]=${element}
            idx=${idx}+1
        fi
    done

    for element in "${nodelist[@]}"; do
        echo "${element}"
    done
    """

    def assign_nodes(run_type, need_length=False, start_node=0, num_nodes_first_model=0):
        template = f"""
        # Assign nodes for {run_type}
        {run_type}=""
        %%NEED_LENGTH%%
        for idx in $srbseq {start_node} $srbsrb???-1erberberb; do
            if ssbssb $idx == $srbsrb???-1erberb esbesb; then
                {run_type}="$scb{run_type}ecb$scbnodelist[$idx]ecb"
            else
                {run_type}="$scb{run_type}ecb$scbnodelistssb$idxesbecb,"
            fi
        done
        echo "{run_type} nodes: $scb{run_type}ecb"
        """
        # Since Python f-strings and other braces don't play nicely together,
        # we replace some stuff:
        #
        # For the confused:
        # scb = start curly brace {
        # ecb = end curly brace }
        # ssb = start square brace [
        # esb = end square brace ]
        # srb = start round brace (
        # erb = end round brace )
        template = template.replace("scb", "{")
        template = template.replace("ecb", "}")
        template = template.replace("ssb", "[")
        template = template.replace("esb", "]")
        template = template.replace("srb", "(")
        template = template.replace("erb", ")")
        # Get rid of the starting spaces (they come from Python as the string
        # is defined inside of this function which is indented (facepalm))
        template = textwrap.dedent(template)
        # TODO: Some replacements
        if need_length:
            length_stuff = r"length=${#nodelist[@]}"
            template = template.replace("%%NEED_LENGTH%%", length_stuff)
            template = template.replace("???", "length")
        else:
            template = template.replace("%%NEED_LENGTH%%", "")
            template = template.replace("???", str(num_nodes_first_model))
        return template


    commands.append(textwrap.dedent(job_node_extraction))
    for idx, run_type in enumerate(config['general']['multi_srun']):
        if idx == 0:
            start_node = run_type
            num_nodes_first_model = config['general']['multi_srun'][run_type]['total_tasks'] / config['computer']['cores_per_node']
            num_nodes_first_model = int(num_nodes_first_model)
            nodes = assign_nodes(run_type, need_length=False, num_nodes_first_model=num_nodes_first_model)
        else:
            nodes = assign_nodes(run_type, need_length=True, start_node=start_node)
        commands.append(nodes)
    for run_type in config['general']['multi_srun']:
        new_exec_command = default_exec_command.replace("hostfile_srun", config['general']['multi_srun'][run_type]['hostfile'])
        new_exec_command += f" --nodelist ${run_type}"
        commands.append("time " + new_exec_command + " &")
    return commands
