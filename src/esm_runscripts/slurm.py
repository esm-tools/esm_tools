"""
Contains functions for dealing with SLURM-based batch systems
"""
import os
import shutil
import subprocess
import sys


class Slurm:
    """
    Deals with SLURM, allowing you to check if a job is submitted, get the
    current job ID, generate a srun hostfile, get the current job state, and
    check if a job is still running.


    Attributes
    ----------
    filename : str
        The filename for srun commands, defaults to ``hostfile_srun``
    path : str
        Full path to this file, defaults to ``thisrun_scripts_dir / filename``

    Parameters
    ----------
    config : dict
        The run configuration, needed to determine where the script directory
        for this particular run is.
    """

    def __init__(self, config):
        folder = config["general"]["thisrun_scripts_dir"]
        self.filename = "hostfile_srun"
        self.path = folder + "/" + self.filename

    @staticmethod
    def check_if_submitted():
        """
        Determines if a job is submitted in the currently running shell by
        checking for ``SLURM_JOB_ID`` in the environment

        Returns
        -------
        bool
        """
        return "SLURM_JOB_ID" in os.environ

    @staticmethod
    def get_jobid():
        """
        Gets the current SLURM JOB ID

        Returns
        -------
        str or None
        """
        return os.environ.get("SLURM_JOB_ID")

    def prepare_launcher(self, config, cluster):
        if "multi_srun" in config["general"]:
            for run_type in list(config["general"]["multi_srun"]):
                current_hostfile = self.path + "_" + run_type
                write_one_hostfile(current_hostfile, config)
        else:
            self.write_one_hostfile(self.path, config)

        hostfile_in_work = (
            config["general"]["work_dir"] + "/" + os.path.basename(self.path)
        )
        shutil.copyfile(self.path, hostfile_in_work)

        return config

    def write_one_hostfile(self, hostfile, config):
        """
        Gathers previously prepared requirements
        (batch_system.calculate_requirements) and writes them to ``self.path``.
        """

        with open(hostfile, "w") as hostfile:
            for model in config["general"]["valid_model_names"]:
                end_proc = config[model].get("end_proc", None)
                start_proc = config[model].get("start_proc", None)

                if start_proc == None or end_proc == None:
                    continue

                if config["computer"].get("heterogeneous_parallelization", False):
                    command = "./" + config[model].get(
                        "execution_command_het_par", None
                    )
                elif "execution_command" in config[model]:
                    command = "./" + config[model]["execution_command"]
                elif "executable" in config[model]:
                    command = "./" + config[model]["executable"]
                else:
                    continue
                hostfile.write(
                    str(start_proc) + "-" + str(end_proc) + "  " + command + "\n"
                )

    @staticmethod
    def get_job_state(jobid):
        """
        Returns the jobstate full name. See ``man squeue``, section ``JOB STATE CODES`` for more details.

        Parameters
        ----------
        jobid :
            ``str`` or ``int``. The SLURM job id as displayed in, e.g. ``squeue``

        Returns
        -------
        str :
            The short job state.
        """
        state_command = ["squeue -j" + str(jobid) + ' -o "%T"']

        squeue_output = subprocess.Popen(
            state_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        ).communicate()[0]
        if len(squeue_output) == 2:
            return squeue_output[0]

    @staticmethod
    def job_is_still_running(jobid):
        """Returns a boolean if the job is still running"""
        return bool(Slurm.get_job_state(jobid))

    ############# HETEROGENOUS PARALLELIZATION STUFF (MPI + OMP) #################

    def add_pre_launcher_lines(self, config, runfile):
        """
        Adds pre-launcher lines to the ``runfile``.

        Parameters
        ----------
        config : dict
            Configuration dictionary containing information about the experiment and
            experiment directory.
        runfile : io.TextIOWrapper
            File wrapper object for writing of the lines
            (``runfile.write("<your_line_here>")``).
        """
        if config["computer"].get("heterogeneous_parallelization", False):
            self.add_hostlist_file_gen_lines(config, runfile)

    @staticmethod
    def write_het_par_wrappers(config):
        cores_per_node = config["computer"]["partitions"]["compute"]["cores_per_node"]

        scriptfolder = config["general"]["thisrun_scripts_dir"] + "../work/"
        if config["computer"].get("heterogeneous_parallelization", False):
            for model in config["general"]["valid_model_names"]:
                if "oasis3mct" == model:
                    continue
                command = "./" + config[model].get(
                    "execution_command", config[model]["executable"]
                )
                scriptname = "script_" + model + ".ksh"
                with open(scriptfolder + scriptname, "w") as f:
                    f.write("#!/bin/ksh" + "\n")
                    f.write(
                        "export OMP_NUM_THREADS="
                        + str(config[model].get("omp_num_threads", 1))
                        + "\n"
                    )
                    f.write(command + "\n")
                os.chmod(scriptfolder + scriptname, 0o755)

                progname = "prog_" + model + ".sh"

                start_core = config[model]["start_core"]
                end_core = config[model]["end_core"]

                with open(scriptfolder + progname, "w") as f:
                    f.write("#!/bin/sh" + "\n")
                    f.write("(( init = " + str(start_core) + " + $1 ))" + "\n")
                    f.write(
                        "(( index = init * "
                        + str(config[model].get("omp_num_threads", 1))
                        + " ))"
                        + "\n"
                    )
                    f.write("(( slot = index % " + str(cores_per_node) + " ))" + "\n")
                    f.write(
                        "echo "
                        + model
                        + " taskset -c $slot-$((slot + "
                        + str(config[model].get("omp_num_threads", 1))
                        + " - 1"
                        + "))"
                        + "\n"
                    )
                    f.write(
                        "taskset -c $slot-$((slot + "
                        + str(config[model].get("omp_num_threads", 1))
                        + " - 1)) ./script_"
                        + model
                        + ".ksh"
                        + "\n"
                    )
                os.chmod(scriptfolder + progname, 0o755)
                execution_command_het_par = f"prog_{model}.sh %o %t"
                config[model]["execution_command_het_par"] = execution_command_het_par
        return config

    @staticmethod
    def add_hostlist_file_gen_lines(config, runfile):
        cores_per_node = config["computer"]["partitions"]["compute"]["cores_per_node"]
        runfile.write(
            "\n"
            + "#Creating hostlist for MPI + MPI&OMP heterogeneous parallel job"
            + "\n"
        )
        runfile.write("rm -f ./hostlist" + "\n")
        runfile.write(
            f"export SLURM_HOSTFILE={config['general']['thisrun_work_dir']}/hostlist\n"
        )
        runfile.write("IFS=$'\\n'; set -f" + "\n")
        runfile.write(
            "listnodes=($(< <( scontrol show hostnames $SLURM_JOB_NODELIST )))" + "\n"
        )
        runfile.write("unset IFS; set +f" + "\n")
        runfile.write("rank=0" + "\n")
        runfile.write("current_core=0" + "\n")
        runfile.write("current_core_mpi=0" + "\n")
        for model in config["general"]["valid_model_names"]:
            if model != "oasis3mct":
                runfile.write(
                    "mpi_tasks_" + model + "=" + str(config[model]["nproc"]) + "\n"
                )
                runfile.write(
                    "omp_threads_"
                    + model
                    + "="
                    + str(config[model].get("omp_num_threads", 1))
                    + "\n"
                )
        import pdb

        # pdb.set_trace()
        runfile.write(
            "for model in "
            + str(config["general"]["valid_model_names"])[1:-1]
            .replace(",", "")
            .replace("'", "")
            + " ;do"
            + "\n"
        )
        runfile.write("    eval nb_of_cores=\${mpi_tasks_${model}}" + "\n")
        runfile.write("    eval nb_of_cores=$((${nb_of_cores}-1))" + "\n")
        runfile.write("    for nb_proc_mpi in `seq 0 ${nb_of_cores}`; do" + "\n")
        runfile.write(
            "        (( index_host = current_core / "
            + str(cores_per_node)
            + " ))"
            + "\n"
        )
        runfile.write("        host_value=${listnodes[${index_host}]}" + "\n")
        runfile.write(
            "        (( slot =  current_core % " + str(cores_per_node) + " ))" + "\n"
        )
        runfile.write("        echo $host_value >> hostlist" + "\n")
        runfile.write(
            "        (( current_core = current_core + omp_threads_${model} ))" + "\n"
        )
        runfile.write("    done" + "\n")
        runfile.write("done" + "\n\n")

    ############# MULTI SRUN STUFF ##############

    @staticmethod
    def determine_nodelist(config):
        setup_name = config["general"]["setup_name"]
        if config["general"].get("multi_srun"):
            for run_type in config["general"]["multi_srun"]:
                print(run_type)
                total_tasks = 0
                for model in config["general"]["multi_srun"][run_type]["models"]:
                    print(total_tasks)
                    # determine how many nodes that component needs
                    if "nproc" in config[model]:
                        print("Adding to total_tasks")
                        total_tasks += int(config[model]["nproc"])
                        print(total_tasks)
                    elif "nproca" in config[model] and "nprocb" in config[model]:
                        print("Adding to total_tasks")
                        total_tasks += int(config[model]["nproca"]) * int(
                            config[model]["nprocb"]
                        )
                        print(total_tasks)

                        # KH 30.04.20: nprocrad is replaced by more flexible
                        # partitioning using nprocar and nprocbr
                        if "nprocar" in config[model] and "nprocbr" in config[model]:
                            if (
                                config[model]["nprocar"] != "remove_from_namelist"
                                and config[model]["nprocbr"] != "remove_from_namelist"
                            ):
                                print("Adding to total_tasks")
                                total_tasks += (
                                    config[model]["nprocar"] * config[model]["nprocbr"]
                                )
                                print(total_tasks)

                    else:
                        continue
                config["general"]["multi_srun"][run_type]["total_tasks"] = total_tasks
            print(config["general"]["multi_srun"])
        return config


def get_run_commands_multisrun(config, commands):
    default_exec_command = config["computer"]["execution_command"]
    print("---> This is a multi-srun job.")
    print("The default command:")
    print(default_exec_command)
    print("Will be replaced")
    # Since I am already confused, I need to write comments.
    #
    # The next part is actually a shell script fragment, which will be injected
    # into the "run" file, the shell script to be run by the job scheduler.
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

    def assign_nodes(
        run_type, need_length=False, start_node=0, num_nodes_first_model=0
    ):
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
    cores_per_node = config["computer"]["partitions"]["compute"]["cores_per_node"]
    for idx, run_type in enumerate(config["general"]["multi_srun"]):
        if idx == 0:
            start_node = run_type
            num_nodes_first_model = (
                config["general"]["multi_srun"][run_type]["total_tasks"]
                / cores_per_node
            )
            num_nodes_first_model = int(num_nodes_first_model)
            nodes = assign_nodes(
                run_type, need_length=False, num_nodes_first_model=num_nodes_first_model
            )
        else:
            nodes = assign_nodes(run_type, need_length=True, start_node=start_node)
        commands.append(nodes)
    for run_type in config["general"]["multi_srun"]:
        new_exec_command = default_exec_command.replace(
            "hostfile_srun", config["general"]["multi_srun"][run_type]["hostfile"]
        )
        new_exec_command += f" --nodelist ${run_type}"
        commands.append("time " + new_exec_command + " &")
    return commands
