"""
Contains functions for dealing with PBS-based batch systems
"""
import os
import subprocess
import sys
import esm_parser


class Pbs:
    """
    Deals with PBS, allowing you to check if a job is submitted, get the current job
    ID, generate the job launcher commands (e.g. ``aprun``), get the current job
    state, and check if a job is still running. Note: there is not an equivalent file
    for the slurm hostfile, because all the job information is contained in the
    launcher command.


    Parameters
    ----------
    config : dict
        Configuration dictionary containing information about the experiment and
        experiment directory.
    """

    def __init__(self, config):
        # No hostfile for PBS
        self.filename = ""
        self.path = ""

    @staticmethod
    def check_if_submitted():
        """
        Determines if a job is submitted in the currently running shell by checking for
        ``PBS_JOBID`` variable in the environment.

        Returns
        -------
        bool
            Boolean standing for submission state.
        """
        return "PBS_JOBID" in os.environ

    @staticmethod
    def get_jobid():
        """
        Gets the current ``PBS_JOBID``.

        Returns
        -------
        str or None
            If ``PBS_JOBID`` is defined returns the jobid as a string.
        """
        return os.environ.get("PBS_JOBID")

    @staticmethod
    def calc_launcher_flags(config, model, cluster):
        """
        Calculates the launcher flags for the job luncher based on the ``nproc`` of the
        different components, with the possibility of using heterogeneous
        parallelization (using both MPI and OpenMPI).

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

        Returns
        -------
        launcher_flags : str
            Launcher flags string with the calculated numbers for the ``model`` already
            substituted in the tags.
        """
        launcher = config["computer"]["launcher"]
        launcher_flags = config["computer"]["launcher_flags"]
        if "nproc" in config[model]:
            # aprun flags commented following the conventions in p. 14 of the ALEPH ppt
            # manual day_1.session_2.advanced_use_of_aprun.ppt

            # Total number of PEs (MPI-ranks) (e.g. aprun -n)
            nproc = config[model]["nproc"]
            # Cores per node
            # cores_per_node = config["computer"]["cores_per_node"]
            if cluster == "compute":
                cores_per_node = config["computer"]["partitions"]["compute"][
                    "cores_per_node"
                ]
            else:
                cores_per_node = config["computer"]["partitions"]["pp"][
                    "cores_per_node"
                ]
            # Get the OMP number of threads
            omp_num_threads = config[model].get("omp_num_threads", 1)
            # CPUs per MPI-rank (e.g. aprun -d)n
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
            # Number of nodes needed
            nodes = int(nproc * cpus_per_proc / cores_per_node) + (
                (nproc * cpus_per_proc) % cores_per_node > 0
            )
            # PEs (MPI-ranks) per compute node (e.g. aprun -N)
            nproc_per_node = int(nproc / nodes)
        elif "nproca" in config[model] and "procb" in config[model]:
            esm_parser.user_error(
                "nproc", "nproca and nprocb not supported yet for pbs"
            )

        # Replace tags in the laucher flags
        replacement_tags = [
            ("@nproc@", nproc),
            ("@nproc_per_node@", nproc_per_node),
            ("@cpus_per_proc@", cpus_per_proc),
            ("@omp_num_threads@", omp_num_threads),
        ]
        # Replace all tags
        for (tag, repl) in replacement_tags:
            launcher_flags = launcher_flags.replace(tag, str(repl))
        # Substitute @MODEL@ with the model name
        launcher_flags = launcher_flags.replace("@MODEL@", model.upper())

        return launcher_flags

    def prepare_launcher(self, config, cluster):
        """
        Loops through the components to generate job launcher flags and execution
        commands, to be appended in substitution to the ``@components@`` tag, in
        ``computer.execution_command``, that would later be used in the writing of the
        ``.run`` file, in ``batch_system.py``.

        .. Note: PBS does not support yet multi_apruns

        Parameters
        ----------
        config : dict
            Configuration dictionary containing information about the experiment and
            experiment directory.
        cluster : str
        """
        # PBS does not support yet multi_apruns
        # if config['general'].get('multi_apruns'):
        #    self.calc_requirements_multi_aprun(config)
        #    return

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
            if command:
                launcher = config["computer"].get("launcher")
                launcher_flags = self.calc_launcher_flags(config, model, cluster)
                component_lines.append(f"{launcher_flags} ./{command} ")

        # Merge each component flags and commands into a single string
        components = sep.join(component_lines)
        # Substitute the ``@components@`` tag for the final launch command
        config["computer"]["execution_command"] = config["computer"][
            "execution_command"
        ].replace("@components@", components)

    @staticmethod
    def add_pre_launcher_lines(config, runfile):
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
        # This changes the name of the output stream to include the $PBS_JOBID. This
        # cannot be done in the header because PBS does not support its own variables
        # to be used there (at least in the ALEPH's version).
        #runfile.write(f'qalter $PBS_JOBID -o {config["computer"]["thisrun_logfile"]}\n')
        pass

    @staticmethod
    def write_het_par_wrappers(config):
        if config["general"].get("verbose", False):
            print(
                f"Skipping the het-par wrapper as it is not needed for {config['computer']['batch_system']}"
            )
        return config

    @staticmethod
    def get_job_state(jobid):
        """
        Returns the jobstate. See ``man qstat`` for more details.

        Parameters
        ----------
        jobid : str
            The PBS job id as displayed in, e.g. ``qstat``.

        Returns
        -------
        str or None
            The short job state.
        """
        state_command = f"qstat {str(jobid)}"

        qstat_output = subprocess.Popen(
            state_command.split(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        ).communicate()[0]
        qstat_split = str(qstat_output).split()
        if len(qstat_split) > 2:
            return qstat_split[-3]

    @staticmethod
    def job_is_still_running(jobid):
        """
        Returns ``True`` if the job is still running or queueing.

        Parameters
        ----------
        jobid : str
            The PBS job id as displayed in, e.g. ``qstat``.

        Returns
        -------
        bool
            Boolean standing for a job running or queueing.
        """
        return bool(Pbs.get_job_state(jobid))
