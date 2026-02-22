"""
Contains functions for dealing with PBS-based batch systems
"""

import os
import subprocess


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

    def prepare_launcher(self, config, cluster):
        config["general"]["batch"].het_par_launcher_lines(config, cluster)

    @staticmethod
    def add_pre_launcher_lines(config, cluster, runfile):
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
        thisrun_logfile = config["computer"]["thisrun_logfile"].replace(
            "@jobtype@", cluster
        )
        runfile.write(f"qalter $PBS_JOBID -o {thisrun_logfile}\n")

    # TODO: remove it once it's not needed anymore (substituted by packjob)
    @staticmethod
    def write_het_par_wrappers(config):
        if config["general"].get("verbose", False):
            print(
                f"Skipping the het-par wrapper as it is not needed for {config['computer']['batch_system']}"
            )
        return config

    @staticmethod
    def het_par_headers(config, cluster, all_values):
        """
        Heterogeneous parallelization headings are not needed for PBS.
        """
        return all_values

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
