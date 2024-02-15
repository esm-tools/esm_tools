"""
Documentation goes here
"""
import sys
import os

from . import config_initialization
from . import prepare
from . import prepexp
from . import workflow
from . import resubmit
from . import helpers
from . import logfiles
from . import prev_run

import esm_parser

import pdb

class SimulationSetup(object):

    def __init__(self, command_line_config=None, user_config=None):
        """
        Initializes the ``SimulationSetup`` object, and prepares the ``self.config`` by
        taking the information from the ``command_line_config`` and/or the
        ``user_config`` and expanding it with the configuration files from `ESM-Tools`
        (in `esm_tools/configs`), and then running the ``prepare`` recipe. In essence,
        ``__init__`` takes care of loading and baking all the config information,
        resolving the ``chooses``, ``add_``, etc. It is used by ``esm_runscripts`` and
        ``esm_master``. Below, a more detailed description of the steps of the
        ``__init__``:

        1. Check that at least one input is given
        2. Initialize user_config (command line arguments + content of the runscript)
        3. Initialize information about interactive sessions
        4. Initialize interactive coupling information (offline coupling)
        5. Load total config from all the configuration files involved in this
           simulation. Input: user_config -> returns: self.config
        6. Add the defaults in ``configs/esm_software/esm_runscripts/defaults.yaml``
           to missing key-values in self.config
        7. Check if the ``account`` is missing in ``general``
        8. Complete information for inspect
        9. Store the ``command_line_config`` in ``general``
        10. Initialize the ``prev_run`` object
        11. Run ``prepare`` recipe (resolve the `ESM-Tools` syntax)

        Input
        -----
        command_line_config : dict
            Dictionary containing the information coming from the command line
        user_config : dict, DictWithProvenance
            Dictionary containing the basic user information. Is only an input in
            ``esm_master``, not in ``esm_runscripts`` (i.e. ``esm_master`` does not need
            to read a runscript)

        Raises
        ------
        ValueError :
            If neither ``command_line_config`` nor ``user_config`` are defined
        """
        # 1. Check that at least one input is given
        if not command_line_config and not user_config:
            raise ValueError(
                "SimulationSetup needs to be initialized with either "
                "command_line_config or user_config."
            )

        # 2. Initialize user_config (command line arguments + content of the runscript)
        if not user_config:
            user_config = config_initialization.get_user_config_from_command_line(
                command_line_config
            )

        # 3. Initialize information about interactive sessions
        user_config = config_initialization.init_interactive_info(user_config, command_line_config)

        # 4. Initialize iterative coupling information (offline coupling)
        user_config = config_initialization.init_iterative_coupling(
            command_line_config, user_config
        )

        # 5. Load total config from all the configuration files involved in this
        # simulation
        self.config = config_initialization.get_total_config_from_user_config(
            user_config
        )

        # 6. Add the defaults in ``configs/esm_software/esm_runscripts/defaults.yaml``
        # to missing key-values in self.config
        self.config = config_initialization.add_esm_runscripts_defaults_to_config(
            self.config
        )

        # 7. Check if the ``account`` is missing in ``general``
        self.config = config_initialization.check_account(self.config)

        # 8. Complete information for inspect
        self.config = config_initialization.complete_config_with_inspect(
            self.config
        )

        # 9. Store the ``command_line_config`` in ``general``
        self.config = config_initialization.save_command_line_config(
            self.config, command_line_config
        )

        # 10. Initialize the ``prev_run`` object
        self.config["prev_run"] = prev_run.PrevRunInfo(self.config)

        # 11. Run ``prepare`` recipe (resolve the `ESM-Tools` syntax)
        self.config = prepare.run_job(self.config)

        # esm_parser.pprint_config(self.config)
        # sys.exit(0)

    def __call__(self, kill_after_submit=True):
        # Trigger inspect functionalities
        if self.config["general"]["jobtype"] == "inspect":
            # esm_parser.pprint_config(self.config)
            self.inspect()
            helpers.end_it_all(self.config)

        # Run the prepexp recipe always before every jobtype/cluster
        self.config = prepexp.run_job(self.config)

        # self.pseudocall(kill_after_submit)
        # call to observe here..
        org_jobtype = str(self.config["general"]["jobtype"])
        self.config = logfiles.initialize_logfiles(self.config, org_jobtype)

        # if not check run???
        # set stdout and stderr to lofile
        if self.config["general"]["submitted"]:
            old_stdout = sys.stdout
            old_stderr = sys.stderr
            sys.stdout = logfiles.logfile_handle
            sys.stderr = logfiles.logfile_handle

        if self.config["general"]["jobtype"] == "prepcompute":
            self.prepcompute()
        elif self.config["general"]["jobtype"] == "tidy":
            self.tidy()
        elif self.config["general"]["jobtype"] == "viz":
            self.viz()
        elif self.config["general"]["jobtype"].startswith("observe"):
            pid = self.config["general"]["command_line_config"].get(
                "launcher_pid", -666
            )
            if not pid == -666:
                self.observe()

            self.config["general"]["jobtype"] = self.config["general"][
                "jobtype"
            ].replace("observe_", "")
            # that last line is necessary so that maybe_resubmit knows which
            # cluster to look up in the workflow
            # because all cluster with batch_or_shell=sbatch will be called
            # esm_runscripts ... -t observe_<cluster> ...

        else:
            # write .run file for all workflow phases.

            # Is this dunction call needed here?
            self.assembler()

        #resubmit.maybe_resubmit(self.config)

        # if this line is reached, the run is submitted and running or finished
        self.config = logfiles.finalize_logfiles(self.config, org_jobtype)

        if self.config["general"]["submitted"]:
            sys.stdout = old_stdout
            sys.stderr = old_stderr

        if kill_after_submit:
            if self.config["general"].get("experiment_over", False):
                helpers.write_to_log(self.config, ["# Experiment over"], message_sep="")
            helpers.end_it_all(self.config)

        return self.config["general"].get("experiment_over", False)

    #########################     OBSERVE      #############################################################

    def observe(self):

        from . import observe

        self.config = observe.run_job(self.config)

    def assembler(self):
        from . import assembler

        self.config = assembler.run_job(self.config)

    ###################################     TIDY      #############################################################
    def tidy(self):
        """
        Performs steps for tidying up a simulation after a job has finished and
        submission of following jobs.

        This method uses two lists, ``all_files_to_copy`` and
        ``all_listed_filetypes`` to sort finished data from the **current run
        folder** back to the **main experiment folder** and submit new
        **compute** and **post-process** jobs. Files for ``log``, ``mon``,
        ``outdata``, and ``restart_out`` are gathered. The program waits until
        the job completes or an error is found (See ~self.wait_and_observe).
        Then, if necessary, the coupler cleans up it's files (unless it's a
        standalone run), and the files in the lists are copied from the **work
        folder** to the **current run folder**. A check for unknown files is
        performed (see ~self.check_for_unknown_files), files are
        moved from the  the **current run folder** to the **main experiment
        folder**.
        """
        from . import tidy

        self.config = tidy.run_job(self.config)

    ###################################     INSPECT      #############################################################
    def inspect(self):
        from . import inspect

        print(f"Inspecting {self.config['general']['experiment_dir']}")
        self.config = inspect.run_job(self.config)

    ###################################     PREPCOMPUTE      #############################################################
    def prepcompute(self):
        """
        All steps needed for a model computation.

        Parameters
        ----------
        kill_after_submit : bool
            Default ``True``. If set, the entire Python instance is killed with
            a ``sys.exit()`` as the very last after job submission.
        """
        from . import prepcompute

        self.config = prepcompute.run_job(self.config)

    ###################################     VIZ     #############################################################

    def viz(self):
        """
        Starts the Viz job.

        Parameters
        ----------
        kill_after_submit: bool
            Default ``True``. If set, the entire Python instance is killed with ``sys.exit()``.
        """
        # NOTE(PG): Local import, not everyone will have viz yet...
        import esm_viz as viz

        self.config = viz.run_job(self.config)
