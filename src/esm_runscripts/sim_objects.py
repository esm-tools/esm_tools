"""
Documentation goes here
"""

import sys
import warnings

import deprecation
from loguru import logger

import esm_parser
import esm_tools.esm_warnings  # noqa: F401  installs the deprecation visibility filter
from esm_tools import __version__

from . import (config_initialization, helpers, last_minute, logfiles, plan,
               prepare, prepexp, prev_run, resubmit, workflow)
from .simulation_config import ConfigBuilder, RunState, RunStateBuilder

# SimulationSetup's deprecation schedule. Reuses the same target removal
# version as workflow.py's legacy-key deprecations
# (configs/esm_software/esm_runscripts/workflow_legacy_keys.yaml:
# deprecated_since/removed_in), since both are deprecations introduced in
# this same refactor and share the same next-major removal target. There is
# no equivalent on-disk spec for SimulationSetup itself (it is a class
# rename, not a data-driven key table), so the version strings are given
# directly here rather than loaded from a yaml file.
_SIMULATION_SETUP_DEPRECATED_SINCE = __version__
_SIMULATION_SETUP_REMOVED_IN = "7.0.0"


class Simulation(object):
    @classmethod
    def from_config(cls, config: "SimulationConfig") -> "Simulation":
        """
        Build a ``Simulation`` from an already-built ``SimulationConfig``.

        Takes the baked configuration produced by ``ConfigBuilder``, wires up
        the ``prev_run`` object, then dispatches on the configured workflow
        engine:

        - ``pplan`` (native pydantic-plan engine): builds the per-invocation
          ``RunState`` via ``RunStateBuilder`` (all steps, in the order Task 2
          fixed), snapshots the resolved ``Workflow``, and runs the pplan tail
          steps. Legacy ``prepare.run_job`` never runs in this case.
        - anything else (default ``legacy``): runs ``prepare.run_job`` wholesale,
          exactly as before.

        Parameters
        ----------
        config : SimulationConfig
            The baked simulation configuration (product of ``ConfigBuilder``).

        Returns
        -------
        Simulation
            A new instance with ``.config`` always set, and ``.run_state`` /
            ``.plan`` set only for the ``pplan`` engine.
        """
        self = cls()
        self.config = config.data
        self.config["prev_run"] = prev_run.PrevRunInfo(self.config)
        self.store_prev_objects()

        # ``wf_engine`` is set by the CLI into ``command_line_config`` (cli.py),
        # which ``ConfigBuilder.remember_command_line`` nests at
        # ``config["general"]["command_line_config"]["wf_engine"]`` -- it is NOT
        # promoted to ``config["general"]["wf_engine"]``. A runscript/user_config
        # may, however, set ``general.wf_engine`` directly. Check the direct
        # location first, then the nested command-line location, then default.
        general = self.config.get("general", {})
        wf_engine = (
            general.get("wf_engine")
            or general.get("command_line_config", {}).get("wf_engine")
            or "legacy"
        )
        if wf_engine == "pplan":
            self.run_state = (
                RunStateBuilder.from_config(config)
                .with_date_file()
                .with_run_in_chunk_updated()
                .with_lresume_checked()
                .with_last_minute_changes()
                .with_conda_env_recorded()
                .with_openmp_found()
                .with_choose_blocks_resolved()
                .with_calendar()
                .with_choose_blocks_resolved()
                .with_folders()
                .with_prev_date()
                .with_parent_info()
                .with_finalized_config()
                .build()
            )
            # The pplan engine's per-invocation facts (dates, calendar,
            # folders, modify_config_memo, ...) live only on
            # ``self.run_state.data`` -- ``RunStateBuilder.from_config``
            # deepcopies before computing them, and by design never syncs
            # them back into ``self.config``. The downstream consumers must
            # therefore read ``self.run_state.data``, not ``self.config``:
            # e.g. ``restore_protected_last_minute_changes`` (first tail step)
            # reads ``config["general"]["modify_config_memo"]``, a key only
            # ``with_last_minute_changes()`` sets, and only on the run_state.
            self.plan = plan.Workflow.resolve(self.run_state.data)
            self.run_state = RunState(self._run_pplan_tail_steps(self.run_state.data))
        else:
            self.config = prepare.run_job(self.config)

        self.config["general"]["esm_tools_version"] = __version__
        return self

    def _run_pplan_tail_steps(self, config):
        """
        Run the pplan engine's equivalent of the legacy recipe's steps after
        ``assemble_workflow`` (``configs/esm_software/esm_runscripts/esm_runscripts.yaml``):
        ``restore_protected_last_minute_changes``, ``add_submission_info``,
        ``initialize_batch_system``, ``initialize_coupler``, ``set_logfile``,
        ``check_config_for_warnings_errors``.

        ``restore_protected_last_minute_changes`` lives in ``last_minute.py``,
        not ``prepare.py``. It is included here because Task 2's
        ``RunStateBuilder.with_last_minute_changes()`` only calls
        ``last_minute.apply_last_minute_changes`` -- it never restores
        ``config["general"]["modify_config"]`` from the memo, nor deletes the
        ``modify_config_memo`` object that ``apply_last_minute_changes``
        attaches to ``config["general"]``. Without this call, that memo object
        would leak into the pplan path's final config.

        Returns
        -------
        dict
            The configuration after all tail steps have run.
        """
        config = last_minute.restore_protected_last_minute_changes(config)
        config = prepare.add_submission_info(config)
        config = prepare.initialize_batch_system(config)
        config = prepare.initialize_coupler(config)
        config = prepare.set_logfile(config)
        config = prepare.check_config_for_warnings_errors(config)
        return config

    def run(self, kill_after_submit=True):
        # Trigger inspect functionalities
        if self.config["general"]["jobtype"] == "inspect":
            self.inspect()
            helpers.end_it_all(self.config)

        # Run the preexp recipe
        self.config = prepexp.run_job(self.config)

        org_jobtype = str(self.config["general"]["jobtype"])
        logfiles.set_logfile_name(self.config)
        logger.stdout_sink.def_path(self.config["general"]["logfile_path"])

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

        else:
            self.assembler()

        resubmit.maybe_resubmit(self.config)

        logfiles.finalize_experiment_logfile(self.config, org_jobtype)

        if kill_after_submit:
            if self.config["general"].get("experiment_over", False):
                logger.progress("Experiment over")
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

        logger.info(f"Inspecting {self.config['general']['experiment_dir']}")
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

    #########################     HELPERS      #############################################################

    def store_prev_objects(self):
        self.config.prev_objects = ["prev_run"]
        self.config.prev_objects.extend(
            self.config["general"].get("prev_chunk_objs", [])
        )
        if "prev_chunk_objs" in self.config["general"]:
            del self.config["general"]["prev_chunk_objs"]

        self.config["general"]["system_components"].extend(self.config.prev_objects)


class SimulationSetup(Simulation):
    """Deprecated. Use :meth:`Simulation.from_config` instead.

    Kept as a thin, behavior-preserving shim for the callers this refactor
    does not touch (``esm_master.py``, ``resubmit.py``, and the
    ``fake_experiment`` test fixture's ``resolver.py``): it builds a
    ``SimulationConfig`` via ``ConfigBuilder`` using the exact same chain
    ``Simulation.from_config`` itself would receive, then delegates to
    ``Simulation.from_config`` to do the actual work. ``.config`` and
    callability (``__call__`` forwarding to ``Simulation.run``) are
    preserved exactly as before.
    """

    def __init__(self, command_line_config=None, user_config=None):
        """
        Builds ``self.config`` from ``command_line_config`` and/or
        ``user_config`` via :class:`~.simulation_config.ConfigBuilder`, then
        constructs the underlying :class:`Simulation` via
        ``Simulation.from_config`` and adopts its state.

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
        warnings.warn(
            deprecation.DeprecatedWarning(
                "SimulationSetup",
                _SIMULATION_SETUP_DEPRECATED_SINCE,
                _SIMULATION_SETUP_REMOVED_IN,
                "Use Simulation.from_config(...) instead.",
            ),
            stacklevel=2,
        )

        if not command_line_config and not user_config:
            raise ValueError(
                "SimulationSetup needs to be initialized with either "
                "command_line_config or user_config."
            )

        if user_config:
            builder = ConfigBuilder.from_user_config(user_config)
        else:
            builder = ConfigBuilder.from_runscript(
                command_line_config["runscript_abspath"], **command_line_config
            )

        config = (
            builder.with_interactive_info()
            .with_iterative_coupling()
            .with_execution_mode_default()
            .expand_from_esm_tools_configs()
            .with_runscript_defaults()
            .validate_account()
            .with_inspect_info()
            .remember_command_line()
            .build()
        )

        sim = Simulation.from_config(config)
        self.__dict__.update(sim.__dict__)

    def __call__(self, kill_after_submit=True):
        return self.run(kill_after_submit=kill_after_submit)
