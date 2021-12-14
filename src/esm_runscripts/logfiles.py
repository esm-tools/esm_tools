import os, sys, _io
from . import helpers


def initialize_logfiles(config, org_jobtype):
    global logfile_handle
    global logfile_run_number

    logfile_run_number = str(config["general"]["run_number"])

    log_stuff = False
    if os.path.isdir(os.path.dirname(config["general"]["experiment_log_file"])):
        if not org_jobtype == "inspect":
            log_stuff = True

    config = set_logfile_name(config, "")

    if log_stuff:

        helpers.write_to_log(
            config,
            [
                org_jobtype,
                logfile_run_number,
                str(config["general"]["current_date"]),
                str(config["general"]["jobid"]),
                "- start",
            ],
        )
        logfile = RuntimeLogger(
            config["general"]["logfile_path"],
            "w",
            buffering=1,
        )
    else:
        logfile = sys.stdout

    logfile_handle = logfile
    return config


def finalize_logfiles(config, org_jobtype):

    if os.path.isdir(os.path.dirname(config["general"]["experiment_log_file"])):
        log_stuff = True

    if log_stuff:
        helpers.write_to_log(
            config,
            [
                org_jobtype,
                logfile_run_number,
                str(config["general"]["current_date"]),
                str(config["general"]["jobid"]),
                "- done",
            ],
        )

    if not logfile_handle == sys.stdout:
        logfile_handle.close()
    return config


def set_logfile_name(config, jobtype=None):

    if not jobtype:
        jobtype = config["general"]["jobtype"]

    filejobtype = jobtype
    # if "observe" in filejobtype:
    #    filejobtype = filejobtype.replace("observe_", "")

    # if "newrun" in filejobtype:
    #    filejobtype = config["general"]["workflow"]["subjob_clusters"][jobtype].get("next_submit")[0]

    # if filejobtype == "prepcompute":
    #    filejobtype = "compute"

    # if "_" + config["general"]["setup_name"] in filejobtype:
    #    filejobtype = filejobtype.replace("_" + config["general"]["setup_name"], "")

    # called_from = config["general"]["workflow"]["subjob_clusters"][jobtype].get("called_from", "SOMETHINgUSELESS")

    # if "_" + called_from in filejobtype:
    #    filejobtype = filejobtype.replace("_" + called_from, "")

    filename = (
        config["general"]["expid"]
        + "_"
        + config["general"]["setup_name"]
        + "_"
        + filejobtype
        + "_"
        + config["general"]["run_datestamp"]
        + ".log"
    )

    config["general"]["logfile_path"] = (
        config["general"]["experiment_log_dir"] + "/" + filename
    )

    config["general"]["logfile_path_in_run"] = (
        config["general"]["thisrun_log_dir"] + "/" + filename
    )

    # if os.path.isfile(config["general"]["logfile_path"]):
    #    if not os.path.isfile(config["general"]["logfile_path_in_run"]):
    #        os.symlink(
    #            config["general"]["logfile_path"],
    #            config["general"]["logfile_path_in_run"]
    #            )

    return config


class RuntimeLogger(_io.TextIOWrapper):
    """
    Logger object that takes care of both, writing the stdout and stderr to the
    'mini-log' file corresponding to the different elements of the workflow (observe,
    tidy, ...), and writing to the system stdout/stderr so that it is catched by SLURM
    (or I guess PBS) and put into the ``*compute*.log`` specified in the ``.run`` file.
    The ``*compute*.log`` file is then used to catch errors by ``observe``.

    .. Note: This is a fast and minimal solution for release 6.0, but all this could be done
    without needing this object, once ``loguru`` is implemented everywhere in some
    point in the future.

    .. Note: This is a subclass of ``_io.TextIOWrapper`` which is horrible! I could
    have used simply ``object`` instead, but this was the only way I (MA) found for
    ``f90nml`` package to be able to print namelists in the following lines of
    ``namelist.py``::
        def nmls_output(mconfig):
            all_nmls = {}

            for nml_name, nml_obj in six.iteritems(mconfig.get("namelists", {})):
                all_nmls[nml_name] = nml_obj  # PG: or a string representation?
            for nml_name, nml in all_nmls.items():
                message = f'\nFinal Contents of {nml_name}:'
                six.print_(message)
                six.print_(len(message) * '-')
                nml.write(sys.stdout)  # Problematic line if the sys.stdout is not a file-like object
                print('-' * 80)
                print(f'::: end of the contents of {nml_name}\n')
            return mconfig
    Hopefully, we are not using any other methods than the ones specified here...
    """

    def __init__(self, *args, **kwargs):
        """
        Initializes the logger object, and opens the mini-log file, to be used as a
        target for ``sys.stdout``::
            sys.stdout = LogFile(args, kwargs)

        Parameters
        ----------
        args, kwargs
            Use the same parameters as you would with the ``open`` command.
        """
        # Direct system's stderr to stdout
        sys.stderr = sys.stdout
        # Store the system's stdout as an object
        self.stdout = sys.stdout
        # Open the mini-log file
        self.file_obj = open(*args, **kwargs)

    def write(self, *args, **kwargs):
        """
        Writes messages to the mini-log file and to the system's stdout.
        """
        # Write into the mini-log file
        self.file_obj.write(*args, **kwargs)
        # Write into the system's stdout (so that it makes it to *compute*.log defined
        # in the .run file)
        self.stdout.write(args[0])

    def flush(self):
        pass

    def close(self, *args, **kwargs):
        """
        Closes the mini-log file.
        """
        # Close the mini-log file
        self.file_obj.close(*args, **kwargs)
