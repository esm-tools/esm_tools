import datetime
import os
import site

import questionary

import pathlib
import subprocess
import sys
import venv

esm_tools_modules = [
    "esm_calendar",
    "esm_database",
    "esm_environment",
    "esm_master",
    "esm_parser",
    "esm_rcfile",
    "esm_runscripts",
    "esm_tools",
    "esm_plugin_manager",
    "esm_version_checker",
]


def get_base_prefix_compat():
    """Get base/real prefix, or sys.prefix if there is none."""
    return (
        getattr(sys, "base_prefix", None)
        or getattr(sys, "real_prefix", None)
        or sys.prefix
    )


def in_virtualenv():
    return get_base_prefix_compat() != sys.prefix


class _EnvBuilder(venv.EnvBuilder):
    def __init__(self, *args, **kwargs):
        self.context = None
        super().__init__(*args, **kwargs)

    def post_setup(self, context):
        self.context = context


def _venv_create(venv_path):
    venv_builder = _EnvBuilder(with_pip=True)
    venv_builder.create(venv_path)
    return venv_builder.context


def _run_python_in_venv(venv_context, command):
    command = [venv_context.env_exe] + command
    return subprocess.check_call(command)


def _run_bin_in_venv(venv_context, command):
    command[0] = str(pathlib.Path(venv_context.bin_path).joinpath(command[0]))
    return subprocess.check_call(command)


def _source_and_run_bin_in_venv(venv_context, command, shell):
    source_command = " ".join(
        ["source", venv_context.bin_path + "/activate", "&&", " "]
    )
    command = source_command + command
    return subprocess.check_call(command, shell=shell)


def _install_tools(venv_context, config):
    """
    Installs the ESM-Tools packages for a virtual environment, taking into account
    the user's specifications for editable packages and desired branches.

    To control which packages are installed in editable mode the user can add the
    following to their runscript:

    .. code-block:: yaml

       general:
           install_<esm_package>_editable: True/False

    To control which package branch is installed (compatible with editable mode and
    non-editable mode) the user can add the following to their runscript:

    .. code-block:: yaml

       general:
           install_<esm_package>_branch: <branch_name>

    Parameters
    ----------
    venv_context : type
        Some description
    config : dict
        Configuration dictionary for this run
    """

    # First installation of all packages, with the desired mode (editable/non-editable),
    # and branches, together with all their dependencies
    _install_tools_general(venv_context, config)
    # Some packages, such as `esm_tools` have other packages as dependencies (i.e.
    # `esm_parser`). In the previous step, if a package is installed for which a
    # dependency is editable and/or branched, this dependency gets back to non-editable
    # realese-branch. The following line resinstalls all the editable/branched packages
    # again, this time without dependencies.
    _install_tools_general(venv_context, config, deps=False)


def _install_tools_general(venv_context, config, deps=True):
    """
    Actual installer of ESM-Tools packages for virtual environments. Used by
    `_install_tools` method to correctly install packages with the user's requested
    options for each package (editable/non-editable and branch). See `_install_tools`
    documentation for more information.

    Parameters
    ----------
    venv_context : type
        Some description
    config : dict
        Configuration dictionary for this run
    deps : bool
        Boolean indicating whether dependencies should be installed or not
    """
    # Setup the --no-deps flag if necessary
    if not deps:
        no_deps_flag = ["--no-deps"]
    else:
        no_deps_flag = []
    # Loop through the esm_tools packages to be installed
    for tool in esm_tools_modules:
        # Module info (url, editable install, branch...)
        url = f"https://github.com/esm-tools/{tool}"
        user_wants_editable = config["general"].get(f"install_{tool}_editable", False)
        user_wants_branch = config["general"].get(f"install_{tool}_branch")
        # If the package is editable install it in <EXP_PATH>/src/esm-tools/
        if user_wants_editable:
            # Make sure the directory exists:
            src_dir = pathlib.Path(
                config["general"]["experiment_dir"] + f"/src/esm-tools/{tool}"
            )
            if not src_dir.exists():
                src_dir.mkdir(parents=True, exist_ok=True)
            # Select branch if necessary
            if user_wants_branch:
                branch_command = f" -b {user_wants_branch} "
            else:
                branch_command = ""
            # Clone from git
            if deps:
                subprocess.check_call(
                    f"git clone --quiet {branch_command} {url} {src_dir}", shell=True
                )
            # Carry out the editable installation (with or without dependencies)
            _run_bin_in_venv(
                venv_context,
                [
                    "pip",
                    "install",
                    "-q",
                    f"--find-links={os.environ.get('HOME')}/.cache/pip/wheels",
                    "-e",
                    src_dir,
                ]
                + no_deps_flag,
            )
            _run_bin_in_venv(
                venv_context,
                [
                    "pip",
                    "wheel",
                    "-q",
                    f"--wheel-dir={os.environ.get('HOME')}/.cache/pip/wheels",
                    "-e",
                    src_dir,
                ]
                + no_deps_flag,
            )
        # If the package is not editable then do a standard installation.
        # Note: this step only runs with the `--no-deps` flag if the user has specified
        # a branch, as this flags means also that is the second time passing through
        # here, and we don't want to waste time on installing everything a second time
        # if not necessary.
        elif deps or (not deps and user_wants_branch):
            url = f"git+{url}"
            if user_wants_branch:
                url += f"@{user_wants_branch}"
            # NOTE(PG): We need the -U flag to ensure the branch is actually installed.
            _run_bin_in_venv(
                venv_context,
                [
                    "pip",
                    "install",
                    "-q",
                    f"--find-links={os.environ.get('HOME')}/.cache/pip/wheels",
                    "-U",
                    url,
                ]
                + no_deps_flag,
            )
            _run_bin_in_venv(
                venv_context,
                [
                    "pip",
                    "wheel",
                    "-q",
                    f"--wheel-dir={os.environ.get('HOME')}/.cache/pip/wheels",
                    url,
                ]
                + no_deps_flag,
            )


def _install_required_plugins(venv_context, config):
    required_plugins = []
    for sub_cfg_key, sub_cfg in config.items():
        if isinstance(sub_cfg, dict):
            if "required_plugins" in sub_cfg:
                try:
                    assert isinstance(sub_cfg["required_plugins"], list)
                except AssertionError:
                    print(f"ERROR -- required plugins in {sub_cfg_key} must be a list!")
                    sys.exit(1)
                required_plugins += sub_cfg["required_plugins"]
    for required_plugin in required_plugins:
        _run_bin_in_venv(venv_context, ["pip", "install", "-q", required_plugin])


def venv_bootstrap(config):
    """Bootstraps your run into a virtual environment"""
    if not config["general"].get("use_venv"):
        if (
            config["general"].get("use_venv") is None
            and config["general"]["command_line_config"]["use_venv"] is None
        ):
            config = _integorate_user_venv(config)
            config["general"]["command_line_config"]["use_venv"] = config["general"][
                "use_venv"
            ]
    if config["general"].get("use_venv", False):
        if not in_virtualenv():
            venv_path = pathlib.Path(config["general"]["experiment_dir"]).joinpath(
                ".venv_esmtools"
            )
            if venv_path.exists():
                print(f"{venv_path} already exists, reusing...")
                venv_context = _EnvBuilder(with_pip=True).ensure_directories(venv_path)
            else:
                print(
                    f"Building virtual env, please be patient (this takes about 3 minutes)..."
                )
                start_time = datetime.datetime.now()
                venv_path = pathlib.Path(config["general"]["experiment_dir"]).joinpath(
                    ".venv_esmtools"
                )
                venv_context = _venv_create(venv_path)
                _run_python_in_venv(
                    venv_context, ["-m", "pip", "-q", "install", "-U", "pip"]
                )
                _run_python_in_venv(
                    venv_context, ["-m", "pip", "-q", "install", "-U", "wheel"]
                )
                _install_tools(venv_context, config)
                _install_required_plugins(venv_context, config)
                print(
                    f"...finished {datetime.datetime.now() - start_time}, restarting your job in the virtual env"
                )
            sys.argv[0] = pathlib.Path(sys.argv[0]).name
            # NOTE(PG): This next line allows the job to restart itself in the
            # virtual environment.
            _source_and_run_bin_in_venv(
                venv_context, " ".join(sys.argv) + " --contained-run", shell=True
            )
            sys.exit(0)
    if not config["general"].get("use_venv"):
        if "--open-run" not in config["general"]["original_command"]:
            config["general"]["original_command"] += " --open-run"
        if (
            "--open-run"
            not in config["general"]["command_line_config"]["original_command"]
        ):
            config["general"]["command_line_config"][
                "original_command"
            ] += " --open_run"
    return config


def find_package(pkg):
    pool_root = os.path.abspath(site.getsitepackages()[0] + f"/{pkg}")
    if not os.path.exists(pool_root):
        # Check if an egg link exists:
        if os.path.isfile(site.getusersitepackages() + f"/{pkg}.egg-link"):
            with open(site.getusersitepackages() + f"/{pkg}.egg-link") as egg_file:
                egg_contents = egg_file.readlines()[0].strip()
            pool_root = egg_contents + f"/{pkg}"
        else:
            pool_root = os.path.abspath(site.getusersitepackages() + f"{pkg}")
    return pool_root


def _integorate_user_venv(config):
    questionary.print("\t" + 100 * "=")
    questionary.print(
        "\t\tRunning jobs can optionally be encapsulated into a virtual environment\n"
    )
    questionary.print(
        "\t\tThis shields the run from changes made to the remainder of the ESM-Tool installation\n"
    )
    questionary.print(
        "\t\tBefore the first run, a local copy will be installed in the experiment tree,"
    )
    questionary.print("\t\tand **this** installation will be used:")
    questionary.print(
        f"\t\t\t {config['general']['experiment_dir']}/.venv_esmtools/lib/python-{sys.version_info.major}.{sys.version_info.minor}/site-packages/esm-tools"
    )
    questionary.print("\t\tinstead of:")
    questionary.print(f"\t\t\t{find_package('esm-tools')}\n")
    questionary.print(
        "\n\t\tIf you choose to use a virtual environment, a folder named `.venv_esmtools`"
    )
    questionary.print(
        "\t\twill be created at the root of your experiment. This contains all the Python"
    )
    questionary.print(
        "\t\tlibraries used by the esm-tools. The first installation induces some overhead (~2-3 minutes)."
    )
    questionary.print(
        "\n\t\tYou may also select to install some packages in 'editable mode', in which case"
    )
    questionary.print(
        "\t\tthey will be installed in a folder `src/esm_tools/<package_name>` in the root of"
    )
    questionary.print(
        "\t\tyour experiment. Any changes made to code in that folder **will** influence how the"
    )
    questionary.print("\t\tesm-tools behave.")
    questionary.print("\n\t\tNote regarding config yamls and namelists")
    questionary.print("\t\t-----------------------------------------")
    questionary.print(
        "\t\tWhen using a virtual environment, config files and namelists will come of the"
    )
    questionary.print(
        "\t\tfolder .venv_esmtools listed above and **not** from your user install directory."
    )
    questionary.print(
        "\t\tYou should make **all** changes to the namelists and config files via your user"
    )
    questionary.print("\t\trunscript. This is recommended in all cases!!!")
    questionary.print(
        "\t\t --> Changing any settings in your user install ($HOME or similar) will therefore not change your run!! <--",
        style="fg:red",
    )
    questionary.print("\n\t\tNotes from the development team:")
    questionary.print("\t\t--------------------------------")
    questionary.print(
        "\n\t\t --> Using an encapsulated environment is recommended for production runs <--\n",
        style="fg:green",
    )
    questionary.print("\t Happy simulating!")
    questionary.print(
        "\t Dirk Barbi, Paul Gierz, Nadine Wieters, Miguel Andrés-Martínez, Deniz Ural, and the rest of the development team."
    )
    questionary.print("\t" + 100 * "=")

    user_confirmed = False
    while not user_confirmed:
        response = questionary.select(
            "What do you want to do?",
            choices=[
                "Run in virtualenv (You may set the flag `--contained-run` during your run call or set `general.use_venv: True`)",
                "Run using default installation (You may set the flag `--open-run` during your run call or set `general.use_venv: False`)",
                "Quit right now to adapt your runscript",
            ],
        ).ask()  # returns value of selection
        if "Quit" in response:
            sys.exit(0)
        config["general"]["use_venv"] = "Run in virtualenv" in response
        user_confirmed = questionary.confirm("Are you sure?").ask()
    if "Run in virtualenv" in response:
        editable_mode_choices = questionary.checkbox(
            "Which of the following packages do you want to install in 'editable mode'? End with <Enter>",
            choices=esm_tools_modules,
        ).ask()
        for choice in editable_mode_choices:
            config["general"][f"install_{choice}_editable"] = True
    return config


if __name__ == "__main__":
    dummy_config = {"general": {"experiment_dir": "/foo/bar"}}
    config = _integorate_user_venv(dummy_config)
    print(config)
