import os
import sys
import warnings

import esm_parser
import esm_tools

######################################################################################
##################################### globals ########################################
######################################################################################

ESM_MASTER_DIR = os.getenv("PWD")

COMPONENTS_DIR = esm_tools.get_config_filepath("/components/")
SETUPS_DIR = esm_tools.get_config_filepath("/setups/")
COUPLINGS_DIR = esm_tools.get_config_filepath("/couplings/")
DEFAULTS_DIR = esm_tools.get_config_filepath("/defaults/")
ESM_SOFTWARE_DIR = esm_tools.get_config_filepath("/esm_software/")
CONFIG_YAML = esm_tools.get_config_filepath("/esm_software/esm_master/esm_master.yaml")
VCS_FOLDER = esm_tools.get_config_filepath("/other_software/vcs/")

ESM_MASTER_PICKLE = ESM_SOFTWARE_DIR + "/esm_master/esm_master.pkl"

######################################################################################
##################################### tab completion #################################
######################################################################################


def tab_completion(parsed_args, setups2models):
    if parsed_args["list_all_targets"]:
        all_commands = []
        for package in setups2models.all_packages:
            for command in package.command_list:
                all_commands.append(command + "-" + package.raw_name)
        print("\n".join(all_commands))
        sys.exit()

    if parsed_args["generate_tab_complete"]:
        with open("esm_master_tabcomplete.bash", "w") as tab_comp:
            tab_comp.write("#/usr/bin/env bash\n")
            tab_comp.write("_esm_master_completions() {\n")
            tab_comp.write(
                '\tCOMPREPLY=($(compgen -W "$(esm_master --list_all_targets)" "${COMP_WORDS[1]}"))'
            )
            tab_comp.write("\n}\n\ncomplete -F _esm_master_completions esm_master\n")
        print("Wrote file: esm_master_tabcomplete.bash")
        print(
            "Have your shell source this file to allow tab completion of available targets"
        )
        print("This works for both bash and zsh")
        return 0


######################################################################################
############################## Write a tiny user script ##############################
######################################################################################


def write_minimal_user_config(config):
    """

    In order to generate a SimulationSetup using esm_parser, we need a small and
    simple user_config. It doesn't really matter for esm_master if all the entries
    are correct, as we don't need most of them, but the esm_parser fails if they
    don't exist as all...
    Needs the name of the model / setup, and version (if exists) as input.

    """
    # FIXME(PG): The comment above screams "HORRIBLE DESIGN CHOICE"
    user_config = {}

    for model in config["components"]:
        version = config["components"][model]["version"]
        model_dir = config["components"][model]["model_dir"]
        user_config.update(
            {model: {"model": model, "version": version, "model_dir": model_dir}}
        )

    if "setups" in config:
        coupled = "true"
        setup = list(config["setups"])[0]
        version = config["setups"][setup]["version"]
        model_dir = config["setups"][setup]["model_dir"]

    else:
        coupled = "false"
        setup = list(config["components"])[0]
        version = config["components"][setup]["version"]

    user_config.update(
        {
            "general": {
                "jobtype": "compute",
                "account": "none",
                "setup_name": setup,
                "version": version,
                "coupled": "false",
                "initial_date": "2000-01-01",
                "final_date": "2000-01-02",
                "compute_time": "00:00:01",
                "model_dir": model_dir,
                "base_dir": "/on/a/road/to/nowwhere",
            }
        }
    )

    if "general.yaml" in os.listdir(DEFAULTS_DIR):
        general_config = esm_parser.yaml_file_to_dict(f"{DEFAULTS_DIR}/general.yaml")
        user_config["general"].update(general_config)

    return user_config


######################################################################################
############################## class "GeneralInfos" ##################################
######################################################################################


class GeneralInfos:
    def __init__(self, parsed_args):

        # Parses the ``esm_master.yaml`` configuration file
        self.config = esm_parser.yaml_file_to_dict(CONFIG_YAML)
        self.meta_todos, self.meta_command_order = self.get_meta_command()
        self.display_kinds = self.get_display_kinds()

        if parsed_args.get("verbose", False):
            self.output()

    def get_meta_command(self):
        """
        Gets ``meta_todos`` and ``meta_command_order``, which are a combination
        of other tasks. e.g. "install" does get, conf, and comp.

        Returns
        -------
        Tuple of List, Dict
        The list contains all meta_todos, the dict contains the todo as the
        key, and the steps as the value.
        """
        meta_todos = []
        meta_command_order = {}
        for entry in self.config:
            if entry.endswith("_meta_command"):
                todo = entry.replace("_meta_command", "")
                meta_todos.append(todo)
                meta_command_order.update({todo: self.config[entry]})
        return meta_todos, meta_command_order

    def get_display_kinds(self):
        if "display_kinds" in self.config.keys():
            return self.config["display_kinds"]

    def output(self):
        print()
        print("Meta commands: ")
        for key in self.meta_command_order:
            print("    " + key + ": " + str(self.meta_command_order[key]))


######################################################################################
########################### class "version_control_infos" ############################
######################################################################################


class VersionControlInfos:
    def __init__(self, parsed_args):
        self.config = {}
        vcs_files = [f for f in os.listdir(VCS_FOLDER)]
        self.known_repos = []
        for vcs_file in vcs_files:
            if os.path.isfile(VCS_FOLDER + "/" + vcs_file):
                repo_type = vcs_file.replace(".yaml", "")
                self.config.update(
                    {
                        repo_type: esm_parser.yaml_file_to_dict(
                            VCS_FOLDER + "/" + vcs_file
                        )
                    }
                )
                self.known_repos.append(repo_type)
        self.known_todos = []
        for repo in self.known_repos:
            for entry in self.config[repo].keys():
                if entry.endswith("_command"):
                    todo = entry.replace("_command", "")
                    if todo not in self.known_todos:
                        self.known_todos.append(todo)
        if parsed_args.get("verbose", False):
            self.output()

    def assemble_command(self, package, todo, setup_info, general):
        if package.repo_type and package.repo:
            if todo in setup_info.meta_todos:
                return None
            try:
                raw_command = self.config[package.repo_type][todo + "_command"]

                # kh 11.09.20 support git options like --recursive
                # repo_options in the model.yaml is assigned to define_options
                if package.repo_options:
                    define_options = self.config[package.repo_type]["define_options"]
                    raw_command = raw_command.replace(
                        "${define_options}", define_options
                    )
                    raw_command = raw_command.replace(
                        "${repo_options}", package.repo_options
                    )
                else:
                    raw_command = raw_command.replace("${define_options} ", "")
                    raw_command = raw_command.replace(
                        "${repo_options}", ""
                    )  # kh 11.09.20 should not really be necessary
                if package.branch:
                    define_branch = self.config[package.repo_type]["define_branch"]
                    raw_command = raw_command.replace("${define_branch}", define_branch)
                    raw_command = raw_command.replace("${branch}", package.branch)
                else:
                    raw_command = raw_command.replace("${define_branch} ", "")
                    raw_command = raw_command.replace("${branch}", "")

                if package.tag:
                    define_tag = self.config[package.repo_type]["define_tag"]
                    raw_command.replace("${define_tag}", define_tag)
                    raw_command.replace("${tag}", package.tag)
                else:
                    raw_command = raw_command.replace("${define_tag} ", "")
                    raw_command = raw_command.replace("${tag}", "")

                # deniz: pipe support. Eg. curl foo.tar.gz | tar xz
                # pipe_options is given in model yaml file
                if package.pipe_options:
                    raw_command = raw_command.replace(
                        "${pipe_options}", package.pipe_options
                    )
            except:
                print("Sorry, no " + todo + "_command defined for " + package.repo_type)
                sys.exit(42)
            if isinstance(package.repo, list):
                repo = package.repo[0]
            else:
                repo = package.repo
            if os.environ.get("CI"):
                if "gitlab.awi.de" in repo:
                    awi_user = os.environ.get("GITLAB_AWI_USER_NAME", "")
                    if awi_user:
                        awi_user += "@"
                    repo = repo.replace("gitlab.awi.de", f"{awi_user}gitlab.dkrz.de")
                elif "swrepo1.awi.de" in repo:
                    print(
                        f"swrepo1.awi.de is decommisioned, please consider correcting {repo}"
                    )
                    if os.environ.get("esm_tools_pedantic") or os.environ.get(
                        "ESM_TOOLS_PEDANTIC"
                    ):
                        print("Pedantic mode is on, exiting instead of warning only!")
                        sys.exit(1)
                elif "gitlab.dkrz.de" in repo:
                    # NOTE(PG): Empty string as return value to ensure that the f-string works nicely
                    dkrz_user = os.environ.get("GITLAB_DKRZ_USER_NAME", "")
                    if dkrz_user:
                        dkrz_user += "@"
                    repo = repo.replace("gitlab.dkrz.de", f"{dkrz_user}gitlab.dkrz.de")
                elif "github.com" in repo:
                    print("No token needed for github.com repositories!")
                else:
                    print(f"Sorry, no CI token defined for {repo}")
                    # FIXME(PG): We should come up with a sensible convention for this:
                    if os.environ.get("esm_tools_pedantic") or os.environ.get(
                        "ESM_TOOLS_PEDANTIC"
                    ):
                        print("Pedantic mode is on, exiting instead of warning only!")
                        sys.exit(1)

            raw_command = raw_command.replace("${repository}", repo)
            if todo == "get":
                if package.repo_type == "curl":
                    raw_command = raw_command.replace("${curl-repository}", repo)
                    # return so that it is not overwritten
                    return raw_command
                if package.clone_destination:
                    raw_command = raw_command + " " + package.clone_destination
                elif package.destination:
                    raw_command = raw_command + " " + package.destination
                else:
                    raw_command = raw_command + " " + package.raw_name

            commands = [raw_command]

            if package.permissions:
                commands.append(f"chmod {package.permissions} -R {package.destination}")
        else:
            commands = None

        return commands

    def output(self):
        print()
        esm_parser.dict_to_yaml.yaml_dump(self.config)
        print("Known repos: " + str(self.known_repos))
        print("Known vcs-commands: " + str(self.known_todos))


class version_control_infos(VersionControlInfos):
    # Deprecated class name
    def __init__(self, *args, **kwargs):
        warnings.warn(
            "class 'version_control_infos' is deprecated, use 'VersionControlInfos' instead",
            DeprecationWarning,
        )
        super().__init__(*args, **kwargs)
