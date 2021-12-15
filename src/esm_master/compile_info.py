import os, sys, re
import copy
import asyncio

import pickle

from .general_stuff import (
    COMPONENTS_DIR,
    COUPLINGS_DIR,
    SETUPS_DIR,
    DEFAULTS_DIR,
    ESM_SOFTWARE_DIR,
    ESM_MASTER_PICKLE,
    ESM_MASTER_DIR,
)

import esm_parser
from .software_package import *
import colorama

######################################################################################
############################## Combine all YAMLS #####################################
######################################################################################


def save_pickle(obj, path):
    with open(path, "wb") as f:
        pickle.dump(obj, f, pickle.HIGHEST_PROTOCOL)


def load_pickle(path):
    if os.path.isfile(path):
        with open(path, "rb") as f:
            return pickle.load(f)
    else:
        return None


def combine_components_yaml(parsed_args):
    """
    Combines various YAML files in esm_master config directory.

    The esm_master config directory is taken from the ``.esmtoolsrc`` file as
    ``${FUNCTION_PATH}/esm_master/``. All files under the ``components``,
    ``setups``, and ``couplings`` sub-directories are read into the dictionary.

    Returns
    -------
    dict :
        A dictionary equivalent of all components, couplings, setups, and
        general information.
    """

    relevant_entries = [
        "git-repository",
        "curl-repository",  # deniz
        "repo_options",  # kh 11.09.20 support git options like --recursive
        "branch",
        "tag",
        "comp_command",
        "conf_command",
        "clean_command",
        "components",
        "coupling_changes",
        "requires",
        "couplings",
        "install_bins",
        "install_libs",
        "destination",
        "clone_destination",
        "archfile",
        "use_oasis",
        "pipe_options",  # deniz: Linux pipe support
    ]

    categories = ["components", "couplings", "setups", "esm_software"]

    relevant_dirs = {
        "components": COMPONENTS_DIR,
        "couplings": COUPLINGS_DIR,
        "setups": SETUPS_DIR,
        "esm_software": ESM_SOFTWARE_DIR,
    }

    components_dict = {}

    for cat in categories:

        components_dict[cat] = {}
        cat_dir = relevant_dirs[cat]

        # for package in os.listdir(cat_dir):

        asyncio.get_event_loop().run_until_complete(
            get_all_package_info(
                os.listdir(cat_dir),
                cat,
                cat_dir,
                components_dict,
                relevant_entries,
                parsed_args,
            )
        )
        # TODO(PG): Switch around async optional
        # get_all_package_info(os.listdir(cat_dir), cat, cat_dir, components_dict, relevant_entries, parsed_args)
    default_infos = {}

    # If general.yaml file exits ignore it
    yaml_files = os.listdir(DEFAULTS_DIR)
    if "general.yaml" in yaml_files:
        yaml_files.remove("general.yaml")

    for yaml_file in yaml_files:
        if os.getenv("ESM_MASTER_DEBUG"):
            print(f"Reading file {DEFAULTS_DIR}/{yaml_file}")
        file_contents = esm_parser.yaml_file_to_dict(f"{DEFAULTS_DIR}/{yaml_file}")
        default_infos.update(file_contents)

    components_dict["defaults"] = default_infos

    # esm_parser.pprint_config(components_dict)
    # sys.exit(0)
    return components_dict, relevant_entries


async def get_all_package_info(
    packages, cat, cat_dir, components_dict, relevant_entries, parsed_args
):
    # TODO(PG): Switch around async optional
    # def get_all_package_info(packages, cat, cat_dir, components_dict, relevant_entries, parsed_args):
    tasks = []
    # TODO(PG): Better logging (see GH Issue #116)
    if os.getenv("ESM_MASTER_DEBUG"):
        print(f"packages={packages}")
    for package in packages:
        # TODO(PG): Better logging (see GH Issue #116)
        if os.getenv("ESM_MASTER_DEBUG"):
            print(f"Getting {package}")
        # TODO(PG): Switch around async optional
        # task = get_one_package_info(package, cat, cat_dir, components_dict, relevant_entries)
        task = asyncio.ensure_future(
            get_one_package_info(
                package, cat, cat_dir, components_dict, relevant_entries, parsed_args
            )
        )
        tasks.append(task)
    # TODO(PG): Switch around async optional
    # return tasks
    await asyncio.gather(*tasks, return_exceptions=False)


async def get_one_package_info(
    package, cat, cat_dir, components_dict, relevant_entries, parsed_args
):
    # TODO(PG): Switch around async optional
    # def get_one_package_info(package, cat, cat_dir, components_dict, relevant_entries, parsed_args):

    # TODO(PG): Better logging (see GH Issue #116)
    if os.getenv("ESM_MASTER_DEBUG"):
        print(f"Working on package={package}, cat={cat}, cat_dir={cat_dir}")

    package_dir = cat_dir + package + "/"

    default_file = package_dir + package + ".yaml"
    # TODO(PG): Better logging (see GH Issue #116)
    if os.getenv("ESM_MASTER_DEBUG"):
        print(f"default_file={default_file}")

    versioned_files = [
        package_dir + i
        for i in os.listdir(package_dir)
        if i.startswith(package + "-")
        if i.endswith(".yaml")
    ]
    # TODO(PG): Better logging (see GH Issue #116)
    if os.getenv("ESM_MASTER_DEBUG"):
        print(f"versioned_files={versioned_files}")

    comp_config = esm_parser.yaml_file_to_dict(default_file)
    # TODO(PG): Better logging (see GH Issue #116)
    if os.getenv("ESM_MASTER_DEBUG"):
        if not comp_config:
            print(f"Whoops, got False-y thingy!")
    if os.getenv("ESM_MASTER_DEBUG"):
        print(f"...reading file {default_file}")
    if get_correct_entry(comp_config, {}, "version") == {}:
        if os.getenv("ESM_MASTER_DEBUG"):
            print(f'Var "version" is missing in yaml file for package {package}. ')
            print('Trying to set to "*"...')
        comp_config["version"] = "*"

    package_conf = get_relevant_info(relevant_entries, comp_config)

    for conf_file in versioned_files:
        if os.getenv("ESM_MASTER_DEBUG"):
            print(f"...reading file {conf_file}")
        add_config = esm_parser.yaml_file_to_dict(conf_file)
        if get_correct_entry(add_config, {}, "version") == {}:
            if os.getenv("ESM_MASTER_DEBUG"):
                print(f'Var "version" is missing in yaml file for package {package}. ')
                print('Trying to set to "*"...')
            add_config["version"] = "*"
        package_conf = get_relevant_info(relevant_entries, add_config, package_conf)

    package_conf = remove_globbing_char(package_conf)
    if not package_conf == {}:
        components_dict[cat][package] = package_conf


def remove_globbing_char(conf):
    if "available_versions" in conf and conf["available_versions"] == ["*"]:
        if "choose_version" in conf and list(conf["choose_version"].keys()) == ["*"]:
            if not conf["choose_version"]["*"] == {}:
                conf.update(conf["choose_version"]["*"])
            del conf["choose_version"]
            del conf["available_versions"]

    if "choose_version" in conf and "*" in conf["choose_version"]:
        if "available_versions" in conf:
            for entry in conf["available_versions"]:
                if not entry in conf["choose_version"]:
                    conf["choose_version"].update({entry: conf["choose_version"]["*"]})
            del conf["choose_version"]["*"]

    if "available_versions" in conf:
        if "choose_version" in conf:
            for entry in conf["choose_version"]:
                if not entry in conf["available_versions"]:
                    conf["available_versions"].append(entry)
        if "*" in conf["available_versions"]:
            conf["available_versions"].remove("*")

    if "choose_version" in conf:
        for entry in list(conf["choose_version"].keys()):
            if conf["choose_version"][entry] == {}:
                del conf["choose_version"][entry]
                if "available_versions" in conf:
                    if entry in conf["available_versions"]:
                        conf["available_versions"].remove(entry)
        if conf["choose_version"] == {}:
            del conf["choose_version"]

    if "available_versions" in conf and conf["available_versions"] == []:
        del conf["available_versions"]

    return conf


def get_correct_entry(in_config, out_config, entry, default=None):
    compile_tag = "compile_infos"

    if compile_tag in in_config and entry in in_config[compile_tag]:
        out_config[entry] = in_config[compile_tag][entry]
    elif (
        "general" in in_config
        and compile_tag in in_config["general"]
        and entry in in_config["general"][compile_tag]
    ):
        out_config[entry] = in_config["general"][compile_tag][entry]
    elif "general" in in_config and entry in in_config["general"]:
        out_config[entry] = in_config["general"][entry]
    elif entry in in_config:
        out_config[entry] = in_config[entry]
    else:
        if default:
            out_config[entry] = default

    return out_config


def get_relevant_info(relevant_entries, raw_config, merge_into_this_config=None):
    """
    Gets relevant information from the raw configuration and update the given
    configuration dictionary ``merge_into_this_config``.

    Parameters
    ----------
    relevant_entries : list
        A list of relevant entries from which information needs to be extracted.
    raw_config : dict
        A dictionary containing the raw information read from the `yaml` file.
    merge_into_this_config : dict
        A dictionary in which the relevant information will be added.

    Returns
    -------
    merge_into_this_config : dict
        A dictionary given as input, then updated with the relevant information.
    """

    relevant_info = {}
    for entry in relevant_entries:
        relevant_info = get_correct_entry(raw_config, relevant_info, entry)

    # Load default version from the raw configuration and turn it into a string
    default_version = get_correct_entry(raw_config, {}, "version")["version"]
    default_version = str(default_version)

    comp_config = get_correct_entry(
        raw_config, {}, "available_versions", [default_version]
    )
    comp_config = get_correct_entry(
        raw_config, comp_config, "choose_version", {default_version: {}}
    )

    if default_version not in comp_config["choose_version"]:
        comp_config["choose_version"][default_version] = {}

    for version in comp_config["choose_version"]:
        for entry, value in relevant_info.items():
            if not entry in comp_config["choose_version"][version]:
                comp_config["choose_version"][version][entry] = value
        for entry in list(comp_config["choose_version"][version].keys()):
            if entry not in relevant_entries:
                del comp_config["choose_version"][version][entry]

    if merge_into_this_config:
        for version in comp_config["available_versions"]:
            if version not in merge_into_this_config["available_versions"]:
                merge_into_this_config["available_versions"].append(version)

        for version in comp_config["choose_version"]:
            if version in merge_into_this_config["choose_version"]:
                print(f"Error: Version {version} defined two times.")
                sys.exit(-1)
        merge_into_this_config["choose_version"].update(comp_config["choose_version"])

    else:
        merge_into_this_config = copy.deepcopy(comp_config)

    return merge_into_this_config


######################################################################################
########################### class "setup_and_model_infos" ############################
######################################################################################


class setup_and_model_infos:
    def __init__(self, vcs, general, parsed_args):

        if not os.path.isfile(ESM_MASTER_PICKLE):
            self.config, self.relevant_entries = combine_components_yaml(parsed_args)
            save_pickle(self.config, ESM_MASTER_PICKLE)

        elif "list_all_packages" in parsed_args:
            self.config = load_pickle(ESM_MASTER_PICKLE)

        else:
            self.config, self.relevant_entries = combine_components_yaml(parsed_args)
            save_pickle(self.config, ESM_MASTER_PICKLE)

        self.model_kinds = list(self.config.keys())
        self.meta_todos = general.meta_todos
        self.meta_command_order = general.meta_command_order
        self.display_kinds = general.display_kinds

        self.model_todos = []
        for kind in self.model_kinds:
            for model in self.config[kind].keys():
                version = None
                if "choose_version" in self.config[kind][model]:
                    for version in self.config[kind][model]["choose_version"]:
                        for entry in self.config[kind][model]["choose_version"][
                            version
                        ]:
                            if entry.endswith("_command"):
                                todo = entry.replace("_command", "")
                                if todo not in self.model_todos:
                                    self.model_todos.append(todo)
                for entry in self.config[kind][model]:
                    if entry.endswith("_command"):
                        todo = entry.replace("_command", "")
                        if todo not in self.model_todos:
                            self.model_todos.append(todo)

        self.known_todos = self.model_todos + vcs.known_todos + general.meta_todos

        self.all_packages = self.list_all_packages(vcs, general)
        self.update_packages(vcs, general)

        if parsed_args.get("verbose", False):
            self.output()

    def append_to_conf(self, target, reduced_config, toplevel=""):
        (todo, kind, model, version, only_subtarget, raw) = self.split_raw_target(
            target, self
        )
        if not version:
            version = "default"

        if model in self.config[kind]:
            reduced_config[model] = self.config[kind][model]
            reduced_config[model]["version"] = version
            reduced_config[model]["kind"] = kind
            esm_parser.choose_blocks(reduced_config)
        if kind == "setups":
            toplevel = model + "-" + version
            reduced_config[model]["model_dir"] = ESM_MASTER_DIR + "/" + toplevel
            if "couplings" in self.config[kind][model]:
                for coupling in self.config[kind][model]["couplings"]:
                    reduced_config = self.append_to_conf(
                        coupling, reduced_config, toplevel
                    )
        elif kind == "couplings":
            if toplevel == "":
                toplevel = model + "-" + version
            reduced_config[model]["model_dir"] = ESM_MASTER_DIR + "/" + toplevel
            if "components" in self.config[kind][model]:
                for component in self.config[kind][model]["components"]:
                    reduced_config = self.append_to_conf(
                        component, reduced_config, toplevel
                    )
        elif kind == "components":
            sep = ""
            if toplevel == "":
                if "requires" in self.config[kind][model]:
                    toplevel = model + "-" + version
                    sep = "/"
            else:
                sep = "/"

            if "destination" in reduced_config[model]:
                reduced_config[model]["model_dir"] = (
                    ESM_MASTER_DIR
                    + "/"
                    + toplevel
                    + sep
                    + reduced_config[model]["destination"]
                )
            else:
                reduced_config[model]["model_dir"] = (
                    ESM_MASTER_DIR + "/" + toplevel + sep + model + "-" + version
                )

            if "requires" in self.config[kind][model]:
                for requirement in self.config[kind][model]["requires"]:
                    reduced_config = self.append_to_conf(
                        requirement, reduced_config, toplevel
                    )

        return reduced_config

    # def reduce(self, target, env):
    def reduce(self, target):
        blacklist = [re.compile(entry) for entry in ["computer.*"]]

        reduced_config = {}
        reduced_config["defaults"] = self.config["defaults"]
        reduced_config = self.append_to_conf(target, reduced_config)

        esm_parser.choose_blocks(reduced_config)
        esm_parser.recursive_run_function(
            [],
            reduced_config,
            "atomic",
            esm_parser.find_variable,
            reduced_config,
            blacklist,
            True,
        )

        new_config = {}
        for headline in reduced_config:
            if "kind" in reduced_config[headline]:
                if not reduced_config[headline]["kind"] in new_config:
                    new_config[reduced_config[headline]["kind"]] = {
                        headline: reduced_config[headline]
                    }
                else:
                    new_config[reduced_config[headline]["kind"]].update(
                        {headline: reduced_config[headline]}
                    )
            else:
                new_config.update({headline: reduced_config[headline]})

        # esm_parser.pprint_config(new_config)
        # sys.exit(0)
        return new_config

    def replace_last_vars(self, env):

        self.config["computer"] = copy.deepcopy(env.config)
        esm_parser.recursive_run_function(
            [],
            self.config,
            "atomic",
            esm_parser.find_variable,
            self.config,
            [],
            True,
        )

    def update_relevant_entries_with_config(self, config):
        for component in config:
            for entry in self.relevant_entries:
                if (
                    entry in config[component]
                    and component in self.config["components"]
                ):
                    self.config["components"][component][entry] = config[component][
                        entry
                    ]

    def update_packages(self, vcs, general):
        for package in self.all_packages:
            package.fill_in_infos(self, vcs, general)

    def list_all_packages(self, vcs, general):
        packages = []
        config = self.config
        for kind in self.model_kinds:
            for model in config[kind]:
                version = None
                if "available_versions" in config[kind][model]:
                    for version in config[kind][model]["available_versions"]:
                        packages.append(
                            software_package(
                                (kind, model, version),
                                self,
                                vcs,
                                general,
                                no_infos=True,
                            )
                        )
                else:
                    packages.append(
                        software_package(
                            (kind, model, version), self, vcs, general, no_infos=True
                        )
                    )
        return packages

    def has_target(self, package, target, vcs):
        if target in self.meta_todos:
            for subtarget in self.meta_command_order[target]:
                if self.has_target(package, subtarget, vcs):
                    return True
        if target in vcs.known_todos:
            for repo in vcs.known_repos:
                answer = self.get_config_entry(package, repo + "-repository")
                if answer:
                    return True
        else:
            answer = self.get_config_entry(package, target + "_command")
            if answer:
                return True
        return False

    def has_target2(self, package, target):
        for testpackage in self.all_packages:
            if (
                testpackage.raw_name == package.raw_name
                and target in testpackage.targets
            ):
                return True
        return False

    def has_package(self, package):
        if package in self.all_packages:
            return True
        else:
            return False

    def has_model(self, model):
        for kind in self.model_kinds:
            for test_model in self.config[kind]:
                if test_model == model:
                    return True
        return False

    def split_raw_target(self, rawtarget, setup_info):
        todo = kind = only_subtarget = None
        model = version = ""
        if "/" in rawtarget:
            rawtarget, only_subtarget = rawtarget.rsplit("/", 1)

        raw = rawtarget
        for this_todo in setup_info.known_todos:
            if rawtarget == this_todo:
                return this_todo, None, None, None, None, raw
            elif rawtarget.startswith(this_todo + "-"):
                todo = this_todo
                rawtarget = rawtarget.replace(todo + "-", "")
                break

        for package in self.all_packages:
            if package.raw_name == rawtarget:
                return (
                    todo,
                    package.kind,
                    package.model,
                    package.version,
                    only_subtarget,
                    raw,
                )

        # package not found:
        self.output_available_targets(rawtarget)
        sys.exit(0)

    def assemble_raw_name(self, todo, kind, model, version):
        raw = sep = ""
        if todo:
            raw = todo
            sep = "-"
        if model:
            raw = raw + sep + model
            sep = "-"
        if version:
            raw = raw + sep + version
            sep = "-"
        if not raw == "":
            return raw
        return None

    def setup_or_model(self, model):
        kind_of_model = "unknown"
        for kind in self.model_kinds:
            if model in self.config[kind]:
                kind_of_model = kind
        return kind_of_model

    def output_available_targets(self, search_keyword):
        colorama.init(autoreset=True)
        display_info = []
        if search_keyword == "":
            display_info = self.all_packages
        else:
            for package in self.all_packages:
                if package.targets:
                    for target in package.targets:
                        if search_keyword in target + "-" + package.raw_name:
                            if package not in display_info:
                                display_info.append(package)

        if display_info == []:
            print()
            print(
                "No targets found for keyword "
                + search_keyword
                + ". Type 'esm_master' to get a full list"
            )
            print("of available targets.")
            print()
        elif display_info == self.all_packages:
            print()
            print(
                colorama.Fore.YELLOW
                + "Master Tool for ESM applications, including download and compiler wrapper functions"
            )
            print(
                colorama.Fore.YELLOW
                + "		originally written by Dirk Barbi (dirk.barbi@awi.de)"
            )
            print(
                colorama.Fore.YELLOW
                + "       further developed as OpenSource, coordinated and maintained at AWI"
            )
            print()
            print(
                colorama.Fore.YELLOW
                + "Obtain from:         https://github.com/esm-tools/esm_master.git"
            )
            print()
            self.print_nicely(display_info)
            print()
        else:
            print()
            print(
                search_keyword
                + " is not an available target. Did you mean one of those:"
            )
            self.print_nicely(display_info)
            print()

    def print_nicely(self, display_info):
        """Will display all supported models when esm_master is run without
        any arguments or for the selected model

        Parameters
        ----------
        display_info : list of `software_package` objects

        """
        all_available_options = set()  # eg. get, install, clean, ...
        sorted_display = {}
        for kind in self.display_kinds:
            for package in display_info:
                # add the targets of the current package to all targets
                all_available_options.update(package.targets)

                if package.kind == kind:
                    if not kind in sorted_display.keys():
                        sorted_display[kind] = dict()

                    # if model is not encountered yet, then create it first
                    if not package.model in sorted_display[kind]:
                        sorted_display[kind][package.model] = dict()

                    # then add the version : targets node
                    if package.version:
                        sorted_display[kind][package.model][
                            package.version
                        ] = package.targets
                    else:
                        sorted_display[kind][package.model] = package.targets

        # ===
        # print the available models and versions
        # ===
        all_available_options = sorted(all_available_options)
        # sort the dictionary alphabetically for better user experience
        # kind is 'setups', 'components'
        # Each item of the sorted_display[kind].items() is a tuple of "model"
        # and dictionary of version and available options. Sort alphabetically
        # according to the model name, ie. first variable of the tuple
        alphabetical_dict = {}
        for kind in sorted_display:
            # sort the models within each `kind`
            d = dict(
                sorted(sorted_display[kind].items(), key=lambda item: item[0].lower())
            )
            alphabetical_dict[kind] = d

            # sort the versions within each `model`
            for model in alphabetical_dict[kind]:
                # only if it has version : target node, ie omit the models
                # without version
                if isinstance(alphabetical_dict[kind][model], dict):
                    d2 = dict(
                        sorted(
                            alphabetical_dict[kind][model].items(),
                            key=lambda item: item[0].lower(),
                        )
                    )
                    alphabetical_dict[kind][model] = d2

        colorama.init(autoreset=True)
        for kind in alphabetical_dict:
            print(f"{colorama.Fore.GREEN}{kind.upper()}:")
            for model in alphabetical_dict[kind]:
                print(f"    {colorama.Fore.CYAN}{model}:")
                # some models (eg. setups -> oifsamip don't have a version)
                if isinstance(alphabetical_dict[kind][model], list):
                    targets = alphabetical_dict[kind][model]
                    print(f"        ", end="")
                    print(*targets, sep="  ")

                # for the rest of the models [kind][model] will further yield
                # version : targets
                elif isinstance(alphabetical_dict[kind][model], dict):
                    max_version_length = max(
                        [len(ver) for ver in alphabetical_dict[kind][model]]
                    )

                    for version in alphabetical_dict[kind][model]:
                        targets = alphabetical_dict[kind][model][version]
                        print(
                            f"{colorama.Fore.MAGENTA}        "
                            f"{version:<{max_version_length}} :",
                            end=" ",
                        )
                        print(*targets, sep="  ")
            print()

    def get_config_entry(self, package, entry):
        try:
            answer = self.config[package.kind][package.model]["choose_version"][
                package.version
            ][entry]
        except:
            try:
                answer = self.config[package.kind][package.model][entry]
            except:
                answer = None
        return answer

    def output(self):
        print()
        print("Model kinds: " + str(self.model_kinds))
        print("Model todos: " + str(self.model_todos))
        print("All known todos: " + str(self.known_todos))
        for package in self.all_packages:
            package.output()
