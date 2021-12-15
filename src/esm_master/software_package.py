######################################################################################
############################## class "software_package" ##############################
######################################################################################
import sys


def replace_var(var, tag, value):
    if var and tag and value:
        # deniz, TODO: In the future I will implement a better way around
        if isinstance(var, (str, int, float)):
            var = str(var)
            return var.replace("${" + tag + "}", value)
        elif type(var) == list:
            newlist = []
            for entry in var:
                newlist.append(replace_var(entry, tag, value))
            return newlist
        else:
            print(f"ERROR: unresolved variable: {var}")
            sys.exit(1)


class software_package:
    def __init__(
        self, raw, setup_info, vcs, general, no_infos=False
    ):  # model_and_version):
        if type(raw) == str:
            (
                dummy,
                self.kind,
                self.model,
                self.version,
                dummy2,
                self.raw_name,
            ) = setup_info.split_raw_target(raw, setup_info)
        else:  # tupel:
            (self.kind, self.model, self.version) = raw
            self.raw_name = setup_info.assemble_raw_name(
                None, self.kind, self.model, self.version
            )

        self.tag = None

        # deniz: Linux pipe support: eg. curl foo.tar.gz | tar zx
        # !!! if these lines are not here esm_master crashes on xios.
        self.pipe_options = None

        # deniz: I don't like the following lines. This is not good OOP.
        # Constructor should initialize all variables and they should not be
        # defined in an else block
        if not no_infos:
            self.fill_in_infos(setup_info, vcs, general)
        else:
            self.targets = self.subpackages = None

            # kh 11.09.20 support git options like --recursive
            self.repo_type = None
            self.repo = None
            self.branch = None
            self.repo_options = None
            self.bin_type = None
            self.bin_names = [None]
            self.command_list = None
            self.destination = None
            self.clone_destination = None
            self.coupling_changes = None
            # deniz: Linux pipe support: eg. curl foo.tar.gz | tar zx
            self.pipe_options = None

    def fill_in_infos(self, setup_info, vcs, general):

        self.targets = self.get_targets(setup_info, vcs)
        self.subpackages = self.get_subpackages(setup_info, vcs, general)
        self.complete_targets(setup_info)

        # kh 11.09.20 support git options like --recursive
        self.repo_type, self.repo, self.branch, self.repo_options = self.get_repo_info(
            setup_info, vcs
        )
        self.destination = setup_info.get_config_entry(self, "destination")
        self.clone_destination = setup_info.get_config_entry(self, "clone_destination")
        if not self.destination:
            self.destination = self.raw_name

        self.coupling_changes = self.get_coupling_changes(setup_info)
        self.repo = replace_var(self.repo, self.model + ".version", self.version)
        self.branch = replace_var(self.branch, self.model + ".version", self.version)
        self.repo_options = replace_var(
            self.repo_options, self.model + ".version", self.version
        )

        # deniz: Linux pipe support
        self.pipe_options = setup_info.get_config_entry(self, "pipe_options")

        self.bin_type, self.bin_names = self.get_comp_type(setup_info)
        self.command_list = self.get_command_list(setup_info, vcs, general)

    def get_targets(self, setup_info, vcs):
        config = setup_info.config
        targets = []
        for todo in setup_info.known_todos:
            if setup_info.has_target(self, todo, vcs):
                targets.append(todo)
        return targets

    def complete_targets(self, setup_info):
        for todo in setup_info.known_todos:
            for package in self.subpackages:
                if todo in package.targets:
                    if todo not in self.targets:
                        self.targets.append(todo)

    def get_coupling_changes(self, setup_info):
        config = setup_info.config
        changes = []
        if self.kind == "couplings":
            these_changes = setup_info.get_config_entry(self, "coupling_changes")
            if these_changes:
                changes = changes + these_changes
        elif self.kind == "setups":
            couplings = setup_info.get_config_entry(self, "couplings")
            if couplings:
                for coupling in couplings:
                    changes = []
                    if "coupling_changes" in config["couplings"][coupling]:
                        these_changes = config["couplings"][coupling][
                            "coupling_changes"
                        ]
                        if these_changes:
                            changes = changes + these_changes
        return changes

    def get_subpackages(self, setup_info, vcs, general):
        subpackages = []
        config = setup_info.config

        if self.kind == "setups":
            couplings = setup_info.get_config_entry(self, "couplings")
            if couplings:
                for coupling in couplings:
                    newpackage = software_package(coupling, setup_info, vcs, general)
                    subpackages += newpackage.get_subpackages(setup_info, vcs, general)

        if self.kind == "couplings":
            components = setup_info.get_config_entry(self, "components")
            if components:
                for component in components:
                    found = False
                    for package in subpackages:
                        if component == package.raw_name:
                            found = True
                            break
                    if not found:
                        newpackage = software_package(
                            component, setup_info, vcs, general
                        )
                        subpackages += newpackage.get_subpackages(
                            setup_info, vcs, general
                        )
                        subpackages.append(newpackage)

        elif self.kind == "components":
            requirements = setup_info.get_config_entry(self, "requires")
            if requirements:
                for component in requirements:
                    found = False
                    for package in subpackages:
                        if component == package.raw_name:
                            found = True
                            break
                    if not found:
                        newpackage = software_package(
                            component, setup_info, vcs, general
                        )
                        subpackages += newpackage.get_subpackages(
                            setup_info, vcs, general
                        )
                        subpackages.append(newpackage)

        # if self.kind == "couplings":
        #    components = setup_info.get_config_entry(self, "components")
        #    if components:
        #        for component in components:
        #            comp_tupel = software_package(component, setup_info, vcs, general)
        #            if comp_tupel not in subpackages:
        #                subpackages.append(comp_tupel)
        # elif self.kind == "setups":
        #    couplings = setup_info.get_config_entry(self, "couplings")
        #    if couplings:
        #        for coupling in couplings:
        #            for component in config["couplings"][coupling]["components"]:
        #                 found = False
        #                 for package in subpackages:
        #                    if component == package.raw_name:
        #                        found = True
        #                        break
        #                if not found:
        #                    subpackages.append(
        #                        software_package(component, setup_info, vcs, general)
        #                    )
        # elif self.kind == "components":
        #    requirements = setup_info.get_config_entry(self, "requires")
        ##    if requirements:
        #        for requirement in requirements:
        #            found = False
        #            if subpackages:
        #                for package in subpackages:
        #                    if requirement == package.raw_name:
        #                        found = True
        #                        break
        #                if not found:
        #                    subpackages.append(
        #                      software_package(requirement, setup_info, vcs, general)
        #                    )
        #            else:
        #                subpackages.append(
        #                  software_package(requirement, setup_info, vcs, general)
        #                )
        return subpackages

    def get_comp_type(self, setup_info):
        exec_names = setup_info.get_config_entry(self, "install_bins")
        if exec_names:
            if type(exec_names) == str:
                exec_names = [exec_names]
            return "bin", exec_names
        exec_names = setup_info.get_config_entry(self, "install_libs")
        if exec_names:
            if type(exec_names) == str:
                exec_names = [exec_names]
            return "lib", exec_names
        return "bin", []

    def get_repo_info(self, setup_info, vcs):
        repo = branch = repo_type = None
        for check_repo in vcs.known_repos:
            repo = setup_info.get_config_entry(self, check_repo + "-repository")
            if repo:
                repo_type = check_repo
                break
        branch = setup_info.get_config_entry(self, "branch")

        # kh 11.09.20 support git options like --recursive
        repo_options = setup_info.get_config_entry(self, "repo_options")

        return repo_type, repo, branch, repo_options

    def get_command_list(self, setup_info, vcs, general):
        command_list = {}
        for todo in self.targets:
            if todo in vcs.known_todos:
                commands = vcs.assemble_command(self, todo, setup_info, general)
            else:
                commands = setup_info.get_config_entry(self, todo + "_command")
            if commands:
                if type(commands) == str:
                    commands = [commands]
                if not todo == "get":
                    commands.insert(0, "cd " + self.destination)
                    commands.append("cd ..")
            if todo == "get":
                if self.coupling_changes:
                    commands = []
                    for change in self.coupling_changes:
                        commands.append(change)
            command_list.update({todo: commands})
        return command_list

    def output(self):
        print()
        print(self.raw_name)
        print(
            "    Model:", self.model, ", Version:", self.version, ", Kind:", self.kind
        )
        if self.subpackages:
            for package in self.subpackages:
                print("    Subpackage: ", package.raw_name)
        if self.targets:
            print("    Targets: ", self.targets)
        if self.repo_type:
            print(
                "    Repo_type: ",
                self.repo_type,
                ", repo: ",
                self.repo,
                ", branch: ",
                self.branch,
                # kh 11.09.20 support git options like --recursive
                ", repo_options: ",
                self.repo_options,
            )
        if self.bin_type:
            print("    Bin_type: ", self.bin_type, ", bin_names: ", self.bin_names)
        if self.command_list:
            print("    Commands:")
            for todo in self.command_list.keys():
                print("        ", todo, self.command_list[todo])
        if self.coupling_changes:
            print("    Coupling Changes:")
            for todo in self.coupling_changes:
                print("        ", todo)
