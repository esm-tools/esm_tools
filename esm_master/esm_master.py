#!/usr/bin/env python
# import fileinput, os, sys, getopt

import sys, copy, os, re
import argparse

import esm_rcfile

import esm_parser
import esm_environment

from .cli import verbose

######################################################################################
##################################### globals ########################################
######################################################################################

function_path = esm_rcfile.get_rc_entry("FUNCTION_PATH")
ESM_MASTER_DIR=os.getenv("PWD")

components_yaml = function_path + '/esm_master/setups2models.yaml'
config_yaml = function_path + '/esm_master/esm_master.yaml'
vcs_folder = function_path + '/vcs'

overall_conf_file = esm_rcfile.rcfile

######################################################################################
############################## class "general_infos" #################################
######################################################################################


class general_infos:
    def __init__(self):

        self.config = esm_parser.yaml_file_to_dict(config_yaml)
        self.emc = self.read_and_update_conf_files()
        self.meta_todos, self.meta_command_order = self.get_meta_command()
        self.display_kinds = self.get_display_kinds()

        if verbose > 1:
            self.output()

    def read_and_update_conf_files(self):
        complete = True
        emc = {}
        for conffile in [overall_conf_file]:
            if os.path.isfile(conffile):
                with open(conffile) as myfile:
                    for line in myfile:
                        name, var = line.partition("=")[::2]
                        emc[name.strip()] = var.strip()
        if "basic_infos" in self.config.keys():
            for basic_info in self.config["basic_infos"]:
                question = self.config["basic_infos"][basic_info]["question"]
                default = self.config["basic_infos"][basic_info]["default"]
                if not basic_info in emc.keys():
                    if complete:
                        print("The configuration files are incomplete or non-existent.")
                        print(
                            "Please answer the following questions to configure esm-tools:"
                        )
                        print("(Hit enter to accept default values.)")
                        complete = False
                    user_input = input(question + " (default = " + default + "): ")
                    if user_input.strip() == "":
                        user_input = default
                    emc.update({basic_info.strip(): user_input.strip()})
        if not complete:
            with open(overall_conf_file, "w") as new_conf_file:
                for oldentry in emc.keys():
                    new_conf_file.write(oldentry + "=" + emc[oldentry] + "\n")
        return emc

    def get_meta_command(self):
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
        for key in self.emc:
            print(key + ": " + str(self.emc[key]))
        print("Meta commands: ")
        for key in self.meta_command_order:
            print("    " + key + ": " + str(self.meta_command_order[key]))


######################################################################################
########################### class "version_control_infos" ############################
######################################################################################


class version_control_infos:
    def __init__(self):
        self.config = {}
        vcs_files = [f for f in os.listdir(vcs_folder)]
        self.known_repos = []
        for vcs_file in vcs_files:
            if os.path.isfile(vcs_folder + "/" + vcs_file):
                repo_type = vcs_file.replace(".yaml", "")
                self.config.update(
                    {
                        repo_type: esm_parser.yaml_file_to_dict(
                            vcs_folder + "/" + vcs_file
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
        if verbose > 1:
            self.output()

    def assemble_command(self, package, todo, setup_info, general):
        if package.repo_type and package.repo:
            if todo in setup_info.meta_todos:
                return None
            try:
                raw_command = self.config[package.repo_type][todo + "_command"]
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
            except:
                print("Sorry, no " + todo + "_command defined for " + package.repo_type)
                sys.exit(42)
            if type(package.repo) == list:
                repo = package.repo[0]
            else:
                repo = package.repo
            if "https://gitlab.dkrz.de" in repo:
                repo = "https://" + general.emc["GITLAB_DKRZ_USER_NAME"] +"@"+ repo.replace("https://", "")
            raw_command = raw_command.replace("${repository}", repo)
            if todo == "get":
                if package.destination:
                    raw_command = raw_command + " " + package.destination
                else:
                    raw_command = raw_command + " " + package.raw_name
        else:
            raw_command = None
        return raw_command

    def output(self):
        print()
        esm_parser.pprint_config(self.config)
        print("Known repos: " + str(self.known_repos))
        print("Known vcs-commands: " + str(self.known_todos))


######################################################################################
############################## class "software_package" ##############################
######################################################################################


def replace_var(var, tag, value):
    if var and tag and value:
        if type(var) == str:
            return var.replace("${" + tag + "}", value)
        elif type(var) == list:
            newlist = []
            for entry in var:
                newlist.append(replace_var(entry, tag, value))
            return newlist


class software_package:
    def __init__(
        self, raw, setup_info, vcs, general, no_infos=False
    ):  # model_and_version):
        if type(raw) == str:
            dummy, self.kind, self.model, self.version, dummy2, self.raw_name = setup_info.split_raw_target(
                raw, setup_info
            )
        else:  # tupel:
            (self.kind, self.model, self.version) = raw
            self.raw_name = setup_info.assemble_raw_name(
                None, self.kind, self.model, self.version
            )

        self.tag = None
        if not no_infos:
            self.fill_in_infos(setup_info, vcs, general)
        else:
            self.targets = self.subpackages = None
            self.repo_type = self.repo = self.branch = None
            self.bin_type = None
            self.bin_names = [None]
            self.command_list = None
            self.destination = None
            self.coupling_changes = None

    def fill_in_infos(self, setup_info, vcs, general):

        self.targets = self.get_targets(setup_info, vcs)
        self.subpackages = self.get_subpackages(setup_info, vcs, general)
        self.complete_targets(setup_info)
        self.repo_type, self.repo, self.branch = self.get_repo_info(setup_info, vcs)
        self.destination = setup_info.get_config_entry(self, "destination")
        if not self.destination:
            self.destination = self.raw_name

        self.coupling_changes = self.get_coupling_changes(setup_info)
        self.repo = replace_var(self.repo, self.model+".version", self.version)
        self.branch = replace_var(self.branch, self.model+".version", self.version)

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
                        these_changes = config["couplings"][coupling]["coupling_changes"]       
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
                        newpackage = software_package(component, setup_info, vcs, general)
                        subpackages += newpackage.get_subpackages(setup_info, vcs, general)
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
                        newpackage = software_package(component, setup_info, vcs, general)
                        subpackages += newpackage.get_subpackages(setup_info, vcs, general)
                        subpackages.append(newpackage)


        #if self.kind == "couplings":
        #    components = setup_info.get_config_entry(self, "components")
        #    if components:
        #        for component in components:
        #            comp_tupel = software_package(component, setup_info, vcs, general)
        #            if comp_tupel not in subpackages:
        #                subpackages.append(comp_tupel)
        #elif self.kind == "setups":
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
        #elif self.kind == "components":
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
        return repo_type, repo, branch

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
                    commands.insert(0, "cd "+self.destination)
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
            )
        if self.bin_type:
            print("    Bin_type: ", self.bin_type, ", bin_names: ", self.bin_names)
        if self.command_list:
            print("    Commands:")
            for todo in self.command_list.keys():
                print ("        ", todo, self.command_list[todo])
        if self.coupling_changes:
            print ("    Coupling Changes:")
            for todo in self.coupling_changes:
                print ("        ", todo)
    



######################################################################################
################################# class "task" #######################################
######################################################################################


class task:
    def __init__(self, raw, setup_info, vcs, general):
        if raw == "default":
            raw = ""
        if raw == "drytestall":
            # panic
            for package in setup_info.all_packages:
                for todo in package.targets:
                    try:
                        print(todo + "-" + package.raw_name)
                        newtask = task(todo + "-" + package.raw_name, setup_info, vcs)
                        newtask.output_steps()
                    except:
                        print("Problem found with target " + newtask.raw_name)
                        sys.exit(1)
            sys.exit(0)

        if type(raw) == str:
            self.todo, kind, model, version, self.only_subtask, self.raw_name = setup_info.split_raw_target(
                raw, setup_info
            )
            self.package = software_package(
                (kind, model, version), setup_info, vcs, general
            )
        else:  # tupel:
            (self.todo, kind, model, version, self.only_subtask) = raw
            self.package = software_package(
                (kind, model, version), setup_info, vcs, general
            )
            self.raw_name = setup_info.assemble_raw_name(
                self.todo, kind, model, version
            )

        if not self.todo in setup_info.meta_todos:
            self.check_if_target(setup_info)

        self.subtasks=self.get_subtasks(setup_info, vcs, general)
        self.only_subtask=self.validate_only_subtask()
        self.ordered_tasks=self.order_subtasks(setup_info, vcs, general)
   
        self.will_download = self.check_if_download_task(setup_info)
        self.folders_after_download=self.download_folders()
        self.binaries_after_compile=self.compile_binaries()
        self.dir_list=self.list_required_dirs()
        self.command_list, self.shown_command_list =self.assemble_command_list()

        if verbose > 1:
            self.output()

    def get_subtasks(self, setup_info, vcs, general):
        subtasks = []
        if self.todo in setup_info.meta_todos:
            todos = setup_info.meta_command_order[self.todo]
        else:
            todos = [self.todo]
        for todo in todos:
            for subpackage in self.package.subpackages:
                if todo in subpackage.targets:
                    subtasks.append(
                        task(
                            (
                                todo,
                                subpackage.kind,
                                subpackage.model,
                                subpackage.version,
                                None,
                            ),
                            setup_info,
                            vcs,
                            general,
                        )
                    )
        #if subtasks == [] and self.todo in setup_info.meta_todos:
        if self.todo in setup_info.meta_todos:
            for todo in todos:
                if todo in self.package.targets:
                    subtasks.append(
                        task(
                            (
                                todo,
                                self.package.kind,
                                self.package.model,
                                self.package.version,
                                None,
                            ),
                            setup_info,
                            vcs,
                            general,
                        )
                    )
        return subtasks

    def validate_only_subtask(self):
        only = None
        if self.only_subtask:
            only = []
            for task in self.subtasks:
                if task.package.raw_name.startswith(self.only_subtask):
                    only.append(task)
            if self.package.raw_name.startswith(self.only_subtask):
                self.subtasks = []
                return None
            if only == []:
                print()
                print(
                    "Given subtask "
                    + self.only_subtask
                    + " is not a valid subtask of package "
                    + self.raw_name
                    + "."
                )
                print()
                sys.exit(0)

        return only

    def order_subtasks(self, setup_info, vcs, general):
        subtasks = self.subtasks
        if self.only_subtask:
            if self.only_subtask == "NONE":
                return []
            elif type(self.only_subtask) == str:
                return [self.only_subtask]
            else:
                subtasks = self.only_subtask
        if subtasks == []:
            return [self]
        if self.todo in setup_info.meta_todos:
            todos = setup_info.meta_command_order[self.todo]
        else:
            todos = [self.todo]
        
        ordered_tasks = []
        for todo in todos:
            for task in subtasks:
                if task.todo == todo and task.package.bin_type == "lib":
                    ordered_tasks.append(task)
            for task in subtasks:
                if task.todo == todo and not task.package.bin_type == "lib":
                    ordered_tasks.append(task)#
        if self.package.kind=="components" and not self.only_subtask:
            ordered_tasks.append(self)
        return ordered_tasks

    def check_if_download_task(self, setup_info):
        if self.todo == "get":
            return True
        if self.todo in setup_info.meta_todos:
            if "get" in setup_info.meta_command_order[self.todo]:
                return True
        return False

    def download_folders(self):
        #if self.package.kind in ["setups", "couplings"]:
        if self.package.subpackages:
            dir_list = [self.package.raw_name]
            for task in self.ordered_tasks:
                if self.package.raw_name+"/"+task.package.destination not in dir_list:
                    dir_list.append(self.package.raw_name+"/"+task.package.destination)
        else:
            dir_list = []
            for task in self.ordered_tasks:
                if task.package.destination not in dir_list:
                    dir_list.append(task.package.destination)
        return dir_list

    def compile_binaries(self):
        file_list = []
        for task in self.ordered_tasks:
            for binfile in task.package.bin_names:
                if self.package.raw_name+"/"+task.package.bin_type+"/"+binfile.split("/")[-1] not in file_list:
                    file_list.append(self.package.raw_name+"/"+task.package.bin_type+"/"+binfile.split("/")[-1])
        return file_list

    def list_required_dirs(self):
        toplevel = self.package.raw_name
        if self.package.kind in ["setups", "couplings"] and self.will_download:
            dir_list = [self.package.raw_name]
        else:
            dir_list = []
        for task in self.ordered_tasks:
            if task.todo == "comp":
                if task.package.bin_names:
                    newdir = toplevel + "/" + task.package.bin_type
                    if not newdir in dir_list:
                        dir_list.append(newdir)
        return dir_list

    def assemble_command_list(self):
        command_list = []
        toplevel = self.package.destination
        #if self.package.kind in ["setups", "couplings"]:
        if self.package.subpackages: #???
            command_list.append("mkdir -p " + toplevel)
            command_list.append("cd " + toplevel)
            toplevel = "."
        real_command_list = command_list.copy()
        for task in self.ordered_tasks:
            if task.todo in ["get"]:
                if not task.package.command_list[task.todo] == None:
                    for command in task.package.command_list[task.todo]:
                        command_list.append(command)
                        real_command_list.append(command)

        if self.package.coupling_changes:
            for change in self.package.coupling_changes:
                command_list.append(change)
                real_command_list.append(change)






        for task in self.ordered_tasks:
            if not task.todo in ["get"]:
                if task.todo in ["conf", "comp"]:
                    #if self.package.kind in ["setups", "couplings"]:
                    if not task.package.kind in ["setups", "couplings"]:
                        if self.package.subpackages: 
                            real_command_list.append("cp ../"+task.raw_name+"_script.sh .")
                        real_command_list.append("./"+task.raw_name+"_script.sh")
                else:
                    if not task.package.command_list[task.todo] == None:
                        for command in task.package.command_list[task.todo]:
                            real_command_list.append(command)
                if not task.package.command_list[task.todo] == None:
                    for command in task.package.command_list[task.todo]:
                        command_list.append(command)
                if task.todo == "comp":
                    if task.package.bin_names:
                        command_list.append("mkdir -p "+toplevel+"/"+task.package.bin_type)
                        real_command_list.append("mkdir -p "+toplevel+"/"+task.package.bin_type)
                        for binfile in task.package.bin_names:
                            command_list.append("cp "+task.package.destination+"/"+binfile + " "+ toplevel+"/"+task.package.bin_type)
                            real_command_list.append("cp "+task.package.destination+"/"+binfile + " "+ toplevel+"/"+task.package.bin_type)
                elif task.todo == "clean":
                    if task.package.bin_names:
                        for binfile in task.package.bin_names:
                            command_list.append("rm "+toplevel+"/"+task.package.bin_type+"/"+binfile.split("/", -1)[-1])
                            real_command_list.append("rm "+toplevel+"/"+task.package.bin_type+"/"+binfile.split("/", -1)[-1])
        if self.package.kind in ["setups", "couplings"]:
            command_list.append("cd ..")
            real_command_list.append("cd ..")

        return real_command_list, command_list
            

    def check_if_target(self, setup_info):
        if not setup_info.has_target2(self.package, self.todo):
            setup_info.output_available_targets(self.raw_name)
            sys.exit(0)

    def check_requirements(self):
        if self.will_download:
            return True
        requirements = self.folders_after_download
        for folder in requirements:
            if not os.path.isdir(folder):
                print ()
                print ("Missing folder "+ folder + " detected. Please run 'make get-"+self.package.raw_name+ "' first.")
                print ()
                sys.exit(0)
        return True

    def validate(self):
        self.check_requirements()

    def execute(self, env):
        for task in self.ordered_tasks:
            if task.todo in ["conf", "comp"]:
                newfile = env.add_commands(
                    task.package.command_list[task.todo], task.raw_name
                )
                if os.path.isfile(newfile):
                    os.chmod(newfile, 0o755)
        for command in self.command_list:
            if command.startswith("mkdir"):
                os.system(command)
            elif command.startswith("cp "):
                os.system(command)
            elif command.startswith("cd ") and not ";" in command:
                os.chdir(command.replace("cd ", ""))
            else:
                os.system(command)

        #print ("Still here")
        #try:
        #    sys.exit(0)
        #except:
        #    pass

    def output(self):
        print()
        subtasklist = []
        osubtasklist = []
        self.package.output()
        print("    Todo: ", self.todo)
        if self.only_subtask:
            if self.only_subtask == "NONE":
                print("    NO VALID SUBTASKS!!!")
            else:
                for subtask in self.only_subtask:
                    print("    Only Subtask:", subtask.package.raw_name)
        for subtask in self.subtasks:
            subtasklist.append(subtask.raw_name)
        for osubtask in self.ordered_tasks:
            osubtasklist.append(osubtask.raw_name)
        if not subtasklist == []:
            print("    Subtasks:", subtasklist)
        if not osubtasklist == []:
            print("    Ordered Subtasks:", osubtasklist)
        self.output_steps()
        if not self.folders_after_download == []:
            print("    The following folders should exist after download:")
            for folder in self.folders_after_download:
                print("        ", folder)
        if not self.binaries_after_compile == []:
            print("    The following files should exist after compiling:")
            for binfile in self.binaries_after_compile:
                print("        ", binfile)

    def output_steps(self):
        if not self.command_list == []:
            print ("    Executing commands in this order:")
            for command in self.shown_command_list:
                print ("        ", command)




######################################################################################
########################### class "setup_and_model_infos" ############################
######################################################################################


class setup_and_model_infos:
    def __init__(self, vcs, general):
        self.config = esm_parser.yaml_file_to_dict(components_yaml)
        self.model_kinds = list(self.config.keys())
        self.meta_todos = general.meta_todos
        self.meta_command_order = general.meta_command_order
        self.display_kinds = general.display_kinds

        self.model_todos = []
        for kind in self.model_kinds:
            for model in self.config[kind].keys():
                version = None
                if "choose_versions" in self.config[kind][model]:
                    for version in self.config[kind][model]["choose_versions"]:
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

        if verbose > 1:
            self.output()


    def write_minimal_user_config(self):
        user_config = {}

        for model in self.config["components"]:
            version = self.config["components"][model]["version"]
            model_dir = self.config["components"][model]["model_dir"]
            user_config.update({model: 
                                    {
                                     "model": model,
                                     "version": version,
                                     "model_dir": model_dir
                                     }
                                })
        if "setups" in self.config:
            coupled = "true"
            setup = list(self.config["setups"])[0]
            version = self.config["setups"][setup]["version"]
            model_dir = self.config["setups"][setup]["model_dir"]

        else:
            coupled = "false"
            setup = list(self.config["components"])[0]
            version = self.config["components"][setup]["version"]


        user_config.update({"general":
                                {
                                "jobtype": "compute",
                                "account": "none",
                                "setup_name": setup,
                                "version" : version,
                                "coupled" : "false",
                                "initial_date": "2000-01-01",
                                "final_date": "2000-01-02",
                                "compute_time": "00:00:01",
                                "model_dir": model_dir,
                                "base_dir": "/on/a/road/to/nowwhere"
                                }
                            })

        return user_config



    def append_to_conf(self, target, reduced_config,  toplevel = ""):
        (todo, kind, model, version, only_subtarget, raw ) = self.split_raw_target(target, self)
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
                    reduced_config = self.append_to_conf(coupling, reduced_config, toplevel)
        elif kind == "couplings":
            if toplevel == "":
                toplevel = model + "-" + version
            reduced_config[model]["model_dir"] = ESM_MASTER_DIR + "/" + toplevel
            if "components" in self.config[kind][model]:
                for component in self.config[kind][model]["components"]:
                    reduced_config = self.append_to_conf(component, reduced_config, toplevel)
        elif kind == "components":
            sep = ""
            if toplevel == "":
                if "requires" in self.config[kind][model]:
                    toplevel = model + "-" + version 
                    sep = "/"
            else:
                sep = "/"

            if "destination" in reduced_config[model]:
                reduced_config[model]["model_dir"] = ESM_MASTER_DIR + "/" + toplevel + sep + reduced_config[model]["destination"]
            else:
                reduced_config[model]["model_dir"] = ESM_MASTER_DIR + "/" + toplevel + sep + model + "-" + version

            if "requires" in self.config[kind][model]:
                for requirement in self.config[kind][model]["requires"]:
                    reduced_config = self.append_to_conf(requirement, reduced_config, toplevel)

        return reduced_config 


    #def reduce(self, target, env):
    def reduce(self, target):
        blacklist = [
            re.compile(entry)
            for entry in ["computer.*"]
        ]

        reduced_config={}
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
                    new_config[reduced_config[headline]["kind"]] = {headline: reduced_config[headline]}
                else:
                    new_config[reduced_config[headline]["kind"]].update({headline: reduced_config[headline]})
            else:
                new_config.update({headline: reduced_config[headline]})

        #esm_parser.pprint_config(new_config)
        #sys.exit(0)
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
        display_info = []
        if search_keyword == "":
            display_info = self.all_packages
        else:
            for package in self.all_packages:
                for target in package.targets:
                    if search_keyword in target + "-" + package.raw_name:
                        if package not in display_info:
                            display_info.append(package)

        if display_info == []:
            print()
            print(
                "No targets found for keyword "
                + search_keyword
                + ". Type 'make' to get a full list"
            )
            print("of available targets.")
            print()
        elif display_info == self.all_packages:
            print()
            print(
                "Master Tool for ESM applications, including download and compiler wrapper functions"
            )
            print("		originally written by Dirk Barbi (dirk.barbi@awi.de)")
            print(
                "       further developed as OpenSource, coordinated and maintained at AWI"
            )
            print()
            print(
                "Obtain from:         https://gitlab.dkrz.de/esm-tools/esm-master.git"
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
        sorted_display = {}
        for kind in self.display_kinds:
            for package in display_info:
                if package.kind == kind:
                    if not kind in sorted_display.keys():
                        sorted_display.update({kind: {}})
                    if not package.model in sorted_display[kind]:
                        sorted_display[kind].update({package.model: []})
                    if package.version:
                        sorted_display[kind][package.model].append(
                            package.version + ": " + str(package.targets)
                        )
                    else:
                        sorted_display[kind][package.model].append(str(package.targets))

        for kind in sorted_display.keys():
            print(kind + ": ")
            for model in sorted_display[kind]:
                if len(sorted_display[kind][model]) == 1:
                    print("    " + model + ": " + sorted_display[kind][model][0])
                else:
                    print("    " + model + ": ")
                    for version in sorted_display[kind][model]:
                        print("       " + version)

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


