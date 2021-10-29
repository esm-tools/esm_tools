import os
import sys
import subprocess
import shlex  # contains shlex.split that respects quoted strings

# deniz: it is better to use more pathlib in the future so that dir/path
# operations will be more portable (supported since Python 3.4, 2014)
import pathlib

from .software_package import software_package

import esm_environment
import esm_plugin_manager


# Yes, Type Hints. Python >= 3.5 supports them. Small steps towards stability,
# until Paul goes crazy and redoes everything in Go. Or Rust. Or Brainfuck
# (yes, that's not made up: https://en.wikipedia.org/wiki/Brainfuck)
# Docs for typing: https://docs.python.org/3/library/typing.html
def install(package: str) -> None:
    """
    Checks if a package is already installed in the system and if it's not, then it
    installs it.

    Parameters
    ----------
    package : str
        Name of the package or get operation. Can be a package name (e.g.
        ``numpy``) or a full pip address (e.g.
        ``git@https://github.com/esm-tools/esm_tools.git``)

    Returns
    -------
    None
    """
    package_name = package.split("/")[-1].replace(".git", "")
    installed_packages = esm_plugin_manager.find_installed_plugins()
    if not package_name in installed_packages:
        try:
            subprocess.check_call([sys.executable, "-m", "pip", "install", package])
        except OSError:  # PermissionDeniedError would be nicer...
            subprocess.check_call(
                [sys.executable, "-m", "pip", "install", "--user", package]
            )


######################################################################################
################################# class "task" #######################################
######################################################################################


class Task:
    """What you can do with a software_package, e.g. comp-awicm-2.0"""

    def __init__(self, raw, setup_info, vcs, general, complete_config, parsed_args):
        if raw == "default":
            raw = ""
        if raw == "drytestall":
            # panic
            for package in setup_info.all_packages:
                for todo in package.targets:
                    try:
                        print(todo + "-" + package.raw_name)
                        newtask = Task(
                            todo + "-" + package.raw_name, setup_info, vcs, parsed_args
                        )
                        newtask.output_steps()
                    except:
                        print("Problem found with target " + newtask.raw_name)
                        sys.exit(1)
            sys.exit(0)

        self.required_plugins = {}
        for key in complete_config:
            if "required_plugins" in complete_config[key]:
                self.required_plugins[key] = complete_config[key]["required_plugins"]
        # Initialize the ``already_installed_plugins`` attribute if it does not exist
        general.already_installed_plugins = self.already_installed_plugins = getattr(
            general, "already_installed_plugins", []
        )

        if type(raw) == str:
            (
                self.todo,
                kind,
                model,
                version,
                self.only_subtask,
                self.raw_name,
            ) = setup_info.split_raw_target(raw, setup_info)
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

        if kind == "components":
            self.env = esm_environment.esm_environment.EnvironmentInfos(
                "compiletime", complete_config, model
            )
        else:
            self.env = None
        if not self.todo in setup_info.meta_todos:
            self.check_if_target(setup_info)

        self.subtasks = self.get_subtasks(
            setup_info, vcs, general, complete_config, parsed_args
        )
        self.only_subtask = self.validate_only_subtask()
        self.ordered_tasks = self.order_subtasks(setup_info, vcs, general)

        self.will_download = self.check_if_download_task(setup_info)
        self.folders_after_download = self.download_folders()
        self.binaries_after_compile = self.compile_binaries()
        self.dir_list = self.list_required_dirs()
        self.command_list, self.shown_command_list = self.assemble_command_list()

        if parsed_args.get("verbose", False):
            self.output()

    def get_subtasks(self, setup_info, vcs, general, complete_config, parsed_args):
        subtasks = []
        if self.todo in setup_info.meta_todos:
            todos = setup_info.meta_command_order[self.todo]
        else:
            todos = [self.todo]
        for todo in todos:
            for subpackage in self.package.subpackages:
                if todo in subpackage.targets:
                    subtasks.append(
                        Task(
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
                            complete_config,
                            parsed_args,
                        )
                    )
        if subtasks == [] and self.todo in setup_info.meta_todos:
            # if self.todo in setup_info.meta_todos:
            for todo in todos:
                if todo in self.package.targets:
                    subtasks.append(
                        Task(
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
                            complete_config,
                            parsed_args,
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
                    ordered_tasks.append(task)  #
        if self.package.kind == "components" and not self.only_subtask:
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
        # if self.package.kind in ["setups", "couplings"]:
        if self.package.subpackages:
            dir_list = [self.package.raw_name]
            for task in self.ordered_tasks:
                if (
                    self.package.raw_name + "/" + task.package.destination
                    not in dir_list
                ):
                    dir_list.append(
                        self.package.raw_name + "/" + task.package.destination
                    )
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
                if (
                    self.package.raw_name
                    + "/"
                    + task.package.bin_type
                    + "/"
                    + binfile.split("/")[-1]
                    not in file_list
                ):
                    file_list.append(
                        self.package.raw_name
                        + "/"
                        + task.package.bin_type
                        + "/"
                        + binfile.split("/")[-1]
                    )
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
                    if newdir not in dir_list:
                        dir_list.append(newdir)
        return dir_list

    def assemble_command_list(self):
        command_list = []
        toplevel = self.package.destination
        # if self.package.kind in ["setups", "couplings"]:
        if self.package.subpackages:  # ???
            command_list.append("mkdir -p " + toplevel)
            command_list.append("cd " + toplevel)
            toplevel = "."
        real_command_list = command_list.copy()
        for task in self.ordered_tasks:
            if task.todo in ["get"]:
                if task.package.command_list[task.todo] is not None:
                    for command in task.package.command_list[task.todo]:
                        command_list.append(command)
                        real_command_list.append(command)

        if self.package.coupling_changes:
            for change in self.package.coupling_changes:
                command_list.append(change)
                real_command_list.append(change)

        for task in self.ordered_tasks:
            if task.todo not in ["get"]:
                if task.todo in ["conf", "comp"]:
                    # if self.package.kind in ["setups", "couplings"]:
                    if task.package.kind not in ["setups", "couplings"]:
                        if self.package.subpackages:
                            real_command_list.append(
                                "cp ../" + task.raw_name + "_script.sh ."
                            )
                        real_command_list.append("./" + task.raw_name + "_script.sh")
                else:
                    if task.package.command_list[task.todo] is not None:
                        for command in task.package.command_list[task.todo]:
                            real_command_list.append(command)
                if task.package.command_list[task.todo] is not None:
                    for command in task.package.command_list[task.todo]:
                        command_list.append(command)
                if task.todo == "comp":
                    if task.package.bin_names:
                        command_list.append(
                            "mkdir -p " + toplevel + "/" + task.package.bin_type
                        )
                        real_command_list.append(
                            "mkdir -p " + toplevel + "/" + task.package.bin_type
                        )
                        for binfile in task.package.bin_names:
                            # PG: Only copy if source and dest aren't the same!
                            # (Prevents cp: ‘/temp/test.txt’ and
                            # ‘/temp/test/test.txt’ are the same file)
                            toplevel_bin_path = (
                                toplevel
                                + "/"
                                + task.package.bin_type
                                + "/"
                                + binfile.split("/")[-1]
                            )
                            # MA: If there are already commands that will clean the
                            # bin folder, the following `if` is required to be true
                            clean_command_list = ["rm -f " + toplevel_bin_path]
                            clean_command = any(
                                cc in command_list for cc in clean_command_list
                            )

                            # deniz: bug fix for the fesom compilation issue
                            # at some point these string concats need to be
                            # replaced by pathlib
                            bin_path_target = pathlib.Path(
                                toplevel + "/" + task.package.bin_type
                            )
                            binary_file_path = pathlib.Path(
                                task.package.destination + "/" + binfile
                            )
                            binary_file_parent = binary_file_path.parent
                            # path2bin_origin = "/".join(binary_file_path.split('/')[:-1])

                            # deniz: don't copy the files if the paths are same.
                            # I think pathlib.Path.resolve() is a better option
                            # than simple string comparison
                            should_copy_files = (
                                binary_file_parent.resolve()
                                != bin_path_target.resolve()
                            )

                            # add the remaining conditions
                            should_copy_files = should_copy_files and (
                                not os.path.exists(toplevel_bin_path) or clean_command
                            )

                            if should_copy_files:
                                # deniz: I think " ".join() is a more Pythonic
                                # way to construct the command strings
                                cmd_str = "  ".join(
                                    ["cp", str(binary_file_path), str(bin_path_target)]
                                )
                                command_list.append(cmd_str)
                                real_command_list.append(cmd_str)

                elif task.todo == "clean":
                    if task.package.bin_names:
                        for binfile in task.package.bin_names:
                            command_list.append(
                                f"rm -f {toplevel}/{task.package.bin_type}/{binfile.split('/', -1)[-1]}"
                            )
                            real_command_list.append(
                                f"rm -f {toplevel}/{task.package.bin_type}/{binfile.split('/', -1)[-1]}"
                            )
        if task.todo in ["comp"]:
            for component in self.required_plugins:
                for plugin in self.required_plugins[component]:
                    # Install the plugin if is not already installed
                    if plugin not in self.already_installed_plugins:
                        # Actually only works because Paul put the gfw_creator
                        # required plugin for awiesm onto PyPI...
                        install(plugin)
                        self.already_installed_plugins.append(plugin)

        if self.package.kind in ["setups", "couplings"]:
            command_list.append("cd ..")
            real_command_list.append("cd ..")

        return real_command_list, command_list

    def cleanup_script(self):
        try:
            os.remove("./dummy_script.sh")
        except OSError:
            print("No dummy script to remove!")
        for task in self.ordered_tasks:
            if task.todo in ["conf", "comp"]:
                try:
                    os.remove("./" + task.raw_name + "_script.sh")
                except OSError:
                    print("No file to remove for ", task.raw_name)

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
                print()
                print(
                    "Missing folder "
                    + folder
                    + " detected. Please run 'make get-"
                    + self.package.raw_name
                    + "' first."
                )
                print()
                sys.exit(0)
        return True

    def validate(self):
        self.check_requirements()

    def execute(self, ignore_errors=False):
        for task in self.ordered_tasks:
            if task.todo in ["conf", "comp"]:
                if task.package.kind == "components":
                    task.env.write_dummy_script()
                    newfile = task.env.add_commands(
                        task.package.command_list[task.todo], task.raw_name
                    )
                    if os.path.isfile(newfile):
                        os.chmod(newfile, 0o755)
        for command in self.command_list:
            if command.startswith("mkdir"):
                # os.system(command)
                subprocess.run(command.split(), check=not ignore_errors)
            elif command.startswith("cp "):
                subprocess.run(command.split(), check=not ignore_errors)
            elif command.startswith("cd ") and ";" not in command:
                os.chdir(command.replace("cd ", ""))
            # deniz: add pipe support
            elif "|" in command:
                # if there is a pipe in the command, then separate these in to
                # two parts. Eg. curl foo.tar.gz | tar zx
                curl_command, pipe_command = command.split("|")
                curl_process = subprocess.Popen(
                    curl_command.split(), stdout=subprocess.PIPE
                )
                output = subprocess.check_output(
                    pipe_command.split(), stdin=curl_process.stdout
                )
                curl_process.wait()
            else:
                # os.system(command)
                # deniz: I personally did not like the iterator and the list
                # having the same name. for com in command.split(';') would be
                # better IMHO
                for command in command.split(";"):
                    # seb-wahl: use shlex split as sed commands that use spaces
                    # need to be quoted, shlex split doesn't split quoted
                    # strings on spaces
                    # example: sed -i '/COUPLENEMOFOCI = /s/.FALSE./.TRUE./g' oifs-43r3-foci/src/ifs/module/yommcc.F90
                    # will fail if the "'" is removed
                    command_spl = shlex.split(command)
                    if "cd" == command_spl[0]:
                        os.chdir(command_spl[1])
                    else:
                        subprocess.run(
                            command_spl,
                            check=True,
                            shell=(
                                command.startswith("./") and command.endswith(".sh")
                            ),
                        )

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
            print("    Executing commands in this order:")
            for command in self.shown_command_list:
                print("        ", command)
