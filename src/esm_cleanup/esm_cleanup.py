disclaimer = """
\033[91m
====================
     DISCLAIMER
====================
\033[0m
ESM_CLEANUP is a tool designed to reduce space blocked by simulations produced using ESM-Tools.
Before continuing, please be aware that automatically removing data from your experiment folders
includes the risk of loosing information that you might need in the future. Though some things
might be relatively safe to remove, other (like restart files) might not.
By continuing, you accept that neither the ESM-Tools developers nor anyone else but yourself 
take the responsibility of what happens to your data. 
"""

import os
import sys
import shutil
import questionary
import yaml
import time

per_model_folders = ["restart", "outdata", "input", "forcing", "bin", "config", "log"]


def is_experiment_folder(checkpath):
    all_files = os.listdir(checkpath)
    if (
        "scripts" in os.listdir(checkpath)
        and "bin" in os.listdir(checkpath)
        and "restart" in os.listdir(checkpath)
    ):
        return True
    return False


def pick_experiment_folder(folder):
    all_files = os.listdir(folder)
    all_with_size = [add_size_information(folder, dirname) for dirname in all_files] + [
        "The current folder is already the experiment folder I want to clean up."
    ]
    response = questionary.select(
        "Which experiment do you want to clean up?",
        choices=all_with_size,
    ).ask()
    if "The current folder" in response:
        return folder
    response_without_size = remove_size_information(response)
    return os.path.join(folder, response_without_size)


def pick_subfolder(folder):
    all_files = os.listdir(folder)
    all_with_size = [add_size_information(folder, dirname) for dirname in all_files]
    response = questionary.select(
        "Files from which folder do you want to remove?",
        choices=all_with_size,
    ).ask()
    response_without_size = remove_size_information(response)
    return response_without_size


def add_size_information(toplevel, item):
    full_filepath = os.path.join(toplevel, item)
    size, unit = format_size(inspect_size(full_filepath))
    return f"{item} ({size:.2f} {unit})"


def remove_size_information(name_with_file):
    name, size, unit = name_with_file.split()
    return name


def print_disclaimer():
    os.system("clear")
    print(disclaimer)
    response = questionary.select(
        f"Sure that you want to continue?",
        choices=[
            "Hell no, let me get out of here.",
            "Yes, I know what I am doing, and will be responsible for the outcome.",
        ],
    ).ask()
    if "Hell no" in response:
        sys.exit(0)


def read_in_yaml_file(filename):
    with open(filename, "r") as stream:
        runscript_dict = yaml.load(stream, Loader=yaml.FullLoader)

    print(runscript_dict)
    return runscript_dict


def assert_question(question):
    response = questionary.select(
        question,
        choices=[
            "No, there is something wrong.",
            "Yes, that is exactly what I want.",
            "I want to quit.",
        ],
    ).ask()
    if "quit" in response:
        sys.exit(0)
    if "Yes" in response:
        return True
    return False


def check_if_folder_exists(folder):
    if not os.path.isdir(folder):
        print(f"Folder not found: {folder}")
        sys.exit(-1)
    return


def print_folder_content(checkpath, saved_space):
    os.system("clear")
    all_files = os.listdir(checkpath)
    print(f"Files in folder {checkpath}:")
    print("=" * len(f"Files in folder {checkpath}:"))
    print()
    for thisfile in sorted(all_files):
        with_size = add_size_information(checkpath, thisfile)
        print("-- " + with_size)
    print()
    space_size, space_unit = format_size(saved_space)
    print(f"(Space saved so far in this session: {space_size:.2f} {space_unit})")
    print()


def file_size(somepath):
    return os.path.getsize(somepath)


def dir_size(somepath):
    size = 0
    for path, subdirs, files in os.walk(somepath):
        for somefile in files:
            full_filepath = os.path.join(path, somefile)
            if not os.path.islink(full_filepath):
                size += os.path.getsize(full_filepath)
    return size


def inspect_size(thisfile):
    if os.path.isdir(thisfile):
        total_size = dir_size(thisfile)
    else:
        total_size = file_size(thisfile)
    return total_size


def format_size(total_size):
    unit = "B"
    if total_size >= 1024:
        total_size = total_size / 1024.0
        unit = "kB"
    if total_size >= 1024:
        total_size = total_size / 1024.0
        unit = "MB"
    if total_size >= 1024:
        total_size = total_size / 1024.0
        unit = "GB"
    if total_size >= 1024:
        total_size = total_size / 1024.0
        unit = "TB"
    return total_size, unit


def ask_for_action(folder):
    question = "What do you want to do?"
    choices = ["Remove a WHOLE subfolder", "Quit"]

    subfolders = sorted(os.listdir(folder))

    if "restart" in subfolders:
        choices.insert(0, "Remove some of the files in a folder")
    run_folders = [
        subfolder for subfolder in subfolders if subfolder.startswith("run_")
    ]
    if len(run_folders) > 0:
        choices.insert(0, "Remove all run_* subfolders (recommended)")

    post_folders = [
        subfolder for subfolder in subfolders if subfolder.startswith("post_")
    ]
    if len(post_folders) > 0:
        choices.insert(0, "Remove all post_* subfolders (recommended)")

    response = questionary.select(
        question,
        choices=choices,
    ).ask()

    if "WHOLE" in response:
        return "remove_subfolder"
    elif "run_* subfolders" in response:
        return "remove_run_subfolders"
    elif "post_* subfolders" in response:
        return "remove_post_subfolders"
    elif "some of the files" in response:
        return "remove_some_files"

    return "quit"


def remove_subfolder(folder, saved_space):
    permitted_folders = ["restart", "scripts", "bin", "outdata"]
    choices = sorted(os.listdir(folder)) + ["(You know what, forget it.)"]
    choices = [choice for choice in choices if not choice in permitted_folders]
    question = "Pick a subfolder to remove:"

    response = questionary.select(question, choices=choices).ask()

    if "You know what" in response:
        sys.exit(0)

    if "restart" in response or "bin" in response:
        print(
            f"Folder {response} should not be removed entirely, and esm_cleanup won't do it."
        )

    full_path = os.path.join(folder, response)
    if not assert_question(
        f"Do you really want to remove the whole subfolder {full_path}?"
    ):
        return saved_space

    if os.path.isdir(full_path):
        size = inspect_size(full_path)
        saved_space += size
        shutil.rmtree(full_path, ignore_errors=True)
    return saved_space


def remove_run_subfolders(folder, saved_space):
    if not assert_question(f"Do you really want to remove ALL run_* subfolders?"):
        return saved_space
    subfolders = sorted(os.listdir(folder))
    run_folders = [
        subfolder for subfolder in subfolders if subfolder.startswith("run_")
    ]
    for run_folder in run_folders:
        full_path = os.path.join(folder, run_folder)
        if os.path.isdir(full_path):
            size = inspect_size(full_path)
            saved_space += size
            shutil.rmtree(full_path, ignore_errors=True)
    return saved_space


def remove_post_subfolders(folder, saved_space):
    if not assert_question(f"Do you really want to remove ALL post_* subfolders?"):
        return saved_space
    subfolders = sorted(os.listdir(folder))
    post_folders = [
        subfolder for subfolder in subfolders if subfolder.startswith("post_")
    ]
    for post_folder in post_folders:
        full_path = os.path.join(folder, post_folder)
        if os.path.isdir(full_path):
            size = inspect_size(full_path)
            saved_space += size
            shutil.rmtree(full_path, ignore_errors=True)
    return saved_space


def remove_some_files(folder, saved_space):
    subfolder = pick_subfolder(folder)
    basedir = folder + "/" + subfolder

    if subfolder in per_model_folders:
        models = os.listdir(basedir)
        if models == []:
            print("Folder seems to be empty.")
            time.wait(2)
            return saved_space
        models_with_sizes = [add_size_information(basedir, model) for model in models]
        model_with_size = questionary.select(
            "Restart files from which subfolder shall be removed?",
            choices=models_with_sizes,
        ).ask()

        model = remove_size_information(model_with_size)
        basedir = basedir + "/" + model

    contents = sorted(os.listdir(basedir))
    if contents == []:
        print()
        print("Folder seems to be empty.")
        time.sleep(2)
        return saved_space
    content_with_size = [add_size_information(basedir, content) for content in contents]
    answers = questionary.checkbox(
        "Select files to remove:",
        choices=content_with_size,
    ).ask()

    answers = [remove_size_information(answer) for answer in answers]

    if answers == []:
        return saved_space
    print(f"You have chosen to remove the following files:")
    for filename in answers:
        print(f" ---  {filename}")
    if not assert_question(f"Do you really want to remove these files?"):
        return saved_space

    for filename in answers:
        full_path = basedir + "/" + filename
        if os.path.isdir(full_path):
            size = inspect_size(full_path)
            saved_space += size
            shutil.rmtree(full_path, ignore_errors=True)
        elif os.path.isfile(full_path):
            size = inspect_size(full_path)
            saved_space += size
            os.remove(full_path)

    return saved_space
