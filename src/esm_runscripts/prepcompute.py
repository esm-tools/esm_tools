import os
import time
import subprocess
import copy

import f90nml
import yaml
import stat

import esm_calendar
import esm_parser
import esm_runscripts

from .batch_system import batch_system
from .filelists import copy_files, log_used_files
from .helpers import evaluate 
from .namelists import Namelist

#####################################################################
#                                   compute jobs                    #
#####################################################################


def run_job(config):
    config["general"]["relevant_filetypes"] = [
        "bin",
        "config",
        "forcing",
        "input",
        "restart_in",
    ]
    config = evaluate(config, "prepcompute", "prepcompute_recipe")
    return config


def compile_model(config):
    """Compiles the desired model before the run starts"""
    model = config["general"]["setup_name"]
    version = config["general"].get("version") or config[model].get("version")
    if not version:
        return config
    if config.get("general", {}).get("run_number") == 1:
        print("First year, checking if we need to compile...")
        if not config.get("general", {}).get("use_compiled_model", True):
            print(f"Huhu --> compiling {model}-{version}")
            subprocess.run(
                f"esm_master install-{model}-{version}",
                shell=True,
                cwd=config["general"]["experiment_src_dir"],
            )
            config["general"]["model_dir"] = (
                config["general"]["experiment_src_dir"] + f"/{model}-{version}"
            )
    return config


def all_files_to_copy_append(
    config, model, filetype, categ, file_source, file_interm, file_target
):
    if file_source:
        if not filetype + "_sources" in config[model]:
            config[model][filetype + "_sources"] = {}
        config[model][filetype + "_sources"][categ] = file_source
    if file_interm:
        if not filetype + "_intermediate" in config[model]:
            config[model][filetype + "_intermediate"] = {}
        config[model][filetype + "_intermediate"][categ] = file_interm
    if file_target:
        if (
            filetype in config["general"]["in_filetypes"]
            and filetype + "_in_work" in config[model]
        ):
            config[model][filetype + "_in_work"][categ] = file_target
        else:
            # print (filetype)
            # print (file_target)
            # print (categ)
            # print (config["general"]["out_filetypes"])
            if not filetype + "_targets" in config[model]:
                config[model][filetype + "_targets"] = {}
            config[model][filetype + "_targets"][categ] = file_target

    if filetype + "_files" in config[model]:
        config[model][filetype + "_files"][categ] = categ

    return config


def prepare_coupler_files(config):
    if config["general"]["standalone"] is False:
        coupler_filename = config["general"]["coupler"].prepare(
            config, config["general"]["coupler_config_dir"]
        )
        coupler_name = config["general"]["coupler"].name
        if coupler_name == "yac":
            couplingfile = "coupling.xml"
        else:
            couplingfile = "namcouple"

        all_files_to_copy_append(
            config,
            coupler_name,
            "config",
            couplingfile,
            config["general"]["coupler_config_dir"] + "/" + coupler_filename,
            None,
            None,
        )
    return config


def create_empty_folders(config):
    for model in list(config):
        if "create_folders" in config[model]:
            folders = config[model]["create_folders"]
            if not type(folders) == list:
                folders = [folders]
            for folder in folders:
                if not os.path.isdir(folder):
                    os.mkdir(folder)
    return config


def create_new_files(config):
    for model in list(config):
        for filetype in config["general"]["all_filetypes"]:
            if "create_" + filetype in config[model]:
                filenames = config[model]["create_" + filetype].keys()

                for filename in filenames:

                    full_filename = (
                        config[model]["thisrun_" + filetype + "_dir"] + "/" + filename
                    )
                    if not os.path.isdir(os.path.dirname(full_filename)):
                        os.mkdir(os.path.dirname(full_filename))
                    with open(
                        full_filename,
                        "w",
                    ) as createfile:
                        actionlist = config[model]["create_" + filetype][filename]
                        for action in actionlist:
                            if "<--append--" in action:
                                appendtext = action.replace("<--append--", "")
                                createfile.write(appendtext.strip() + "\n")
                    # make executable, just in case
                    filestats = os.stat(full_filename)
                    os.chmod(full_filename, filestats.st_mode | stat.S_IEXEC)

                    all_files_to_copy_append(
                        config,
                        model,
                        filetype,
                        filename,
                        config[model]["thisrun_" + filetype + "_dir"] + "/" + filename,
                        filename,
                        filename,
                    )
    return config


def modify_files(config):
    # for model in config:
    #     for filetype in config["general"]["all_model_filetypes"]:
    #         if filetype == "restart":
    #             nothing = "nothing"
    return config


def modify_namelists(config):
    # Load and modify namelists:

    if config["general"]["verbose"]:
        print("\n" "- Setting up namelists for this run...")
        for index, model in enumerate(config["general"]["valid_model_names"]):
            print(f'{index+1}) {config[model]["model"]}')
        print()

    for model in config["general"]["valid_model_names"]:
        config[model] = Namelist.nmls_load(config[model])
        config[model] = Namelist.nmls_remove(config[model])
        if model == "echam":
            config = Namelist.apply_echam_disturbance(config)
            config = Namelist.echam_transient_forcing(config)
        if model == "fesom":
            config = Namelist.apply_iceberg_calving(config)
        config[model] = Namelist.nmls_modify(config[model])
        config[model] = Namelist.nmls_finalize(
            config[model], config["general"]["verbose"]
        )

    if config["general"]["verbose"]:
        print("::: end of namelist section\n")
    return config


def copy_files_to_thisrun(config):
    if config["general"]["verbose"]:
        print("PREPARING EXPERIMENT")
        # Copy files:
        print("\n" "- File lists populated, proceeding with copy...")
        print("- Note that you can see your file lists in the config folder")
        print("- You will be informed about missing files")

    counter = 0
    count_max = 90
    if (
        config["general"].get("iterative_coupling", False)
        and config["general"]["chunk_number"] > 1
    ):
        if "files_to_wait_for" in config["general"]:
            for file_base in config['general'].get('files_to_wait_for'):
                file = os.path.join(config['general']['experiment_couple_dir'], file_base)
                while counter < count_max:
                    counter = counter + 1
                    if os.path.isfile(file):
                        print("File found: ", file)
                        break
                    else:
                        print("Waiting for file: ", file)
                        print("Sleep for 10 seconds...")
                        time.sleep(10)

    # MA: TODO: this should go somewhere else, maybe on its on module and then inserted on a recipe
    if "fesom" in config["general"]["valid_model_names"]:
        if config["fesom"].get("use_icebergs", False) and config["fesom"].get("use_icesheet_coupling", False):
            if config["general"].get("run_number", 0) == 1:
                if not os.path.isfile(
                    config["general"]["experiment_couple_dir"] + "/num_non_melted_icb_file"
                ):
                    with open(config["general"]["experiment_couple_dir"] + "/num_non_melted_icb_file", "w") as f:
                        f.write("0")
            else:
                num_lines = sum(1 for line in open(os.path.join(config["fesom"]["experiment_restart_in_dir"], "iceberg.restart.ISM")))
                with open(config["general"]["experiment_couple_dir"] + "/num_non_melted_icb_file", "w") as f:
                    f.write(str(num_lines))

    log_used_files(config)

    config = copy_files(
        config, config["general"]["in_filetypes"], source="init", target="thisrun"
    )
    return config


def copy_files_to_work(config):
    if config["general"]["verbose"]:
        print("PREPARING WORK FOLDER")
    config = copy_files(
        config, config["general"]["in_filetypes"], source="thisrun", target="work"
    )
    return config


def _write_finalized_config(config):
    """Writes <expid>_finished_config.yaml file
    Parameters
    ----------
    config : esm-tools config object
    """
    # first define the representers for the non-built-in types, as recommended
    # here: https://pyyaml.org/wiki/PyYAMLDocumentation
    def date_representer(dumper, date):
        return dumper.represent_str(f"{date.output()}")

    def calendar_representer(dumper, calendar):
        # Calendar has a __str__ method
        return dumper.represent_str(str(calendar))

    def batch_system_representer(dumper, batch_system):
        return dumper.represent_str(f"{batch_system.name}")

    def coupler_representer(dumper, coupler):
        # prevent dumping of whole namcouple
        return dumper.represent_str(f"{coupler.name}")

    def oasis_representer(dumper, oasis):
        return dumper.represent_str(f"{oasis.name}")

    def namelist_representer(dumper, f90nml):
        return dumper.represent_str(f"f90nml.name")

    # dumper object for the ESM-Tools configuration
    class EsmConfigDumper(yaml.dumper.Dumper):
        pass

    # pyyaml does not support tuple and prints !!python/tuple
    EsmConfigDumper.add_representer(
        tuple, yaml.representer.SafeRepresenter.represent_list
    )

    # Determine how non-built-in types will be printed be the YAML dumper
    EsmConfigDumper.add_representer(esm_calendar.Date, date_representer)

    EsmConfigDumper.add_representer(
        esm_calendar.esm_calendar.Calendar, calendar_representer
    )
    # yaml.representer.SafeRepresenter.represent_str)

    EsmConfigDumper.add_representer(
        esm_parser.esm_parser.ConfigSetup,
        yaml.representer.SafeRepresenter.represent_dict,
    )

    EsmConfigDumper.add_representer(batch_system, batch_system_representer)

    # format for the other ESM data structures
    EsmConfigDumper.add_representer(
        esm_runscripts.coupler.coupler_class, coupler_representer
    )

    EsmConfigDumper.add_representer(
        f90nml.namelist.Namelist, namelist_representer
    )

    if "oasis3mct" in config:
        EsmConfigDumper.add_representer(esm_runscripts.oasis.oasis, oasis_representer)

    thisrun_config_dir = config["general"]["thisrun_config_dir"]
    expid = config["general"]["expid"]
    it_coupled_model_name = config["general"]["iterative_coupled_model"]
    config_file_path = (
        f"{thisrun_config_dir}/"
        f"{expid}_{it_coupled_model_name}finished_config.yaml"
    )
    with open(config_file_path, "w") as config_file:
        # Avoid saving ``prev_run`` information in the config file
        config_final = copy.deepcopy(config)  # PrevRunInfo
        del config_final["prev_run"]  # PrevRunInfo

        out = yaml.dump(
            config_final, Dumper=EsmConfigDumper, width=10000, indent=4
        )  # PrevRunInfo
        config_file.write(out)
    return config


def _show_simulation_info(config):
    print()
    print(80 * "=")
    print("STARTING SIMULATION JOB!")
    print(f"Experiment ID = {config['general']['expid']}")
    print(f"Setup = {config['general']['setup_name']}")
    if "coupled_setup" in config["general"]:
        print("This setup consists of:")
        for model in config["general"]["valid_model_names"]:
            print(f"- {model}")
    print("Experiment is installed in:")
    print(f"       {config['general']['base_dir']}/{config['general']['expid']}")
    print(80 * "=")
    print()
    return config
