"""
Unit tests for the new file-dict feature

Some considerations
~~~~~~~~~~~~~~~~~~~
* It might be clever to put the "fake config" somewhere in the top level, but
  then again, it would be nice to have a unique config for each test. This is
  undeniably more verbose, but we then have very clear examples to follow when
  we want to translate later on.
* You _could_ use the config in each function to generate the fake files, to
  avoid repeating yourself; however, that adds real programming logic into the
  unit test, which you don't really want.  
"""
import os
import sys
from collections import namedtuple
from collections.abc import Callable
from io import StringIO
from pathlib import Path

import pytest
import yaml

import esm_runscripts.filedicts
import esm_runscripts.filedicts as filedicts


class Capturing(list):
    """Taken from https://stackoverflow.com/questions/16571150/how-to-capture-stdout-output-from-a-python-function-call"""

    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self

    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        del self._stringio  # free up some memory
        sys.stdout = self._stdout


@pytest.fixture()
def config_tuple():
    """setup function
    Generates fake config to be used before each test. NamedTuple has 2 fields:
    - config: configuration dictionary
    - attr_address: path to retrieve from the config
    """
    config_str = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        exp_dir: "/work/ollie/pgierz/some_exp"
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: /work/ollie/pool/ECHAM/T63
                filetype: NetCDF
                description: >
                    Initial values used for the simulation, including
                    properties such as geopotential, temperature, pressure
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    """
    config = yaml.safe_load(config_str)
    attr_address = "echam.files.jan_surf"
    # create a named tuple since configuration and the attribute address is tightly coupled to each other
    Config_Tuple = namedtuple("Config_Tuple", ["config", "attr_address"])
    fake_config_tuple = Config_Tuple(config, attr_address)
    yield fake_config_tuple


@pytest.fixture()
def simulation_file(fs, config_tuple):
    """setup function
    Generates 
      - fake SimulationFile
      - source directory (computer)
      - target directory (work)
    to be used before each test
    """
    config = config_tuple.config
    attr_address = config_tuple.attr_address
    fake_simulation_file = filedicts.SimulationFile(config, attr_address)

    path_of_computer = fake_simulation_file.path_in_computer
    name_in_computer = fake_simulation_file.names["computer"]
    file_path_in_computer = path_of_computer / name_in_computer

    path_of_work = fake_simulation_file.path_in_work
    name_in_work = fake_simulation_file.names["work"]
    file_path_in_work = path_of_work / name_in_work

    # create files and directories
    # file_path_in_computer : source file path
    # path_of_work          : target directory path
    fs.create_file(file_path_in_computer)
    fs.create_dir(path_of_work)

    yield fake_simulation_file


def test_example(fs):
    # Make a fake config:
    config = """
    general:
        base_dir: /some/dummy/location/
    echam:
        files:
            jan_surf:
                name: ECHAM Jan Surf File
                path_in_computer: /work/ollie/pool/ECHAM
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                type: input
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
    """
    config = yaml.safe_load(config)
    # Create some fake files and directories you might want in your test
    fs.create_file("/work/ollie/pool/ECHAM/T63CORE2_jan_surf.nc")
    fs.create_dir("/some/dummy/location/expid/run_18500101-18501231/work")
    # This module also have functions for link files, globbing, etc.
    config_out = esm_runscripts.filedicts.copy_files(config)
    assert os.path.exists("/some/dummy/location/expid/run_18500101-18501231/work/")
    assert os.path.exists("/work/ollie/pool/ECHAM/T63CORE2_jan_surf.nc")


def test_filedicts_basics(fs):
    """Tests basic attribute behavior of filedicts"""

    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: /work/ollie/pool/ECHAM/T63
                filetype: NetCDF
                type: input
                description: >
                    Initial values used for the simulation, including
                    properties such as geopotential, temperature, pressure
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    config = yaml.safe_load(dummy_config)
    # Not needed for this test, just a demonstration:
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    assert sim_file["name_in_work"] == "unit.24"
    assert sim_file.locations["work"] == Path(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    )
    assert sim_file._config == config
    assert sim_file.locations["computer"] == Path("/work/ollie/pool/ECHAM/T63")


def test_cp_file(fs):
    """Tests for ``filedicts.cp`` copying file"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                type: input
                path_in_computer: /work/ollie/pool/ECHAM/T63/
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    config = yaml.safe_load(dummy_config)

    # Set source and targets
    target_folder = config["general"]["thisrun_work_dir"]
    source = Path(
        config["echam"]["files"]["jan_surf"]["path_in_computer"],
        config["echam"]["files"]["jan_surf"]["name_in_computer"],
    )
    target = Path(
        target_folder,
        config["echam"]["files"]["jan_surf"]["name_in_work"],
    )
    # Create files and folders
    fs.create_file(source)
    fs.create_dir(target_folder)

    # Test the method
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    sim_file.cp("computer", "work")

    assert os.path.exists(target)


def test_cp_folder(fs):
    """Tests for ``filedicts.cp`` copying folder"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
    oifs:
        files:
            o3_data:
                name_in_computer: o3chem_l91
                name_in_work: o3chem_l91
                type: input
                path_in_computer: /work/ollie/pool/OIFS/159_4
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """
    config = yaml.safe_load(dummy_config)

    # Set source and targets
    target_folder = config["general"]["thisrun_work_dir"]
    source = Path(
        config["oifs"]["files"]["o3_data"]["path_in_computer"],
        config["oifs"]["files"]["o3_data"]["name_in_computer"],
    )
    target = Path(
        target_folder,
        config["oifs"]["files"]["o3_data"]["name_in_work"],
    )
    # Create files and folders
    fs.create_dir(source)
    fs.create_dir(target_folder)

    # Test the method
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.o3_data")
    sim_file.cp("computer", "work")

    assert os.path.exists(target)


def test_resolve_file_movements(config_tuple):
    # arrange config-in
    config = config_tuple.config
    attr_address = config_tuple.attr_address
    simulation_file = filedicts.SimulationFile(config, attr_address)
    config = filedicts.resolve_file_movements(config)

    # check config-out
    assert isinstance(config, dict)


def test_mv(fs):
    """Tests for mv"""
    dummy_config = """
    general:
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                path_in_computer: /work/ollie/pool/ECHAM/T63/
                type: input
                name_in_work: unit.24
                path_in_work: .
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    config = yaml.safe_load(dummy_config)
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    fs.create_dir("/work/ollie/pgierz/some_exp/run_20010101-20010101/work")
    assert os.path.exists("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    sim_file.mv("computer", "work")
    assert not os.path.exists("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    assert os.path.exists(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work/unit.24"
    )


# ===
# Tests for SimulationFile.ln() method
# + 1) check if function is callable
# + 2) check if linking occurs
# + 3) check for self pointing
# + 4) check if target path is a directory and not a file
# + 5) check if target file already exists or a symlink
# + 6) check if source file does not exists
# + 8) check if target directory does not exist
# ===
def test_ln_iscallable(simulation_file):
    assert isinstance(simulation_file.ln, Callable)


def test_ln_links_file_from_computer_to_work(simulation_file, fs):
    path_of_computer = simulation_file.path_in_computer
    name_in_computer = simulation_file.names["computer"]
    file_path_in_computer = path_of_computer / name_in_computer
    path_of_work = simulation_file.path_in_work
    name_in_work = simulation_file.names["work"]
    file_path_in_work = path_of_work / name_in_work

    simulation_file.ln("computer", "work")
    assert os.path.exists(file_path_in_computer)
    assert os.path.exists(file_path_in_work)


def test_ln_raises_exception_when_pointing_to_itself(simulation_file, fs):
    with pytest.raises(OSError):
        simulation_file.ln("computer", "computer") 

    
def test_ln_raises_exception_when_target_is_a_directory_and_not_a_file(simulation_file, fs):
    # erase the file name so that we only have a directory path instead
    simulation_file.names["work"] = ""
    with pytest.raises(OSError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_target_path_exists(simulation_file, fs):
    path_of_work = simulation_file.path_in_work
    name_in_work = simulation_file.names["work"]
    file_path_in_work = path_of_work / name_in_work
    # create the target file so that it will raise an exception
    fs.create_file(file_path_in_work)

    with pytest.raises(FileExistsError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_source_file_does_not_exist(simulation_file, fs):
    path_of_computer = simulation_file.path_in_computer
    name_in_computer = simulation_file.names["computer"]
    file_path_in_computer = path_of_computer / name_in_computer
    fs.remove_object(file_path_in_computer)

    with pytest.raises(FileNotFoundError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_target_path_does_not_exist(simulation_file, fs):
    path_of_work = simulation_file.path_in_work
    fs.remove_object(path_of_work)
    with pytest.raises(FileNotFoundError):
        simulation_file.ln("computer", "work")

# ========== end of ln() tests ==========

def test_check_path_in_computer_is_abs(fs):
    """
    Tests that ``esm_parser.user_error`` is used when the ``path_in_computer``
    is not absolute
    """

    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    echam:
        files:
            jan_surf:
                type: input
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: pool/ECHAM/T63
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    config = yaml.safe_load(dummy_config)

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file = esm_runscripts.filedicts.SimulationFile(
                config, "echam.files.jan_surf"
            )

    # error needs to occur as the path is not absolute
    assert any(["ERROR: File Dictionaries" in line for line in output])

def test_resolve_paths(fs):
    """
    Tests ``_resolve_paths``
    """

    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    echam:
        files:
            jan_surf:
                type: input
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: /work/ollie/pool/ECHAM/T63/
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    config = yaml.safe_load(dummy_config)

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")

    assert sim_file["absolute_path_in_work"] == Path(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work/unit.24"
    )
    assert sim_file["absolute_path_in_computer"] == Path(
        "/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc"
    )
    assert sim_file["absolute_path_in_exp_tree"] == Path(
        "/work/ollie/pgierz/some_exp/input/echam/T63CORE2_jan_surf.nc"
    )
    assert sim_file["absolute_path_in_run_tree"] == Path(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam/T63CORE2_jan_surf.nc"
    )

def test_resolve_paths_old_config():
    """
    Tests ``_resolve_paths``
    """
    # Load an old config
    tests_path = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "."
    )
    with open(f"{tests_path}/awicm3_config.yaml", "r") as f:
        config = yaml.safe_load(f)
    # Add the new ``files`` dictionary
    config["oifs"]["files"] = {"o3_data": {
        "name_in_computer": "o3chem_l91",
        "name_in_work": "o3chem_l91",
        "type": "input",
        "path_in_computer": "/work/ollie/jstreffi/input/oifs-43r3/43r3/climate/95_4",
    }}

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.o3_data")

    assert sim_file["absolute_path_in_work"] == Path(
        "/work/ollie/mandresm/testing/run/awicm3//awicm3-v3.1-TCO95L91-CORE2_initial/run_20000101-20000101/work/o3chem_l91"
    )
    assert sim_file["absolute_path_in_computer"] == Path(
        "/work/ollie/jstreffi/input/oifs-43r3/43r3/climate/95_4/o3chem_l91"
    )
    assert sim_file["absolute_path_in_exp_tree"] == Path(
        "/work/ollie/mandresm/testing/run/awicm3//awicm3-v3.1-TCO95L91-CORE2_initial/input/oifs/o3chem_l91"
    )
    assert sim_file["absolute_path_in_run_tree"] == Path(
        "/work/ollie/mandresm/testing/run/awicm3//awicm3-v3.1-TCO95L91-CORE2_initial/run_20000101-20000101/input/oifs/o3chem_l91"
    )
