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

import esm_calendar
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
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                type: input
                allowed_to_be_missing: False
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: /work/ollie/pool/ECHAM/T63
                filetype: NetCDF
                description: >
                    Initial values used for the simulation, including
                    properties such as geopotential, temperature, pressure
        experiment_input_dir: "/work/ollie/pgierz/some_exp/input/echam"
        thisrun_input_dir: "/work/ollie/pgierz/some_exp/run_20000101-20000101/input/echam"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(config_str)
    config["general"]["current_date"] = date
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
      - fake file on computer
    to be used before each test
    """
    config = config_tuple.config
    attr_address = config_tuple.attr_address
    fake_simulation_file = filedicts.SimulationFile(config, attr_address)

    fs.create_dir(fake_simulation_file.locations["work"])
    fs.create_dir(fake_simulation_file.locations["computer"])
    fs.create_dir(fake_simulation_file.locations["exp_tree"])
    fs.create_dir(fake_simulation_file.locations["run_tree"])
    fs.create_file(fake_simulation_file["absolute_path_in_computer"])

    yield fake_simulation_file


def test_filedicts_basics(fs):
    """Tests basic attribute behavior of filedicts"""

    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
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
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    # Not needed for this test, just a demonstration:
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    assert sim_file["name_in_work"] == "unit.24"
    assert sim_file.locations["work"] == Path(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    )
    assert sim_file._config == config
    assert sim_file.locations["computer"] == Path("/work/ollie/pool/ECHAM/T63")


# ===
# tests for SimulationFile._path_type() method
# ===
def test_path_type_raises_exception_on_incompatible_input(simulation_file, fs):
    # check for incompatible type
    with pytest.raises(TypeError):
        output = simulation_file._path_type(12312312)


def test_path_type_detects_non_existing_file(simulation_file, fs):
    # check for non-existent directory / file
    path = "/this/path/does/not/exist"
    output = simulation_file._path_type(path)
    assert output == filedicts.FileTypes.NOT_EXISTS


def test_path_type_detects_symbolic_link(simulation_file, fs):
    # check for link
    mylink = "/tmp/mylink"
    fs.create_symlink(mylink, simulation_file["absolute_path_in_computer"])
    output = simulation_file._path_type(mylink)
    assert output == filedicts.FileTypes.LINK


def test_path_type_detects_file(simulation_file, fs):
    # check for file
    output = simulation_file._path_type(simulation_file["absolute_path_in_computer"])
    assert output == filedicts.FileTypes.FILE


def test_path_type_detects_directory(simulation_file, fs):
    # check for directory
    output = simulation_file._path_type(simulation_file.locations["work"])
    assert output == filedicts.FileTypes.DIR


# === end of the tests for _path_type() method


# ===
# tests for SimulationFile._check_source_and_target() method
# ===
def test_check_source_and_targets_works_as_expected(simulation_file, fs):
    # successful return
    path_str = "/home/ollie/dural/test_dir"
    fs.create_dir(path_str)
    source_path = simulation_file.locations["computer"]
    target_path = Path(f"{path_str}/file.txt")
    output = simulation_file._check_source_and_target(source_path, target_path)
    assert output == True


def test_check_source_and_targets_raises_exception_on_incompatible_input_type(
    simulation_file, fs
):
    # check incompatible types for file paths
    with pytest.raises(TypeError):
        simulation_file._check_source_and_target(Path("/usr/bin"), {"foo": 1})
    with pytest.raises(TypeError):
        simulation_file._check_source_and_target(1234, 3.1415)


def test_check_source_and_targets_raises_exception_on_nonexisting_directory(
    simulation_file, fs
):
    # source does not exist
    source_path = Path("/this/does/not/exist")
    target_path = simulation_file.locations["work"]
    with pytest.raises(FileNotFoundError):
        simulation_file._check_source_and_target(source_path, target_path)

    # target does not exist
    source_path = simulation_file.locations["computer"]
    target_path = Path("/this/does/not/exist")
    with pytest.raises(FileNotFoundError):
        simulation_file._check_source_and_target(source_path, target_path)


# === end of the tests for check_source_and_target() method


def test_allowed_to_be_missing_attr():
    """Ensures the property allowed_to_be_missing works correctly"""
    dummy_config = """
    general:
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
        files:
            human_readable_tag_001:
                allowed_to_be_missing: True
                path_in_computer: "/some/location/on/ollie"
                name_in_computer: "foo"
                type: "input"
            human_readable_tag_002:
                allowed_to_be_missing: False
                path_in_computer: "/some/location/on/ollie"
                name_in_computer: "bar"
                type: "input"
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    # Not needed for this test, just a demonstration:
    sim_file_001 = esm_runscripts.filedicts.SimulationFile(
        config, "echam.files.human_readable_tag_001"
    )
    sim_file_002 = esm_runscripts.filedicts.SimulationFile(
        config, "echam.files.human_readable_tag_002"
    )

    assert sim_file_001.allowed_to_be_missing == True
    assert sim_file_002.allowed_to_be_missing == False


def test_allowed_to_be_missing_mv(fs):
    """Checks that files in move mode which are allowed to be missing are skipped"""
    dummy_config = """
    general:
        expid: expid
        base_dir: /some/dummy/location/
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20011231/work"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20011231"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20011231/input/echam
        files:
            human_readable_tag_001:
                type: input
                allowed_to_be_missing: True
                name_in_computer: foo
                path_in_computer: /work/data/pool
                name_in_work: foo
                path_in_work: .
                movement_type: move
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    fs.create_dir("/work/data/pool")
    fs.create_file("/work/data/pool/not_foo_at_all")
    sim_file = esm_runscripts.filedicts.SimulationFile(
        config, "echam.files.human_readable_tag_001"
    )
    sim_file.mv("computer", "work")
    assert not os.path.exists(
        "/work/ollie/pgierz/some_exp/run_20010101-20011231/work/foo"
    )


def test_allowed_to_be_missing_mv_if_exists(fs):
    """Checks that a file which is allowed to be missing is still moved if it exists"""
    dummy_config = """
    general:
        expid: expid
        base_dir: "/work/ollie/pgierz/some_exp"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20011231/work"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20011231"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20011231/input/echam
        files:
            human_readable_tag_001:
                type: input
                allowed_to_be_missing: True
                name_in_computer: foo
                path_in_computer: /work/data/pool
                name_in_work: foo
                path_in_work: .
                movement_type: move
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    fs.create_dir("/work/data/pool")
    fs.create_file("/work/data/pool/foo")
    fs.create_dir("/work/ollie/pgierz/some_exp/run_20010101-20011231/work")
    sim_file = esm_runscripts.filedicts.SimulationFile(
        config, "echam.files.human_readable_tag_001"
    )
    sim_file.mv("computer", "work")
    assert os.path.exists("/work/ollie/pgierz/some_exp/run_20010101-20011231/work/foo")


def test_cp_file(fs):
    """Tests for ``filedicts.cp`` copying file"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
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
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

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
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    oifs:
        files:
            o3_data:
                name_in_computer: o3chem_l91
                name_in_work: o3chem_l91
                type: input
                path_in_computer: /work/ollie/pool/OIFS/159_4
                datestamp_method: never
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

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
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
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
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
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
# + 4) check if target path is a directory and not a file # <-- PG NO!!!
# + 5) check if target file already exists or a symlink
# + 6) check if source file does not exists
# + 8) check if target directory does not exist
# ===
def test_ln_iscallable(simulation_file):
    assert isinstance(simulation_file.ln, Callable)


def test_ln_links_file_from_computer_to_work(simulation_file, fs):
    file_path_in_work = simulation_file["absolute_path_in_work"]
    file_path_in_computer = simulation_file["absolute_path_in_computer"]

    simulation_file.ln("computer", "work")
    assert os.path.exists(file_path_in_computer)
    assert os.path.exists(file_path_in_work)


def test_ln_raises_exception_when_source_is_a_broken_link(simulation_file, fs):
    # create a symbolic link that points to a non-existing file
    broken_path_str = "/this/does/not/exist"
    link_str = "/tmp/broken_link"
    fs.create_symlink(link_str, broken_path_str)
    # overwrite the path in computer with the link to trigger the exception
    simulation_file["absolute_path_in_computer"] = Path(link_str).absolute()

    with pytest.raises(FileNotFoundError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_target_is_a_directory_and_not_a_file(
    simulation_file, fs
):
    # Since the simulation_file fixture is a fake_jan_surf, we need the work folder:
    simulation_file["absolute_path_in_work"] = simulation_file.locations["work"]
    with pytest.raises(OSError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_target_path_exists(simulation_file, fs):
    file_path_in_work = simulation_file["absolute_path_in_work"]
    # create the target file so that it will raise an exception
    fs.create_file(file_path_in_work)
    simulation_file.datestamp_method = "never"

    with pytest.raises(FileExistsError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_source_file_does_not_exist(simulation_file, fs):
    fs.remove_object(str(simulation_file["absolute_path_in_computer"]))
    with pytest.raises(FileNotFoundError):
        simulation_file.ln("computer", "work")


def test_ln_raises_exception_when_target_path_does_not_exist(simulation_file, fs):
    fs.remove_object(str(simulation_file.locations["work"]))
    with pytest.raises(FileNotFoundError):
        simulation_file.ln("computer", "work")


# ========== end of ln() tests ==========


def test_check_file_syntax_type_missing():
    """Tests for ``type`` variable missing"""
    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    echam:
        files:
            jan_surf:
                name_in_work: unit.24
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file = esm_runscripts.filedicts.SimulationFile(
                config, "echam.files.jan_surf"
            )

    error_text = "the \x1b[31mtype\x1b[0m variable is missing"
    assert any([error_text in line for line in output])


def test_check_file_syntax_type_incorrect():
    """Tests for ``type`` variable being incorrectly defined"""
    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    echam:
        files:
            jan_surf:
                type: is_wrong
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file = esm_runscripts.filedicts.SimulationFile(
                config, "echam.files.jan_surf"
            )

    error_text = "is_wrong\x1b[0m is not a supported \x1b[31mtype"
    assert any([error_text in line for line in output])


def test_check_file_syntax_input():
    """
    Tests for missing ``name_in_computer`` and ``path_in_computer`` for input file types
    """
    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    echam:
        files:
            jan_surf:
                type: input
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file = esm_runscripts.filedicts.SimulationFile(
                config, "echam.files.jan_surf"
            )

    error_text = "the \x1b[31mpath_in_computer\x1b[0m variable is missing"
    assert any([error_text in line for line in output])
    error_text = "the \x1b[31mname_in_computer\x1b[0m variable is missing"
    assert any([error_text in line for line in output])


def test_check_file_syntax_output():
    """Tests for missing ``name_in_work`` for output file types"""
    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    echam:
        files:
            jan_surf:
                type: outdata
        experiment_input_dir: /work/ollie/pgierz/some_exp/input/echam
        thisrun_input_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/echam
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file = esm_runscripts.filedicts.SimulationFile(
                config, "echam.files.jan_surf"
            )

    error_text = "the \x1b[31mname_in_work\x1b[0m variable is missing"
    assert any([error_text in line for line in output])


def test_check_path_in_computer_is_abs(simulation_file, fs):
    """
    Tests that ``esm_parser.user_error`` is used when the ``path_in_computer``
    is not absolute
    """
    simulation_file.path_in_computer = Path("foo/bar")

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            simulation_file._check_path_in_computer_is_abs()

    # error needs to occur as the path is not absolute
    assert any(["ERROR: File Dictionaries" in line for line in output])


def test_resolve_abs_paths(fs):
    """
    Tests ``_resolve_abs_paths``
    """

    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
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
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

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
    tests_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), ".")
    with open(f"{tests_path}/awicm3_config.yaml", "r") as f:
        config = yaml.safe_load(f)
    # Add the new ``files`` dictionary
    config["oifs"]["files"] = {
        "o3_data": {
            "name_in_computer": "o3chem_l91",
            "name_in_work": "o3chem_l91",
            "type": "input",
            "path_in_computer": "/work/ollie/jstreffi/input/oifs-43r3/43r3/climate/95_4",
        }
    }

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


def test_datestamp_method_attr(simulation_file):
    assert hasattr(simulation_file, "datestamp_method")
    assert isinstance(simulation_file.datestamp_method, str)
    assert simulation_file.datestamp_method in ["never", "always", "avoid_overwrite"]
    with pytest.raises(ValueError, match="one of never, always, or avoid_overwrite"):
        simulation_file["datestamp_method"] = "blah"
    with pytest.raises(ValueError, match="one of never, always, or avoid_overwrite"):
        simulation_file.datestamp_method = "blah"
    with pytest.raises(ValueError, match="one of never, always, or avoid_overwrite"):
        simulation_file.update(dict(datestamp_method="blah"))


def test_datestamp_format_attr(simulation_file):
    assert hasattr(simulation_file, "datestamp_format")
    assert isinstance(simulation_file.datestamp_format, str)
    assert simulation_file.datestamp_format in ["check_from_filename", "append"]
    with pytest.raises(ValueError, match="one of check_from"):
        simulation_file["datestamp_format"] = "blah"
    with pytest.raises(ValueError, match="one of "):
        simulation_file.datestamp_format = "blah"
    with pytest.raises(ValueError, match="one of check_from"):
        simulation_file.update(dict(datestamp_format="blah"))


@pytest.mark.parametrize("movement_type", ["mv", "ln", "cp"])
def test_datestamp_added_by_default_mv_ln_cp(simulation_file, fs, movement_type):
    """
    Checks that the simulation file gets a timestamp if a file already exists with that name

    Defaults checked:
    -----------------
        * datestamp_format: append
        * datestamp_method: avoid_overwrite
    """
    simulation_file_meth = getattr(simulation_file, movement_type)
    simulation_file2 = simulation_file  # Make a copy
    simulation_file2_meth = getattr(simulation_file2, movement_type)
    simulation_file_meth("computer", "work")
    if movement_type == "mv":
        assert not os.path.exists("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
        # Recreate the moved file in computer (let's pretend we compied it):
        fs.create_file(simulation_file2["absolute_path_in_computer"])
    assert os.path.exists(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work/unit.24"
    )
    simulation_file2_meth("computer", "work")
    assert os.path.exists(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work/unit.24_2000-01-01T00:00:00"
    )
    simulation_file_meth("work", "run_tree")
    assert os.path.exists(
        "/work/ollie/pgierz/some_exp/run_20000101-20000101/input/echam/T63CORE2_jan_surf.nc"
    )
    simulation_file_meth("run_tree", "exp_tree")


def test_datestamp_not_added_if_attr_set(simulation_file):
    pass  # Still to be implemented


def test_datestamp_not_added_if_in_filename(simulation_file):
    pass  # Still to be implemented


def test_fname_has_date_stamp_info():
    fname = "blah_2000-01-01.nc"
    fname2 = "blah_2000-01.nc"
    date = esm_calendar.Date("2000-01-01T00:00:00")
    assert filedicts._fname_has_date_stamp_info(fname, date)
    assert not filedicts._fname_has_date_stamp_info(fname2, date)


def test_wild_card_check():
    """Tests that wild card check is passed"""
    source_name = "a_wild_card*name*.txt"
    target_name = "another_wild_card*newname*.txt1"
    source_pattern = source_name.split("*")
    target_pattern = target_name.split("*")

    assert esm_runscripts.filedicts.SimulationFile._wild_card_check(
        source_pattern, target_pattern
    )


def test_wild_card_check_fails():
    """
    Tests that, when given an incorrect wildcard pattern (more wildcards in source than
    in target), a ``user_error`` is reported
    """
    source_name = "a_wild_card*name*.txt"
    target_name = "another_wild_cardnewname*.txt1"
    source_pattern = source_name.split("*")
    target_pattern = target_name.split("*")

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            esm_runscripts.filedicts.SimulationFile._wild_card_check(
                source_pattern, target_pattern
            )

    # error needs to occur as the path is not absolute
    assert any(["The wild card pattern of the source" in line for line in output])


def test_find_globbing_files(fs):
    """
    Tests for files with the globbing pattern not found
    """

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    oifs:
        files:
            oifsnc:
                name_in_work: input_expid_*_DATE_*.nc
                name_in_exp_tree: new_input_expid_*_NEW_DATE_*.nc
                type: outdata
        experiment_outdata_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_outdata_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """
    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    # These files are incorrect since they do not have input_expid and instead have input_<NUMBER>
    files = [
        "input_11_DATE_12.nc",
        "input_21_DATE_22.nc",
        "input_31_DATE_32.nc",
    ]
    for f in files:
        fs.create_file(Path(config["general"]["thisrun_work_dir"]).joinpath(f))

    fs.create_dir(config["oifs"]["experiment_outdata_dir"])

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.oifsnc")

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file.cp("work", "exp_tree")

    # error needs to occur as the path is not absolute
    assert any(["No files found for the globbing pattern" in line for line in output])


def test_globbing_cp(fs):
    """Tests globbing for copying"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    oifs:
        files:
            oifsnc:
                name_in_work: input_expid_*_DATE_*.nc
                name_in_exp_tree: new_input_expid_*_NEW_DATE_*.nc
                type: outdata
        experiment_outdata_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_outdata_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """

    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    files = [
        "input_expid_11_DATE_12.nc",
        "input_expid_21_DATE_22.nc",
        "input_expid_31_DATE_32.nc",
    ]
    for f in files:
        fs.create_file(Path(config["general"]["thisrun_work_dir"]).joinpath(f))

    fs.create_dir(config["oifs"]["experiment_outdata_dir"])

    new_files = [
        "new_input_expid_11_NEW_DATE_12.nc",
        "new_input_expid_21_NEW_DATE_22.nc",
        "new_input_expid_31_NEW_DATE_32.nc",
    ]
    expected_new_paths = []
    for f in new_files:
        expected_new_paths.append(
            Path(config["oifs"]["experiment_outdata_dir"]).joinpath(f)
        )

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.oifsnc")
    sim_file.cp("work", "exp_tree")

    for nf in expected_new_paths:
        assert os.path.exists(nf)


def test_globbing_mv(fs):
    """Tests globbing for moving"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    oifs:
        files:
            oifsnc:
                name_in_work: input_expid_*_DATE_*.nc
                name_in_exp_tree: new_input_expid_*_NEW_DATE_*.nc
                type: outdata
        experiment_outdata_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_outdata_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """

    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    files = [
        "input_expid_11_DATE_12.nc",
        "input_expid_21_DATE_22.nc",
        "input_expid_31_DATE_32.nc",
    ]
    for f in files:
        fs.create_file(Path(config["general"]["thisrun_work_dir"]).joinpath(f))

    fs.create_dir(config["oifs"]["experiment_outdata_dir"])

    new_files = [
        "new_input_expid_11_NEW_DATE_12.nc",
        "new_input_expid_21_NEW_DATE_22.nc",
        "new_input_expid_31_NEW_DATE_32.nc",
    ]
    expected_new_paths = []
    for f in new_files:
        expected_new_paths.append(
            Path(config["oifs"]["experiment_outdata_dir"]).joinpath(f)
        )

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.oifsnc")
    sim_file.mv("work", "exp_tree")

    for nf in expected_new_paths:
        assert os.path.exists(nf)


def test_globbing_ln(fs):
    """Tests globbing for linking"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    oifs:
        files:
            oifsnc:
                name_in_work: input_expid_*_DATE_*.nc
                name_in_exp_tree: new_input_expid_*_NEW_DATE_*.nc
                type: outdata
        experiment_outdata_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_outdata_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """

    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date
    files = [
        "input_expid_11_DATE_12.nc",
        "input_expid_21_DATE_22.nc",
        "input_expid_31_DATE_32.nc",
    ]
    for f in files:
        fs.create_file(Path(config["general"]["thisrun_work_dir"]).joinpath(f))

    fs.create_dir(config["oifs"]["experiment_outdata_dir"])

    new_files = [
        "new_input_expid_11_NEW_DATE_12.nc",
        "new_input_expid_21_NEW_DATE_22.nc",
        "new_input_expid_31_NEW_DATE_32.nc",
    ]
    expected_new_paths = []
    for f in new_files:
        expected_new_paths.append(
            Path(config["oifs"]["experiment_outdata_dir"]).joinpath(f)
        )

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.oifsnc")
    sim_file.ln("work", "exp_tree")

    for nf in expected_new_paths:
        assert os.path.exists(nf)

def test_makedirs_in_name(fs):
    """Tests for creating the subfolders included in the target name"""

    dummy_config = """
    general:
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
        all_model_filetypes: [analysis, bin, config, forcing, input, couple, log, mon, outdata, restart, viz, ignore]
    oifs:
        files:
            ICMGG:
                name_in_work: ICMGG_input_expid+in_work
                name_in_exp_tree: out/date/folder/ICMGG_input_expid
                type: outdata
        experiment_outdata_dir: /work/ollie/pgierz/some_exp/input/oifs
        thisrun_outdata_dir: /work/ollie/pgierz/some_exp/run_20010101-20010101/input/oifs
    """

    date = esm_calendar.Date("2000-01-01T00:00:00")
    config = yaml.safe_load(dummy_config)
    config["general"]["current_date"] = date

    fs.create_file(Path(config["general"]["thisrun_work_dir"]).joinpath(f"ICMGG_input_expid+in_work"))

    fs.create_dir(config["oifs"]["experiment_outdata_dir"])

    #config["oifs"]["experiment_outdata_dir"] = "/work/ollie/pgierz/some_exp/input/oif" 

    expected_new_path = Path(config["oifs"]["experiment_outdata_dir"]).joinpath(
        "out/date/folder/ICMGG_input_expid"
    )

    sim_file = esm_runscripts.filedicts.SimulationFile(config, "oifs.files.ICMGG")
    sim_file.cp("work", "exp_tree")

    assert os.path.exists(expected_new_path)

