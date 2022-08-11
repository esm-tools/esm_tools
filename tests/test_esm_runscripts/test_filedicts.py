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
from io import StringIO
from pathlib import Path

import yaml
import pytest

import esm_runscripts.filedicts as filedicts

import esm_runscripts.filedicts


class Capturing(list):
    """Taken from https://stackoverflow.com/questions/16571150/how-to-capture-stdout-output-from-a-python-function-call"""
    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self
    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        del self._stringio    # free up some memory
        sys.stdout = self._stdout


@pytest.fixture()
def config():
    """Generates fake config to be used before each test"""
    fake_config = dict()
    yield fake_config


def test_example(fs):
    # Make a fake config:
    config = """
    general:
        base_dir: /some/dummy/location/
    echam:
        simulation_files:
            jan_surf:
                name: ECHAM Jan Surf File
                path_in_computer: /work/ollie/pool/ECHAM
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
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
    config = yaml.safe_load(dummy_config)
    # Not needed for this test, just a demonstration:
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    assert sim_file["name_in_work"] == "unit.24"
    assert sim_file.path_in_work == Path(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    )
    assert sim_file._config == config
    assert sim_file.locations["computer"] == Path("/work/ollie/pool/ECHAM/T63")


def test_cp_file(fs):
    """Tests for ``filedicts.cp`` copying file"""

    dummy_config = """
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: /work/ollie/pool/ECHAM/T63/
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
    """
    config = yaml.safe_load(dummy_config)

    # Set source and targets
    target_folder = config["echam"]["thisrun_work_dir"]
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
    oifs:
        files:
            o3_data:
                name_in_computer: o3chem_l91
                name_in_work: o3chem_l91
                path_in_computer: /work/ollie/pool/OIFS/159_4
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
    """
    config = yaml.safe_load(dummy_config)

    # Set source and targets
    target_folder = config["oifs"]["thisrun_work_dir"]
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


def test_resolve_file_movements(config):
    # act
    config = filedicts.resolve_file_movements(config)

    # assert
    assert isinstance(config, dict)


def test_mv(fs):
    """Tests for mv"""
    dummy_config = """
    general:
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                path_in_computer: /work/ollie/pool/ECHAM/T63/
                name_in_work: unit.24
                path_in_work: .
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
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


def test_check_path_in_computer_is_abs(fs):
    """
    Tests that ``esm_parser.user_error`` is use when the ``path_in_computer``
    is not absolute
    """

    dummy_config = """
    general:
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
        exp_dir: "/work/ollie/pgierz/some_exp"
    echam:
        files:
            jan_surf:
                name_in_computer: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_computer: pool/ECHAM/T63
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    """
    config = yaml.safe_load(dummy_config)

    # Captures output (i.e. the user-friendly error)
    with Capturing() as output:
        with pytest.raises(SystemExit) as error:
            sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")

    # error needs to occur as the path is not absolute
    assert any(["ERROR: File Dictionaries" in line for line in output])
