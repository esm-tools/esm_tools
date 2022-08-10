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
from pathlib import Path

import yaml

import esm_runscripts.filedicts


def test_example(fs):
    # Make a fake config:
    config = """
    general:
        base_dir: /some/dummy/location/
    echam:
        simulation_files:
            jan_surf:
                name: ECHAM Jan Surf File
                path_in_pool: /work/ollie/pool/ECHAM/T63CORE2_jan_surf.nc
                name_in_work: unit.24
    """
    config = yaml.safe_load(config)
    # Create some fake files and directories you might want in your test
    fs.create_file("/work/ollie/pool/ECHAM/T63CORE2_jan_surf.nc")
    fs.create_dir("/some/dummy/location/expid/run_18500101-18501231/work")
    # This module also have functions for link files, globbing, etc.
    config_out = esm_runscripts.filedicts.copy_files(config)
    assert os.path.exists(
        "/some/dummy/location/expid/run_18500101-18501231/work/unit.24"
    )


def test_filedicts_basics(fs):
    """Tests basic attribute behavior of filedicts"""

    dummy_config = """
    general:
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                name_in_pool: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                filetype: NetCDF
                description: >
                    Initial values used for the simulation, including
                    properties such as geopotential, temperature, pressure
    """
    config = yaml.safe_load(dummy_config)
    # Not needed for this test, just a demonstration:
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    assert sim_file["name_in_work"] == "unit.24"
    assert sim_file.work == Path(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
    )
    assert sim_file._config == config
    assert sim_file.locations["pool"] == Path("/work/ollie/pool")


def test_cp(fs):
    """Tests for ``filedicts.cp``"""

    dummy_config = """
    echam:
        files:
            jan_surf:
                name_in_pool: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                path_in_pool: /work/ollie/pool/ECHAM/T63/
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
    """
    config = yaml.safe_load(dummy_config)

    # Set source and targets
    target_folder = config["echam"]["thisrun_work_dir"]
    source = Path(
        config["echam"]["files"]["jan_surf"]["path_in_pool"],
        config["echam"]["files"]["jan_surf"]["name_in_pool"],
    )
    target = Path(
        target_folder,
        config["echam"]["files"]["jan_surf"]["name_in_work"],
    )

    # Create files and folders
    fs.create_file(source)
    fs.create_dir(target_folder)

    # Test the method
    esm_runscripts.filedicts.SimulationFile.cp(source, target)

    assert os.path.exists(target)


def test_mv(fs):
    """Tests for mv"""
    dummy_config = """
    general:
        thisrun_work_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101/work"
        exp_dir: "/work/ollie/pgierz/some_exp"
        thisrun_dir: "/work/ollie/pgierz/some_exp/run_20010101-20010101"
    computer:
        pool_dir: "/work/ollie/pool"
    echam:
        files:
            jan_surf:
                name_in_pool: T63CORE2_jan_surf.nc
                path_in_pool: ECHAM/T63/
                name_in_work: unit.24
                path_in_work: .
        thisrun_work_dir: /work/ollie/mandresm/awiesm/run_20010101-20010101/work/
    """
    config = yaml.safe_load(dummy_config)
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    fs.create_dir("/work/ollie/pgierz/some_exp/run_20010101-20010101/work")
    assert os.path.exists("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = esm_runscripts.filedicts.SimulationFile(config, "echam.files.jan_surf")
    sim_file.mv("pool", "work")
    assert not os.path.exists("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    assert os.path.exists(
        "/work/ollie/pgierz/some_exp/run_20010101-20010101/work/unit.24"
    )
