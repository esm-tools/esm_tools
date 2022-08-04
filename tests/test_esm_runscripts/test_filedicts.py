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
