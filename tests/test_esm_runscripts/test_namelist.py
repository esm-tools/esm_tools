#!/usr/bin/env python

""" Tests for ``esm_runscripts.namelist``"""
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
import yaml

from io import StringIO

from esm_runscripts import namelists
import esm_tools


# This is an example of an in-code minimialized yaml that can be used for
# testing the namelist functionality:
CONFIG_YAML = """
echam:
    namelists:
        - "namelist.echam"
"""
TEST_NML_PATH = (
    "namelists/echam/6.3.05p2-concurrent_radiation-paleodyn/PI-CTRL/namelist.echam"
)


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


class TestNamelist(unittest.TestCase):
    """Test the various static methods of Namelist"""

    def setUp(self):
        """Prepares everything to might be needed"""
        self.config = yaml.safe_load(CONFIG_YAML)
        self.mconfig = self.config["echam"]
        self._nml_name = self.mconfig["namelists"][0]
        self._thisrun_config_dir = tempfile.mkdtemp()
        self.mconfig["thisrun_config_dir"] = self._thisrun_config_dir
        self._thisrun_namelist_path = os.path.join(self._thisrun_config_dir, self._nml_name)
        shutil.copy2(TEST_NML_PATH, self._thisrun_namelist_path)

    def tearDown(self):
        shutil.rmtree(self._thisrun_config_dir)

    def test_nmls_load(self):
        """Tests whether namelists can be loaded correctly"""
        config = namelists.Namelist.nmls_load(self.mconfig)
        print(config)
        assert config

    def test_nmls_syntax(self):
        """Checks that the syntax errors are catched and reported to the user"""
        # Break the namelist by removing the last line
        with open(self._thisrun_namelist_path, "r") as f:
            lines = f.readlines()
        with open(self._thisrun_namelist_path, "w") as f:
            for line in lines[:-1]:
                f.write(line)
        # Captures output (i.e. the user-friendly error)
        with Capturing() as output:
            try:
                namelists.Namelist.nmls_load(self.mconfig)
            except SystemExit as e:
                error = e

        # error needs to exist as the namelist has incorrect syntax
        assert isinstance(error, SystemExit)
        for line in output:
            print(line)
        assert any(["ERROR: Namelist format" in line for line in output])
