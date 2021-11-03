#!/usr/bin/env python

""" Tests for ``esm_runscripts.namelist``"""
import unittest
import tempfile
import shutil
import yaml

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


class TestNamelist(unittest.TestCase):
    """Test the various static methods of Namelist"""

    def setUp(self):
        """Prepares everything to might be needed"""
        self.config = yaml.safe_load(CONFIG_YAML)
        self.mconfig = self.config["echam"]
        self._thisrun_config_dir = tempfile.mkdtemp()
        self.mconfig["thisrun_config_dir"] = self._thisrun_config_dir
        print(self.mconfig)

    def tearDown(self):
        shutil.rmtree(self._thisrun_config_dir)

    def test_nmls_load(self):
        """Tests whether namelists can be loaded correctly"""
        config = namelists.Namelist.nmls_load(self.mconfig)
        print(config)
        assert config
