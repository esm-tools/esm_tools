#!/usr/bin/env python3
"""
ESM Environment Tests
=====================

Provides tests for the ``esm_environment`` module
"""
import unittest

from esm_environment.esm_environment_new import ComputerSpecifications


class TestComputerSpecifications(unittest.TestCase):
    """Tests for ComputerSpecifications"""

    def test_init_with_computer(self):
        comp_spec = ComputerSpecifications(computer="ollie")
        self.assertTrue(
            all(
                module_cmd.startswith("module")
                for module_cmd in comp_spec.module_actions
            )
        )
        self.assertIn("module purge", comp_spec.module_actions)
        self.assertIn("F77", comp_spec.export_vars)
        self.assertEqual("#!/usr/bin/bash", comp_spec.shebang)


if __name__ == "__main__":
    unittest.main()
