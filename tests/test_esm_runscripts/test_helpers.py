import unittest

import esm_runscripts


class TestHelpersUpdateReusableFiles(unittest.TestCase):
    def setUp(self) -> None:
        self.config = {
            "general": {
                "verbose": True,
                "command_line_config": {
                    "update_files": [],
                },
                "potentially_reusable_filetypes": ["bin", "log", "src", "forcing"],
                "reusable_filetypes": ["bin", "log", "src"],
            },
        }
        return super().setUp()

    def test_cli_bin_in_general(self) -> None:
        """ "
        Mimic a call such as::

            $ esm_runscripts run.yaml -e exp1 --update-filetypes bin
        """
        # NOTE(PG): It would be nice to find a way to call the actual cli code here...
        self.config["general"]["command_line_config"]["update_filetypes"] = ["bin"]
        out_config = esm_runscripts.helpers.update_reusable_filetypes(self.config)
        self.assertNotIn("bin", out_config["general"]["reusable_filetypes"])

    def test_cli_extra_in_model(self) -> None:
        """
        Mimic a call such as::

            $ esm_runscripts run.yaml -e exp1 --update-filetypes extra

        Where the run.yaml has a model ``foo_model`` with the following
        ``reusable_fletypes``::

            foo_model:
               reusable_filetypes: ["input", "forcing", "extra"]
        """
        self.config["general"]["command_line_config"]["update_filestypes"] = ["extra"]
        self.config["foo_model"] = {}
        self.config["foo_model"]["reusable_filetypes"] = ["input", "forcing", "extra"]
        out_config = esm_runscripts.helpers.update_reusable_filetypes(self.config)
        self.assertNotIn("extra", out_config["general"]["reusable_filetypes"])
        self.assertNotIn("extra", out_config["foo_model"]["reusable_filetypes"])


if __name__ == "__main__":
    unittest.main()
