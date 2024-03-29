import unittest

import esm_runscripts


class TestHelpersUpdateReusableFiles(unittest.TestCase):
    def setUp(self) -> None:
        self.config = {
            "general": {
                "verbose": True,
                "command_line_config": {
                    "update_filetypes": [],
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
        self.config["general"]["command_line_config"]["update_filetypes"] = ["extra"]
        self.config["general"]["potentially_reusable_filetypes"].append("extra")
        self.config["foo_model"] = {}
        self.config["foo_model"]["reusable_filetypes"] = ["input", "forcing", "extra"]
        # NOTE(PG): Remember, this gives back a list if the second argument
        # given to update_reusable_filetypes is defined as a list!
        modified_reusable_filetypes = esm_runscripts.helpers.update_reusable_filetypes(
            self.config, self.config["foo_model"]["reusable_filetypes"]
        )
        self.assertNotIn("extra", modified_reusable_filetypes)


if __name__ == "__main__":
    unittest.main()
