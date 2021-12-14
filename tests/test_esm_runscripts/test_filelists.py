import unittest

import esm_runscripts


class TestAllowedMissingFiles(unittest.TestCase):
    def setUp(self) -> None:
        self.test_config = {
            "general": {
                "valid_model_names": ["foo_model"],
                "files_missing_when_preparing_run": {
                    "/some/long/directory/in/pool/foo.dat": "/some/directory/in/experiment/work/foo.dat",
                    "/some/other/directory/in/user/bar.dat": "/some/directory/in/experiment/work/baz.dat",
                },
            },
            "foo_model": {},
        }
        return super().setUp()

    def test_undefined_missing(self):
        self.assertEqual(
            self.test_config,
            esm_runscripts.filelists.filter_allowed_missing_files(self.test_config),
        )

    def test_allowed_missing_foo_explicit(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["foo.dat"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/long/directory/in/pool/foo.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/long/directory/in/pool/foo.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_foo_dotglob(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["foo.*"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/long/directory/in/pool/foo.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/long/directory/in/pool/foo.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_foo_glob(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["foo*"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/long/directory/in/pool/foo.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/long/directory/in/pool/foo.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_bar_explicit(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["bar.dat"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_bar_dotglob(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["bar.*"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_bar_glob(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["bar*"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_baz_explicit(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["baz.dat"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_baz_dotglob(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["baz.*"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["allowed_missing_files"],
        )

    def test_allowed_missing_baz_glob(self):
        self.test_config["foo_model"].update({"allowed_missing_files": ["baz*"]})
        self.modifed_config = esm_runscripts.filelists.filter_allowed_missing_files(
            self.test_config
        )
        self.assertNotIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["files_missing_when_preparing_run"],
        )
        self.assertIn(
            "/some/other/directory/in/user/bar.dat",
            self.test_config["general"]["allowed_missing_files"],
        )


if __name__ == "__main__":
    unittest.main()
