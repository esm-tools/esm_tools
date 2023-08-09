import argparse
import os


class Info(dict):
    """
    Dictionary subclass that contains all the information about the tests. The
    information is stored under the following `keys`:

    - ``actually_compile``: ``True`` if the test should compile or ``False`` if it
      should run a check instead.

    - ``actually_run``: ``True`` if the test should run or ``False`` if it should
      run a check instead.

    - ``bulletpoints``: when printing results or the state should it use bulletpoints
      for the items or no.

    - ``system_exit_on_errors``: triggers a system exit on errors or file differences
      so that GitHub actions can catch that as a failing test (defined by the ``-e``
      argument).

    - ``hold``: should it wait for user input after each test or not.

    - ``ignore``: lines to be ignored during comparison, loaded from
      ``<PATH>/esm_tools/src/esm_tests/resources/ignore_compare.yaml``.

    - ``ignore_user_info``: user-specific Info to remove from the files ``last_tested``
      files.

    - ``in_github``: indicates whether it is running in GitHub or not (defined by the
      ``-g`` argument).

    - ``last_tested_dir``: directory of the configuration files for the last tested
      experiments (i.e. ``<PATH>/esm_tools/src/esm_tests/resources/last_tested/``).

    - ``rm_user_info``: User-specific Info to remove from the files ``last_tested``
      files.

    - ``repo_update``: does it need to update the ``resources`` or not
      (specified by the user).

    - ``scripts``: dictionary including all information of every runscript to be run
      in the current computer.

    - ``script_dir``: directory of the ``esm_tests`` code, useful to call the
      ``resources`` and the ``resources/runscripts``.

    - ``this_computer``: name of the current computer

    - ``user``: ``esm_tests`` configurations specific to the user, as read from
      ``<PATH>/esm_tools/src/esm_tests/user_config.yaml``.

    - ``branch``: branch in the ``esm_tests_info`` to be used as ``last-state``

    """

    def __init__(self):
        self["script_dir"] = os.path.join(
            os.path.dirname(os.path.realpath(__file__)), "."
        )

    def argparse(self):
        """
        Parses the arguments from the ``esm_tests`` call.

        Returns
        -------
         Returns a tuple with the following entries:
        save_flag : str
            It's value can be ``true``, ``True``, ``false``, ``False``, or
            ``Not defined``.
        print_state : bool
            Indicates if ``esm_tests`` needs to report the state of the last actual
            or run tests (if ``False``).
        delete_tests : bool
            If ``True`` deletes the comp and experiment files before running the tests.
        """

        parser = argparse.ArgumentParser(
            description="Automatic testing for ESM-Tools devs"
        )
        parser.add_argument(
            "-n",
            "--no-user",
            default=False,
            help="Avoid loading user config",
            action="store_true",
        )
        parser.add_argument(
            "-c",
            "--check",
            default=False,
            help="Check mode on (does not compile or run, but produces some files that can "
            + "be compared to previous existing files in 'last_tested' folder)",
            action="store_true",
        )
        parser.add_argument(
            "-u",
            "--update",
            default=False,
            help="Updates the resources with the release branch, including runscripts"
            + "and last_tested files",
            action="store_true",
        )
        parser.add_argument(
            "-d",
            "--delete",
            default=False,
            help="Delete previous tests",
            action="store_true",
        )
        parser.add_argument(
            "-s",
            "--save",
            default="Not defined",
            help="Save files for comparisson in 'last_tested' folder",
        )
        parser.add_argument(
            "-t",
            "--state",
            default=False,
            help="Print the state stored in state.yaml",
            action="store_true",
        )
        parser.add_argument(
            "-o",
            "--hold",
            default=False,
            help="Hold before operation, to give time to check the output",
            action="store_true",
        )
        parser.add_argument(
            "-b",
            "--bulletpoints",
            default=False,
            help="bullet points for printing the results",
            action="store_true",
        )
        parser.add_argument(
            "-g",
            "--github",
            default=False,
            help="use this flag when running in GitHub servers",
            action="store_true",
        )
        parser.add_argument(
            "-e",
            "--system-exit-on-errors",
            default=False,
            help="trigger a system exit on errors or file differences so that GitHub actions can "
            + "catch that as a failing test",
            action="store_true",
        )
        parser.add_argument(
            "-r",
            "--branch",
            help="use the given esm_tests_info branch",
            default="release",
        )

        args = vars(parser.parse_args())

        save_flag = args["save"]
        print_state = args["state"]
        delete_tests = args["delete"]

        self["ignore_user_info"] = args["no_user"]
        self["actually_compile"] = not args["check"]
        self["actually_run"] = not args["check"]
        self["hold"] = args["hold"]
        self["bulletpoints"] = args["bulletpoints"]
        self["repo_update"] = args["update"]
        self["in_github"] = args["github"]
        self["system_exit_on_errors"] = args["system_exit_on_errors"]
        self["resources_branch"] = args["branch"]

        if self["in_github"]:
            self["group_output"] = {"startg": "::group::{0}", "endg": "::endgroup::"}
        else:
            self["group_output"] = {"startg": "{0}", "endg": ""}

        return save_flag, print_state, delete_tests
