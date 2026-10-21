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

    - ``this_computer``: name of the current computer (from ``--computer`` or from the
      hostname)

    - ``computers``: computers given with ``--computer`` (``None`` if not given).

    - ``computer_explicit``: ``True`` if the computer was given with ``--computer``,
      then ``esm_master`` and ``esm_runscripts`` are also called with ``--computer``.

    - ``model`` / ``runscript``: selection given with ``--model`` / ``--runscript``.
      When given, ``test_config.yaml`` is not read.

    - ``list``: print the tests that would run and exit (``--list``).

    - ``json``: print the ``--list`` output as JSON to stdout (``--json``).

    - ``results_file``: absolute path of the file to write the errors and differences
      to (``--results-file``), or ``None``.

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
        parser.add_argument(
            "--computer",
            action="append",
            default=None,
            help="test for this computer (name as in 'configs/machines/<name>.yaml') "
            + "instead of detecting it from the hostname. It can be given several "
            + "times only together with --list",
        )
        parser.add_argument(
            "--model",
            default=None,
            help="only test this model (folder name in 'resources/runscripts'), "
            + "instead of the selection in 'test_config.yaml'",
        )
        parser.add_argument(
            "--runscript",
            default=None,
            help="only test this runscript of --model (e.g. 'pi.yaml')",
        )
        parser.add_argument(
            "--list",
            default=False,
            help="print the tests (model, version, runscript, computer) that would run "
            + "and exit",
            action="store_true",
        )
        parser.add_argument(
            "--json",
            default=False,
            help="with --list, print the tests as JSON (list of computer, model, "
            + "runscript) to stdout",
            action="store_true",
        )
        parser.add_argument(
            "--results-file",
            default=None,
            help="write the errors and the differences with 'last_tested' of each test "
            + "to this file (JSON)",
        )

        args = vars(parser.parse_args())

        if args["json"] and not args["list"]:
            parser.error("--json requires --list")
        if args["runscript"] and not args["model"]:
            parser.error("--runscript requires --model")
        if args["computer"] and len(args["computer"]) > 1 and not args["list"]:
            parser.error("--computer can only be given several times with --list")

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
        self["computers"] = args["computer"]
        self["computer_explicit"] = bool(args["computer"])
        self["model"] = args["model"]
        self["runscript"] = args["runscript"]
        self["list"] = args["list"]
        self["json"] = args["json"]
        # Absolute, because ``esm_tests`` changes directory while testing
        self["results_file"] = args["results_file"] and os.path.abspath(
            args["results_file"]
        )

        # No ``::group::`` in GitHub either: GitHub collapses groups and they cannot be
        # expanded by default, which hides the differences of each test behind a click
        # if self["in_github"]:
        #     self["group_output"] = {"startg": "::group::{0}", "endg": "::endgroup::"}
        # else:
        #     self["group_output"] = {"startg": "{0}", "endg": ""}
        self["group_output"] = {"startg": "{0}", "endg": ""}

        return save_flag, print_state, delete_tests
