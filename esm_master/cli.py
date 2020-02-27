"""Console script for esm_master."""
import argparse
import sys

check = False
verbose = 0

from .esm_master import *


def main():

    #global check, verbose

    parser = argparse.ArgumentParser(
        prog="esm_master",
        description="tool for downloading, configuring and compiling.",
    )
    parser.add_argument(
        "target",
        metavar="target",
        nargs="?",
        type=str,
        help="name of the target (leave empty for full list of targets)",
    )
    parser.add_argument(
        "--check",
        "-c",
        action="store_true",
        default=False,
        help="show what would be done, not doing anything",
    )
    parser.add_argument(
        "--verbose", "-v", action="count", default=0, help="toggle verbose mode"
    )
    parser.add_argument(
        "--version", action="version", version="%(prog)s 3.0 (Oct 01, 2019)"
    )
    parsed_args = vars(parser.parse_args())

    check = False
    verbose = 0
    target = ""

    if parsed_args:
        if "target" in parsed_args:
            target = parsed_args["target"]
        if "check" in parsed_args:
            check = parsed_args["check"]
        if "verbose" in parsed_args:
            verbose = parsed_args["verbose"]

    if not target:
        target = ""

    main_infos = general_infos()
    vcs = version_control_infos()
    setups2models = setup_and_model_infos(vcs, main_infos)
    env = esm_environment.environment_infos()
    setups2models.config = setups2models.reduce(target, env)

    env.apply_config_changes("compiletime", setups2models.config)
    env.add_esm_var()

    user_task = task(target, setups2models, vcs, main_infos)
    if verbose > 0:
        user_task.output()

    user_task.output_steps()

    if check:
        sys.exit(0)
    user_task.validate()
    env.write_dummy_script()

    user_task.execute(env)

    sys.exit(0)
    """Console script for esm_master."""
    parser = argparse.ArgumentParser()
    parser.add_argument('_', nargs='*')
    args = parser.parse_args()

    print("Arguments: " + str(args._))
    print("Replace this message by putting your code into "
          "esm_master.cli.main")
    return 0


if __name__ == "__main__":
    main(sys.argv[1:])
