"""Console script for esm_master."""
import argparse
import sys

# import logging
# logging.basicConfig(level=logging.DEBUG)
from . import __version__
from esm_motd import check_all_esm_packages
from .esm_master import main_flow


def main():

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
        "--verbose", "-v", action="count", default=False, help="toggle verbose mode"
    )
    parser.add_argument(
        "--version", action="version", version="%(prog)s " + __version__
    )

    parser.add_argument(
        "--modify-config",
        "-m",
        dest="modify",
        help="[m]odify configuration",
        default="",  # kh 15.07.20 "usermods.yaml"
    )

    # kh 21.07.20
    parser.add_argument(
        "--ignore-errors",
        "-i",
        dest="ignore",
        help="Ignore errors",
        default=False,
        action="store_true",
    )

    parser.add_argument(
        "--keep-task-script",
        "-k",
        dest="keep",
        action="store_true",
        default=False,
        help="Keep shell script generated to perform compilation/configuration jobs",
    )
    parser.add_argument("--generate_tab_complete", action="store_true")
    parser.add_argument("--list_all_targets", action="store_true")

    parser.add_argument(
        "--no-motd",
        help="supress the printing of MOTD",
        default=False,
        action="store_true",
    )

    parsed_args = vars(parser.parse_args())

    target = ""
    check = False
    verbose = False
    modify_config_file = False
    no_motd = False

    if parsed_args:
        if "target" in parsed_args:
            target = parsed_args["target"]
        if "check" in parsed_args:
            check = parsed_args["check"]
        if "verbose" in parsed_args:
            verbose = parsed_args["verbose"]
        if "keep" in parsed_args:
            keep = parsed_args["keep"]
        if "modify" in parsed_args:
            modify_config_file = parsed_args["modify"]
        if "no_motd" in parsed_args:
            no_motd = parsed_args["no_motd"]

    if not target:
        target = ""

    if not no_motd:
        check_all_esm_packages()

    main_flow(parsed_args, target)


if __name__ == "__main__":
    main(sys.argv[1:])
