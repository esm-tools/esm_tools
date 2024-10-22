"""Console script for esm_master."""

import rich_click as click

from esm_motd import check_all_esm_packages

from . import __version__
from .esm_master import main_flow


@click.command()
@click.argument("target", default="", required=False, metavar="target")
@click.option(
    "--check",
    "-c",
    is_flag=True,
    help="Show what would be done, without making any changes.",
)
@click.option("--verbose", "-v", count=True, help="Toggle verbose mode.")
@click.option("--modify-config", "-m", default="", help="Modify configuration.")
@click.option("--ignore-errors", "-i", is_flag=True, help="Ignore errors.")
@click.option(
    "--keep-task-script",
    "-k",
    is_flag=True,
    help="Keep shell script generated to perform compilation/configuration jobs.",
)
@click.option("--generate-tab-complete", is_flag=True, help="Generate tab completion.")
@click.option("--list-all-targets", is_flag=True, help="List all available targets.")
@click.option("--no-motd", is_flag=True, help="Suppress the printing of MOTD.")
@click.version_option(version=__version__, help="Show the version and exit.")
def cli(
    target,
    check,
    verbose,
    modify_config,
    ignore_errors,
    keep_task_script,
    generate_tab_complete,
    list_all_targets,
    no_motd,
):
    """Tool for downloading, configuring, and compiling."""

    # Check ESM packages if not suppressing MOTD
    if not no_motd:
        check_all_esm_packages()

    # Prepare arguments for main flow
    parsed_args = {
        "target": target,
        "check": check,
        "verbose": verbose,
        "modify": modify_config,
        "ignore": ignore_errors,
        "keep": keep_task_script,
        "generate_tab_complete": generate_tab_complete,
        "list_all_targets": list_all_targets,
        "no_motd": no_motd,
    }

    # Execute main flow
    return main_flow(parsed_args, target)


def create_command(command_name, docstring=None):
    """Dynamically create command functions."""
    # FIXME(PG): I don't like needing to decorate everything twice...

    @click.command(name=command_name, help=docstring)
    @click.argument("arg", default="", required=False, metavar="target")
    @click.option(
        "--check",
        "-c",
        is_flag=True,
        help="Show what would be done, without making any changes.",
    )
    @click.option("--verbose", "-v", count=True, help="Toggle verbose mode.")
    @click.option("--modify-config", "-m", default="", help="Modify configuration.")
    @click.option("--ignore-errors", "-i", is_flag=True, help="Ignore errors.")
    @click.option(
        "--keep-task-script",
        "-k",
        is_flag=True,
        help="Keep shell script generated to perform compilation/configuration jobs.",
    )
    @click.option(
        "--generate-tab-complete", is_flag=True, help="Generate tab completion."
    )
    @click.option(
        "--list-all-targets", is_flag=True, help="List all available targets."
    )
    @click.option("--no-motd", is_flag=True, help="Suppress the printing of MOTD.")
    @click.version_option(version=__version__, help="Show the version and exit.")
    def command(
        arg,
        check,
        verbose,
        modify_config,
        ignore_errors,
        keep_task_script,
        generate_tab_complete,
        list_all_targets,
        no_motd,
    ):
        target = f"{command_name}-{arg}" if arg else command_name
        # Check ESM packages if not suppressing MOTD
        if not no_motd:
            check_all_esm_packages()

        # Prepare arguments for main flow
        parsed_args = {
            "target": target,
            "check": check,
            "verbose": verbose,
            "modify": modify_config,
            "ignore": ignore_errors,
            "keep": keep_task_script,
            "generate_tab_complete": generate_tab_complete,
            "list_all_targets": list_all_targets,
            "no_motd": no_motd,
        }

        # Execute main flow
        return main_flow(parsed_args, target)

    return command


def create_meta_command(command_name, steps, docstring=None):
    """Dynamically create composite command functions.

    Args:
        command_name: Name of the composite command (e.g., "install")
        steps: List of steps to execute in sequence (e.g., ["get", "conf", "comp"])
        docstring: Help text for the command
    """
    if docstring is None:
        step_text = ", ".join(steps)
        docstring = f"Run sequence: {step_text}"

    @click.command(name=command_name, help=docstring)
    @click.argument("arg", default="", required=False, metavar="target")
    @click.option(
        "--check",
        "-c",
        is_flag=True,
        help="Show what would be done, without making any changes.",
    )
    @click.option("--verbose", "-v", count=True, help="Toggle verbose mode.")
    @click.option("--modify-config", "-m", default="", help="Modify configuration.")
    @click.option("--ignore-errors", "-i", is_flag=True, help="Ignore errors.")
    @click.option(
        "--keep-task-script",
        "-k",
        is_flag=True,
        help="Keep shell script generated to perform compilation/configuration jobs.",
    )
    @click.option(
        "--generate-tab-complete", is_flag=True, help="Generate tab completion."
    )
    @click.option(
        "--list-all-targets", is_flag=True, help="List all available targets."
    )
    @click.option("--no-motd", is_flag=True, help="Suppress the printing of MOTD.")
    @click.version_option(version=__version__, help="Show the version and exit.")
    def command(
        arg,
        check,
        verbose,
        modify_config,
        ignore_errors,
        keep_task_script,
        generate_tab_complete,
        list_all_targets,
        no_motd,
    ):
        # Check ESM packages if not suppressing MOTD
        if not no_motd:
            check_all_esm_packages()

        # Prepare base arguments that will be used for all steps
        base_args = {
            "check": check,
            "verbose": verbose,
            "modify": modify_config,
            "ignore": ignore_errors,
            "keep": keep_task_script,
            "generate_tab_complete": generate_tab_complete,
            "list_all_targets": list_all_targets,
            "no_motd": no_motd,
        }

        # Execute each step in sequence
        for step in steps:
            target = f"{step}-{arg}" if arg else step
            parsed_args = base_args.copy()
            parsed_args["target"] = target

            # Execute the step
            result = main_flow(parsed_args, target)

            # If a step fails and we're not ignoring errors, stop the sequence
            if result != 0 and not ignore_errors:
                return result

        return 0

    return command


if __name__ == "__main__":
    cli()
