"""Console script for esm_master."""

import rich_click as click

from esm_motd import check_all_esm_packages

from . import __version__
from .esm_master import main_flow


@click.group()
@click.version_option(version=__version__, help="Show the version and exit.")
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
@click.pass_context
def cli(
    ctx,
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
    ctx.ensure_object(dict)
    ctx.obj["check"] = check
    ctx.obj["verbose"] = verbose
    ctx.obj["modify_config"] = modify_config
    ctx.obj["ignore_errors"] = ignore_errors
    ctx.obj["keep_task_script"] = keep_task_script
    ctx.obj["generate_tab_complete"] = generate_tab_complete
    ctx.obj["list_all_targets"] = list_all_targets
    ctx.obj["no_motd"] = no_motd

    # Check if the first argument is not a registered subcommand
    if ctx.invoked_subcommand is None:
        target = ctx.args[0]  # Use the first argument as the target
        main(target, **ctx.obj)  # Call main with the user-supplied target


def create_command(command_name, docstring=None):
    """Dynamically create command functions."""

    @click.command(name=command_name, help=docstring)
    @click.argument("arg", required=False, metavar="arg")
    @click.pass_context
    def command(ctx, arg=None):
        options = ctx.obj
        target = f"{command_name}-{arg}" if arg else command_name
        main(target, **options)

    return command


# Create commands dynamically
cli.add_command(create_command("get", "Download model code"))
cli.add_command(create_command("conf", "Configure code for compilation"))
cli.add_command(create_command("comp", "Compile code"))
cli.add_command(create_command("clean", "Clean up compilation artifacts"))


def main(
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
    """Run the main flow."""
    options = {
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

    if not no_motd:
        check_all_esm_packages()

    main_flow(options, target)


if __name__ == "__main__":
    cli()
