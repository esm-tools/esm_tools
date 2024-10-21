import rich_click as click

from .esm_database import DisplayDatabase


@click.command()
@click.argument("table", required=False)
def main(table):
    """Interactive tool to display database information."""
    DisplayDatabase(table)


if __name__ == "__main__":
    main()
