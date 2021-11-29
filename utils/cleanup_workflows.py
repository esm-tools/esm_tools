#!/usr/bin/env python3
"""
cleanup_workflows.py
--------------------

You can use this script to cleanup and remove workflows from GitHub Actions.
All runs associated with a particular workflow are deleted online::

    $ export GITHUB_TOKEN=abc12345  # Put your own token here
    $ python cleanup_workflows.py
    ? Which workflows to erase? (Use arrow keys to move, <space> to select, <a> to toggle, <i> to invert)
     » ○ esm-tools main test
       ○ esm-tools no Docker
       ○ Flake8
       ○ autoblack
       ○ esm-tools Ollie
       ○ Tests for FESOM Standalone
       ○ Tests for PISM Standalone
       ○ Tests for ECHAM Standlone
       ○ Tests for AWICM
       ○ Tests for AWIESM

After selection, all associated runs with that workflow are erased, and the
workflow history will disappear online on GitHub actions.
"""
from github import Github
import questionary
import os
import sys


def main():
    github_token = os.environ.get("GITHUB_TOKEN")
    if not github_token:
        print("You need to define GITHUB_TOKEN as an environment variable!")
        sys.exit(1)

    g = Github(github_token)

    esm_tools = g.get_repo("esm-tools/esm_tools")
    workflows = list(esm_tools.get_workflows())

    workflows_to_delete = questionary.checkbox(
        "Which workflows to erase?", choices=[w.name for w in workflows]
    ).ask()

    filtered_workflows = [w for w in workflows if w.name in workflows_to_delete]
    for w in filtered_workflows:
        print(f"Removing all runs for {w.name}")
        runs = w.get_runs()
        for run in runs:
            print(f"Deleting: {run.id}")
            run.delete()


if __name__ == "__main__":
    main()
