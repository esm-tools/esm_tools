#!/usr/bin/env python3
from github import Github
import questionary
import os
import sys


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
