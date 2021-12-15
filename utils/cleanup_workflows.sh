#!/bin/bash -e

gh workflow list # Pick-up the workflow ID for which you want to delete all runs
echo "Which workflow to delete?"
read WORKFLOW_ID

# List last 10 runs of the workflow you picked to double check the id
gh run list -L 10 -w $WORKFLOW_ID

# Some set up
REPO_INFO=$(gh repo view --json name,owner)
REPO_FULL_NAME="$(echo $REPO_INFO | jq '.owner.login' -r)/$(echo $REPO_INFO | jq '.name' -r)"

# Ready? Let's delete some runs!
gh api -X GET "/repos/$REPO_FULL_NAME/actions/workflows/$WORKFLOW_ID/runs?per_page=100" | jq '.workflow_runs[] | .id' -r | xargs -t -I{} gh api --silent -X DELETE /repos/$REPO_FULL_NAME/actions/runs/{}
