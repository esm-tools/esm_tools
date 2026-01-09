"""
Jacamar CI integration for ESM Tools

This module provides functionality to submit SLURM jobs via GitLab CI pipeline
triggers using the Jacamar executor, as an alternative to direct sbatch submission.
"""

import os
import re
import subprocess

import requests
from loguru import logger


class JacamarSubmitter:
    """
    Submit jobs to SLURM via GitLab CI pipeline triggers with Jacamar executor.

    This class handles:
    - Auto-detection of GitLab project ID and ref from git remote
    - Formatting script sections for GitLab CI job structure
    - Triggering GitLab pipelines via API
    - Error handling and logging

    Attributes
    ----------
    gitlab_url : str
        Base URL of the GitLab instance
    token : str
        GitLab trigger token for authentication
    project_id : str
        URL-encoded GitLab project path
    ref : str
        Git branch name
    """

    def __init__(self, config):
        """
        Initialize the Jacamar submitter.

        Parameters
        ----------
        config : dict
            ESM tools configuration dictionary

        Raises
        ------
        Exception
            If GitLab configuration is invalid or git detection fails
        """
        self.gitlab_url = config["computer"].get("jacamar_gitlab_url", "")
        self.token = config["computer"].get("jacamar_trigger_token", "")

        # Validate required configuration
        if not self.gitlab_url:
            raise Exception(
                "jacamar_gitlab_url not configured. "
                "Add it to your SLURM configuration file."
            )

        if not self.token:
            raise Exception(
                "jacamar_trigger_token not set. "
                "Set the CI_JOB_TOKEN environment variable or configure "
                "jacamar_trigger_token in your configuration."
            )

        # Auto-detect project ID and ref from git
        self.project_id, self.ref = self._detect_git_info()
        logger.info(f"Detected GitLab project: {self.project_id}, ref: {self.ref}")

    def _detect_git_info(self):
        """
        Extract GitLab project ID and current branch from CI env or git remote.

        Prefers GitLab CI environment variables (CI_PROJECT_PATH, CI_COMMIT_REF_NAME)
        when available, falling back to git commands for local execution.

        Returns
        -------
        tuple
            (project_id, ref) where project_id is URL-encoded project path
            and ref is the current git branch name

        Raises
        ------
        Exception
            If git repository is not found or remote URL cannot be parsed
        """
        # Prefer GitLab CI environment variables when running in CI
        ci_project_path = os.environ.get("CI_PROJECT_PATH")
        ci_ref = os.environ.get("CI_COMMIT_REF_NAME")

        if ci_project_path and ci_ref:
            # Running in GitLab CI - use environment variables
            project_id = ci_project_path.replace("/", "%2F")
            logger.debug(
                f"Using CI env vars: PROJECT_PATH={ci_project_path}, REF={ci_ref}"
            )
            return project_id, ci_ref

        # Fallback to git detection for local development
        try:
            # Get remote URL
            remote_url = (
                subprocess.check_output(
                    ["git", "remote", "get-url", "origin"], stderr=subprocess.DEVNULL
                )
                .decode()
                .strip()
            )

            # Parse project path from URL
            # Handles both HTTPS and SSH formats:
            # https://codebase.helmholtz.cloud/user/project.git
            # git@codebase.helmholtz.cloud:user/project.git
            match = re.search(r"[:/]([\w\-\.]+/[\w\-\.]+)(?:\.git)?$", remote_url)
            if not match:
                raise ValueError(f"Could not parse project from remote: {remote_url}")

            project_path = match.group(1)
            # Strip .git suffix if present (can be captured by [\w\-\.]+ pattern)
            if project_path.endswith(".git"):
                project_path = project_path[:-4]
            # URL-encode the project path for GitLab API
            project_id = project_path.replace("/", "%2F")

            # Get current branch
            ref = (
                subprocess.check_output(
                    ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                    stderr=subprocess.DEVNULL,
                )
                .decode()
                .strip()
            )

            # In detached HEAD state, try to get branch from symbolic ref
            if ref == "HEAD":
                try:
                    ref = (
                        subprocess.check_output(
                            ["git", "symbolic-ref", "--short", "HEAD"],
                            stderr=subprocess.DEVNULL,
                        )
                        .decode()
                        .strip()
                    )
                except subprocess.CalledProcessError:
                    logger.warning(
                        "Detached HEAD state detected. Using 'main' as default ref. "
                        "Set CI_COMMIT_REF_NAME environment variable to override."
                    )
                    ref = "main"

            return project_id, ref

        except subprocess.CalledProcessError as e:
            logger.error("Failed to detect git information")
            raise Exception(
                "Could not auto-detect GitLab project. Ensure you're in a git repository "
                "with a remote pointing to your GitLab instance."
            )

    def submit_via_api(self, sbatch_headers, script_sections, config):
        """
        Submit job by triggering GitLab pipeline.

        Parameters
        ----------
        sbatch_headers : list of str
            SBATCH directives without #SBATCH prefix
        script_sections : dict
            Dictionary with 'before_script', 'script', 'after_script' keys
        config : dict
            Full ESM tools configuration dictionary

        Returns
        -------
        dict
            GitLab pipeline data including web_url and id

        Raises
        ------
        Exception
            If GitLab API request fails
        """
        endpoint = (
            f"{self.gitlab_url}/api/v4/projects/{self.project_id}" f"/trigger/pipeline"
        )

        # Format SBATCH headers for Jacamar SCHEDULER_PARAMETERS
        scheduler_params = " ".join(sbatch_headers)

        payload = {
            "token": self.token,
            "ref": self.ref,
            "variables[SCHEDULER_PARAMETERS]": scheduler_params,
            "variables[BEFORE_SCRIPT]": script_sections["before_script"],
            "variables[EXECUTION_SCRIPT]": script_sections["script"],
            "variables[AFTER_SCRIPT]": script_sections["after_script"],
            "variables[EXPERIMENT_ID]": config["general"]["expid"],
            "variables[RUN_DATE]": str(config["general"]["current_date"]),
            # Skip prepare, build, and child pipeline creation for SLURM resubmission
            "variables[SKIP_CHECK_MODEL_EXISTS]": "true",
            "variables[SKIP_TRIGGER_BUILD]": "true",
            "variables[SKIP_TRIGGER_COMPUTE]": "true",
            # Flag to indicate this is an esm_tools compute job trigger
            "variables[ESM_TOOLS_COMPUTE]": "true",
        }

        logger.info("PAYLOAD INFORMATION")
        for key, value in payload.items():
            logger.info(f"{key}: {value}")

        logger.info(f"Triggering GitLab pipeline at {endpoint}")
        logger.debug(f"Ref: {self.ref}, Project: {self.project_id}")

        response = requests.post(endpoint, data=payload)

        if response.status_code == 201:
            pipeline_data = response.json()
            logger.info(f"Pipeline triggered successfully!")
            logger.info(f"Pipeline URL: {pipeline_data['web_url']}")
            return pipeline_data
        else:
            logger.error(f"GitLab API request failed: {response.status_code}")
            logger.error(f"Response: {response.text}")
            raise Exception(
                f"Failed to trigger GitLab pipeline (HTTP {response.status_code}). "
                f"Check token, project ID, and network connectivity."
            )
