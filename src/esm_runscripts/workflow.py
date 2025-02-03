"""
The ESM-Tools Workflow Manager

Terminology
-----------
job:        An encapsulated esm-tools workflow such as tidy, prepexp, prepcompute,
            designated by a Prefect Flow (using the @flow decorator)
cluster:    A collection of jobs that are grouped together (flow of flows) --> all
            written down in 1 HPC Job Scheduler script, better name soon...
task:       A single unit of work within a job
"""

from enum import Enum

import matplotlib.pyplot as plt
import networkx as nx
import randomname
from loguru import logger
from prefect import flow
from rich.columns import Columns
from rich.console import Console, ConsoleOptions, RenderResult
from rich.panel import Panel
from rich.text import Text


class SubmissionType(Enum):
    """Enum for the different submission types"""

    BATCH = "batch"
    SHELL = "shell"
    SIM_OBJECT = "sim_object"

    def __rich__(self) -> str:
        colors = {
            SubmissionType.BATCH: "blue",
            SubmissionType.SHELL: "green",
            SubmissionType.SIM_OBJECT: "magenta",
        }
        return Text(self.value, style=colors[self])


class Workflow:
    """A cyclable collection of clusters"""


class Cluster:
    """A collection of jobs"""

    def __init__(self, jobs=None, name=None):
        self.name = name or randomname.get_name()
        self.jobs = jobs or []

        self._call_order = []
        self.G = nx.DiGraph()
        self.order_jobs()

    def order_jobs(self):
        """Order jobs using topological sorting and detect execution levels."""
        self.G.clear()
        job_lookup = {job.name: job for job in self.jobs}

        # Add nodes
        for job in self.jobs:
            self.G.add_node(job.name, submission_type=job.submission_type)

        # Add edges based on dependencies
        for job in self.jobs:
            if job.run_after:
                for dep in job.run_after:
                    if dep not in job_lookup:
                        raise ValueError(f"Dependency '{dep}' not found in jobs!")
                    self.G.add_edge(job_lookup[dep].name, job.name)

            if job.run_before:
                for dep in job.run_before:
                    if dep not in job_lookup:
                        raise ValueError(f"Dependency '{dep}' not found in jobs!")
                    self.G.add_edge(job.name, job_lookup[dep].name)

        # Detect cycles
        if not nx.is_directed_acyclic_graph(self.G):
            raise ValueError("Circular dependency detected in job ordering!")

        # Perform topological sort
        sorted_job_names = list(nx.topological_sort(self.G))
        self._call_order = [job_lookup[name] for name in sorted_job_names]

        # Compute job levels (execution layers)
        levels = {job_name: 0 for job_name in sorted_job_names}
        for job in sorted_job_names:
            for dep in self.G.predecessors(job):
                levels[job] = max(levels[job], levels[dep] + 1)

        # Store levels as node attributes
        nx.set_node_attributes(self.G, levels, "subset")

        # Create a dictionary of levels and their jobs
        self._call_order_by_level = {}
        for job_name, level in levels.items():
            if level not in self._call_order_by_level:
                self._call_order_by_level[level] = []
            self._call_order_by_level[level].append(job_lookup[job_name])

        logger.warning(f"Job execution order: {sorted_job_names}")

    def draw_graph(self):
        """Draw the job dependency graph with execution levels and colored nodes."""
        plt.figure(figsize=(10, 6))

        # Define colors for different SubmissionTypes
        type_colors = {
            SubmissionType.BATCH: "blue",
            SubmissionType.SHELL: "green",
            SubmissionType.SIM_OBJECT: "magenta",
        }
        node_colors = [
            type_colors[self.G.nodes[node]["submission_type"]] for node in self.G.nodes
        ]

        # Position nodes based on execution level stored in "subset"
        pos = nx.multipartite_layout(self.G, subset_key="subset")

        # Draw the graph
        nx.draw(
            self.G,
            pos,
            with_labels=True,
            node_color=node_colors,
            edge_color="gray",
            arrows=True,
            node_size=2000,
            font_size=10,
        )

        plt.title(f"Dependency Graph for {self.name}")
        plt.show()

    def add_job(self, job):
        self.jobs.append(job)
        self.order_jobs()

    def __rich_console__(
        self, console: Console, options: ConsoleOptions
    ) -> RenderResult:
        job_panels = []
        for job in self.jobs:
            job_panels.append(job)
        job_columns = Columns(job_panels)
        panel = Panel(
            job_columns,
            title="Cluster",
            title_align="left",
            subtitle=self.name,
            border_style="blue",
        )
        yield panel


class Job:
    """One phase of esm-tools"""

    def __init__(
        self,
        name=None,
        steps=None,
        submission_type=None,
        nproc=None,
        run_after=None,
        run_before=None,
        script=None,
        order_in_cluster="sequential",
    ):
        self.name = name or randomname.get_name()
        self.steps = steps or []
        self.submission_type = submission_type or SubmissionType.SIM_OBJECT
        self.nproc = nproc or 1
        self.run_after = run_after
        self.run_before = run_before
        self.script = script
        self.order_in_cluster = order_in_cluster

        # Sanity checks: run_after and run_before must be lists
        if self.run_after and not isinstance(self.run_after, list):
            self.run_after = [self.run_after]
        if self.run_before and not isinstance(self.run_before, list):
            self.run_before = [self.run_before]

        # Sanity checks: steps must be callable
        for step in self.steps:
            if not callable(step):
                raise ValueError(f"Step '{step}' is not callable!")

        # Sanity check: if submission type is script, script must be provided
        if self.submission_type == SubmissionType.SHELL and not self.script:
            raise ValueError("Submission type is shell but no script provided!")

    def __call__(self, config):
        @flow(name=self.name)
        def job_task(config):
            for step in self.steps:
                config = step(config)
            return config

        return job_task(config)

    def __rich__(self) -> RenderResult:
        step_list = Text()
        for index, step in enumerate(self.steps):
            step_list.append(
                f"* [{index+1}/{len(self.steps)}] {step.__name__}\n", style="bold"
            )
        step_list.append("\n\n")
        step_list.append("Submission type: ", style="dim")
        step_list.append(self.submission_type.__rich__())
        panel = Panel.fit(
            step_list,
            title="Job",
            title_align="left",
            subtitle=self.name,
            border_style="green",
        )
        return panel
