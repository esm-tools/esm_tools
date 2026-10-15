This is not a finished document! A lot of things are not yet set in stone or unclear.
It is meant to be use for starting developing and help us lock into a final design.

# Workflow manager execution plan

From https://github.com/esm-tools/esm_tools/issues/1613

ESM-Tools’ workflow manager is designed for HPC systems where no daemon can run on a login node or server. Instead, an initial plan sets up the workflow, prepares the first job script (sbatch/PBS) and submits it to the scheduler. Each job script contains a `plan`: a local daemon (an `esm_runscripts` call running in the background) that monitors the special inline `compute` subplan, writes batch scripts for the following plans and launches them. When the cluster finishes, the daemon submits the next cluster_plan, so the workflow resubmits itself.

This design follows two main requirements:

* No persistent daemon: the scheduler drives the workflow through self-submission.
* Close to a plain runscript: modelers care about how the simulation runs (environment, launch command, etc.), not about ESM-Tools. The compute block reads like a hand-written job script, so the ESM-Tools parts can be commented out and the simulation run as-is, which is useful for debugging.

## Common workflow vocabulary and WF elements hierarchy

Modified from https://github.com/esm-tools/esm_tools/issues/1519

From Most Coarse to Most Granular

| Classic term | Example | Current Name | New Name | CURIE Label | Ref |
| --- | --- | --- | --- | --- | --- |
| total workflow | a full simulation wf | experiment plan | P-Plan:Plan | <ref URL> |
| cluster | postprocess | cluster | plan | P-Plan:Plan | <ref URL> |
| phase | prepexp | phase, job | plan | P-Plan:Sub-Plan | <ref URL> |
| recipe step | `_read_date_file` | step | step | P-Plan:Step | <ref URL> |

## Definition, Requirements and decisions on the workflow elements

Needs revision, this are just some initial ideas.

### Experiment plan

Group of plans and their dependencies that once run leads to a finished experiment.

**Parent**: None
**Child**: Plans

### Plans

Group of sub-plans or steps and the building blocks of the experiment plan.

**Parent**: Experiment plan
**Child**: sub-plans

* A plan is either written down in a batch script (sbatch, pbs script, etc. with extension `.run`) or executed in the shell (to cover the experiment initialization plan)
* The plan writes the batch scripts that have direct dependencies to it and submits them
* The plan is the level in the workflow where cycling happens

### Sub-plans

Group of steps and their ordering.

**Parent**: sub-plans
**Child**: steps

* The compute subplan is a special sub-plan in the sense that it only has the compute step that is actually run directly from the batch script, and not triggered by the local daemon. It could also have another step that is observe, that could run concurrently, to observe and trigger the killing of jobs when the simulation fails and it detects it in the logs.
* We could consider making the sub-plans be possible to run concurrently, but this will complicate things a lot. The only use for concurrent sub-plans that we have is that observe-compute subplan needs to run in parallel to the compute, but we can also rethink that completely and make the compute subplan a special one where the observing part is included, so that concurrency is not a requirement, but something that can happen only internally. The parallelization can maybe happen at the slurm level (plans) and at dask level (steps)

### Steps

Python functions or bash scripts that make a subplan.

* Are run sequentially. Parallelization can be achieved via dask in python or internally in the bash script

**Parent**: sub-plans

## Classes

### Cluster plans

A description here

#### Methods

List of methods and their description.

### Subplan

A description here

#### Methods

List of methods and their description.

### Steps

A description here

#### Methods

List of methods and their description.

Note: More sections will be written later on



# Syntax examples

From https://github.com/esm-tools/esm_tools/issues/1521

First ideas on how the syntax could look like (not final at all and not complete, as we need the execution plan to fully understand what syntax do we need):

### Example for an AWICM3 config

```yaml
general:
    [ ... ]
fesom:
    [ ... ]
workflow:
    subplans:
        prep_exp: # name of the plan
            steps:
                - function_1 # old recipe steps
                - function_2
                - ...
            cluster_plan: initialization
            preceded_by: plan_starting_point
            type: esm_subplan
        prepare_first_run:
        # this is virtually the same as prepare_run but the dependency is in prep_exp instead of tidy
        # so that it can be run in the login node directory for checks before submitting anything,
        # without complicating the logic on the wf manager.
            steps: ...
            cluster_plan: initialization
            preceded_by: prep_exp
            type: esm_subplan
        compute:
            type: bash
            launch_mode: inline # in contrast with "daemon": other bash scripts that are run via the job's daemon
            file: ${esm_tools_configs}/other_software/batch/slurm/plan_cluster.run.j2
            cluster_plan: simulation
            preceded_by: prepare_first_run or prepare_run
        observe_compute:
            steps: ...
            concurrent: true
            preceded_by: compute
            type: esm_subplan
            cluster_plan: simulation
        tidy:
            steps: ...
            preceded_by: observe_compute
            type: esm_subplan
            cluster_plan: simulation
        catalog:
            steps: ...
            preceded_by: tidy
            cluster_plan: catalog
            type: esm_subplan
       prepare_run:
           preceded_by: tidy
           type: esm_subplan
           cluster_plan: simulation
           steps: ...
        tripyview:
            preceded_by: observe_compute # not sure if that's really the case or if it runs after tidy
            type: esm_subplan
            plan: tripyview
            steps:
                - tripyview:
                    script: ${esm_tools}/configs/fesom/tripyview.sh input1 input2...
                    environment:
                    ... # as already in the current wf manager

    plans: # clusters are plans whose subplans are executed together in the same host.
    # At the end of each cluster plan, the next cluster plan is executed
        initialization:
            execution: login_node
        simulation:
            execution: sbatch
            cycle:
                  key: vals for defining the cycling conditions
        catalog:
            execution: sbatch
        tripyview:
            execution: sbatch
```
