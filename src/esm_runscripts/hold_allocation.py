"""
Keep a concurrent-coupling chain's allocation between legs (general.hold_allocation).

In concurrent mode (see concurrent_coupling.py) every leg of every chain is its own
batch job: a chain whose next leg still lacks the sibling's done-marker parks and
exits, and a chain that may continue submits its next compute job, which then
queues. With hold_allocation the chain keeps the nodes it already has:

  1. at leg end, if the sibling marker for the next leg is missing, the tail WAITS
     for it inside the job (polling every hold_poll_seconds) instead of parking;
  2. when the next leg's compute script is written, it is not sbatch'ed but queued
     into this allocation (a hold file); the compute job script, after its tail
     returns, finds the hold file and execs the next leg on the same nodes.

Both steps fall back to the normal behaviour whenever holding cannot work: no
remaining walltime for another leg, a next leg that asks for different
resources, a sibling that does not deliver within hold_max_wait_minutes (it may
have crashed), or anything unexpected. The fallback is exactly today's path
(park + revive, or sbatch), so nothing is lost but the saved queue time.

Nothing here runs unless the compute job script exported ESM_HOLD_CHAIN for this
very chain: a sibling revived from this allocation's tail (concurrent_coupling.
revive_parked_sibling) inherits the environment, sees a different chain name and
is submitted the normal way.

Configuration (general section of the coupling driver; all optional except the
switch):
    hold_allocation: true          # default false
    hold_max_wait_minutes: 120     # give up waiting for the sibling after this
    hold_poll_seconds: 30
    hold_min_leg_minutes: 0        # floor for the leg-length estimate
The compute job's walltime has to cover several legs for this to help, e.g.
computer.walltime / the model's time setting raised to 08:00:00 on levante.
"""

import os
import re
import subprocess
import time

from loguru import logger

from . import chunky_parts

ENV_CHAIN = "ESM_HOLD_CHAIN"
ENV_LEG_START = "ESM_HOLD_LEG_START"
ENV_WAITED = "ESM_HOLD_WAITED"
SAFETY_FACTOR = 1.1
SAFETY_MARGIN = 300  # seconds on top of the scaled estimate


def _log(message):
    try:
        logger.progress(message)
    except AttributeError:
        logger.info(message)


def requested(config):
    """The switch is on and the run couples concurrently."""
    general = config.get("general", {})
    return bool(general.get("hold_allocation", False)) and (
        chunky_parts._coupling_mode(config) == "concurrent"
    )


def active(config):
    """Holding applies to this process: switch on, inside a batch job, and that job's
    compute script was written for this chain (see module docstring)."""
    if not requested(config):
        return False
    if not os.environ.get("SLURM_JOB_ID"):
        return False
    return os.environ.get(ENV_CHAIN) == config["general"].get("coupling_chain")


def hold_file(config, job_id=None):
    """Where the tail leaves the next leg's compute script for the job script."""
    job_id = job_id or os.environ.get("SLURM_JOB_ID", "nojob")
    return os.path.join(
        config["general"]["experiment_scripts_dir"],
        f".hold_next.{config['general']['coupling_chain']}.{job_id}",
    )


def hold_file_shell(config):
    """The same path as a shell expression, resolved in the job at run time."""
    return os.path.join(
        config["general"]["experiment_scripts_dir"],
        f".hold_next.{config['general']['coupling_chain']}." + "${SLURM_JOB_ID}",
    )


def _parse_slurm_time(text):
    """squeue %L: [days-]hours:minutes:seconds, minutes:seconds, or a word."""
    text = text.strip()
    match = re.fullmatch(r"(?:(\d+)-)?(?:(\d+):)?(\d+):(\d+)", text)
    if not match:
        return None
    days, hours, minutes, seconds = (int(g) if g else 0 for g in match.groups())
    return ((days * 24 + hours) * 60 + minutes) * 60 + seconds


def remaining_seconds():
    """Walltime left in the current job, or None if it cannot be determined."""
    job_id = os.environ.get("SLURM_JOB_ID")
    if not job_id:
        return None
    try:
        out = subprocess.run(
            ["squeue", "-h", "-j", job_id, "-o", "%L"],
            capture_output=True, text=True, timeout=60,
        ).stdout
    except (OSError, subprocess.SubprocessError):
        return None
    return _parse_slurm_time(out.splitlines()[0]) if out.strip() else None


def leg_estimate_seconds(config):
    """Time the next leg needs, from the leg this job just ran (its compute plus the
    tail so far, minus the time spent waiting for the sibling), never below
    hold_min_leg_minutes, scaled up by a safety factor and margin."""
    floor = float(config["general"].get("hold_min_leg_minutes", 0)) * 60.0
    measured = 0.0
    start = os.environ.get(ENV_LEG_START)
    if start:
        try:
            measured = time.time() - float(start) - float(os.environ.get(ENV_WAITED, 0))
        except ValueError:
            measured = 0.0
    return max(floor, measured) * SAFETY_FACTOR + SAFETY_MARGIN


def wait_for_marker(config, needed):
    """Block until the sibling marker ``needed`` exists, as long as another leg still
    fits into this job and hold_max_wait_minutes is not exceeded. Returns True if the
    marker is there. The wait is recorded in ESM_HOLD_WAITED so that it does not
    count towards the leg-length estimate."""
    if not needed or os.path.isfile(needed):
        return True
    remaining = remaining_seconds()
    if remaining is None:
        _log("hold_allocation: cannot read the remaining walltime; not waiting")
        return False
    estimate = leg_estimate_seconds(config)
    max_wait = float(config["general"].get("hold_max_wait_minutes", 120)) * 60.0
    budget = min(max_wait, remaining - estimate)
    if budget <= 0:
        _log(
            f"hold_allocation: {remaining / 60:.0f} min left, next leg needs about "
            f"{estimate / 60:.0f} min; not waiting for {os.path.basename(needed)}"
        )
        return False
    poll = max(5.0, float(config["general"].get("hold_poll_seconds", 30)))
    _log(
        f"hold_allocation: waiting up to {budget / 60:.0f} min in this allocation for "
        f"{os.path.basename(needed)}"
    )
    t0 = time.time()
    while time.time() - t0 < budget:
        if os.path.isfile(needed):
            break
        time.sleep(poll)
    waited = time.time() - t0
    os.environ[ENV_WAITED] = str(float(os.environ.get(ENV_WAITED, 0)) + waited)
    if os.path.isfile(needed):
        _log(f"hold_allocation: {os.path.basename(needed)} arrived after {waited / 60:.1f} min")
        return True
    _log(
        f"hold_allocation: {os.path.basename(needed)} still missing after "
        f"{waited / 60:.0f} min; falling back to park"
    )
    return False


def _requested_resources(runfilename):
    """#SBATCH --nodes / --ntasks of a job script (None where absent)."""
    nodes = ntasks = None
    with open(runfilename) as fid:
        for line in fid:
            if not line.startswith("#SBATCH"):
                continue
            m = re.search(r"--nodes[= ](\d+)\b", line) or re.search(r"\s-N\s*(\d+)\b", line)
            if m:
                nodes = int(m.group(1))
            m = re.search(r"--ntasks[= ](\d+)\b", line) or re.search(r"\s-n\s*(\d+)\b", line)
            if m:
                ntasks = int(m.group(1))
    return nodes, ntasks


def try_hold_compute(config, runfilename):
    """Queue the next leg's compute script into this allocation instead of
    submitting it. Returns True if it was queued (the caller must then not submit)."""
    if not active(config):
        return False
    try:
        nodes, ntasks = _requested_resources(runfilename)
    except OSError as error:
        _log(f"hold_allocation: cannot read {runfilename} ({error}); submitting")
        return False
    have_nodes = os.environ.get("SLURM_JOB_NUM_NODES") or os.environ.get("SLURM_NNODES")
    have_tasks = os.environ.get("SLURM_NTASKS")
    if nodes is not None and have_nodes and int(have_nodes) != nodes:
        _log(f"hold_allocation: next leg wants {nodes} nodes, this job has {have_nodes}; submitting")
        return False
    if ntasks is not None and have_tasks and int(have_tasks) != ntasks:
        _log(f"hold_allocation: next leg wants {ntasks} tasks, this job has {have_tasks}; submitting")
        return False
    remaining = remaining_seconds()
    estimate = leg_estimate_seconds(config)
    if remaining is None or remaining < estimate:
        _log(
            "hold_allocation: "
            + ("remaining walltime unknown" if remaining is None
               else f"{remaining / 60:.0f} min left < {estimate / 60:.0f} min needed")
            + "; submitting the next leg"
        )
        return False
    target = hold_file(config)
    tmp = target + f".tmp.{os.getpid()}"
    with open(tmp, "w") as fid:
        fid.write(os.path.abspath(runfilename) + "\n")
    os.replace(tmp, target)
    _log(
        f"hold_allocation: next leg runs in this allocation ({remaining / 60:.0f} min left, "
        f"about {estimate / 60:.0f} min needed): {os.path.basename(runfilename)}"
    )
    return True


def job_script_head_lines(config):
    """Exports at the top of a compute job script (after the batch header)."""
    return [
        "# hold_allocation: identify this chain's own compute job and time its leg",
        f'export {ENV_CHAIN}={config["general"]["coupling_chain"]}',
        f"export {ENV_LEG_START}=$(date +%s)",
        f"export {ENV_WAITED}=0",
    ]


def job_script_tail_lines(config):
    """Lines at the very end of a compute job script: run the queued next leg, if
    any, in this allocation. Its output goes to the log file its own #SBATCH
    --output names, with %j expanded to this job's id."""
    hold = hold_file_shell(config)
    return [
        "# hold_allocation: the tail may have queued the next leg into this allocation",
        f'_hold="{hold}"',
        'if [ -f "$_hold" ]; then',
        '    _next=$(head -1 "$_hold"); rm -f "$_hold"',
        "    # Steps the tail left running still hold this allocation's CPUs: the dask",
        "    # scheduler and workers of the coupling scripts are started with srun and never",
        "    # shut down (normally the job's end kills them). The next leg's srun would then",
        '    # wait forever with "step creation temporarily disabled (Requested nodes are',
        '    # busy)", as holdchk did on levante, 2026-10-13. Cancel them and wait until gone.',
        '    for _s in $(squeue -h -s -j "$SLURM_JOB_ID" -o %i | grep -v -E "\\.(batch|extern)$"); do',
        '        echo "hold_allocation: cancelling leftover step $_s"; scancel "$_s"',
        "    done",
        "    for _i in $(seq 1 60); do",
        '        squeue -h -s -j "$SLURM_JOB_ID" -o %i | grep -q -v -E "\\.(batch|extern)$" || break',
        "        sleep 2",
        "    done",
        '    _out=$(sed -n \'s/^#SBATCH --output=\\([^ ]*\\).*/\\1/p\' "$_next" | head -1)',
        '    _out=${_out//%j/$SLURM_JOB_ID}',
        '    echo "hold_allocation: continuing in job $SLURM_JOB_ID with $_next"',
        '    cd "$(dirname "$_next")"',
        '    if [ -n "$_out" ]; then exec bash "$_next" > "$_out" 2>&1; else exec bash "$_next"; fi',
        "fi",
    ]
