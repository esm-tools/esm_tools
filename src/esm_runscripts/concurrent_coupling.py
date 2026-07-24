"""
Event-driven rendezvous for concurrent iterative coupling (coupling_mode:
concurrent). Two esm_runscripts chains (one per model) run their cycles
simultaneously with a one-cycle lag; each chain's leg N (N>1) requires the
sibling's chunk-(N-1) done-marker in the couple dir.

No waiting process exists anywhere: a chain that finds the marker missing at a
rendezvous point PARKS -- it writes an atomic ``couple/.parked.<chain>`` state
file and exits. When the sibling's leg end touches its own done-marker, it
checks for a parked chain, atomically claims the park file and re-invokes the
parked chain in its allocation tail (the same class of 1-core tail work the
serial chain runs today). The park->recheck / mark->check-parked ordering plus
rename-atomicity close the simultaneous-arrival race.

Mutual parking is impossible: markers are monotone per chain, and a chain only
parks on a marker the sibling has not yet produced, which implies the sibling
is still running (or crashed -- in which case relaunching it revives everyone).
"""

import os
import subprocess
import sys

from loguru import logger

from . import chunky_parts


def _log(message):
    """PROGRESS-level if the custom level is registered, info otherwise."""
    try:
        logger.progress(message)
    except AttributeError:
        logger.info(message)


def marker_path(config, model, chunk):
    return os.path.join(
        config["general"]["experiment_couple_dir"], f"{model}_chunk_{chunk}.done"
    )


def sibling_name(config):
    """setup_name of the other model chain."""
    own = chunky_parts._chain_name(config)
    index = 1
    while "model" + str(index) in config:
        name = config["model" + str(index)]["setup_name"]
        if name != own:
            return name
        index += 1
    return None


def _park_file(config, chain=None):
    chain = chain or chunky_parts._chain_name(config)
    return os.path.join(config["general"]["experiment_couple_dir"], f".parked.{chain}")


def required_marker(config, upcoming_chunk):
    """Marker the upcoming own chunk needs, or None (chunk 1 boots from the pool)."""
    if int(upcoming_chunk) <= 1:
        return None
    sibling = sibling_name(config)
    if not sibling:
        return None
    return marker_path(config, sibling, int(upcoming_chunk) - 1)


def park_if_needed(config, upcoming_chunk):
    """Park this chain if the sibling marker for ``upcoming_chunk`` is missing.

    Returns True if the chain parked (caller must stop the leg/resubmission);
    False if the marker is present (or not needed) and the chain may continue.
    """
    needed = required_marker(config, upcoming_chunk)
    if needed is None or os.path.isfile(needed):
        return False

    park = _park_file(config)
    tmp = park + f".tmp.{os.getpid()}"
    with open(tmp, "w") as fid:
        fid.write(needed + "\n")
    os.replace(tmp, park)

    # re-check: the sibling may have produced the marker (and checked for parked
    # chains) between our first check and the park write
    if os.path.isfile(needed):
        try:
            os.rename(park, park + f".unparked.{os.getpid()}")
            os.remove(park + f".unparked.{os.getpid()}")
            return False  # we won the unpark -- continue ourselves
        except FileNotFoundError:
            pass  # sibling claimed it and is reviving us -- stop this instance

    _log(
        f"Concurrent coupling: parked -- waiting for {os.path.basename(needed)} "
        f"(chain resumes automatically when the sibling produces it)"
    )
    return True


def revive_parked_sibling(config):
    """At leg end: if the sibling is parked and its awaited marker now exists,
    claim the park file and re-invoke the sibling chain in this tail."""
    sibling = sibling_name(config)
    if not sibling:
        return
    park = _park_file(config, chain=sibling)
    if not os.path.isfile(park):
        return
    try:
        with open(park) as fid:
            needed = fid.read().strip()
    except OSError:
        return
    if not needed or not os.path.isfile(needed):
        return  # parked on something we have not produced yet

    claimed = park + f".claimed.{os.getpid()}"
    try:
        os.rename(park, claimed)
    except FileNotFoundError:
        return  # someone else claimed it
    os.remove(claimed)

    scriptname = config["general"]["scriptname"]
    expid = config["general"]["expid"]
    command = [
        "esm_runscripts",
        scriptname,
        "-e",
        expid,
        "--coupling-chain",
        sibling,
    ]
    _log(
        f"Concurrent coupling: reviving parked chain {sibling} "
        f"({os.path.basename(needed)} is now available)"
    )
    try:
        subprocess.check_call(
            command, cwd=config["general"]["experiment_scripts_dir"]
        )
    except (OSError, subprocess.CalledProcessError) as error:
        logger.error(f"Reviving chain {sibling} failed ({error}); restoring park file")
        tmp = park + f".tmp.{os.getpid()}"
        with open(tmp, "w") as fid:
            fid.write(needed + "\n")
        os.replace(tmp, park)


def entry_park_check(config):
    """Rendezvous check on a FRESH invocation of a chain (cold start, recovery
    relaunch, revival): if this leg's sibling marker is missing, park and exit.

    In-tail resubmissions pass an explicit jobtype and skip this (their marker
    presence was checked at the previous leg's end)."""
    if chunky_parts._coupling_mode(config) != "concurrent":
        return config
    jobtype = config["general"].get("command_line_config", {}).get("jobtype", "unknown")
    if jobtype != "unknown":
        return config
    if park_if_needed(config, config["general"]["chunk_number"]):
        sys.exit(0)
    return config
