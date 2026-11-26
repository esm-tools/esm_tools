"""Resolve a shipped ESM-Tools runscript into a concrete experiment layout.

The fixture is *config-anchored*: rather than hand-declaring component names and
output filenames (which drift from reality), it drives ESM-Tools' own config
assembly to discover what a given model actually writes. This module runs that
assembly in-process -- no HPC machine, no compile, no model run, no forcing
files -- and hands back the per-component ``outdata_targets`` (the glob-style
filename stems a scan reads) plus the assembled config that becomes the
``finished_config``.

The recipe (see the ``jobtype='inspect'`` path in esm_runscripts) resolves
``choose_`` blocks and populates ``<component>_outdata_targets`` without creating
the experiment tree or scheduling anything. We deliberately stop at the outdata
subset (:func:`filelists.rename_sources_to_targets`) and never call the full
``filelists.assemble()`` -- its input-side completion ``sys.exit``\\s on forcing
files that need a real pool, and the fixture only cares about *output*.
"""

from __future__ import annotations

import tempfile
from dataclasses import dataclass
from pathlib import Path

from upath import UPath

# repo root: .../tests/test_esm_catalog/fixtures/fake_experiment/resolver.py
_REPO_ROOT = Path(__file__).resolve().parents[4]

# Model -> the simplest shipped runscript that exercises its real component set.
_RUNSCRIPTS: dict[str, str] = {
    "awiesm-2.1": "runscripts/awiesm/v2.1/PI_ctrl_awiesm-2.1_LR_ollie.yaml",
}


@dataclass(frozen=True)
class ResolvedLayout:
    """The config-derived shape of one experiment, as a scan would see it.

    Attributes
    ----------
    expid : str
        The experiment id the layout was resolved under.
    outdata_targets : dict[str, dict[str, str]]
        ``{component: {target_name: glob}}`` -- the output-file stems each
        component writes, e.g. ``{"echam": {"echam": "<expid>_2000*.01_echam", ...}}``.
    config : dict
        The assembled config (the source of the ``finished_config`` a scan reads
        for identity, metadata, and per-component ``outdata_targets``).
    """

    expid: str
    outdata_targets: dict[str, dict[str, str]]
    restart_out_targets: dict[str, dict[str, str]]
    config: dict

    @property
    def components(self) -> list[str]:
        """The component names that write output, in resolution order."""
        return list(self.outdata_targets)


def runscript_for(model: str) -> Path:
    """Return the absolute path to *model*'s shipped runscript.

    Raises
    ------
    KeyError
        If *model* has no registered runscript.
    """
    if model not in _RUNSCRIPTS:
        raise KeyError(
            f"no runscript registered for model {model!r}; "
            f"known: {sorted(_RUNSCRIPTS)}"
        )
    return _REPO_ROOT / _RUNSCRIPTS[model]


def resolve(model: str, expid: str = "fake01") -> ResolvedLayout:
    """Resolve *model*'s runscript into a :class:`ResolvedLayout` for *expid*.

    Runs ESM-Tools config assembly in inspect mode against a throwaway
    ``base_dir``; nothing is written and no scheduler is touched.

    Parameters
    ----------
    model : str
        A registered model key, e.g. ``"awiesm-2.1"``.
    expid : str
        The experiment id to resolve under (appears in the target stems).

    Returns
    -------
    ResolvedLayout
    """
    from esm_runscripts import filelists
    from esm_runscripts.sim_objects import SimulationSetup

    runscript = runscript_for(model)
    base_dir = tempfile.mkdtemp(prefix="fake_experiment_resolve_")
    command_line_config = {
        "jobtype": "inspect",
        "use_venv": False,
        "profile": None,
        "runscript_abspath": str(runscript),
        "scriptname": str(runscript),
        "started_from": "command_line",
        "last_jobtype": "command_line",
        "base_dir": base_dir,
        "expid": expid,
    }
    config = SimulationSetup(command_line_config=command_line_config).config
    config = filelists.rename_sources_to_targets(config)

    return ResolvedLayout(
        expid=expid,
        outdata_targets=_targets_by_component(config, "outdata_targets"),
        restart_out_targets=_targets_by_component(config, "restart_out_targets"),
        config=config,
    )


def _targets_by_component(config: dict, filetype: str) -> dict[str, dict[str, str]]:
    """Extract ``{component: {name: target}}`` for one *filetype* from *config*.

    Keys/values are coerced to plain ``str``: the assembled config wraps them in
    esm_parser provenance types that a plain YAML dumper cannot serialize.
    """
    return {
        str(component): {str(k): str(v) for k, v in block[filetype].items()}
        for component, block in config.items()
        if isinstance(block, dict) and block.get(filetype)
    }
