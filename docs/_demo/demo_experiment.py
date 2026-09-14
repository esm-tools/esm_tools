"""Build a small, self-contained demo experiment for the esm_catalog handbook.

The handbook's executable snippets (``.. testcode::``) run against this
experiment, so what the reader sees is what the code does. It reuses the
config-anchored fake experiment from the test suite (a real awiesm-2.1 layout
with synthetic netCDF files), adds two namelists so the namelist fields show
up, and scans it into ``<root>/catalog``.

Run ``make doctest`` in ``docs/`` to execute the snippets, or import
:func:`build_demo` to get a scanned experiment for your own experiments with
the API.
"""

from __future__ import annotations

import sys
import tempfile
import warnings
from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
if str(_REPO / "tests") not in sys.path:
    sys.path.insert(0, str(_REPO / "tests"))

_NAMELISTS = {
    "echam/namelist.echam": """&runctl
  lcouple = .true.
/
&radctl
  co2vmr = 284.7e-6
  ch4vmr = 791.6e-9
/
""",
    "fesom/namelist.config": """&paths
  meshpath = '/pool/data/meshes/core2/'
/
&timestep
  step_per_day = 32
/
""",
}


def build_demo(expid: str = "pi-ctrl-001", root: Path | None = None) -> Path:
    """Materialise and scan a demo experiment in a subprocess; return its root.

    Runs :func:`_build` in a fresh interpreter, exactly as a user would run the
    CLI, so the handbook snippets only ever read what the scan wrote.
    """
    import subprocess

    args = [sys.executable, __file__, expid]
    if root is not None:
        args.append(str(root))
    out = subprocess.run(args, check=True, capture_output=True, text=True).stdout
    return Path(out.strip().splitlines()[-1])


def _build(expid: str, root: Path | None) -> Path:
    warnings.filterwarnings("ignore")
    from upath import UPath

    from esm_catalog.scan.ingest import scan_experiment
    from test_esm_catalog.fixtures.fake_experiment.base import DiskBackend
    from test_esm_catalog.fixtures.fake_experiment.config_builder import Segment, build

    root = Path(root or tempfile.mkdtemp(prefix="esm-catalog-demo-"))
    exp = build(
        backend=DiskBackend(UPath(root)),
        expid=expid,
        segments=(Segment(1850, "cold"), Segment(1851, "cold")),
        months=2,
    )
    exp_root = Path(str(exp.root))
    for rel, text in _NAMELISTS.items():
        target = exp_root / "config" / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text)
    scan_experiment(UPath(exp_root))
    return exp_root


if __name__ == "__main__":
    _expid = sys.argv[1] if len(sys.argv) > 1 else "pi-ctrl-001"
    _root = Path(sys.argv[2]) if len(sys.argv) > 2 else None
    print(_build(_expid, _root))
