# esm_catalog PR-A1b — Scanners (ingest layer) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the scan/ingest layer — "scan run dirs" (Miguel's unit #1) — that turns files into in-memory STAC collections + items, completing the rewiring the STAC foundation (PR-A1a) deferred to the scan layer.

**Architecture:** Rebuild-from-end-state, same as A1a. The scan layer depends one-directionally on the STAC model (`scan → stac`, the cycle stays broken). Because `scan/` is ~2.6k LOC and `detect.py` couples the three scanners, this is split into **two stacked PRs**:
- **PR-A1b-1 — ingest core + namelist + NetCDF** (this plan, full detail). Establishes ALL the rewiring + a working end-to-end NetCDF scan.
- **PR-A1b-2 — GRIB + ECHAM scanners** (outline at the end; full plan later).

**Tech Stack:** Python, xarray + numpy + cftime + h5netcdf (NetCDF scan), f90nml (namelists), universal-pathlib (remote), Click (CLI), pytest.

**Base branch:** stacks on `esm-catalog/pr-a1a-stac-foundation`. Work in the existing worktree `…/esm_tools-catalog-stack`. Extraction source `$SRC = origin/esm-tools-plus/simcat/collapsed-collections`.

**Design reference:** `docs/superpowers/specs/2026-06-13-esm-catalog-pr-decomposition-design.md`; predecessor plan `docs/superpowers/plans/2026-06-13-esm-catalog-pr0-pr-a1a.md`.

---

## The rewiring this PR must complete (deferred from PR-A1a)

PR-A1a made the STAC model pure and left these for the scan layer:
1. **`CollectionContext.collection_title`** — A1a dropped it (YAGNI). The scan layer's `_make_ctx` sets it, so **re-add `collection_title: str = ""`** to `esm_catalog/context.py`.
2. **`scan/context.py` must use the neutral context** — replace its locally-defined `CollectionContext` dataclass with `from esm_catalog.context import CollectionContext`. (Keep all the `resolve_context`/`is_restart_file`/`_from_config`/`_from_path` logic.)
3. **`get_namelist_config_path`** — A1a removed it from `stac/extensions/namelist.py`. **Reintroduce it in the scan layer** (`scan/namelist.py`).
4. **Populate `ctx.namelists_by_component`** — A1a's `add_namelist_item_extension(item, ctx)` reads pre-scanned namelists from `ctx`. The scan layer must scan ALL component config dirs and set `ctx.namelists_by_component` before building items. (This replaces the scanning the old item extension did itself.)
5. **`make_collection` call** — A1a changed the signature to `make_collection(ctx, namelists=None, fesom_info=None)`. Update `scan/context.py:_ensure_collection` (and any caller) to scan and pass `namelists`/`fesom_info` instead of the old `experiment_path=` arg.
6. **URI helpers** — `parse_uri`/`to_uri` now live in `esm_catalog.uri`. `scan/upath.py` should **re-export** them (`from esm_catalog.uri import parse_uri, to_uri  # noqa: F401`) for back-compat, and `scan/netcdf.py`/`scan/detect.py` should import from `esm_catalog.uri`.

---

## File Structure (PR-A1b-1)

All under `src/esm_catalog/`:
- `context.py` (modify) — re-add `collection_title` field.
- `uri.py` (modify) — add `list_files` (and `list_all_files`) here, OR keep in `scan/upath.py`. **Decision:** keep file-listing in `scan/upath.py` (it's scan behavior); only `parse_uri`/`to_uri` stay in `uri.py`.
- `scan/__init__.py` (new) — package marker.
- `scan/upath.py` (new) — full remote-FS helpers, with `parse_uri`/`to_uri` re-exported from `esm_catalog.uri`.
- `scan/namelist.py` (new) — namelist parsing + **`get_namelist_config_path`** added here.
- `scan/netcdf.py` (new) — NetCDF scanner; lazy uri imports point at `esm_catalog.uri`.
- `scan/detect.py` (new) — format dispatch; **grib/echam imports made lazy** so this PR lands without those modules.
- `scan/context.py` (new) — collection-context resolution; uses neutral `CollectionContext`; namelist population.
- `scan/ingest.py` (new, small) — orchestration: `scan_tree(root, config=None) -> {"collections": [...], "items": [...]}` building in-memory STAC (no DuckDB; storage is PR-A2).
- `cli.py` (modify) — add `esm-catalog scan <path> [--output catalog.json]` subcommand calling `scan_tree` and dumping JSON.
- Tests: `tests/test_context_rewire.py`, `tests/test_namelist_scan.py`, `tests/test_netcdf_scan.py`, `tests/test_detect.py`, `tests/test_scan_ingest.py`, `tests/test_cli_scan.py`.

Add to the `catalog` extra in `setup.py` and to the CI install list: `xarray`, `numpy`, `cftime`, `h5netcdf`, `f90nml`.

---

## Task A1b1.1 — re-add `collection_title`; CI/extra deps

**Files:** `src/esm_catalog/context.py`, `setup.py`, `.github/workflows/esm-catalog-tests.yml`
**Test:** `src/esm_catalog/tests/test_context_rewire.py`

- [ ] **Step 1: Failing test**

Create `src/esm_catalog/tests/test_context_rewire.py`:
```python
"""CollectionContext regains collection_title for the scan layer."""

from __future__ import annotations

from esm_catalog.context import CollectionContext


def test_context_has_collection_title_default():
    ctx = CollectionContext(experiment_id="e", component="c", collection_id="e")
    assert ctx.collection_title == ""


def test_context_collection_title_settable():
    ctx = CollectionContext(
        experiment_id="e", component="c", collection_id="e", collection_title="My Exp"
    )
    assert ctx.collection_title == "My Exp"
```

- [ ] **Step 2: Run — expect failure** (`unexpected keyword argument 'collection_title'`)

```bash
.venv-catalog-312/bin/python -m pytest src/esm_catalog/tests/test_context_rewire.py -v
```

- [ ] **Step 3: Add the field**

In `src/esm_catalog/context.py`, add to the `CollectionContext` dataclass (after `collection_id`, before `experiment_path`):
```python
    collection_title: str = ""
```
And add to the docstring Attributes: `collection_title: Human-readable title (defaults to experiment_id at the scan layer).`

- [ ] **Step 4: Run — expect pass.**

- [ ] **Step 5: Add scan deps to the `catalog` extra and CI**

In `setup.py` `extras_require["catalog"]`, append: `"xarray", "numpy", "cftime", "h5netcdf", "f90nml"`.
In `.github/workflows/esm-catalog-tests.yml`, extend the explicit install list to include them:
```yaml
          pip install -U "pytest>=7.4" click loguru pystac shapely universal-pathlib xarray numpy cftime h5netcdf f90nml
```

- [ ] **Step 6: Commit**

```bash
git add src/esm_catalog/context.py src/esm_catalog/tests/test_context_rewire.py setup.py .github/workflows/esm-catalog-tests.yml
git commit -m "feat(esm_catalog): re-add collection_title; add scan deps to catalog extra"
```

## Task A1b1.2 — `scan/upath.py` (re-export uri) + `scan/namelist.py` (+ get_namelist_config_path)

**Files:** `src/esm_catalog/scan/__init__.py`, `scan/upath.py`, `scan/namelist.py`
**Test:** `src/esm_catalog/tests/test_namelist_scan.py`

- [ ] **Step 1: Bring in scan/__init__.py and upath.py, rewire uri re-export**

```bash
git checkout $SRC -- src/esm_catalog/scan/__init__.py src/esm_catalog/scan/upath.py
```
Then in `scan/upath.py`, **delete** the local definitions of `parse_uri`, `to_uri`, `_get_upath`, `_has_protocol` and replace with a re-export at the top:
```python
from esm_catalog.uri import parse_uri, to_uri, _get_upath, _has_protocol  # noqa: F401
```
Keep `list_files`, `list_all_files`, `cache_remote_file`, `open_file`, `read_magic_bytes`, `get_file_size`, `is_file`, `is_dir`, `get_protocol`. Verify no duplicate-definition / NameError: `.venv-catalog-312/bin/python -c "import esm_catalog.scan.upath as u; u.parse_uri; u.to_uri; u.list_files; print('ok')"`.

- [ ] **Step 2: Failing test for namelist + get_namelist_config_path**

Create `src/esm_catalog/tests/test_namelist_scan.py`:
```python
"""Namelist scanning + config-path resolution (scan layer)."""

from __future__ import annotations

from esm_catalog.scan.namelist import (
    get_namelist_config_path,
    scan_namelist_directory,
)


def _write_namelist(p):
    p.write_text("&runctl\n    delta_time = 450\n    lcouple = .true.\n/\n")


def test_get_namelist_config_path_standard_layout(tmp_path):
    cfg = tmp_path / "config" / "echam"
    cfg.mkdir(parents=True)
    assert get_namelist_config_path(tmp_path, "echam") == cfg


def test_get_namelist_config_path_missing(tmp_path):
    assert get_namelist_config_path(tmp_path, "echam") is None


def test_scan_namelist_directory(tmp_path):
    cfg = tmp_path / "config" / "echam"
    cfg.mkdir(parents=True)
    _write_namelist(cfg / "namelist.echam")
    out = scan_namelist_directory(cfg, "echam")
    assert out["namelist.echam"]["runctl"]["delta_time"] == 450
    assert out["namelist.echam"]["runctl"]["lcouple"] is True
```

- [ ] **Step 3: Run — expect failure** (`cannot import name 'get_namelist_config_path'`).

- [ ] **Step 4: Bring namelist.py and add get_namelist_config_path**

```bash
git checkout $SRC -- src/esm_catalog/scan/namelist.py
```
The source `scan/namelist.py` already has `scan_namelist`, `scan_namelist_directory`, `extract_fesom_mesh_info`, etc. **Add** the `get_namelist_config_path` function (moved out of stac in A1a) — copy it verbatim from PR-A1a's pre-removal version (it lived in `stac/extensions/namelist.py`):
```python
def get_namelist_config_path(experiment_path, component: str):
    """Determine the config directory for a component (ESM-Tools convention).

    Returns config/{component}/ if it exists, else {component}/config/, else None.
    """
    from pathlib import Path

    experiment_path = Path(experiment_path)
    config_path = experiment_path / "config" / component
    if config_path.is_dir():
        return config_path
    alt_path = experiment_path / component / "config"
    if alt_path.is_dir():
        return alt_path
    return None
```

- [ ] **Step 5: Run namelist tests — expect pass.**

- [ ] **Step 6: Commit**

```bash
git add src/esm_catalog/scan/__init__.py src/esm_catalog/scan/upath.py src/esm_catalog/scan/namelist.py src/esm_catalog/tests/test_namelist_scan.py
git commit -m "feat(esm_catalog): scan upath (re-export uri) + namelist (+ get_namelist_config_path)"
```

## Task A1b1.3 — NetCDF scanner + detect (lazy grib/echam)

**Files:** `scan/netcdf.py`, `scan/detect.py`
**Test:** `src/esm_catalog/tests/test_netcdf_scan.py`, `src/esm_catalog/tests/test_detect.py`

- [ ] **Step 1: Failing test for the NetCDF scanner (build a tiny real .nc)**

Create `src/esm_catalog/tests/test_netcdf_scan.py`:
```python
"""scan_netcdf extracts STAC-relevant metadata from a real NetCDF file."""

from __future__ import annotations

import numpy as np
import pandas as pd
import xarray as xr

from esm_catalog.scan.netcdf import scan_netcdf


def _make_nc(path):
    times = pd.date_range("2000-01-01", periods=3, freq="MS")
    ds = xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((3, 2, 2), dtype="float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    )
    ds["tas"].attrs["standard_name"] = "air_temperature"
    ds["tas"].attrs["units"] = "K"
    ds.attrs["Conventions"] = "CF-1.8"
    ds.to_netcdf(path)


def test_scan_netcdf_basic(tmp_path):
    f = tmp_path / "tas_200001.nc"
    _make_nc(f)
    md = scan_netcdf(f)
    assert md["format"] == "netcdf"
    assert md["variable"] == "tas"
    assert md["conventions"] == "CF-1.8"
    assert md["bbox"] == [0.0, -45.0, 90.0, 45.0]
    assert md["datetime_start"].year == 2000
    assert md["file_size"] > 0
```
> `pandas` is a base esm-tools dep; if absent in the venv, install it. If `pandas` is undesirable in tests, build the time coord with `cftime`/`np.datetime64` instead.

- [ ] **Step 2: Run — expect failure** (no module `esm_catalog.scan.netcdf`).

- [ ] **Step 3: Bring netcdf.py, fix uri imports**

```bash
git checkout $SRC -- src/esm_catalog/scan/netcdf.py
sed -i '' 's/from esm_catalog\.scan\.upath import parse_uri/from esm_catalog.uri import parse_uri/' src/esm_catalog/scan/netcdf.py
sed -i '' 's/from esm_catalog\.scan\.upath import to_uri/from esm_catalog.uri import to_uri/' src/esm_catalog/scan/netcdf.py
```
Confirm: `grep -n "esm_catalog.scan.upath" src/esm_catalog/scan/netcdf.py` → no output.

- [ ] **Step 4: Bring detect.py, make grib/echam imports lazy**

```bash
git checkout $SRC -- src/esm_catalog/scan/detect.py
```
Edit `scan/detect.py`:
- Remove the module-level `from esm_catalog.scan.echam import is_echam_file, scan_echam` and `from esm_catalog.scan.grib import scan_grib`. Keep `from esm_catalog.scan.netcdf import scan_netcdf`.
- Move the grib/echam imports INSIDE `_dispatch_grib`:
```python
def _dispatch_grib(path):
    """Dispatch GRIB file to the appropriate scanner (lazy: GRIB/ECHAM land in PR-A1b-2)."""
    from pathlib import Path as LocalPath
    try:
        from esm_catalog.scan.echam import is_echam_file, scan_echam
        from esm_catalog.scan.grib import scan_grib
    except ImportError as exc:  # pragma: no cover
        raise UnsupportedFormatError(
            "GRIB/ECHAM scanning is not available yet (lands in PR-A1b-2)."
        ) from exc
    local_path = LocalPath(path) if not isinstance(path, LocalPath) else path
    if is_echam_file(local_path):
        return scan_echam(path)
    return scan_grib(path)
```
- Change the lazy `from esm_catalog.scan.upath import parse_uri` inside `scan_file` to `from esm_catalog.uri import parse_uri`.

- [ ] **Step 5: Test detect dispatches NetCDF**

Create `src/esm_catalog/tests/test_detect.py`:
```python
"""scan_file dispatches NetCDF correctly by suffix and magic bytes."""

from __future__ import annotations

import numpy as np
import xarray as xr

from esm_catalog.scan.detect import scan_file


def test_scan_file_netcdf_by_suffix(tmp_path):
    f = tmp_path / "x.nc"
    xr.Dataset({"v": ("t", np.arange(3))}).to_netcdf(f)
    md = scan_file(f)
    assert md["format"] == "netcdf"
```

- [ ] **Step 6: Run both test files — expect pass.**

```bash
.venv-catalog-312/bin/python -m pytest src/esm_catalog/tests/test_netcdf_scan.py src/esm_catalog/tests/test_detect.py -v
```

- [ ] **Step 7: Commit**

```bash
git add src/esm_catalog/scan/netcdf.py src/esm_catalog/scan/detect.py src/esm_catalog/tests/test_netcdf_scan.py src/esm_catalog/tests/test_detect.py
git commit -m "feat(esm_catalog): NetCDF scanner + format detection (grib/echam lazy)"
```

## Task A1b1.4 — `scan/context.py` rewired to neutral context + namelist population

**Files:** `scan/context.py`
**Test:** extend `src/esm_catalog/tests/test_context_rewire.py`

- [ ] **Step 1: Failing tests for context resolution + namelist population**

Append to `src/esm_catalog/tests/test_context_rewire.py`:
```python
from pathlib import Path

from esm_catalog.scan.context import resolve_context, scan_all_namelists


def test_resolve_context_from_path():
    p = Path("/data/experiments/exp-alpha/outdata/echam/tas_200001.nc")
    ctx = resolve_context(p)
    assert ctx.experiment_id == "exp-alpha"
    assert ctx.component == "echam"
    assert ctx.collection_id == "exp-alpha"   # Option A
    assert ctx.collection_title == "exp-alpha"


def test_scan_all_namelists(tmp_path):
    cfg = tmp_path / "config" / "echam"
    cfg.mkdir(parents=True)
    (cfg / "namelist.echam").write_text("&runctl\n delta_time = 450\n/\n")
    by_comp = scan_all_namelists(tmp_path)
    assert by_comp["echam"]["namelist.echam"]["runctl"]["delta_time"] == 450
```

- [ ] **Step 2: Run — expect failure.**

- [ ] **Step 3: Bring scan/context.py and rewire**

```bash
git checkout $SRC -- src/esm_catalog/scan/context.py
```
Edit `scan/context.py`:
- **Delete** the local `@dataclass class CollectionContext` definition. Add at the top: `from esm_catalog.context import CollectionContext`.
- In `_ensure_collection`, replace the old `make_collection(ctx, experiment_path=ctx.experiment_path)` call with namelist-scanning + the new signature:
```python
def _ensure_collection(ctx, db) -> None:
    from esm_catalog.stac.collection import make_collection
    from esm_catalog.scan.namelist import (
        extract_fesom_mesh_info,
        get_namelist_config_path,
        scan_namelist_directory,
    )

    if db.collection_exists(ctx.collection_id):
        db.add_component_to_collection(ctx.collection_id, ctx.component)
        return

    namelists = None
    fesom_info = None
    if ctx.experiment_path is not None:
        cfg = get_namelist_config_path(ctx.experiment_path, ctx.component)
        if cfg is not None:
            namelists = scan_namelist_directory(cfg, ctx.component)
            if ctx.component.lower() in ("fesom", "fesom2"):
                fesom_info = extract_fesom_mesh_info(cfg)
    collection = make_collection(ctx, namelists=namelists, fesom_info=fesom_info)
    db.insert_collection(collection)
    logger.info("Created collection: {}", ctx.collection_id)
```
- **Add** a new function `scan_all_namelists(experiment_path) -> dict` that scans every component config dir (this is what populates `ctx.namelists_by_component` for item building, replacing the scanning the old `add_namelist_item_extension` did):
```python
def scan_all_namelists(experiment_path) -> dict:
    """Scan every component's config dir under experiment_path/config.

    Returns {component_name: {filename: {group: {key: value}}}}, suitable for
    CollectionContext.namelists_by_component (read by the STAC item extension).
    """
    from pathlib import Path
    from esm_catalog.scan.namelist import scan_namelist_directory

    by_component: dict = {}
    if experiment_path is None:
        return by_component
    config_root = Path(experiment_path) / "config"
    if not config_root.is_dir():
        return by_component
    for comp_dir in config_root.iterdir():
        if not comp_dir.is_dir():
            continue
        nls = scan_namelist_directory(comp_dir, comp_dir.name)
        if nls:
            by_component[comp_dir.name] = nls
    return by_component
```

- [ ] **Step 4: Run — expect pass.** (`resolve_context` with `db=None` does not touch storage.)

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/scan/context.py src/esm_catalog/tests/test_context_rewire.py
git commit -m "refactor(esm_catalog): scan context uses neutral CollectionContext; add scan_all_namelists"
```

## Task A1b1.5 — `scan/ingest.py` orchestration (in-memory STAC, no DuckDB)

**Files:** `scan/ingest.py`
**Test:** `src/esm_catalog/tests/test_scan_ingest.py`

- [ ] **Step 1: Failing test (end-to-end: dir of NetCDF → collection + items)**

Create `src/esm_catalog/tests/test_scan_ingest.py`:
```python
"""scan_tree walks a run dir and builds in-memory STAC (no DuckDB)."""

from __future__ import annotations

import numpy as np
import pandas as pd
import xarray as xr

from esm_catalog.scan.ingest import scan_tree


def _make_run(root):
    out = root / "experiments" / "exp-alpha" / "outdata" / "echam"
    out.mkdir(parents=True)
    times = pd.date_range("2000-01-01", periods=2, freq="MS")
    ds = xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((2, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    )
    ds.to_netcdf(out / "tas_200001.nc")
    return root


def test_scan_tree_builds_collection_and_items(tmp_path):
    _make_run(tmp_path)
    cat = scan_tree(tmp_path)
    assert len(cat["collections"]) == 1
    assert cat["collections"][0]["id"] == "exp-alpha"
    assert len(cat["items"]) == 1
    item = cat["items"][0]
    assert item["type"] == "Feature"
    assert item["properties"]["component"] == "echam"
    assert item["collection"] == "exp-alpha"
```

- [ ] **Step 2: Run — expect failure.**

- [ ] **Step 3: Implement `scan/ingest.py`**

Create `src/esm_catalog/scan/ingest.py`:
```python
"""In-memory ingest: walk a run directory and build STAC collections + items.

This is the storage-free pipeline (DuckDB persistence arrives in PR-A2). It ties
together: list files -> detect+scan -> resolve context -> build STAC.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger

from esm_catalog.scan.context import (
    CollectionContextError,
    RestartFileError,
    resolve_context,
    scan_all_namelists,
)
from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
from esm_catalog.scan.namelist import (
    extract_fesom_mesh_info,
    get_namelist_config_path,
    scan_namelist_directory,
)
from esm_catalog.scan.upath import list_files
from esm_catalog.stac.collection import make_collection
from esm_catalog.stac.item import make_item


def scan_tree(root, config: dict | None = None) -> dict:
    """Scan *root* recursively and return {"collections": [...], "items": [...]}."""
    collections: dict[str, dict] = {}
    namelists_cache: dict[Path | None, dict] = {}
    items: list[dict] = []

    for path in list_files(root):
        try:
            ctx = resolve_context(path, config=config)
        except (RestartFileError, CollectionContextError) as exc:
            logger.debug("Skipping {}: {}", path, exc)
            continue

        # Populate pre-scanned namelists once per experiment for item building.
        if ctx.experiment_path not in namelists_cache:
            namelists_cache[ctx.experiment_path] = scan_all_namelists(ctx.experiment_path)
        ctx.namelists_by_component = namelists_cache[ctx.experiment_path]

        if ctx.collection_id not in collections:
            collections[ctx.collection_id] = _build_collection(ctx)
        elif ctx.component not in collections[ctx.collection_id].get("components", []):
            collections[ctx.collection_id]["components"].append(ctx.component)

        try:
            metadata = scan_file(path)
        except UnsupportedFormatError as exc:
            logger.debug("Unsupported {}: {}", path, exc)
            continue
        items.append(make_item(path, metadata, ctx, config))

    return {"collections": list(collections.values()), "items": items}


def _build_collection(ctx) -> dict:
    namelists = None
    fesom_info = None
    if ctx.experiment_path is not None:
        cfg = get_namelist_config_path(ctx.experiment_path, ctx.component)
        if cfg is not None:
            namelists = scan_namelist_directory(cfg, ctx.component)
            if ctx.component.lower() in ("fesom", "fesom2"):
                fesom_info = extract_fesom_mesh_info(cfg)
    return make_collection(ctx, namelists=namelists, fesom_info=fesom_info)
```

- [ ] **Step 4: Run — expect pass.**

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/scan/ingest.py src/esm_catalog/tests/test_scan_ingest.py
git commit -m "feat(esm_catalog): in-memory scan_tree pipeline (files -> STAC)"
```

## Task A1b1.6 — CLI `esm-catalog scan`

**Files:** `cli.py`
**Test:** `src/esm_catalog/tests/test_cli_scan.py`

- [ ] **Step 1: Failing test**

Create `src/esm_catalog/tests/test_cli_scan.py`:
```python
"""`esm-catalog scan` walks a dir and emits a STAC catalog JSON."""

from __future__ import annotations

import json

import numpy as np
import pandas as pd
import xarray as xr
from click.testing import CliRunner

from esm_catalog.cli import main


def test_cli_scan_outputs_json(tmp_path):
    out = tmp_path / "experiments" / "exp-alpha" / "outdata" / "echam"
    out.mkdir(parents=True)
    times = pd.date_range("2000-01-01", periods=1, freq="MS")
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    ).to_netcdf(out / "tas_200001.nc")

    target = tmp_path / "cat.json"
    res = CliRunner().invoke(main, ["scan", str(tmp_path), "--output", str(target)])
    assert res.exit_code == 0, res.output
    cat = json.loads(target.read_text())
    assert cat["collections"][0]["id"] == "exp-alpha"
    assert len(cat["items"]) == 1
```

- [ ] **Step 2: Run — expect failure** (`No such command 'scan'`).

- [ ] **Step 3: Add the subcommand to `cli.py`**

Append to `src/esm_catalog/cli.py` (after the `main` group):
```python
import json as _json
from pathlib import Path as _Path


@main.command()
@click.argument("path", type=click.Path(exists=True))
@click.option("--output", "-o", type=click.Path(),
              help="Write the STAC catalog JSON here (default: stdout).")
def scan(path: str, output: str | None) -> None:
    """Scan a run directory and emit an in-memory STAC catalog as JSON."""
    from esm_catalog.scan.ingest import scan_tree

    catalog = scan_tree(_Path(path))
    text = _json.dumps(catalog, indent=2, default=str)
    if output:
        _Path(output).write_text(text)
        click.echo(f"Wrote {len(catalog['collections'])} collections, "
                   f"{len(catalog['items'])} items to {output}")
    else:
        click.echo(text)
```

- [ ] **Step 4: Run — expect pass.**

- [ ] **Step 5: Full suite + cycle guard**

```bash
.venv-catalog-312/bin/python -m pytest src/esm_catalog/tests -v
grep -rn "esm_catalog.scan" src/esm_catalog/stac/   # expect empty — cycle still broken
.venv-catalog-312/bin/python -c "import esm_catalog.scan.ingest; print('ingest ok')"
```

- [ ] **Step 6: Commit, push, open PR**

```bash
git add src/esm_catalog/cli.py src/esm_catalog/tests/test_cli_scan.py
git commit -m "feat(esm_catalog): add 'esm-catalog scan' CLI command"
git push -u origin esm-catalog/pr-a1b1-scan-netcdf
gh pr create --base esm-catalog/pr-a1a-stac-foundation \
  --title "feat(esm_catalog): PR-A1b-1 — scan ingest + NetCDF" \
  --body "Third slice: the scan/ingest pipeline (Miguel's 'scan run dirs' unit), NetCDF scanner, and 'esm-catalog scan' CLI. Completes the rewiring PR-A1a deferred: scan uses the neutral CollectionContext, re-adds collection_title, reintroduces get_namelist_config_path, and populates namelists_by_component. GRIB/ECHAM scanners follow in PR-A1b-2. Plan: docs/superpowers/plans/2026-06-13-esm-catalog-pr-a1b.md"
```

---

## Self-Review (A1b-1)

- **Spec coverage:** all six deferred-rewiring items from PR-A1a are implemented (collection_title in A1b1.1; neutral context + make_collection signature + scan_all_namelists in A1b1.4; get_namelist_config_path in A1b1.2; uri re-export in A1b1.2/.3). The "scan run dirs → STAC" unit is delivered end-to-end (A1b1.5/.6).
- **Cycle stays broken:** scan imports from stac/uri/context one-directionally; the A1a guard test (`test_no_scan_dependency.py`) still runs and must stay green. No stac file gains a scan import.
- **Storage deferred correctly:** `resolve_context(db=None)` and `scan_tree` build in memory; DuckDB is PR-A2.
- **Type/name consistency:** `scan_all_namelists` / `namelists_by_component` / `make_collection(ctx, namelists=, fesom_info=)` / `get_namelist_config_path` names match across context.py, ingest.py, namelist.py, and tests.

---

## PR-A1b-2 — GRIB + ECHAM scanners (outline; full plan later)

**Goal:** add the remaining scanners so `scan_file` handles GRIB and extension-less ECHAM output.

- Extract `scan/grib.py`, `scan/echam.py` from `$SRC` (fix any `scan.upath` → `esm_catalog.uri` imports).
- In `scan/detect.py`, the lazy `_dispatch_grib` now resolves (modules present); magic-byte sniffing (`_sniff_format`) already covers extension-less GRIB/ECHAM.
- Add deps to the `catalog` extra + CI: `cfgrib`, `eccodes` (and the `.codes`-file ECHAM handling).
- Tests: GRIB scan from a small fixture (or a generated GRIB via cfgrib/eccodes if feasible in CI; otherwise a committed tiny `.grb` — **not** the 17 MB sample, build a minimal one), ECHAM `.codes` detection, magic-byte dispatch for an extension-less file.
- Stacks on PR-A1b-1; base retargets to `release` as the stack merges.

> Decision deferred to its own plan: whether GRIB test fixtures can be generated at test time (preferred — no binary in the repo) or need a tiny committed sample.
