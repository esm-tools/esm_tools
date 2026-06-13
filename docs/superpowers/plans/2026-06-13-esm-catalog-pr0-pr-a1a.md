# esm_catalog PR-0 (scaffold) + PR-A1a (STAC model foundation) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reintroduce `esm_catalog` to `release` as the first two reviewable PRs — a minimal package scaffold (PR-0) and the standalone STAC data-model foundation (PR-A1a) — breaking the `scan`⇄`stac` import cycle on the way in.

**Architecture:** Rebuild-from-end-state. The working code lives on `origin/esm-tools-plus/simcat/collapsed-collections`; we branch off current `release` and bring in only the files each PR owns via `git checkout <branch> -- <paths>`, then apply targeted edits. PR-A1a lands `stac/` + `hpc/` plus two new neutral modules (`uri.py`, `context.py`) that absorb the things `stac` previously reached into `scan` for. After this PR, `import esm_catalog.stac` and all stac/hpc unit tests pass with **no `scan`, `storage`, or `api` present**.

**Tech Stack:** Python (≥3.8 target for catalog; base repo supports 3.6–3.12), setuptools, pytest, Click, loguru, pystac/shapely (optional `catalog` extra), GitHub Actions.

**Source of truth for extraction:** `origin/esm-tools-plus/simcat/collapsed-collections`. Refer to it as `$SRC` below:
```bash
SRC=origin/esm-tools-plus/simcat/collapsed-collections
```

**Design reference:** `docs/superpowers/specs/2026-06-13-esm-catalog-pr-decomposition-design.md`

---

## Background facts the implementer must know

- `release` contains **zero** `esm_catalog` code. `find_packages("src")` in `setup.py` auto-discovers any package created under `src/`, so no manual package listing is needed.
- The `scan`→`stac` coupling is **exactly three** lazy (function-level) imports, all in `stac/`:
  - `stac/item.py:42` → `from esm_catalog.scan.upath import parse_uri`
  - `stac/item.py:192` → `from esm_catalog.scan.upath import to_uri`
  - `stac/collection.py:57` → `from esm_catalog.scan.namelist import extract_fesom_mesh_info, scan_namelist_directory`
  - `stac/extensions/namelist.py:161` → `from esm_catalog.scan.namelist import scan_namelist_directory`
- `CollectionContext` is currently defined in `scan/context.py` but used **duck-typed** in `stac/` (never imported there). It carries: `experiment_id`, `component`, `collection_id`, `experiment_path`. We relocate it to a neutral `esm_catalog/context.py` and add a pre-scanned `namelists_by_component` field so stac never scans.
- The other stac extensions (`datacube`, `contacts`, `hpc`, `paleo`, `registry`) do **not** import `scan`. `hpc/` is a dependency leaf.
- Existing `src/esm_catalog/tests/*` are full-stack integration tests (need `api`+`storage`+fastapi). **Do not** try to run them in these PRs — they land with their dependencies later. Write new focused tests instead.
- `setup.cfg` `[tool:pytest]` uses `norecursedirs` to keep catalog tests out of the **main** esm_tools test run. We add a **separate** CI job for catalog tests.

---

## File Structure (what each PR creates/owns)

**PR-0 (scaffold):**
- `src/esm_catalog/__init__.py` — package marker + `__version__`.
- `src/esm_catalog/cli.py` — minimal Click group exposing `main`; subcommands added by later PRs.
- `src/esm_catalog/tests/__init__.py`, `src/esm_catalog/tests/test_smoke.py` — import + `--version` smoke test.
- `setup.py` (modify) — add `esm-catalog` console script + `catalog` optional extra.
- `.github/workflows/esm-catalog-tests.yml` — dedicated job: install `.[catalog]`, run catalog tests.
- `.gitignore` (modify) — ignore `*.duckdb`, catalog scratch.
- `docs/esm_catalog_index.rst` — one-line docs stub (so the package has a docs anchor).

**PR-A1a (STAC model foundation):** all under `src/esm_catalog/`
- `uri.py` (new) — `parse_uri`, `to_uri`, `_get_upath`, `_has_protocol` (moved out of `scan/upath.py`).
- `context.py` (new) — `CollectionContext` dataclass with `namelists_by_component`.
- `stac/__init__.py`, `stac/collection.py`, `stac/item.py` (extracted + edited).
- `stac/extensions/{__init__,registry,datacube,contacts,hpc,paleo,namelist}.py` (extracted; only `namelist.py` edited).
- `hpc/{__init__,detect,state}.py` (extracted as-is).
- Tests: `tests/test_uri.py`, `tests/test_context.py`, `tests/test_stac_item.py`, `tests/test_stac_collection.py`, `tests/test_stac_namelist_ext.py`, `tests/test_no_scan_dependency.py`.

---

# PR-0 — Package scaffold

### Task 0.1: Create the branch and package skeleton

**Files:**
- Create: `src/esm_catalog/__init__.py`
- Create: `src/esm_catalog/cli.py`

- [ ] **Step 1: Create the PR-0 branch off current release**

```bash
git fetch origin
git switch -c esm-catalog/pr-0-scaffold origin/release
```

- [ ] **Step 2: Write the package `__init__.py`**

Create `src/esm_catalog/__init__.py`:
```python
"""esm_catalog — STAC-based catalog for ESM-Tools experiment output."""

from __future__ import annotations

__version__ = "0.1.0"
```

- [ ] **Step 3: Write the minimal CLI**

Create `src/esm_catalog/cli.py`:
```python
"""esm-catalog command-line interface.

This is the scaffold entry point. Subcommands (scan, serve, ...) are added by
subsequent feature PRs.
"""

from __future__ import annotations

import click

from esm_catalog import __version__


@click.group()
@click.version_option(version=__version__, prog_name="esm-catalog")
def main() -> None:
    """ESM-Tools simulation catalog."""


if __name__ == "__main__":
    main()
```

- [ ] **Step 4: Verify it imports and runs locally**

```bash
pip install -e .
esm-catalog --version
```
Expected: prints `esm-catalog, version 0.1.0` (after Task 0.2 wires the entry point; if run before 0.2, use `python -m esm_catalog.cli --version`).

### Task 0.2: Wire packaging (entry point + catalog extra)

**Files:**
- Modify: `setup.py`

- [ ] **Step 1: Add the console script**

In `setup.py`, inside `entry_points["console_scripts"]`, add (keep alphabetical-ish with the others):
```python
            "esm-catalog=esm_catalog.cli:main",
```

- [ ] **Step 2: Add the `catalog` optional extra (no `mcp` — it is dropped)**

In `setup.py`, add an `extras_require` keyword to the `setup(...)` call (the `release` baseline has none):
```python
    extras_require={
        "catalog": [
            "pystac>=1.8",
            "shapely>=2.0",
            "universal-pathlib>=0.2",
        ],
    },
```
> Only deps the *scaffold + A1a* need go here now. Later PRs append `duckdb`, `cfgrib`, `pyarrow`, `rich-click`, `fastapi`, etc. to this same extra. Do **not** add an `mcp` extra.

- [ ] **Step 3: Verify install with the extra**

```bash
pip install -e ".[catalog]"
esm-catalog --version
```
Expected: `esm-catalog, version 0.1.0`.

- [ ] **Step 4: Commit**

```bash
git add src/esm_catalog/__init__.py src/esm_catalog/cli.py setup.py
git commit -m "feat(esm_catalog): package scaffold + esm-catalog entry point"
```

### Task 0.3: Smoke test + test discovery

**Files:**
- Create: `src/esm_catalog/tests/__init__.py`
- Create: `src/esm_catalog/tests/test_smoke.py`

- [ ] **Step 1: Write the failing smoke test**

Create `src/esm_catalog/tests/__init__.py` (empty file) and `src/esm_catalog/tests/test_smoke.py`:
```python
"""Smoke tests: the package imports and the CLI runs."""

from __future__ import annotations

from click.testing import CliRunner

import esm_catalog
from esm_catalog.cli import main


def test_package_has_version():
    assert isinstance(esm_catalog.__version__, str)
    assert esm_catalog.__version__


def test_cli_version_runs():
    result = CliRunner().invoke(main, ["--version"])
    assert result.exit_code == 0
    assert "esm-catalog" in result.output
```

- [ ] **Step 2: Run it to verify it passes**

```bash
pytest src/esm_catalog/tests/test_smoke.py -v
```
Expected: 2 passed.

- [ ] **Step 3: Confirm the catalog tests stay out of the main esm_tools run**

```bash
grep -n "norecursedirs" setup.cfg
```
Expected: on `release` there is a `norecursedirs = tests/helpers` line but **no** `src/esm_catalog` entry. Add `src/esm_catalog` to a `norecursedirs` line so the main suite ignores it (the dedicated job in Task 0.4 runs it instead):
```ini
norecursedirs = tests/helpers src/esm_catalog
```
> If `release` has no `[tool:pytest]` section yet, add one with this single line.

- [ ] **Step 4: Verify the main suite ignores catalog, dedicated path still works**

```bash
pytest --collect-only -q 2>/dev/null | grep -c esm_catalog   # expect 0
pytest src/esm_catalog/tests -q                              # expect 2 passed
```

### Task 0.4: Dedicated CI workflow

**Files:**
- Create: `.github/workflows/esm-catalog-tests.yml`

- [ ] **Step 1: Write the workflow**

Create `.github/workflows/esm-catalog-tests.yml`:
```yaml
name: esm_catalog tests

on:
  push:
    paths:
      - "src/esm_catalog/**"
      - ".github/workflows/esm-catalog-tests.yml"
      - "setup.py"
  pull_request:
    paths:
      - "src/esm_catalog/**"
      - ".github/workflows/esm-catalog-tests.yml"
      - "setup.py"

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: ["3.9", "3.12"]
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: ${{ matrix.python-version }}
      - name: Install
        run: |
          python -m pip install --upgrade pip
          pip install -e ".[catalog]"
          pip install pytest
      - name: Run esm_catalog tests
        run: pytest src/esm_catalog/tests -v
```

- [ ] **Step 2: Validate YAML locally**

```bash
python -c "import yaml,sys; yaml.safe_load(open('.github/workflows/esm-catalog-tests.yml')); print('ok')"
```
Expected: `ok`.

### Task 0.5: gitignore + docs stub, then open the PR

**Files:**
- Modify: `.gitignore`
- Create: `docs/esm_catalog_index.rst`

- [ ] **Step 1: gitignore additions**

Append to `.gitignore`:
```gitignore
# esm_catalog
*.duckdb
*.duckdb.wal
```

- [ ] **Step 2: docs stub**

Create `docs/esm_catalog_index.rst`:
```rst
ESM Catalog
===========

STAC-based catalog for ESM-Tools experiment output. Full documentation lands
incrementally; see ``src/esm_catalog`` and the decomposition plan in
``docs/superpowers/specs/2026-06-13-esm-catalog-pr-decomposition-design.md``.
```

- [ ] **Step 3: Commit**

```bash
git add src/esm_catalog/tests setup.cfg .github/workflows/esm-catalog-tests.yml .gitignore docs/esm_catalog_index.rst
git commit -m "test(esm_catalog): smoke test + dedicated CI + scaffolding"
```

- [ ] **Step 4: Push and open the PR**

```bash
git push -u origin esm-catalog/pr-0-scaffold
gh pr create --base release --title "feat(esm_catalog): PR-0 — package scaffold" \
  --body "First slice of the esm_catalog decomposition (supersedes #1473). Adds the package skeleton, \`esm-catalog\` entry point, a \`catalog\` optional extra, a smoke test, and a dedicated CI job. No functionality yet — scan/serve land in following PRs. Plan: docs/superpowers/specs/2026-06-13-esm-catalog-pr-decomposition-design.md"
```

---

# PR-A1a — STAC model foundation + cycle-break

Branch off PR-0 (so the scaffold + extra are present):
```bash
git switch -c esm-catalog/pr-a1a-stac-foundation esm-catalog/pr-0-scaffold
```

### Task A1a.1: Neutral `uri.py` (removes `stac → scan.upath`)

**Files:**
- Create: `src/esm_catalog/uri.py`
- Test: `src/esm_catalog/tests/test_uri.py`

- [ ] **Step 1: Write failing tests**

Create `src/esm_catalog/tests/test_uri.py`:
```python
"""Unit tests for the neutral URI helpers (no scan/ dependency)."""

from __future__ import annotations

from pathlib import Path

from esm_catalog.uri import _has_protocol, to_uri


def test_has_protocol_detects_remote_and_local():
    assert _has_protocol("ssh://host/path") is True
    assert _has_protocol("s3://bucket/key") is True
    assert _has_protocol("/local/abs/path") is False


def test_to_uri_local_path_is_file_uri(tmp_path):
    f = tmp_path / "data.nc"
    f.write_bytes(b"x")
    assert to_uri(f) == f"file://{Path(f).resolve()}"
```

- [ ] **Step 2: Run to verify it fails**

```bash
pytest src/esm_catalog/tests/test_uri.py -v
```
Expected: FAIL with `ModuleNotFoundError: No module named 'esm_catalog.uri'`.

- [ ] **Step 3: Create `uri.py` by moving the four functions out of `scan/upath.py`**

Bring the exact implementations of `_get_upath`, `_has_protocol`, `parse_uri`, and `to_uri` from `$SRC:src/esm_catalog/scan/upath.py` into a new `src/esm_catalog/uri.py`. Header:
```python
"""Neutral URI helpers shared by stac/ and scan/ (no internal esm_catalog deps).

parse_uri / to_uri were previously in esm_catalog.scan.upath; they are pure
path<->URI converters with no scanning responsibility, so they live here to
keep the STAC model free of any dependency on the scan layer.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from upath import UPath
```
Then paste `_get_upath`, `_has_protocol`, `parse_uri`, `to_uri` verbatim from `scan/upath.py` (lines 38–84 and 138–170 in the source).

- [ ] **Step 4: Run tests to verify pass**

```bash
pytest src/esm_catalog/tests/test_uri.py -v
```
Expected: 2 passed.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/uri.py src/esm_catalog/tests/test_uri.py
git commit -m "refactor(esm_catalog): extract neutral uri helpers from scan.upath"
```

### Task A1a.2: Neutral `context.py` (`CollectionContext`)

**Files:**
- Create: `src/esm_catalog/context.py`
- Test: `src/esm_catalog/tests/test_context.py`

- [ ] **Step 1: Write failing test**

Create `src/esm_catalog/tests/test_context.py`:
```python
"""Unit tests for CollectionContext."""

from __future__ import annotations

from pathlib import Path

from esm_catalog.context import CollectionContext


def test_context_minimal():
    ctx = CollectionContext(
        experiment_id="exp-alpha",
        component="echam",
        collection_id="exp-alpha",
    )
    assert ctx.experiment_id == "exp-alpha"
    assert ctx.experiment_path is None
    assert ctx.namelists_by_component == {}


def test_context_carries_prescanned_namelists():
    ctx = CollectionContext(
        experiment_id="exp-alpha",
        component="echam",
        collection_id="exp-alpha",
        experiment_path=Path("/exp/alpha"),
        namelists_by_component={"echam": {"namelist.echam": {"runctl": {"dt": 450}}}},
    )
    assert ctx.namelists_by_component["echam"]["namelist.echam"]["runctl"]["dt"] == 450
```

- [ ] **Step 2: Run to verify it fails**

```bash
pytest src/esm_catalog/tests/test_context.py -v
```
Expected: FAIL `No module named 'esm_catalog.context'`.

- [ ] **Step 3: Create `context.py`**

Create `src/esm_catalog/context.py`:
```python
"""Shared CollectionContext value object.

Relocated from esm_catalog.scan.context so that the STAC model (stac/) can use
it without importing the scan layer. The scan layer now imports it from here
and is responsible for populating `namelists_by_component` before STAC items
are built (this is what breaks the former scan<->stac import cycle).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


@dataclass
class CollectionContext:
    """Identity + pre-scanned context for building a collection's items.

    Attributes:
        experiment_id: Experiment name (e.g. "exp-alpha").
        component: Model component for the current scan (e.g. "echam").
        collection_id: STAC collection id (Option A: == experiment_id).
        experiment_path: Optional path to the experiment root.
        namelists_by_component: Pre-scanned namelists, mapping
            component name -> {filename -> {group -> {key -> value}}}.
            Populated by the scan layer; the STAC model only reads it.
    """

    experiment_id: str
    component: str
    collection_id: str
    experiment_path: Optional[Path] = None
    namelists_by_component: dict = field(default_factory=dict)
```
> If `$SRC:scan/context.py`'s `CollectionContext` has additional fields beyond these, copy them across verbatim and append `namelists_by_component`. Inspect with: `git show $SRC:src/esm_catalog/scan/context.py | sed -n '1,80p'`.

- [ ] **Step 4: Run to verify pass**

```bash
pytest src/esm_catalog/tests/test_context.py -v
```
Expected: 2 passed.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/context.py src/esm_catalog/tests/test_context.py
git commit -m "refactor(esm_catalog): add neutral CollectionContext value object"
```

### Task A1a.3: Extract `hpc/` and the unmodified `stac/extensions/*`

**Files:**
- Create: `src/esm_catalog/hpc/{__init__,detect,state}.py`
- Create: `src/esm_catalog/stac/__init__.py`
- Create: `src/esm_catalog/stac/extensions/{__init__,registry,datacube,contacts,hpc,paleo}.py`

- [ ] **Step 1: Bring the files in verbatim from the source branch**

```bash
git checkout $SRC -- \
  src/esm_catalog/hpc \
  src/esm_catalog/stac/__init__.py \
  src/esm_catalog/stac/extensions/__init__.py \
  src/esm_catalog/stac/extensions/registry.py \
  src/esm_catalog/stac/extensions/datacube.py \
  src/esm_catalog/stac/extensions/contacts.py \
  src/esm_catalog/stac/extensions/hpc.py \
  src/esm_catalog/stac/extensions/paleo.py
```

- [ ] **Step 2: Confirm none of these import `scan`**

```bash
grep -rn "esm_catalog.scan\|from esm_catalog import scan" \
  src/esm_catalog/hpc src/esm_catalog/stac/extensions/registry.py \
  src/esm_catalog/stac/extensions/datacube.py src/esm_catalog/stac/extensions/contacts.py \
  src/esm_catalog/stac/extensions/hpc.py src/esm_catalog/stac/extensions/paleo.py \
  src/esm_catalog/stac/__init__.py
```
Expected: no output. If `stac/__init__.py` re-exports `collection`/`item` (added in A1a.4/.5), that is fine — those land in this same PR.

- [ ] **Step 3: Verify these modules import standalone**

```bash
python -c "import esm_catalog.hpc; import esm_catalog.stac.extensions.datacube; import esm_catalog.stac.extensions.paleo; print('ok')"
```
Expected: `ok`.

- [ ] **Step 4: Commit**

```bash
git add src/esm_catalog/hpc src/esm_catalog/stac/__init__.py src/esm_catalog/stac/extensions
git commit -m "feat(esm_catalog): add hpc/ leaf and scan-free stac extensions"
```

### Task A1a.4: Pure `stac/collection.py` (removes `stac → scan.namelist`)

**Files:**
- Create: `src/esm_catalog/stac/collection.py`
- Test: `src/esm_catalog/tests/test_stac_collection.py`

- [ ] **Step 1: Write failing tests**

Create `src/esm_catalog/tests/test_stac_collection.py`:
```python
"""make_collection is pure: accepts pre-scanned namelist data, never scans."""

from __future__ import annotations

from esm_catalog.context import CollectionContext
from esm_catalog.stac.collection import make_collection, update_collection_extent


def _ctx():
    return CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )


def test_make_collection_minimal_skeleton():
    col = make_collection(_ctx())
    assert col["type"] == "Collection"
    assert col["id"] == "exp-alpha"
    assert col["components"] == ["echam"]
    assert "nml:files" not in col  # no namelists passed -> none added


def test_make_collection_applies_prescanned_namelists():
    namelists = {"namelist.echam": {"runctl": {"delta_time": 450, "lcouple": True}}}
    col = make_collection(_ctx(), namelists=namelists)
    assert col["nml:files"] == ["namelist.echam"]
    assert col["nml:parameters"]["runctl:delta_time"] == 450


def test_update_collection_extent_expands_temporal():
    col = make_collection(_ctx())
    item = {"bbox": [0, 0, 1, 1],
            "properties": {"datetime": "2000-01-01T00:00:00"}}
    col = update_collection_extent(col, item)
    assert col["extent"]["temporal"]["interval"][0][0] == "2000-01-01T00:00:00"
```

- [ ] **Step 2: Run to verify it fails**

```bash
pytest src/esm_catalog/tests/test_stac_collection.py -v
```
Expected: FAIL `No module named 'esm_catalog.stac.collection'`.

- [ ] **Step 3: Create the pure `collection.py`**

Bring `update_collection_extent`, `_merge_bbox`, `_parse_iso` verbatim from `$SRC:src/esm_catalog/stac/collection.py`. Replace `make_collection` and **delete `_add_namelists`** so the module never imports `scan`. New `make_collection`:
```python
from __future__ import annotations
"""Create and update STAC Collection objects (pure: no scanning)."""

from datetime import datetime

from esm_catalog.stac.extensions.namelist import add_namelist_extension


def make_collection(ctx, namelists: dict | None = None,
                    fesom_info: dict | None = None) -> dict:
    """Return a STAC Collection dict for the given CollectionContext.

    Args:
        ctx: CollectionContext (experiment_id, component, collection_id).
        namelists: Optional pre-scanned namelists ({filename -> {group -> {k: v}}}).
            Scanned by the scan layer and passed in; this function never scans.
        fesom_info: Optional pre-extracted FESOM mesh fields to merge in.

    Returns:
        STAC Collection dict.
    """
    collection = {
        "type": "Collection",
        "id": ctx.collection_id,
        "stac_version": "1.0.0",
        "stac_extensions": [],
        "title": ctx.experiment_id,
        "description": f"All model output for experiment {ctx.experiment_id}",
        "license": "proprietary",
        "extent": {
            "spatial": {"bbox": [[-180.0, -90.0, 180.0, 90.0]]},
            "temporal": {"interval": [[None, None]]},
        },
        "links": [
            {"rel": "parent", "href": f"#{ctx.experiment_id}",
             "type": "application/json"},
        ],
        "experiment": ctx.experiment_id,
        "components": [ctx.component],
    }

    if namelists:
        collection = add_namelist_extension(collection, namelists)
    if fesom_info:
        collection.update(fesom_info)

    return collection
```
> The namelist/FESOM **scanning** that `_add_namelists` used to do moves to the scan layer in PR-A1b: scan will call `scan_namelist_directory` / `extract_fesom_mesh_info` and pass the results into `make_collection(namelists=..., fesom_info=...)`.

- [ ] **Step 4: Run to verify pass**

```bash
pytest src/esm_catalog/tests/test_stac_collection.py -v
```
Expected: 3 passed.

- [ ] **Step 5: Commit**

```bash
git add src/esm_catalog/stac/collection.py src/esm_catalog/tests/test_stac_collection.py
git commit -m "refactor(esm_catalog): make make_collection pure (no scan dependency)"
```

### Task A1a.5: Pure `stac/extensions/namelist.py` + `stac/item.py`

**Files:**
- Create: `src/esm_catalog/stac/extensions/namelist.py`
- Create: `src/esm_catalog/stac/item.py`
- Test: `src/esm_catalog/tests/test_stac_namelist_ext.py`, `src/esm_catalog/tests/test_stac_item.py`

- [ ] **Step 1: Write failing tests for the pure namelist item extension**

Create `src/esm_catalog/tests/test_stac_namelist_ext.py`:
```python
"""add_namelist_item_extension reads pre-scanned ctx data; it never scans."""

from __future__ import annotations

from esm_catalog.context import CollectionContext
from esm_catalog.stac.extensions.namelist import (
    add_namelist_extension,
    add_namelist_item_extension,
)


def test_collection_extension_flattens_parameters():
    col = {"type": "Collection", "id": "x", "stac_extensions": []}
    col = add_namelist_extension(
        col, {"namelist.echam": {"runctl": {"delta_time": 450, "lcouple": True}}}
    )
    assert col["nml:parameters"]["runctl:delta_time"] == 450
    assert col["nml:parameters"]["runctl:lcouple"] is True


def test_item_extension_uses_prescanned_context():
    ctx = CollectionContext(
        experiment_id="exp", component="echam", collection_id="exp",
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"co2vmr": 0.000284}}},
        },
    )
    item = {"id": "i", "properties": {}, "stac_extensions": []}
    item = add_namelist_item_extension(item, ctx)
    assert item["properties"]["nml:echam:runctl:co2vmr"] == 0.000284


def test_item_extension_noop_without_namelists():
    ctx = CollectionContext(experiment_id="e", component="c", collection_id="e")
    item = {"id": "i", "properties": {}, "stac_extensions": []}
    assert add_namelist_item_extension(item, ctx) == item
```

- [ ] **Step 2: Run to verify it fails**

```bash
pytest src/esm_catalog/tests/test_stac_namelist_ext.py -v
```
Expected: FAIL `No module named 'esm_catalog.stac.extensions.namelist'`.

- [ ] **Step 3: Create the pure `extensions/namelist.py`**

Bring `add_namelist_extension`, `_flatten_for_search`, `get_namelist_config_path` verbatim from `$SRC:src/esm_catalog/stac/extensions/namelist.py`. Replace `add_namelist_item_extension` with a pure version that reads `ctx.namelists_by_component` instead of scanning:
```python
def add_namelist_item_extension(item: dict, ctx) -> dict:
    """Inject namelist parameters from ALL components into a STAC item.

    Reads ctx.namelists_by_component (populated by the scan layer); this
    function performs no scanning and imports nothing from scan/.
    """
    by_component = getattr(ctx, "namelists_by_component", None) or {}
    total_params = 0

    for component_name, namelists in by_component.items():
        for _filename, groups in namelists.items():
            for group_name, values in groups.items():
                for key, value in values.items():
                    if value is None or isinstance(value, dict):
                        continue
                    if isinstance(value, list):
                        if len(value) > 10:
                            continue
                        if not all(
                            isinstance(v, (int, float, str, bool, type(None)))
                            for v in value
                        ):
                            continue
                    item["properties"][f"nml:{component_name}:{group_name}:{key}"] = value
                    total_params += 1

    if total_params > 0:
        url = EXTENSION_URLS.get("namelist")
        if url and url not in item.get("stac_extensions", []):
            item.setdefault("stac_extensions", []).append(url)

    return item
```
> Keep the existing `from esm_catalog.stac.extensions.registry import EXTENSION_URLS` import and the module docstring. Remove the `from esm_catalog.scan.namelist import scan_namelist_directory` line entirely.

- [ ] **Step 4: Create `stac/item.py` with the uri import fixed**

```bash
git checkout $SRC -- src/esm_catalog/stac/item.py
```
Then edit `src/esm_catalog/stac/item.py`:
- Line ~42: change `from esm_catalog.scan.upath import parse_uri` → `from esm_catalog.uri import parse_uri`
- Line ~192: change `from esm_catalog.scan.upath import to_uri` → `from esm_catalog.uri import to_uri`

Apply:
```bash
sed -i '' 's/from esm_catalog\.scan\.upath import parse_uri/from esm_catalog.uri import parse_uri/' src/esm_catalog/stac/item.py
sed -i '' 's/from esm_catalog\.scan\.upath import to_uri/from esm_catalog.uri import to_uri/' src/esm_catalog/stac/item.py
```

- [ ] **Step 5: Write failing test for `make_item`**

Create `src/esm_catalog/tests/test_stac_item.py`:
```python
"""make_item builds a STAC Item from scan metadata + a CollectionContext."""

from __future__ import annotations

from datetime import datetime, timezone

from esm_catalog.context import CollectionContext
from esm_catalog.stac.item import make_item


def test_make_item_basic(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )
    metadata = {
        "variable": "temp",
        "format": "netcdf",
        "file_size": 1,
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
    }
    item = make_item(f, metadata, ctx)
    assert item["type"] == "Feature"
    assert item["properties"]["variable"] == "temp"
    assert item["collection"] == "exp-alpha"
    assert item["assets"]["data"]["href"].startswith("file://")
```

- [ ] **Step 6: Run both test files to verify pass**

```bash
pytest src/esm_catalog/tests/test_stac_namelist_ext.py src/esm_catalog/tests/test_stac_item.py -v
```
Expected: all passed. (Install the extra first if needed: `pip install -e ".[catalog]"`.)

- [ ] **Step 7: Commit**

```bash
git add src/esm_catalog/stac/extensions/namelist.py src/esm_catalog/stac/item.py \
        src/esm_catalog/tests/test_stac_namelist_ext.py src/esm_catalog/tests/test_stac_item.py
git commit -m "refactor(esm_catalog): pure stac item + namelist extension (no scan dependency)"
```

### Task A1a.6: Guard test — prove the cycle is broken

**Files:**
- Test: `src/esm_catalog/tests/test_no_scan_dependency.py`

- [ ] **Step 1: Write the guard test**

Create `src/esm_catalog/tests/test_no_scan_dependency.py`:
```python
"""Regression guard: the STAC model must not depend on the scan layer.

If this fails, someone reintroduced a scan import into stac/ — breaking the
ability to ship the STAC foundation as a standalone, reviewable unit.
"""

from __future__ import annotations

import ast
import pathlib

STAC_DIR = pathlib.Path(__file__).resolve().parent.parent / "stac"


def _imports(py_file: pathlib.Path) -> set[str]:
    tree = ast.parse(py_file.read_text())
    names: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module:
            names.add(node.module)
        elif isinstance(node, ast.Import):
            names.update(a.name for a in node.names)
    return names


def test_stac_has_no_scan_imports():
    offenders = {}
    for py in STAC_DIR.rglob("*.py"):
        bad = {m for m in _imports(py) if m.startswith("esm_catalog.scan")}
        if bad:
            offenders[str(py.relative_to(STAC_DIR))] = bad
    assert not offenders, f"stac/ imports scan: {offenders}"


def test_stac_imports_without_scan(monkeypatch):
    import builtins

    real_import = builtins.__import__

    def blocked(name, *args, **kwargs):
        if name.startswith("esm_catalog.scan"):
            raise ImportError(f"scan import blocked: {name}")
        return real_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", blocked)
    import importlib
    import esm_catalog.stac.item
    import esm_catalog.stac.collection
    import esm_catalog.stac.extensions.namelist
    importlib.reload(esm_catalog.stac.item)
    importlib.reload(esm_catalog.stac.collection)
    importlib.reload(esm_catalog.stac.extensions.namelist)
```

- [ ] **Step 2: Run the full PR-A1a test set**

```bash
pip install -e ".[catalog]"
pytest src/esm_catalog/tests -v
```
Expected: all tests from Tasks 0.3, A1a.1–A1a.6 pass (smoke, uri, context, collection, namelist ext, item, no-scan guard).

- [ ] **Step 3: Commit**

```bash
git add src/esm_catalog/tests/test_no_scan_dependency.py
git commit -m "test(esm_catalog): guard that stac/ never imports scan/"
```

### Task A1a.7: Push and open the PR

- [ ] **Step 1: Final local verification**

```bash
pytest src/esm_catalog/tests -q
python -c "import esm_catalog.stac.item, esm_catalog.stac.collection, esm_catalog.hpc; print('foundation ok')"
```
Expected: green; `foundation ok`.

- [ ] **Step 2: Push and open**

```bash
git push -u origin esm-catalog/pr-a1a-stac-foundation
gh pr create --base esm-catalog/pr-0-scaffold \
  --title "feat(esm_catalog): PR-A1a — STAC data-model foundation" \
  --body "Second slice (supersedes part of #1473). Lands the STAC model (stac/ collection+item+extensions) and hpc/ leaf, and **breaks the scan⇄stac import cycle**: parse_uri/to_uri move to esm_catalog.uri; CollectionContext moves to esm_catalog.context with pre-scanned namelist data; make_collection and the namelist item extension are now pure (the scan layer passes data in). A guard test (test_no_scan_dependency.py) keeps it that way. Scanners arrive in PR-A1b. Plan: docs/superpowers/specs/2026-06-13-esm-catalog-pr-decomposition-design.md"
```
> If the team prefers PRs to target `release` directly rather than stacking, rebase onto `release` once PR-0 merges: `git rebase --onto release esm-catalog/pr-0-scaffold` and retarget the PR base to `release`.

---

## Self-Review

**Spec coverage:** PR-0 (scaffold) and PR-A1a (STAC model + cycle-break) from spec §5 Phase A are fully covered; the `scan→stac` cycle-break risk (spec §9) is implemented in A1a.1/.2/.4/.5 and locked by the guard test in A1a.6. Out of scope here (correctly): scanners (A1b), storage (A2), API (Phase B) — separate plans.

**Placeholder scan:** No TBDs. Every code step shows real code. The two extraction steps that say "bring verbatim from `$SRC`" name exact files and the exact functions/lines to copy, plus the precise edits to apply — not placeholders.

**Type/name consistency:** `CollectionContext` fields (`experiment_id`, `component`, `collection_id`, `experiment_path`, `namelists_by_component`) are used consistently across `context.py`, `make_collection`, `add_namelist_item_extension`, and all tests. `make_collection(ctx, namelists=None, fesom_info=None)` and `add_namelist_extension(collection, namelists)` / `add_namelist_item_extension(item, ctx)` signatures match between implementation and tests. `parse_uri`/`to_uri` import path (`esm_catalog.uri`) is consistent in `item.py` and tests.

**Known follow-through for PR-A1b (noted, not done here):** the scan layer must (a) import `CollectionContext` from `esm_catalog.context`, (b) populate `namelists_by_component`, and (c) call `make_collection(namelists=..., fesom_info=...)` — replacing the deleted `_add_namelists`. `scan/upath.py` should re-export `parse_uri`/`to_uri` from `esm_catalog.uri` for back-compat, or update its internal callers.
