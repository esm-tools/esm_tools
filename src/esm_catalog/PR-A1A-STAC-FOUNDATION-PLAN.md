# ESM Catalog STAC — Development Plan

This file tracks the plan for breaking the `esm-catalog/pr-a1a-stac-foundation` branch
into a series of small, reviewable PRs. Each PR branches from `release`.

Future Claude sessions: read this file first, implement the next unchecked PR,
then tick its checkbox and update the status.

---

## File layout convention

All new files live **flat** inside `src/esm_catalog/` — no `stac/` or `extensions/`
subdirectories. `esm_catalog` IS the STAC catalog layer; the extra nesting is redundant.

```
src/esm_catalog/
    context.py      ← PR-A1b
    registry.py     ← PR-A1b
    collection.py   ← PR-A1b
    item.py         ← PR-A1b
    hpc.py          ← PR-A1c  (extension + detect logic combined)
    datacube.py     ← PR-A1d
    namelist.py     ← PR-A1e
    paleo.py        ← PR-A1f
    contacts.py     ← PR-A1g
```

## Custom STAC extension schemas

The three custom ESM-Tools extensions (`hpc`, `paleo`, `namelist`) need JSON Schema
files. These live **in this repo** under `configs/stac-extensions/`, not on an external
site:

```
configs/stac-extensions/
    hpc/v1.0.0/schema.json       ← PR-A1c
    namelist/v1.0.0/schema.json  ← PR-A1e
    paleo/v1.0.0/schema.json     ← PR-A1f
```

The URLs in `registry.py` point to `https://esm-tools.github.io/stac-extensions/...`
as placeholders. Each extension PR must update its URL to the raw GitHub URL
(`https://raw.githubusercontent.com/esm-tools/esm_tools/release/configs/stac-extensions/...`)
**after** the schema file is merged to `release`. Do not update the URL on the feature
branch — the file won't exist at that path until it's on `release`.

---

## Metadata fields reference

| # | Field | Status | PR |
|---|-------|--------|----|
| 1 | `variable` | confirmed | A1b |
| 2 | `stream` (GRIB stream type) | **explore later** | — |
| 3 | `datetime` / `start_datetime` / `end_datetime` | confirmed | A1b |
| 4 | `file:size` | **explore later** | — |
| 5 | `format` (netcdf/grib) | confirmed | A1b |
| 6 | `keywords` | **explore later** | — |
| 7 | `conventions` (CF conventions string) | **explore later** | — |
| 8 | `output_frequency` | confirmed | A1b |
| 9 | `file:{key}` all global attributes | **explore later** | — |
| 10 | `variables` (multi-variable list) | confirmed | A1b |
| 11 | `cf:parameter` | **explore later** | — |
| 12 | `cube:dimensions` | confirmed | A1d |
| 13 | `cube:variables` | confirmed | A1d |
| 14 | `hpc:facility` | confirmed | A1c |
| 15 | `hpc:system` | confirmed | A1c |
| 16 | `hpc:storage_tier` | confirmed | A1c |
| 17 | `hpc:last_access` | confirmed | A1c |
| 18 | `hpc:storage_type` | confirmed | A1c |
| 19 | `hpc:state` (subprocess dmattr/lfs) | **dropped** | — |
| 20 | `hpc:recall_time_estimate` | **dropped** | — |
| 21 | `paleo:year` | confirmed | A1f |
| 22 | `paleo:display` | confirmed | A1f |
| 23 | `paleo:reference_year` | confirmed | A1f |
| 24 | `paleo:epoch` | confirmed | A1f |
| 25 | `paleo:period` | confirmed | A1f |
| 26 | `paleo:years_bp` | confirmed | A1f |
| 27 | `experiment_type` | confirmed | A1f |
| 28 | `contacts[].name` | confirmed | A1g |
| 29 | `contacts[].identifier` (ORCID) | confirmed | A1g |
| 30 | `contacts[].organization` | confirmed | A1g |
| 31 | `nml:{component}:{group}:{key}` | confirmed | A1e |

---

## PRs

### [x] PR-A1b — Core infrastructure

**Branch:** `esm-catalog/pr-a1b-core-infrastructure` ✓ merged design decisions:
- Function-based API: `make_item(...)` returns `pystac.Item`; `make_collection(...)` returns `pystac.Collection`; `update_extent(collection, item)` is a standalone function
- `CollectionContext` dataclass with `production: bool` flag and `__post_init__` validation; required production fields listed in `PRODUCTION_REQUIRED: ClassVar`; `data_license` field (avoids shadowing Python builtin `license`)
- Flat layout: all files directly in `src/esm_catalog/`, no subdirectories
- `test_no_scan_dependency.py` deferred — add it in the PR that introduces `esm_catalog.scan`
- No dependency on `esm_tools` from `esm_catalog` (avoids `pkg_resources` / Python 3.12 CI breakage)

**Branch from:** `release`
**Goal:** Minimal working STAC Item + Collection with only the confirmed core fields.
No extension calls.

**Files created:**
- `src/esm_catalog/context.py` — `CollectionContext` dataclass with production validation
- `src/esm_catalog/registry.py` — `EXTENSION_URLS` dict
- `src/esm_catalog/collection.py` — `make_collection(ctx)`, `update_extent(collection, item)`
- `src/esm_catalog/item.py` — `make_item(path, metadata, ctx)` with `_build_*` helper functions; core fields only (1, 3, 5, 8, 10); URI conversion in `_to_href` (UPath 0.3.x drops SSH host from `str(path)`, workaround uses `storage_options['host']`)

**Item properties in this PR:**
```
variable, datetime/start_datetime/end_datetime, format, output_frequency, variables (list)
```

**Tests created:**
- `tests/test_esm_catalog/test_context.py`
- `tests/test_esm_catalog/test_stac_collection.py`
- `tests/test_esm_catalog/test_stac_item.py`

---

### [ ] PR-A1c — HPC storage extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add HPC storage metadata to items. Fields: 14, 15, 16, 17, 18.
Fields 19 and 20 (HSM subprocess queries) are permanently dropped.

**Files to create:**
- `src/esm_catalog/hpc.py` — extension + detect logic combined in one file (no subdirectory).
  Replaces both `hpc/detect.py` and `stac/extensions/hpc.py` from pr-a1a branch.
- `configs/stac-extensions/hpc/v1.0.0/schema.json` — JSON Schema for the hpc extension.
  After this PR is merged to `release`, update the `"hpc"` URL in `registry.py` to the
  raw GitHub URL.
- `tests/test_esm_catalog/test_hpc.py` — write new tests

**Key problem with the existing `detect.py` — must be fixed:**
The current code hardcodes machine names as Python path patterns (`/albedo/` → AWI, `/work/` → DKRZ).
This is machine-specific and fragile. The right approach:
- Machine storage info (facility, system, storage_type) belongs in ESM-Tools machine YAML configs.
- Detection should read from the parsed ESM-Tools config/machine dict, not probe the filesystem.
- Drop the `ctypes`/`statfs(2)` approach entirely — it is Linux-only and uses raw C struct layouts.
- Keep `hpc:last_access` from `path.stat().st_atime` (simple, no subprocess).

**Signature for `add_hpc_extension`:**
```python
add_hpc_extension(item, path, machine_config: dict | None = None)
# machine_config is the parsed ESM-Tools machine section;
# if None, only hpc:last_access is populated (graceful degradation).
```

---

### [ ] PR-A1d — Datacube extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add cube:dimensions and cube:variables to items. Fields: 12, 13.

**Files to create:**
- `src/esm_catalog/datacube.py` — copy from `stac/extensions/datacube.py` in pr-a1a branch; no logic changes needed.
- `tests/test_esm_catalog/test_stac_datacube.py` — write new tests

**Note:** datacube uses an external community schema URL (already resolves); no schema file
needed in `configs/stac-extensions/`.

---

### [ ] PR-A1e — Namelist extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add namelist parameters to items and collections. Field: 31.

**Files to create:**
- `src/esm_catalog/namelist.py` — adapted from `stac/extensions/namelist.py` in pr-a1a branch.
- `configs/stac-extensions/namelist/v1.0.0/schema.json` — JSON Schema for the namelist extension.
  After this PR is merged to `release`, update the `"namelist"` URL in `registry.py` to the
  raw GitHub URL.
- `tests/test_esm_catalog/test_stac_namelist_ext.py`

**Changes from pr-a1a branch:**
- Drop `nml:raw` from `add_namelist_extension` — storing the full raw namelist as JSON in the
  collection may be megabytes for complex setups; drop it unless a later session decides it is needed.
- Keep `nml:files`, `nml:groups`, `nml:parameters`.
- Keep `add_namelist_item_extension` as-is.
- The 10-item list-length cap in `_flatten_for_search` is fine; keep it.

---

### [ ] PR-A1f — Paleo + experiment classification

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add paleo fields and experiment_type. Fields: 21–27.

**Files to create:**
- `src/esm_catalog/paleo.py` — adapted from `stac/extensions/paleo.py` in pr-a1a branch.
- `configs/stac-extensions/paleo/v1.0.0/schema.json` — JSON Schema for the paleo extension.
  After this PR is merged to `release`, update the `"paleo"` URL in `registry.py` to the
  raw GitHub URL.
- `tests/test_esm_catalog/test_stac_paleo.py` — write new tests

**Changes from pr-a1a branch:**
- Remove `if TYPE_CHECKING: pass` (dead code).
- Remove `detect_paleo_simulation` — it is not called anywhere in the STAC layer.
- `_add_experiment_type` currently reads `nml:echam:runctl:dt_start` from item properties
  (a namelist field set by PR-A1e). This creates an ordering dependency. **Fix:** use only
  `start_datetime`/`datetime` from the item properties; the namelist coupling is a fragile
  assumption that `echam` is always the component.
- `paleo:years_bp` should only be added when `experiment_type == "paleo"`, not for every item.

---

### [ ] PR-A1g — Contacts extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add PI contact information to items. Fields: 28, 29, 30.

**Files to create:**
- `src/esm_catalog/contacts.py` — adapted from `stac/extensions/contacts.py` in pr-a1a branch.
- `tests/test_esm_catalog/test_stac_contacts.py` — write new tests

**Note:** contacts uses an external community schema URL (already resolves); no schema file
needed in `configs/stac-extensions/`.

**Open question before implementing:** verify that `general.pi_name` / `general.PI_name` etc. are
actually present in any existing ESM-Tools runscript configs. If not, define the canonical key name
and document it. Check `configs/` and `runscripts/` in the repo.

---

## Deferred — explore in a future session

These metadata fields were not confirmed or dropped; they need investigation before deciding
whether and how to implement them.

| # | Field | Question to answer |
|---|-------|--------------------|
| 2 | `stream` | Is GRIB stream type a useful search dimension? Where does it come from in the scan layer? |
| 4 | `file:size` | Useful for data management queries? Overhead to collect? |
| 6 | `keywords` | Is a `[collection_id]` keyword useful in STAC Browser? Better to rely on `collection` field? |
| 7 | `conventions` | Always "CF-1.x"? Worth indexing? |
| 9 | `file:{key}` all global attrs | Too many arbitrary keys; if needed, define an allowlist of specific attributes |
| 11 | `cf:parameter` | Requires CF convention parsing; is this available from the scan layer? |
