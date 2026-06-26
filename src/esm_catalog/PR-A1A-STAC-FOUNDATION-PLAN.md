# ESM Catalog STAC — Development Plan

This file tracks the plan for breaking the `esm-catalog/pr-a1a-stac-foundation` branch
into a series of small, reviewable PRs. Each PR branches from `release`.

Future Claude sessions: read this file first, implement the next unchecked PR,
then tick its checkbox and update the status.

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

### [ ] PR-A1b — Core infrastructure

**Branch from:** `release`
**Goal:** Minimal working STAC Item + Collection with only the confirmed core fields.
No extension calls.

**Files to create:**
- `src/esm_catalog/context.py` — `CollectionContext` dataclass (fields: `experiment_id`, `component`, `collection_id`, `experiment_path`, `namelists_by_component`)
- `src/esm_catalog/uri.py` — `parse_uri`, `to_uri`, `_has_protocol`; keep lazy UPath import but drop the verbose install-hint error message
- `src/esm_catalog/stac/__init__.py`
- `src/esm_catalog/stac/extensions/__init__.py`
- `src/esm_catalog/stac/extensions/registry.py` — `EXTENSION_URLS` dict (keep all URLs; unused ones cost nothing)
- `src/esm_catalog/stac/collection.py` — `make_collection`, `update_collection_extent`; **no** `add_namelist_extension` call
- `src/esm_catalog/stac/item.py` — `make_item` with core fields only (1, 3, 5, 8, 10); **no** extension calls, **no** `_add_experiment_type`

**Item properties in this PR:**
```
variable, datetime/start_datetime/end_datetime, format, output_frequency, variables (list)
```

**Tests to create:**
- `tests/test_esm_catalog/test_context.py`
- `tests/test_esm_catalog/test_uri.py`
- `tests/test_esm_catalog/test_stac_collection.py`
- `tests/test_esm_catalog/test_stac_item.py`
- `tests/test_esm_catalog/test_no_scan_dependency.py`

**Source to copy from `esm-catalog/pr-a1a-stac-foundation`:** use the existing files as
the starting point but strip everything that belongs to a later PR.

---

### [ ] PR-A1c — HPC storage extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add HPC storage metadata to items. Fields: 14, 15, 16, 17, 18.
Fields 19 and 20 (HSM subprocess queries) are permanently dropped.

**Files to create:**
- `src/esm_catalog/hpc/__init__.py`
- `src/esm_catalog/hpc/detect.py` — **rewrite required**
- `src/esm_catalog/stac/extensions/hpc.py` — simplified (no `get_hsm_state` call)

**Key problem with the existing `detect.py` — must be fixed:**
The current code hardcodes machine names as Python path patterns (`/albedo/` → AWI, `/work/` → DKRZ).
This is machine-specific and fragile. The right approach:
- Machine storage info (facility, system, storage_type) belongs in ESM-Tools machine YAML configs.
- `detect.py` should read from the parsed ESM-Tools config/machine dict, not probe the filesystem.
- Drop the `ctypes`/`statfs(2)` approach entirely — it is Linux-only and uses raw C struct layouts.
- Keep `hpc:last_access` from `path.stat().st_atime` (simple, no subprocess).

**Files to delete (vs pr-a1a branch):**
- `src/esm_catalog/hpc/state.py` — entire file; it only served fields 19 and 20.

**Signature change for `add_hpc_extension`:**
```python
# Before (current branch):
add_hpc_extension(item, path)  # detects storage by probing path

# After:
add_hpc_extension(item, path, machine_config: dict | None = None)
# machine_config is the parsed ESM-Tools machine section;
# if None, only hpc:last_access is populated (graceful degradation).
```

---

### [ ] PR-A1d — Datacube extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add cube:dimensions and cube:variables to items. Fields: 12, 13.

**Files to create:**
- `src/esm_catalog/stac/extensions/datacube.py` — copy from pr-a1a branch; no changes needed
- `tests/test_esm_catalog/test_stac_datacube.py` — write new tests

---

### [ ] PR-A1e — Namelist extension

**Branch from:** `release` (after PR-A1b is merged)
**Goal:** Add namelist parameters to items and collections. Field: 31.

**Files to create:**
- `src/esm_catalog/stac/extensions/namelist.py`
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
- `src/esm_catalog/stac/extensions/paleo.py`
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
- `src/esm_catalog/stac/extensions/contacts.py` — copy from pr-a1a; check that `pi_name`/`PI_name`
  dual-lookup is consistent with how ESM-Tools configs actually store PI information.
- `tests/test_esm_catalog/test_stac_contacts.py` — write new tests

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
