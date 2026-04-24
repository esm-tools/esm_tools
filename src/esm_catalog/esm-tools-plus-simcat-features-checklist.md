# ESM STAC Catalog – Features Checklist for Discussion

**Purpose**: Aid the team in deciding which features belong in the final product given limited maintenance funding.
The central trade-off is: **stock STAC Browser** (zero browser maintenance) vs. **custom STAC Browser fork** (capabilities + overhead).

---

## Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Implemented |
| 🚧 | Partially implemented |
| 📋 | Planned / proposed |
| ⭐ | Must-have (core mission) |
| 💡 | Nice-to-have |
| ❓ | Optional / undecided |
| 🟢 | Works with **stock** STAC Browser |
| 🟡 | Needs **custom API** only (no browser changes) |
| 🔴 | Needs **custom Browser fork** (highest maintenance) |

---

## Section 1 · Core Cataloging (Backend)

These features are entirely server-side. They are independent of which browser is used and represent the foundational layer everything else rests on.

|    # | Feature                            | Status | Priority | Maintenance tier | Notes                                                              |
|------+------------------------------------+--------+----------+------------------+--------------------------------------------------------------------|
|  1.1 | **Scan NetCDF files**              | ✅     | ⭐       | 🟢               | xarray-based, CF convention support                                |
|  1.2 | **Scan GRIB files**                | ✅     | ⭐       | 🟢               | cfgrib + ECHAM-specific layout                                     |
|  1.3 | **Extension-less file detection**  | ✅     | ⭐       | 🟢               | Magic byte detection for GRIB without `.grb` suffix                |
|  1.4 | **Remote filesystem scanning**     | ✅     | ⭐       | 🟢               | SSH, ScoutFS (HSM/tape), S3 via UPath                              |
|  1.5 | **Parallel scanning (multi-core)** | ✅     | ⭐       | 🟢               | ProcessPoolExecutor or dask.distributed                            |
|  1.6 | **Batch scanning for SLURM**       | ✅     | ⭐       | 🟡               | Array jobs → Parquet staging → serial merge                        |
|  1.7 | **DuckDB storage**                 | ✅     | ⭐       | 🟢               | Per-experiment `.duckdb`; fast queries; single-file portability    |
|  1.8 | **ESM-Tools live integration**     | ✅     | ⭐       | 🟡               | `add_files()` called from tidy phase; reads `finished_config.yaml` |
|  1.9 | **Catalog federation**             | ✅     | ⭐       | 🟢               | Multiple `.duckdb` files served as one unified API                 |
| 1.10 | **Hot-reload / register-refresh**  | ✅     | 💡       | 🟡               | Add/update catalogs without restarting the server                  |

**Interdependency**: 1.6 depends on 1.7. All of Section 2–5 depends on 1.7 and 1.9.

---

## Section 2 · STAC API (Standard)

Standard STAC-compliant endpoints. All work with the stock STAC Browser out of the box.

|   # | Feature                       | Status | Priority | Maintenance tier | Notes                                                                           |
|-----+-------------------------------+--------+----------+------------------+---------------------------------------------------------------------------------|
| 2.1 | **Standard STAC item search** | ✅     | ⭐       | 🟢               | `/search` GET/POST; bbox, datetime, collection filters                          |
| 2.2 | **CQL2 item filtering**       | ✅     | ⭐       | 🟢               | `variable='ssh'`, `datetime >= '...'`; stock browser "Additional Filters" panel |
| 2.3 | **Queryables (item)**         | ✅     | ⭐       | 🟢               | JSON Schema with enum lists; drives browser filter dropdowns                    |
| 2.4 | **Collection browsing**       | ✅     | ⭐       | 🟢               | `/collections`; stock browser renders these natively                            |
| 2.5 | **CQL2 collection filtering** | ✅     | ⭐       | 🟢               | Filter collections by variable, experiment, etc.                                |
| 2.6 | **Queryables (collection)**   | ✅     | ⭐       | 🟢               | Drives "Search for Collections" filter panel in browser                         |
| 2.7 | **Health / readiness probes** | ✅     | ⭐       | 🟢               | Kubernetes/systemd liveness checks                                              |

---

## Section 3 · Science-Specific Search

Features beyond standard STAC; driven by AWI/ESM-specific metadata.

|   # | Feature                              | Status | Priority | Maintenance tier | Notes                                                                                                                                                                                           |
|-----+--------------------------------------+--------+----------+------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 3.1 | **Namelist (NML) parameter search**  | ✅     | ⭐       | 🔴               | `nml:run_config.use_ice = true`; API works stock; **browser UI requires custom fork** for "Additional Filters" to expose nested NML fields. Paul's CSV: user-requested, needs custom extension. |
| 3.2 | **Paleo time search**                | ✅     | ⭐       | 🟡               | `paleo:year`, `paleo:age_ma`; queryable via API; stock browser shows it as a numeric filter. Paleo preset UI (LGM, Eemian, Mid-Holocene) is custom API only.                                    |
| 3.3 | **Experiment hierarchy**             | ✅     | 💡       | 🟡               | `/experiments` endpoint shows parent-experiment → child-collections tree. Stock browser does not render this hierarchy; users must navigate by collection.                                      |
| 3.4 | **HPC storage state**                | ✅     | ⭐       | 🟡               | `hpc:state` (online/nearline/offline), recall time. No browser customization needed; displayed in standard item properties panel.                                                               |
| 3.5 | **FESOM unstructured grid metadata** | ✅     | ⭐       | 🟢               | Scanned correctly; `cube:dimensions` marks unstructured axes. No browser change; impacts viz and LLM tools.                                                                                     |
| 3.6 | **Compare run configurations**       | ✅     | 💡       | 🟡               | MCP tool: side-by-side diff of NML params across experiments. No browser needed (LLM-accessible only).                                                                                          |

**Interdependency**: 3.1 requires 1.8 (NML params come from `finished_config.yaml`). 3.2 requires 1.8. 3.5 is prerequisite for Section 4 visualization and Section 5 MCP preview.

---

## Section 4 · Visualization

|   # | Feature                                   | Status | Priority | Maintenance tier | Notes                                                                                                                                                                                                       |
|-----+-------------------------------------------+--------+----------+------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 4.1 | **ESM Visualization Service integration** | ✅     | 💡       | 🔴               | Separate `viz-server` renders spatial PNGs. Paul's CSV: "nice to have"; can be disabled if viz-server absent. **Browser panel is custom fork**; without it, vis-server can still be used via MCP (see 5.3). |
| 4.2 | **FESOM triangular mesh rendering**       | ✅     | 💡       | 🟡               | Backend capability (tripcolor + cartopy); surfaced via MCP `preview_item`. No browser change needed.                                                                                                        |
| 4.3 | **Land-sea mask application**             | ✅     | 💡       | 🟡               | Applied automatically in vis-server. Backend only.                                                                                                                                                          |

**Key decision**: If the custom browser is dropped, visualization moves entirely to the MCP/LLM path (feature 5.3) and direct Python scripting. Users lose the click-to-visualize experience in the browser.

---

## Section 5 · LLM / MCP Access

No browser involvement. These features are orthogonal to the browser decision.

|   # | Feature                        | Status | Priority | Maintenance tier | Notes                                              |
|-----+--------------------------------+--------+----------+------------------+----------------------------------------------------|
| 5.1 | **`list_collections` tool**    | ✅     | ⭐       | 🟡               | LLM: lists all experiment collections              |
| 5.2 | **`get_collection_info` tool** | ✅     | ⭐       | 🟡               | LLM: full metadata, variables, NML params          |
| 5.3 | **`search_collections` tool**  | ✅     | ⭐       | 🟡               | LLM: filter by NML expression or variable          |
| 5.4 | **`search_items` tool**        | ✅     | ⭐       | 🟡               | LLM: find files by collection + variable + date    |
| 5.5 | **`preview_item` tool**        | ✅     | 💡       | 🟡               | LLM: generates a spatial PNG via vis-server        |
| 5.6 | **`open_and_run` tool**        | ✅     | 💡       | 🟡               | LLM: finds files and executes user Python code     |
| 5.7 | **`run_python` tool**          | ✅     | 💡       | 🟡               | LLM: general Python execution (xarray, matplotlib) |
| 5.8 | **`compare_collections` tool** | ✅     | 💡       | 🟡               | LLM: side-by-side NML diff                         |
| 5.9 | **Multiple MCP transports**    | ✅     | 💡       | 🟡               | stdio, streamable-HTTP (Open WebUI), SSE, OpenAPI  |

**Note**: The entire MCP/LLM layer is custom code but has **zero browser dependency**. It is a viable alternative path to features 3.1, 3.6, and 4.1 even if the browser is kept stock.

---

## Section 6 · Collaboration & Personal Collections

|   # | Feature                               | Status | Priority | Maintenance tier | Notes                                                                                               |
|-----+---------------------------------------+--------+----------+------------------+-----------------------------------------------------------------------------------------------------|
| 6.1 | **Personal collections (backend)**    | ✅     | ❓       | 🟡               | `/users/{user}/collections`; separate `personal.duckdb`; CRUD + sharing API                         |
| 6.2 | **Personal collections (browser UI)** | 📋     | ❓       | 🔴               | Requires custom browser to expose create/label/share controls                                       |
| 6.3 | **Run annotations / quality flags**   | 📋     | 💡       | 🔴               | e.g. "ocean crashed, don't use this run"; not implemented; would need custom browser                |
| 6.4 | **Python code snippets panel**        | ✅     | 💡       | 🔴               | Browser generates xarray/matplotlib code from search results. Custom `PythonCodeBox.vue` component. |

---

## Section 7 · Access Control & Public Exposure

Relevant to the AWI → Helmholtz metadata-only public access requirement.

|   # | Feature                            | Status | Priority | Maintenance tier | Notes                                                                                                                            |
|-----+------------------------------------+--------+----------+------------------+----------------------------------------------------------------------------------------------------------------------------------|
| 7.1 | **JupyterHub authentication**      | ✅     | ⭐       | 🟡               | Group-based; write access restricted to configured groups                                                                        |
| 7.2 | **Read-only public metadata tier** | 📋     | ⭐       | 🟡               | AWI requirement: expose metadata only (not asset URLs / file paths) to Helmholtz. API-level filtering; no browser change needed. |
| 7.3 | **VPN-only internal access**       | 📋     | ⭐       | 🟡               | Deployment/network concern, not a software feature per se                                                                        |

---

## Section 8 · Browser Fork–Specific Fixes

Changes made exclusively in the custom STAC Browser fork. These are **lost** if the team switches to stock.

|   # | Feature                            | Status | Priority | Maintenance tier | Notes                                                                                                                                                        |
|-----+------------------------------------+--------+----------+------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 8.1 | **CQL2 NOT operator fix**          | ✅     | ⭐       | 🔴               | Stock browser's `CqlNot.toText()` is broken; NOT queries silently drop the operator. **This is a bug in the upstream browser.** Could be upstreamed as a PR. |
| 8.2 | **Collection badge on item cards** | ✅     | 💡       | 🔴               | Shows collection ID on each item card in cross-collection results. Minor UX improvement.                                                                     |
| 8.3 | **Python code snippets panel**     | ✅     | 💡       | 🔴               | See 6.4. Unique to the fork.                                                                                                                                 |
| 8.4 | **Visualization panel**            | ✅     | 💡       | 🔴               | See 4.1. Unique to the fork.                                                                                                                                 |

**Note on 8.1**: If the team switches to stock browser but still needs NOT queries, the fix could be submitted as a PR to the upstream STAC Browser repo. This avoids maintaining a fork while recovering the feature.

---

## Summary: What You Lose with Stock STAC Browser

| Feature lost                  | Workaround available?                                             |
|-------------------------------+-------------------------------------------------------------------|
| NML search UI (3.1)           | Yes – MCP tool `search_collections` (5.3) or direct CQL2 API call |
| Paleo preset UI               | Partial – paleo numeric filter still works in stock browser       |
| Visualization panel (4.1)     | Yes – MCP `preview_item` (5.5) covers this path                   |
| Python code snippets (6.4)    | Yes – users write Python manually; MCP `open_and_run` (5.6) helps |
| Collection badges (8.2)       | No direct workaround; minor UX impact                             |
| CQL2 NOT queries (8.1)        | Submit upstream PR to STAC Browser                                |
| Personal collections UI (6.2) | No UI; backend API exists but no browser to drive it              |
| Run annotations (6.3)         | Not implemented yet; no loss                                      |

---

## Feature Interdependency Map

```
finished_config.yaml
  └─► NML params (3.1) ──────────────────────► MCP search_collections (5.3)
  └─► Paleo time (3.2)
  └─► Experiment context (1.8)

DuckDB (1.7)
  └─► STAC API (Section 2)
  └─► Federation (1.9)
  └─► Personal collections backend (6.1)

STAC API (Section 2)
  └─► MCP tools (Section 5)  ← no browser needed
  └─► Stock STAC Browser     ← works as-is
  └─► Custom Browser fork    ← adds features in 3.1, 4.1, 6.2–6.4, 8.x

vis-server (external)
  └─► Browser viz panel (4.1) ← only with custom browser
  └─► MCP preview_item (5.5) ← works independently of browser
```

---

## Decision Table

| Scenario                                       | Browser effort                   | Features available                                                                          |
|------------------------------------------------+----------------------------------+---------------------------------------------------------------------------------------------|
| **A: Stock browser only**                      | Zero                             | Sections 1–2–5–7 fully; 3 (API only, no NML UI); lose Sections 4 browser viz, 6 UI, 8 fixes |
| **B: Stock browser + upstream PR for NOT fix** | Minimal                          | Same as A plus CQL2 NOT queries in browser                                                  |
| **C: Custom browser fork maintained** ✅ **DECIDED** | High (JS/Vue, upstream tracking) | All implemented features                                                               |
| **D: No browser at all (API + MCP only)**      | Zero                             | Everything except browser-based exploration; LLM path covers most discovery and viz         |

> **Team decision (2026-04-24)**: Option C selected. NML search UI, CQL2 NOT fix, visualization panel, and Python code snippets panel are all deemed necessary. Custom browser fork will be maintained as part of WP SimCat scope.

---

## Open Questions — Resolved / Remaining

- [x] ~~Is maintaining a JS/Vue browser fork sustainable?~~ → **Yes, decided 2026-04-24**
- [x] ~~Is the CQL2 NOT fix worth upstreaming?~~ → **Moot; fork is maintained**
- [x] ~~Is the visualization panel a must-have?~~ → **Yes, deemed necessary**
- [ ] Does the Helmholtz public metadata tier (7.2) require any browser changes, or is it purely API-side?
- [ ] Multi-site: is metadata-only cross-site serving sufficient, or is data access needed too?
- [ ] Data locality: is a lightweight `update-state` sweep command worth implementing, or is periodic re-scan sufficient?
- [ ] Data locality: for hard archive moves (Case B) — is this common at AWI/DKRZ, or do HSM setups keep paths stable?
