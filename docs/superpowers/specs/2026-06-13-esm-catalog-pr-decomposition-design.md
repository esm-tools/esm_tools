# esm_catalog — PR Decomposition & Delivery Strategy

**Date:** 2026-06-13
**Author:** Pavan Siligam (with Claude)
**Status:** Approved design — proceeding to implementation plan
**Context:** WP1 "Simulation Catalogue" (ESM-Tools+, Helmholtz ScienceServe, Jan–Dec 2026)

---

## 1. Problem

PR [#1473](https://github.com/esm-tools/esm_tools/pull/1473) adds the entire `esm_catalog`
package to `release` in a **single squashed commit: +32,533 / −3 across 110 files**. The
reviewer (Miguel Andrés-Martínez) declined to review it:

> "I don't think I can review 32k lines of code. ESM-Tools is a ~85k line code itself …
> Can you attempt to break it down into much smaller units? … 1. Scans experiments or run
> dirs 2. Constructs shards for the DB 3. Attempt to sync the shard with the global DB."

The development focus to date was **feature exploration** (prove what is possible). The focus
now changes to **delivery**: take the existing, working implementation and reintroduce it as a
sequence of small, meaningful, individually reviewable PRs against `release` — rewriting parts
where that makes the units cleaner.

This same exercise applies to **two further codebases** developed in parallel:
- the **`esm-tools/stac-browser` fork**, which renders catalog features in the browser, and
- **`esm_viz`** (PR [#1474](https://github.com/esm-tools/esm_tools/pull/1474)), a standalone
  visualization microservice — itself another single-squashed-commit dump (+5,675 / −3, 22 files).

So this is a **three-workstream campaign**: `esm_catalog` (API/ingest), `esm_viz` (viz service),
and the `stac-browser` fork (UI).

---

## 2. Facts established (fact-gathering, 2026-06-13)

### Branch topology
| Branch | vs `release` | Role |
|---|---|---|
| `release` | — | Target base. Contains **zero** `esm_catalog` code. |
| `pr-esm-catalog` (PR #1473) | +1 / −325 | The 32k squashed dump, built on a **stale** base (merge-base = PR #1400; release is 325 commits newer). |
| `collapsed-collections` (current) | +137 / −325 | Live development line. History is **tangled** (49 `fix`, 28 `feat`, 15 `docs`), fixes scattered across scopes. |
| `prototype` / `pgierz-workbench` (Paul) / `main` | +62 / +118 / +0 | Earlier lines; `main` already merged. |

**Implication:** slicing clean themed PRs out of the 137-commit history is impractical
(intra-feature fixes are interleaved; intermediate commits may not build). The base is also
stale. → **Rebuild from the current working code onto a fresh branch off today's `release`.**

### Code surface (~15k LOC Python, already modular)
| Module | LOC | Role | WP1 status |
|---|---|---|---|
| `api/` | 6,454 | STAC API server: routes, CQL2, queryables, auth, registry, pool, cache | ⭐ core (D2) |
| `scan/` | 2,630 | Scanners: netcdf, grib, echam, namelist, UPath remote | ⭐ core (M1) |
| `storage/` | 1,635 | DuckDB, personal, export | ⭐ core |
| `mcp/` | 1,088 | LLM/MCP layer | **dropped — see §4** |
| `stac/` | 1,048 | Collection, Item, extensions | ⭐ core |
| `tests/` | 946 | — | — |
| `cli.py` | 846 | scan / serve / … entrypoint | ⭐ core |
| `integration/` + `hpc/` | 608 | esm-tools tidy-phase hook, HPC state | ⭐ core |

### Internal dependency graph (drives PR ordering)
```
hpc            (leaf, no internal deps)
 ▲
stac ⇄ scan    (MUTUAL import — a dependency cycle to break)
 ▲      ▲
storage │      (storage → stac)
 ▲      │
integration    (→ scan + stac + storage)   ← Miguel's ingest pipeline
api            (→ storage only)             ← serve layer, cleanly decoupled
mcp            (no internal deps; HTTP client of the running API) ← dropped
cli            (→ everything; grows incrementally per PR)
```
Key findings:
1. **`scan` ⇄ `stac` is a cycle** — must be broken when those modules land.
2. **`api` depends only on `storage`** — the serve layer is genuinely independent.
3. `api/` at 6.4k LOC is too large for one PR — it sub-splits along its existing files.

### Out-of-package payload in PR #1473
A docker CI workflow, 3 `docs/*.rst`, and a large `examples/` tree — including a **17 MB binary
`echam_sample.grb`** that must **not** land in a release PR (permanent repo bloat). Entry point
is a `esm-catalog` console script (`esm_catalog.cli:main`) in `setup.py`.

### Second repo: `esm-tools/stac-browser`
A **fork** of `radiantearth/stac-browser` (forked 2026-03-04). Current branch
`feat/data-preview-component`. Custom work lives entirely on the feature branch — the fork's `main`
is an unmodified upstream snapshot (69 commits behind radiantearth main as of 2026-06-14). Custom
work: comparison grid (AG Grid), NML/quick filters, personal-collections UI, code-snippets panel,
collapsed-collection cards, viz panel. **No `upstream` remote is wired locally**, so the full
delta count vs current upstream stock is not yet confirmed — but the nature of the changes is now
understood (see §6 note on CQL2).

### Pre-existing triage artifact
`src/esm_catalog/esm-tools-plus-simcat-features-checklist.md` already rates every feature
⭐/💡/❓ by priority and 🟢/🟡/🔴 by maintenance tier, and records the team's **2026-04-24
decision: "Option C — maintain the custom browser fork with all features."**

---

## 3. Strategy decisions (locked)

| # | Decision | Choice |
|---|---|---|
| D1 | **Scope** | Everything stays in scope, **sequenced** (core first, optional layers last) — *except* MCP (D4). |
| D2 | **Build mechanism** | **Rebuild from end-state** on a fresh branch off today's `release`. Squash away fix-on-fix churn; the working code is the source of truth. |
| D3 | **Opening structure** | The first PRs mirror **Miguel's three units**: scan → construct shards → sync to global DB. |
| D4 | **MCP/LLM layer** | **Dropped from the stack** (not merely deferred). Archived on a tag. Rationale in §4. |
| D5 | **Repo sequencing** | **esm_catalog API stack first.** Then `esm_viz` and the `stac-browser` fork run as parallel tracks once the API core is up; the browser viz-panel PR lands only after `esm_viz` exists. |
| D6 | **Agent orientation file** | Add a thin `AGENTS.md` (~30–50 lines) at repo root as a dev-experience aid; a product-facing `llms.txt` is deferred to deliverable D5 (tutorial, month 11). |

---

## 4. Why MCP is dropped (not deferred)

- **No deliverable backing.** None of D1–D5 / D13 mention an LLM/MCP layer; it is 1,088 LOC of
  permanent maintenance with no funded obligation.
- **Redundant with a standard interface.** The STAC API is HTTP/OGC-compliant with an OpenAPI
  spec. Natural-language access is recoverable for free later by pointing any LLM at that spec
  or a generic off-the-shelf STAC-MCP server — without hand-maintaining a shim.
- **Security liability.** The `run_python` / `open_and_run` tools execute arbitrary Python on the
  data/HPC host — an unacceptable attack surface for a Helmholtz-federated service with tiered
  public access (WP1 §7).
- **Reviewer + maintenance cost.** One more subsystem for Miguel to learn and the team to keep
  alive, for value already covered elsewhere (viz via the browser panel; querying via the API).

**Action:** archive existing `mcp/` on tag/branch `archive/mcp-llm` before removal so nothing is
lost; revisit only on a concrete user request.

---

## 5. The PR stack — `esm_catalog` repo

Each PR branches from current `release`, **builds and passes tests standalone**, and adds its
CLI subcommand incrementally. Phases land in order; within a phase, order is by dependency.

### Phase 0 — landing zone
- **PR-0 · scaffold** — package skeleton, `setup.py` entry point (`esm-catalog` stub), `.gitignore`
  additions, docker CI workflow, docs index stub. Trivial first merge; establishes where
  everything lands. *(May fold into PR-A1 if reviewers prefer.)*

### Phase A — Ingest *(Miguel's three units)*
PR-A1 is **pre-split into two** so no single PR forces the reviewer through ~3.9k LOC at once.
The split point is the cycle-break: `stac` is made to **not** import `scan`, so the STAC model
becomes a clean foundation that `scan` then builds on (one-directional `scan → stac`).
- **PR-A1a · STAC model foundation** — `stac/` (collection, item, extensions) + `hpc/` (leaf).
  Defines the data model + HPC state. **Breaks the `scan`⇄`stac` cycle** by removing `stac`'s
  dependency on `scan`. No scanning yet. (~1.3k LOC)
- **PR-A1b · scanners** → *"scans run dirs"* — `scan/` (netcdf, grib, echam, namelist, detect,
  upath, context), depending one-way on the PR-A1a model. CLI `esm-catalog scan` yields in-memory
  STAC. (~2.6k LOC; may split again into `scan-core+netcdf` / `grib+echam+remote` if Miguel still
  finds it heavy.)
- **PR-A2 · shard** → *"constructs DB shards"* — `storage/duckdb`; persist STAC items to a
  per-experiment `.duckdb`. (~1.6k LOC)
- **PR-A3 · sync** → *"syncs shard to global DB"* — registry/federation; register many `.duckdb`
  shards as one catalog.
- **PR-A4 · integration** — esm-tools tidy-phase `add_files()` hook (`integration/`) + CLI wiring.
  (~0.4k LOC)

### Phase B — Serve (STAC API; depends only on `storage`)
- **PR-B1 · serve core** — `api/app` + collections/items routes + pool/helpers; minimal STAC API.
- **PR-B2 · search** — CQL2, queryables, filters, validation.
- **PR-B3 · science routes** — experiment hierarchy, paleo presets, HPC-state surfacing,
  namelist/datacube extensions.
- **PR-B4 · federation API** — hot-reload register/refresh endpoints + cache.

### Phase C — Optional layers (last, clearly labeled)
- **PR-C1 · auth + public metadata tier** — `api/auth`, middleware, read-only public filtering
  (WP1 §7).
- **PR-C2 · personal collections** — `storage/personal` + personal routes.
- **PR-C3 · docs + examples** — the `.rst` docs and `examples/` tree, with the **17 MB binary
  `.grb` replaced** by a small fixture or a download script.
- ~~MCP / LLM layer~~ — **dropped** (see §4).

> The `api/` package (6.4k LOC) is intentionally split across B1–B4 rather than shipped whole.

---

## 6. The PR stack — `stac-browser` fork (after API core lands)

1. **Wire an `upstream` remote** (`radiantearth/stac-browser`) and compute the true fork delta —
   prerequisite, not currently possible locally. Fork's `main` is 69 commits behind upstream main.
2. Themed PRs (rebuilt from end-state, same philosophy): collapsed-collection cards →
   quick-filter / NML UI → comparison grid → personal-collections UI → code-snippets panel →
   viz panel.
3. ~~**CQL2 NOT-operator fix** (checklist 8.1) → submit as a **candidate upstream PR** to
   `radiantearth/stac-browser` rather than carrying it in the fork indefinitely.~~
   **MOOT** — investigated 2026-06-14. Commit `e1063f90` ("Fully support not operator") was
   authored by Matthias Mohr and merged upstream via `radiantearth/stac-browser` PR #816 on
   2026-02-26, six days **before** the fork was created. The fork inherited it automatically; there
   is nothing to upstream.

   The actual esm-tools-specific CQL fix is `65759b1` ("fix: Cql property name typo when combining
   manual and quick filters") in `SearchFilter.vue` — a one-char typo (`filters.filter` →
   `filters.filters`) that caused a crash when a manual CQL expression and quick filters were both
   active simultaneously. **This fix is not an upstream candidate**: it lives entirely inside
   `buildQuickFilters()`, a function that doesn't exist in upstream at all. Upstream has no quick
   filters concept, so the code path can never be reached there.

---

## 7. The PR stack — `esm_viz` service (parallel track, after API core)

`esm_viz` is **architecturally independent** of `esm_catalog` — it imports none of it and talks to
the catalog/data over HTTP. It is *nice-to-have* (checklist §4). With MCP dropped, its only
consumer is the **browser viz panel**, so it must land before that panel's browser PR but is
otherwise off the critical path. Same rebuild-from-end-state, anti-dump treatment:

- **PR-V1 · service skeleton + render core** — FastAPI service, item→PNG rendering for regular
  grids, deploy stub.
- **PR-V2 · FESOM unstructured-mesh rendering** — tripcolor + cartopy path (checklist 4.2).
- **PR-V3 · land-sea mask application** (checklist 4.3).
- **PR-V4 · deploy + CI** — docker-compose, `hpc-deploy.sh`, CI workflow.

## 8. Workstream ordering (the three tracks together)

```
esm_catalog: Phase A (ingest) ─► Phase B (serve/API core) ─► Phase C (auth, personal, docs)
                                        │
                                        ├─►  esm_viz:        PR-V1 ─► V2 ─► V3 ─► V4   (parallel)
                                        │
                                        └─►  stac-browser:   core UI PRs ... ─► viz-panel PR
                                                                                   ▲
                                                          (browser viz-panel needs esm_viz PR-V1+) ┘
```
- **Critical path:** `esm_catalog` Phase A → Phase B. Everything else hangs off the API core.
- `esm_viz` and the `stac-browser` core UI PRs proceed in parallel once Phase B core (PR-B1) is up.
- The browser **viz-panel** PR is gated on `esm_viz` PR-V1. Checklist 8.1 (CQL2 NOT-operator
  upstream PR) is moot — see §6 note.

## 9. Risks & cleanups carried by this plan

| Risk | Mitigation | Where |
|---|---|---|
| `scan` ⇄ `stac` import cycle | Break it (one-way `scan → stac`) at the A1a/A1b split point | PR-A1a |
| 17 MB binary `echam_sample.grb` bloats release | Replace with small fixture / downloader | PR-C3 |
| `api/` too large to review as one unit | Pre-split into B1–B4 | Phase B |
| Stale base (325 commits behind) | Every PR rebuilds on current `release` | All |
| Browser fork delta unknown | Wire `upstream` remote before browser stack; fork main is 69 commits behind upstream as of 2026-06-14 | Repo 2, step 1 |
| Existing docs too long to be token-efficient | Thin `AGENTS.md` index instead of more prose | D6 |

---

## 10. Out of scope (explicitly)

- Untangling or cherry-picking the 137-commit history (superseded by rebuild-from-end-state, D2).
- The MCP/LLM subsystem (dropped, D4).
- A product-facing `llms.txt` (deferred to deliverable D5, month 11).
- Helmholtz ID / HIFIS authentication integration (WP1 open question; tracked separately).

---

## 11. Next step

Proceed to a detailed **implementation plan** (per-PR: exact files, the cycle-break in PR-A1,
test strategy, build order, and the rebase-onto-`release` mechanics), via the writing-plans skill.
