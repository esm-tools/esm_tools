# ESM-Catalog Architecture

## Overview

ESM-Catalog is a STAC-based (SpatioTemporal Asset Catalog) system for discovering and managing
climate model output from ESM-Tools experiments. It scans HPC output files (NetCDF, GRIB2),
builds standardized STAC metadata, stores it in DuckDB, and exposes it via a STAC-compliant
FastAPI server with custom extensions.

---

## Directory Structure

```
src/esm_catalog/
├── api/                    # FastAPI STAC server
│   ├── app.py              # Application factory + middleware
│   ├── auth.py             # Pluggable auth (JupyterHub, custom)
│   ├── cache.py            # QueryablesCache, CollectionCache
│   ├── catalog_routes.py   # /catalogs REST endpoints
│   ├── client.py           # DuckDB-backed stac-fastapi CoreClient
│   ├── cql2.py             # CQL2 filter parsing
│   ├── helpers.py          # Utility functions
│   ├── interfaces.py       # Protocol-based abstractions
│   ├── middleware.py       # CORS etc.
│   ├── paleo_presets.py    # Paleoclimate filter presets
│   ├── personal_models.py  # Pydantic models for personal collections
│   ├── personal_routes.py  # /users/{user}/collections REST endpoints
│   ├── pool.py             # Connection pool for multiple DuckDB catalogs
│   ├── queryables.py       # STAC queryables endpoint
│   ├── registry.py         # Dynamic catalog registry with persistence
│   ├── responses.py        # FastAPI response models
│   ├── validation.py       # Request validation
│   └── ui/                 # Admin web UI (served at /ui)
│       └── index.html      # Single-file vanilla JS admin page
├── stac/                   # STAC object builders
│   ├── collection.py       # Build STAC Collection dicts
│   ├── item.py             # Build STAC Item dicts
│   └── extensions/         # Custom STAC extension implementations
│       ├── registry.py     # Extension URL registry
│       ├── contacts.py     # Author/contributor info extension
│       ├── datacube.py     # OGC DataCube extension
│       ├── hpc.py          # HPC storage tier extension
│       ├── namelist.py     # Fortran namelist extension
│       └── paleo.py        # Paleoclimate geological time extension
├── storage/                # Storage backends
│   ├── duckdb.py           # DuckDB catalog (main storage engine)
│   ├── export.py           # Parquet export/import for batch mode
│   └── personal.py         # SQLite-backed personal collections
├── scan/                   # File format detection and metadata extraction
│   ├── context.py          # Resolve file → (experiment, component, collection)
│   ├── detect.py           # Format dispatch (NetCDF, GRIB, ECHAM)
│   ├── echam.py            # ECHAM GRIB scanner
│   ├── grib.py             # Generic GRIB2 scanner
│   ├── namelist.py         # f90nml namelist parser
│   ├── netcdf.py           # NetCDF/HDF5 scanner
│   └── upath.py            # Remote filesystem abstraction (fsspec/UPath)
├── hpc/                    # HPC infrastructure detection
│   ├── detect.py           # Storage tier detection (Lustre, HPSS, etc.)
│   └── state.py            # HSM tape state queries
├── integration/            # ESM-Tools ecosystem integration
│   ├── config.py           # finished_config.yaml / tidy log parsing
│   └── esm_tools.py        # Public API: add_files(), add_run()
├── cli.py                  # Click CLI: scan, serve, register, refresh, ...
├── __init__.py             # Package root, exports add_files
├── Dockerfile              # Multi-stage Docker build (port 23000)
└── environment.yml         # Conda dependencies
```

---

## Core Concepts

### STAC Hierarchy

```
Catalog (DuckDB file, one per experiment)
└── Collection  →  "{experiment}-{component}"  (e.g. "basic-001-echam")
    └── Item    →  one file or group of files
        └── Asset → href to actual data file on disk
```

Each experiment gets its own `catalog.duckdb` file. The API federates across multiple catalogs
transparently at query time.

### Custom STAC Extensions

| Extension   | Prefix    | What it adds |
|-------------|-----------|--------------|
| `hpc`       | `hpc:`    | Storage tier (Lustre/HPSS/tape), HSM state, recall estimate |
| `paleo`     | `paleo:`  | Geological time (Ma / ka display, reference year) |
| `namelist`  | `namelist:` | Embedded Fortran namelist parameters from experiment config |
| `datacube`  | `cube:`   | OGC DataCube dimensions and variables |
| `contacts`  | —         | Author/contributor info |
| `file`      | `file:`   | Size, checksum, format conventions |

---

## Data Flow

### Scan → Store

```
File on disk (NetCDF / GRIB2 / HDF5)
        │
        ▼
scan/detect.py::scan_file(path)
  ├─ netcdf.py → variables, dimensions, bbox, time range, global attrs
  ├─ grib.py   → stream type, parameter list, levels, time range
  └─ echam.py  → ECHAM-specific GRIB with .codes companion
        │
        ▼
scan/context.py::resolve_context(path, config)
  ├─ From finished_config.yaml  (preferred)
  └─ From path pattern  .../experiments/{exp}/outdata/{comp}/...
     → CollectionContext(experiment_id, component, collection_id)
        │
        ▼
stac/item.py::make_item(path, metadata, ctx, config)
  └─ Applies all extensions (hpc, paleo, datacube, contacts, file)
        │
        ▼
storage/duckdb.py::CatalogDB.insert_item(item)
  └─ items table (JSON) + collection_item_props (queryable index)
```

### Batch / SLURM Workflow

```
sbatch array job: scan-batch → per-worker Parquet files
  └─ merge-parquet → single catalog.duckdb
```

### API Query Flow

```
Client (STAC Browser / Python / curl)
        │
        ▼
FastAPI (stac-fastapi)
  DuckDBCatalogClient.item_collection(filter, bbox, datetime)
    └─ For each catalog in CatalogRegistry:
         CatalogPool.get(path) → CatalogDB
         db.search_items(filter_props, limit, offset)
    └─ Merge results → paginated FeatureCollection
```

---

## API Endpoints

### Standard STAC Endpoints (via stac-fastapi)

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/` | Landing page |
| GET | `/conformance` | OGC conformance classes |
| GET | `/collections` | List all collections |
| GET | `/collections/{id}` | Single collection |
| GET | `/collections/{id}/items` | Items in collection |
| GET | `/collections/{id}/items/{item_id}` | Single item |
| GET/POST | `/search` | CQL2-filtered search |
| GET | `/queryables` | Searchable properties |
| GET | `/collections/{id}/queryables` | Collection-level queryables |

### Custom ESM-Catalog Endpoints

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/catalogs` | List registered catalogs |
| POST | `/catalogs` | Register new catalog |
| GET | `/catalogs/{id}` | Catalog info |
| PATCH | `/catalogs/{id}` | Update catalog metadata |
| POST | `/catalogs/{id}/refresh` | Reconnect to updated DuckDB |
| DELETE | `/catalogs/{id}` | Unregister catalog |
| GET | `/health` | Health check |
| GET, HEAD | `/admin` | Redirect → `/ui` |
| GET | `/ui` | Admin web UI (register/list/delete catalogs) |

### Personal Collections Endpoints

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/users/{user}/collections` | List user's collections |
| POST | `/users/{user}/collections` | Create personal collection |
| GET | `/users/{user}/collections/{id}` | Collection detail |
| PATCH | `/users/{user}/collections/{id}` | Update collection |
| POST | `/users/{user}/collections/{id}/items` | Add items |
| DELETE | `/users/{user}/collections/{id}/items/{item_id}` | Remove item |
| POST | `/users/{user}/collections/{id}/share` | Share with user |
| PATCH/DELETE | `/users/{user}/collections/{id}/share/{user}` | Update/revoke share |

---

## Storage Schema (DuckDB)

```sql
CREATE TABLE items (
  id      TEXT PRIMARY KEY,
  collection TEXT,
  experiment TEXT,
  datetime TIMESTAMPTZ,
  bbox    DOUBLE[],
  data    JSON                  -- full STAC Item GeoJSON
);

CREATE TABLE collections (
  id   TEXT PRIMARY KEY,
  data JSON                     -- full STAC Collection JSON
);

CREATE TABLE catalogs (
  id   TEXT PRIMARY KEY,
  data JSON
);

CREATE TABLE collection_item_props (
  collection_id TEXT,
  property      TEXT,
  value         TEXT,
  PRIMARY KEY (collection_id, property, value)
);
```

Items are stored as opaque JSON blobs; the surrounding columns (`collection`, `datetime`, `bbox`)
are indexed for fast filtering without JSON parsing.

---

## Key Classes

| Class | Location | Role |
|-------|----------|------|
| `CatalogDB` | `storage/duckdb.py` | Per-experiment DuckDB (context manager) |
| `CatalogPool` | `api/pool.py` | Manages live connections across catalogs |
| `CatalogRegistry` | `api/registry.py` | Registry of catalog paths; optional JSON persistence |
| `DuckDBCatalogClient` | `api/client.py` | stac-fastapi CoreClient implementation |
| `CollectionContext` | `scan/context.py` | Resolved (experiment, component) for one file |
| `PersonalCollection` | `storage/personal.py` | User-curated collection with RBAC |
| `Authenticator` | `api/auth.py` | Pluggable auth base class |

---

## Authentication

Pluggable via `Authenticator` protocol. Current implementations:
- **JupyterHub token auth** — exchanges JupyterHub tokens for `User` objects
- **No-op** — anonymous access for development

Permissions are checked per-endpoint in `catalog_routes.py` and `personal_routes.py`.

---

## Deployment

```
Docker:   mambaorg/micromamba:1.5-jammy, port 23000
Conda:    environment.yml (Python 3.11)
CLI:      esm-catalog serve --catalog ... --host 0.0.0.0 --port 23000
Reverse proxy: optional nginx in front
```

---

---

# TODO: Planned Features

The items below come from `/albedo/work/projects/paleo_work/esm-catalog/TODO.md`.
Each section explains what the feature means in the context of the current architecture
and sketches an implementation plan.

---

## TODO 1: Tree-like Structure for Catalogs ✅ DONE

**Goal:** Present the catalog hierarchy visually as a tree, e.g.:

```
exp/
  echam/
  fesom/
exp2/
  echam/
exp4/
  echam/
  fesom/
```

**Implementation (complete):** Experiments are derived dynamically from the `experiment` field
on each collection JSON object — no DuckDB schema changes required.

### New endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /experiments` | Paginated list of all experiments with collection counts |
| `GET /experiments/{id}` | STAC Catalog object for one experiment with child links |

### Changes made

- **`storage/duckdb.py`** — Added `iter_experiments()` and `get_collections_for_experiment()`
  methods. Both follow the existing cursor-close-in-finally pattern for thread safety.

- **`api/client.py`** — Added `_inject_experiment_catalog_links()` module-level helper;
  added `_get_all_experiment_ids()` and `_get_collections_for_experiment()` methods to
  `DuckDBCatalogClient`; updated `landing_page()` to emit experiment child links instead of
  collection child links.

- **`api/responses.py`** — Added `ExperimentLink`, `ExperimentCatalog`, `ExperimentSummary`,
  and `ExperimentsListResponse` Pydantic models.

- **`api/experiment_routes.py`** — New file; FastAPI router with both endpoints.

- **`api/app.py`** — Mounts the experiment router after the catalog router.

### Collection parent link fix

Collection `parent` links now point to `/experiments/{experiment}` instead of bare `/`.
Collections without an `experiment` field fall back to `/` (backwards compatible).

### Landing page child links

`GET /` now lists experiments as children (not individual collections), enabling STAC Browser
Browse mode to show the experiment tree.

### No schema change

Experiments are a virtual layer derived from the `experiment` field on each collection.
No new DuckDB tables or columns are required.

See `docs/experiment-hierarchy.md` for full user-facing documentation.

---

## TODO 2: Autocomplete for Item Metadata Search ✅ DONE

**Goal:** The item detail page (`MetadataGroups.vue`) has a `metadata.search` text input that
filters the displayed metadata fields. With items carrying 394+ namelist fields (all prefixed
`nml:`), users have no guidance on what names to type.

**Solution implemented:** Added `<datalist>` autocomplete to the metadata search input. As the
user types, the browser shows dropdown suggestions for all available property labels in that
item. Selecting a suggestion instantly filters the accordion to show matching fields.

**What was done:**

- **`stac-browser/src/components/MetadataGroups.vue`** — Added a `searchSuggestions` computed
  property that collects all unique property labels from `formattedData` and sorts them
  alphabetically. Added a `<datalist>` element bound to the search input via the HTML `list`
  attribute. Uses `$.uid` for a unique datalist ID when multiple MetadataGroups appear on
  the same page (e.g. asset metadata + item metadata).

No API changes needed — this is a pure client-side UX improvement.

---

## TODO 3: Return Complete Experiment from Component-Level Query ✅ DONE

**Goal:** When a user queries at the component level (e.g. fetches all items in collection
`basic-001-echam`), provide a way to get or navigate to the full experiment — all components
(`basic-001-echam`, `basic-001-fesom`, etc.) and their metadata — in one response.

### New endpoint

| Endpoint | Description |
|----------|-------------|
| `GET /collections/{id}/experiment` | Returns the parent experiment STAC Catalog for the given component collection |

### Changes made

- **`api/experiment_routes.py`** — Added `create_collection_experiment_router()` factory.
  The new route looks up the collection across all catalogs, extracts its `experiment` field,
  then returns the same `ExperimentCatalog` JSON as `GET /experiments/{id}`.
  Returns 404 if the collection is not found or has no `experiment` field.

- **`api/app.py`** — Mounts the collection-experiment router after the experiment router.

### Navigation chain

The full STAC navigation chain is now resolvable in both directions:

```
GET /                              → landing page (child links → experiments)
  GET /experiments/{id}            → experiment catalog (child links → collections)
    GET /collections/{id}          → collection (parent link → /experiments/{id})
      GET /collections/{id}/items  → items
      GET /collections/{id}/experiment  → shortcut back to experiment catalog
```

### No schema change

The route is a pure read-only shortcut over the existing
`get_collections_for_experiment()` storage method.

---

## TODO 4: Web UI for Catalog Management ✅ DONE

**Goal:** Replace the CLI command `esm-catalog register ...` / `deploy_albedo.sh register`
with a browser-based web UI page where users can register a catalog path with the running
server, view all registered catalogs, and manage them without using `curl`.

### What was done

- **`api/ui/index.html`** — New single-file vanilla JS admin page. No build step, no
  external dependencies. Served by FastAPI as a static mount.
  - Header with live `GET /health` API status badge
  - Register form: path input with auto-suggested name (strips `.duckdb`), name, description
  - Catalog list loaded from `GET /catalogs` on page load with status icons (green `●` active,
    red `✗` missing/error)
  - Per-catalog **Refresh** (`POST /catalogs/{id}/refresh`) and **Delete**
    (`DELETE /catalogs/{id}`) buttons with confirm dialog
  - All API calls use `window.location.origin` as base — no hardcoded URLs

- **`api/app.py`** — Added imports for `StaticFiles` and `RedirectResponse`; mounted
  `StaticFiles(directory=ui_dir, html=True)` at `/ui`; added `/admin` → `/ui` redirect
  (GET + HEAD).

### Access

```
http://<host>:<port>/ui      # admin page
http://<host>:<port>/admin   # redirects → /ui
```

---

## TODO 5: LLM & MCP Capabilities

**Goal:** Integrate Large Language Model (LLM) and Model Context Protocol (MCP) capabilities
into the catalog, enabling natural-language search, auto-summarization of experiments, and
AI-assisted metadata enrichment.

**Current state:** No LLM/MCP integration exists. The `esm-tools-plus/simcat/llm-mcp` branch
exists in the repo and appears to be the designated development space for this work.

**Possible capabilities:**

1. **Natural-language search** — Translate a plain English query like
   *"find temperature output from the last glacial maximum experiments"* into a CQL2 filter
   and execute it against the catalog. The LLM acts as a query planner.

2. **MCP server** — Expose catalog search, collection listing, and item retrieval as MCP tools
   so that AI agents (Claude, Cursor, etc.) can query the catalog directly in their context.
   This is likely the primary use case given the existing `llm-mcp` branch.

3. **Metadata summarization** — Auto-generate human-readable descriptions for collections
   and experiments based on their STAC metadata (variables, time range, spatial extent, etc.).

4. **Semantic search / embeddings** — Index collection metadata as embeddings (e.g. via
   fastembed — note: `.fastembed_cache/` directory already exists in `src/esm_catalog/`)
   and support vector similarity search alongside keyword search.

**Note on fastembed cache:** The presence of `.fastembed_cache/` and `.rtk/` in `src/esm_catalog/`
suggests embedding-based search has already been prototyped. This work likely lives on the
`llm-mcp` branch and should be reviewed before designing the MCP integration here.

**Files to touch:** New `api/mcp.py` or `mcp/` subpackage, `api/app.py` (mount MCP routes),
`storage/embeddings.py` (if embedding-based search is added).

---
