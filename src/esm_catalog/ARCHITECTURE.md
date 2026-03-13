# ESM Catalog Architecture

## Overview

A STAC-based catalog system for ESM-Tools experiment output, with DuckDB storage and HPC awareness.

## Design Principles

1. **Small files, single responsibility** - Each module does one thing, ~50 lines
2. **DuckDB as primary storage** - One file per experiment, native JSON support
3. **STAC for interoperability** - Standard format, existing tooling (stac-fastapi, STAC Browser)
4. **HPC-aware** - Tape state, rate limiting, SLURM integration
5. **Federated ownership** - Each user owns their experiment's catalog

---

## Directory Structure

```
esm_catalog/
├── cli.py                    # Entry point, arg parsing only
│
├── scan/
│   ├── __init__.py
│   ├── grib.py               # Scan GRIB files, return metadata dict
│   ├── netcdf.py             # Scan NetCDF files, return metadata dict
│   ├── detect.py             # Auto-detect format, dispatch to scanner
│   └── context.py            # Resolve collection membership before item creation
│
├── stac/
│   ├── __init__.py
│   ├── item.py               # metadata dict → STAC Item dict
│   ├── collection.py         # Create/update STAC Collection; called by scan/context.py on first encounter
│   └── extensions/
│       ├── __init__.py
│       ├── hpc.py            # HPC storage extension (tape state, recall time)
│       ├── datacube.py       # Datacube extension (cube:dimensions, cube:variables)
│       ├── contacts.py       # Contacts extension (ORCID, authors)
│       └── registry.py       # Extension URL registry
│
├── storage/
│   ├── __init__.py
│   ├── duckdb.py             # Insert/query catalog.duckdb
│   └── export.py             # Export to JSON/Parquet for interop
│
├── hpc/
│   ├── __init__.py
│   ├── detect.py             # Detect storage type from path/filesystem
│   └── state.py              # HSM state queries (dmattr, scoutfs, etc.)
│
├── integration/
│   ├── __init__.py
│   ├── esm_tools.py          # add_files() - bridge from ESM-Tools tidy phase to catalog
│   └── config.py             # Load and parse finished_config.yaml
│
├── api/
│   ├── __init__.py
│   ├── client.py             # DuckDBCatalogClient — BaseCoreClient implementation (6 methods)
│   └── app.py                # create_app() factory + module-level app for uvicorn
```

---

## Core Flow

### Scanning (CLI)

```python
# What happens when you run: esm-catalog scan file.grb

metadata = scan_grib(path)                    # scan/grib.py - extract variables, time, bbox
ctx      = resolve_context(path, config)      # scan/context.py - experiment, component → collection id
item     = make_item(path, metadata, ctx)     # stac/item.py - build STAC Item dict, collection field set
item     = add_hpc_extension(item, path)      # stac/extensions/hpc.py - add tape state
db.insert(item, collection=ctx.collection_id) # storage/duckdb.py - collection populated at insert time
```

Context resolution (`resolve_context`) must happen **before** item creation. See
[Collection Context and Assignment](#collection-context-and-assignment) for why this ordering is
non-negotiable.

### Serving (API)

```python
# stac-fastapi with DuckDB backend (api/client.py + api/app.py)

# DuckDBCatalogClient implements BaseCoreClient — 6 required abstract methods
class DuckDBCatalogClient(BaseCoreClient):
    catalogs: List[str]   # paths to catalog.duckdb files (at least one)

    def all_collections(self, **kwargs):   # GET /collections — supports query param filters
    def get_collection(self, collection_id, **kwargs):   # GET /collections/{id}
    def item_collection(self, collection_id, ...):       # GET /collections/{id}/items
    def get_item(self, item_id, collection_id, **kwargs):  # GET /collections/{id}/items/{item_id}
    def get_search(self, collections, ids, bbox, datetime, limit, **kwargs):   # GET /search
    def post_search(self, search_request, **kwargs):     # POST /search

# create_app() wires everything together (api/app.py)
api = create_app(catalogs=["/work/exp1/catalog.duckdb", "/work/exp2/catalog.duckdb"])
# CORS allow_origins=["*"] added automatically — required for STAC Browser cross-origin access

# Direct uvicorn invocation:
#   uvicorn esm_catalog.api.app:app
# Configure via env:
#   ESM_CATALOG_DB=/work/exp1/catalog.duckdb:/work/exp2/catalog.duckdb uvicorn esm_catalog.api.app:app
```

Multi-catalog federation: `DuckDBCatalogClient` opens each `CatalogDB` per request and merges
results in Python. This is simpler than DuckDB `ATTACH` and avoids alias conflicts across
independently-named experiments.

Collection-search: `all_collections()` passes any non-pagination query params to
`db.search_collections()`, which checks both native collection fields and the
`collection_item_props` index built at scan time.

---

## Collection Context and Assignment

### The design hole

The DuckDB schema requires a `collection` value at insert time:

```sql
CREATE TABLE items (
    id         TEXT PRIMARY KEY,
    collection TEXT,           -- must be set at insert; NULL breaks API navigation
    ...
);
```

The original core flow (`scan → item → db.insert`) contained no step that resolved this value.
`stac/collection.py` was described as "aggregate items → STAC Collection" but had no defined
input — it cannot know which items to aggregate, or on what grouping key, without external
context. This created two concrete failures:

1. **Silent NULL**: Items inserted with `collection = NULL` are stored successfully. DuckDB does
   not complain. But `GET /collections/{id}/items` returns nothing for those items. They are only
   reachable via `/search`. STAC Browser's tree navigation is broken; the catalog appears empty.

2. **Ordering contradiction**: Collection creation was implied to happen *after* scanning, but the
   `collection` column must be populated *during* insert. Any design that creates collections as a
   post-processing step requires a full second pass over all stored items to backfill the field —
   an approach that breaks down for incremental scans (new files added to an existing experiment).

### Resolution: `scan/context.py`

Collection membership is resolved from the file path and/or ESM-Tools config **before** the item
is constructed. `resolve_context(path, config)` returns a `CollectionContext` dataclass:

```python
@dataclass
class CollectionContext:
    experiment_id:   str          # e.g. "basic-001"
    component:       str          # e.g. "fesom"
    collection_id:   str          # e.g. "basic-001-fesom"  (experiment + component)
    collection_title: str
```

Two resolution strategies, tried in order:

**1. ESM-Tools config** (preferred — used during live tidy phase):

```python
# config is the finished_config.yaml loaded by ESM-Tools at run time
ctx = resolve_context(path, config=esm_config)
# experiment_id from config["general"]["expid"]
# component    from which component block owns this outdata_dir
```

**2. Path parsing** (fallback — used during batch scan of legacy runs):

```python
# Path pattern: .../experiments/{experiment}/outdata/{component}/file.nc
ctx = resolve_context(path, config=None)
# experiment_id = path.parts[path.parts.index("experiments") + 1]
# component     = path.parts[path.parts.index("outdata") + 1]
```

If neither strategy can resolve the context (path does not match the expected pattern and no
config is provided), the scan raises a hard error rather than inserting with `collection = NULL`.
A silent NULL is worse than a failed insert because it produces a catalog that appears to work
but cannot be navigated.

### Collection creation

`stac/collection.py` is not a post-processing aggregator — it is called *by* `scan/context.py`
the first time a `collection_id` is encountered:

```python
# scan/context.py
if not db.collection_exists(ctx.collection_id):
    collection = make_collection(ctx)   # stac/collection.py
    db.insert_collection(collection)    # storage/duckdb.py
```

Subsequent files for the same `(experiment, component)` pair find the collection already present
and update its temporal/spatial extent in place. This makes the scan pipeline fully incremental:
re-running after adding new files extends the existing collection rather than recreating it.

### Collection hierarchy for ESM data

The grouping key `(experiment_id, component)` mirrors the output directory structure that
ESM-Tools already enforces. The resulting catalog hierarchy is:

```
Root Catalog  (API root /)
└── {experiment_id}          Catalog  — one per experiment run
    └── {experiment}-{component}   Collection  — one per model component
        └── {variable}.{component}.{YYYYMM}    Item  — one per output file
```

This is the hierarchy STAC Browser renders as a navigable tree. The intermediate experiment-level
catalog node is created by `stac/collection.py` alongside the component collection, using the
experiment metadata from `CollectionContext`.

---

## Storage Architecture

### Per-Experiment DuckDB

```
/work/user/experiments/
├── picontrol/
│   ├── outdata/...
│   └── catalog.duckdb     # User owns this, can write
├── historical/
│   ├── outdata/...
│   └── catalog.duckdb
```

### Federation via Config

```yaml
# ~/.esm_catalog.yaml
catalogs:
  - /work/ab1234/experiments/picontrol/catalog.duckdb
  - /work/cd5678/experiments/historical/catalog.duckdb  # read access sufficient
```

Query tool ATTACHes readable databases at query time:

```sql
ATTACH '/work/ab1234/.../catalog.duckdb' AS picontrol;
ATTACH '/work/cd5678/.../catalog.duckdb' AS historical;

WITH all_items AS (
    SELECT data FROM picontrol.items
    UNION ALL
    SELECT data FROM historical.items
)
SELECT * FROM all_items
WHERE json_extract(data, '$.properties.variable') = 'tas';
```

### DuckDB Schema

```sql
CREATE TABLE items (
    id         TEXT PRIMARY KEY,
    collection TEXT,
    experiment TEXT,      -- explicit column; avoids json_extract for experiment filter
    datetime   TIMESTAMP,
    bbox       DOUBLE[],  -- DOUBLE[4] is not valid DuckDB syntax
    data       JSON       -- Full STAC Item, query with json_extract()
);

CREATE TABLE collections (
    id   TEXT PRIMARY KEY,
    data JSON             -- Full STAC Collection
);

CREATE TABLE catalogs (
    id   TEXT PRIMARY KEY,
    data JSON             -- Experiment-level STAC Catalog nodes (Root → Experiment → Collection)
);

-- Pre-aggregated item property index for collection search
-- Populated at insert time; allows /collections?filter= to match item-derived properties
-- (e.g. scenario, variable) that may be null in the collection JSON itself
CREATE TABLE collection_item_props (
    collection_id TEXT,
    property      TEXT,
    value         TEXT,
    PRIMARY KEY (collection_id, property, value)
);

-- Indexes
CREATE INDEX idx_collection ON items(collection);
CREATE INDEX idx_experiment ON items(experiment);
CREATE INDEX idx_datetime   ON items(datetime);
CREATE INDEX idx_variable   ON items(json_extract(data, '$.properties.variable'));
```

---

## Batch Processing (SLURM)

### The Problem

- 10,000 files to scan
- Can't write to same DuckDB from multiple SLURM jobs
- Need parallelism without hammering filesystem

### The Solution

```
Step 1: Parallel scan (SLURM array job)
        Each job scans batch of files → writes Parquet

Step 2: Serial insert (single job)
        DuckDB reads all Parquet → single catalog.duckdb
```

### Snakemake Workflow

```python
# Snakefile

rule scan_batch:
    input:
        files=lambda wc: get_batch_files(wc.batch_id),
        config=ancient("finished_config.yaml")   # provides collection context to resolve_context()
    output: "staging/batch_{batch_id}.parquet"
    resources:
        runtime=10,
        mem_mb=4000
    shell:
        # --config passed to resolve_context(); falls back to path parsing if omitted
        "esm-catalog scan-batch {input.files} --config {input.config} --output {output}"

rule merge:
    input: expand("staging/batch_{i}.parquet", i=range(NUM_BATCHES))
    output: "catalog.duckdb"
    shell:
        "esm-catalog merge-parquet {input} --output {output}"
```

Run with SLURM:

```bash
snakemake --executor slurm --jobs 100
```

---

## HPC Storage Extension

### Fields

**Item-level (properties):**
- `hpc:facility` - AWI, DKRZ, NERSC, etc.
- `hpc:system` - albedo, levante, perlmutter
- `hpc:storage_tier` - hot, warm, cold

**Asset-level:**
- `hpc:storage_type` - lustre, gpfs, hpss, dmf, posix
- `hpc:state` - online, nearline, offline, migrating, staged
- `hpc:recall_time_estimate` - seconds
- `hpc:last_access` - ISO timestamp (populated via `os.stat().st_atime` at scan time; subject to rate limiting — read via `hpc/state.py` which applies the same throttle as HSM queries)

### Detection

```python
# hpc/detect.py

def detect_hpc_storage(path: Path) -> dict:
    path_str = str(path.resolve())

    if "/albedo/" in path_str:
        return {
            "hpc:facility": "AWI",
            "hpc:system": "albedo",
            "hpc:storage_type": "lustre",
            "hpc:state": "online",
        }

    if "/arch/" in path_str or "/hpss/" in path_str:
        return {
            "hpc:storage_type": "hpss",
            "hpc:state": "offline",
            "hpc:recall_time_estimate": 300,
        }

    # Fallback: detect from filesystem
    ...
```

---

## ESM-Tools Integration

### Live Path (New Runs)

ESM-Tools tidy phase calls catalog directly - no filesystem scanning needed:

```python
# In ESM-Tools tidy phase
from esm_catalog import add_files

add_files(
    db="/work/user/exp/catalog.duckdb",
    files=finished_output_files,  # ESM-Tools knows what it wrote
    experiment_config=config,      # Rich metadata from config
)
```

### Batch Path (Legacy Runs)

For existing experiments without catalog:

```bash
esm-catalog scan /work/user/old_experiment/outdata/ \
    --rate-limit 10 \
    --checkpoint \
    --resume
```

---

## STAC Extensions Used

| Extension | Purpose |
|-----------|---------|
| [datacube](https://github.com/stac-extensions/datacube) | `cube:dimensions`, `cube:variables` |
| [cf](https://github.com/stac-extensions/cf) | CF Standard Names (`cf:parameter`, units, descriptions) |
| [file](https://github.com/stac-extensions/file) | File size, checksum |
| [contacts](https://github.com/stac-extensions/contacts) | Authors, ORCID |
| [scientific](https://github.com/stac-extensions/scientific) | DOI, citations |
| hpc-storage (custom — spec in `hpc/`) | Tape state, recall time, storage tier |

---

## JSON-LD / Linked Data

For linking to controlled vocabularies:

```json
{
  "@context": {
    "variable": "http://vocab.nerc.ac.uk/standard_name/",
    "creator": "https://orcid.org/"
  },
  "variable": "air_temperature",
  "creator": "0000-0001-1234-5678"
}
```

Vocabularies:
- CF Standard Names for variables
- CMIP6 CV for experiments, models
- ORCID for people
- ROR for institutions

Browser can resolve these to human-readable definitions.

---

## API Endpoints (via stac-fastapi)

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | Landing page / root catalog + conformance declaration |
| `/collections` | GET | List/search collections — supports CQL2 filter |
| `/collections/{id}` | GET | Single collection |
| `/collections/{id}/items` | GET | Items in collection — supports CQL2 filter; offset pagination via `token` |
| `/collections/{id}/queryables` | GET | Per-collection JSON Schema — scoped to that collection's actual values; enables "Additional Filters" in collection items view |
| `/search` | GET/POST | Query items — supports CQL2 filter |
| `/queryables` | GET | Global JSON Schema of filterable properties with enum lists from live catalog |
| `/stac-extensions/hpc/v0.1.0/schema.json` | GET | HPC storage extension schema (served locally; published URL not yet live) |
| `/format` | POST | OGC CQL2 format-negotiation stub (silences STAC Browser 404 probe) |
| `/docs` | GET | Swagger UI |

**Item search** custom query parameters (`/search`):
- `variable` - Filter by variable name
- `experiment` - Filter by experiment
- `model` - Filter by model component
- `filter` / `filter-lang` - CQL2-TEXT or CQL2-JSON expression

**Collection search** query parameters (`/collections`):
- `filter` / `filter-lang` - CQL2-TEXT or CQL2-JSON expression (filters on collection metadata AND item-derived properties)
- `limit` - Page size (default 100)
- `token` - Offset-based pagination token

---

## Collection Search

### Two search modes

The API exposes two distinct search surfaces, both visible as tabs in STAC Browser's Search view:

| Tab | Endpoint | Finds | Response key |
|---|---|---|---|
| Search for Items | `GET/POST /search` | Individual files (STAC Items) | `features` array |
| Search for Collections | `GET /collections?filter=...` | Experiment × component datasets | `collections` array |

STAC Browser shows a tab only when the API advertises the corresponding conformance class.
The API **must** declare both in the landing page (`GET /`) `conformsTo` array:

```python
# stac-fastapi declares these automatically via BaseCoreClient.conformance_classes()
# The "Search for Collections" tab in STAC Browser appears when the API advertises:
#   "https://api.stacspec.org/v1.0.0/collection-search"
# The "Additional filters" CQL2 builder appears when the API advertises:
#   "https://api.stacspec.org/v1.0.0/item-search#filter"
# Both are in the base conformance set declared by stac-fastapi-api v6.
```

If `collection-search` is absent from `conformsTo`, the tab is silently hidden in STAC Browser —
there is no error, the feature simply does not appear.

### Collection search implementation

Collection search filters on two property sources simultaneously:

1. **Native collection fields** — `title`, `description`, `keywords`, `license`, `model`,
   `model_type`, `version`, etc. stored in the `collections` DuckDB table.

2. **Item-derived properties** — properties like `scenario`, `experiment`, `variable` that may be
   null in the collection JSON but are present on every item. These are pre-indexed at scan time
   into a per-collection property index (`_col_item_props`), keyed by collection ID.

The index is built when items are inserted and stored in DuckDB:

```sql
-- Extend schema: pre-aggregated item property index per collection
CREATE TABLE collection_item_props (
    collection_id TEXT,
    property      TEXT,
    value         TEXT,
    PRIMARY KEY (collection_id, property, value)
);
```

At query time, `search_collections()` checks both sources and applies CQL2 constraints using
OR semantics within a property (any matching value satisfies) and AND semantics across properties
(all constraints must be satisfied):

```python
# storage/duckdb.py
def search_collections(self, filter_props, limit, offset):
    matched = []
    for col in self.iter_collections():
        idx = self.get_collection_item_props(col["id"])  # {prop: {val1, val2, ...}}
        if filter_props and not collection_matches(idx, filter_props):
            continue
        matched.append(col)
    return matched[offset:offset + limit], len(matched)
```

### CQL2 filter parsing

Both `/search` and `/collections` accept `filter` (expression) and `filter-lang`
(`cql2-text` or `cql2-json`). Two dedicated parsers in `api/client.py` translate
CQL2 expressions into a `filter_props` dict consumed by the DuckDB query layer.
`_parse_cql2_filter()` dispatches to the correct parser based on `filter-lang`.

**STAC Browser sends different formats depending on context:**
- Collection items view (`GET /collections/{id}/items`) — sends `cql2-text` as a query param
- Global Search tab (`POST /search`) — sends `cql2-json` in the POST body

#### `filter_props` dict format

```
{field: value}           where value is one of:
  (sql_op, val)          — single condition  → field = val  (AND with others)
  [(op,v1), (op,v2)]     — AND duplicate list → field=v1 AND field=v2
  [v1, v2]               — OR value list     → field IN (v1, v2)
```

#### Supported filter combinations

| Input expression | Parsed `filter_props` | SQL generated |
|---|---|---|
| `variable = 'ssh'` | `{'variable': ('=', 'ssh')}` | `json_extract(...) = 'ssh'` |
| `var = 'a' AND var = 'b'` | `{'variable': [('=','a'), ('=','b')]}` | `... = 'a' AND ... = 'b'` |
| `var = 'a' OR var = 'b'` | `{'variable': ['a', 'b']}` | `... = 'a' OR ... = 'b'` |
| `NOT (var = 'ssh')` | `{'variable': ('!=', 'ssh')}` | `json_extract(...) != 'ssh'` |
| `expr = 'e1' AND var = 'v1'` | `{'experiment': ('=','e1'), 'variable': ('=','v1')}` | `experiment = 'e1' AND ...` |

**OR semantics** (STAC Browser "Match any filters"):
- `_parse_cql2_json` collects OR branch values as plain lists `['v1', 'v2']`
- `_parse_cql2_text` splits on `\bOR\b` and collects plain values per field
- `search_items` detects plain lists (not tuple lists) and emits `IN (?, ?)` SQL
- `_collection_matches` uses `any(v in indexed_vals ...)` for OR list matching

**NOT semantics** (STAC Browser "Negate filter"):
- Both parsers support `NOT (...)` wrapper and a `negate` flag
- Operators are inverted via `_CQL2_OP_INVERT`: `=`→`!=`, `<`→`>=`, etc.
- `CqlNot.toText()` in stac-browser's `logical.js` is overridden to emit
  `NOT (inner)` — the base class `join()` on a single-element array drops the
  operator silently

**Temporal literal unwrapping:** STAC Browser sends datetime values as CQL2-JSON
objects (`{"timestamp": "2000-01-01T00:00:00Z"}`) rather than bare strings.
`_cql2_value()` unwraps these before passing values to DuckDB so that TIMESTAMPTZ
binding works correctly.

### Response formats

**Item search** (`/search`):
```json
{ "type": "FeatureCollection", "features": [...], "links": [...] }
```

**Collection search** (`/collections?filter=...`):
```json
{
  "collections": [...],
  "links": [...],
  "numberMatched": 12,
  "numberReturned": 10
}
```

---

## STAC Browser

A fork of STAC Browser lives at `~/repos/esm_tools/stac-browser` and is served
locally (e.g. `npm start -- --port 23005`).  Two bugs were fixed in the fork and
one visual enhancement was added:

### Fork changes

**`src/models/cql2/operators/logical.js` — `CqlNot.toText()` fix:**
The base-class `join(" NOT ")` on a single-element array returns the element alone
(separator is dropped for length-1 arrays).  `CqlNot.toText()` now overrides this to
always emit `NOT (inner)`:

```javascript
toText() {
  if (this.args && this.args.length === 1) {
    return `NOT (${this.args[0].toText()})`;
  }
  return super.toText();
}
```

**`src/components/Item.vue` — collection badge on item cards:**
A `variant="info"` badge showing the collection ID is injected as the first element
in the card intro section.  This is especially useful when viewing cross-collection
search results (e.g. "Additional filters" spans multiple collections).

> **Note:** Vite's HMR does not always hot-reload utility modules (`utils.js`,
> `logical.js`).  After editing these files restart the dev server fully.

### API-side activation

| Feature | What enables it |
|---|---|
| "Search for Collections" tab | API declares `collection-search` conformance class |
| "Additional filters" in Items tab | API declares `item-search#filter` + OGC CQL2 conformance classes **and** `GET /collections/{id}/queryables` returns 200 |
| "Additional filters" in Collections tab | API declares `collection-search#filter` + OGC CQL2 conformance classes **and** `GET /collections` response includes a `rel=queryables` link |

The conformance classes are advertised automatically by `stac-fastapi`.
`GET /collections/{id}/queryables` must exist (404 silently hides the "Additional
Filters" section for the collection items view — no error shown).
The queryables link in `GET /collections` must be added explicitly — STAC Browser's
`SearchFilter.vue` fetches queryables for the Collections tab from that embedded link.

### Usage

```bash
# Start the dev server
cd ~/repos/esm_tools/stac-browser && npm start -- --port 23005
# Point at the running API
# http://localhost:23005/#/search/external/http://localhost:23003
```

The API runs inside the AWI internal network; access via VPN.  Because the browser
runs on a different origin, **CORS must be open** — `create_app()` already sets
`allow_origins=["*"]`.

---

## Data Portal & Self-Registration (Proposal)

> **Status: design proposal — not yet implemented. See Phase 6 in the phase plan.**

### The problem

Once a researcher's SLURM job finishes and a `catalog.duckdb` exists, there is no
frictionless path to make that catalog visible to others.  The current approach
(`esm-catalog serve --port XXXX` + manually editing `servers.json`) puts
unnecessary burden on researchers and requires port coordination across users.

### Design goals

1. Researcher runs **one command** to publish a catalog.  No ports, no server admin.
2. The portal and STAC Browser are **always running** — never need restarting when
   a new catalog is added.
3. Different departments (Paleo, Ocean, Atmosphere, …) get **isolated API servers**
   on reserved port ranges, but researchers never think about port numbers.

### Architecture

```
Fixed infrastructure (one-time setup by sysadmin / long-running SLURM job):

  Port 23000  ── Data Portal
                  nginx serves stac-portal/ (bind-mounted from host)
                  Reads servers.json for the list of department API cards
                  Researcher never edits this

  Port 23000/stac/  ── STAC Browser SPA (built into Apptainer image)

Department STAC API servers (one per group, always running):

  Port 23100  ── Paleo STAC API
  Port 23200  ── Ocean STAC API
  Port 23300  ── Atmosphere STAC API
                  Each server reads its registry.json on every request
                  → new catalogs appear automatically on the next HTTP hit

Researcher action (runs anywhere, takes <1 second):

  esm-catalog register ~/experiments/picontrol/catalog.duckdb \
      --registry /shared/paleo/registry.json \
      --name "PI Control 1000yr" \
      --description "FESOM + ECHAM6 pre-industrial control run"
```

### Why hot-reload is nearly free

`DuckDBCatalogClient._open_catalogs()` already opens catalog files
**per request** — it holds no connection state between requests.  If the list of
paths it reads changes (because `registry.json` was updated), the very next HTTP
request picks up the new catalog automatically.  No file-watch thread, no server
restart, no signal handling required.

```python
# api/client.py — proposed extension
def _open_catalogs(self) -> list[CatalogDB]:
    if self.registry:                               # new: registry-aware path
        paths = _read_registry(self.registry)       # parse JSON, extract paths
    else:
        paths = self.catalogs                       # legacy: static list
    return [CatalogDB(p) for p in paths if Path(p).exists()]
```

Reading a small JSON file per request costs microseconds.  For a research portal
with tens of concurrent users, this overhead is negligible.

### Registry file format

`/shared/<dept>/registry.json` — edited only by `esm-catalog register`:

```json
{
  "catalogs": [
    {
      "id": "pasili001-picontrol",
      "name": "PI Control 1000yr",
      "description": "FESOM + ECHAM6 pre-industrial control run",
      "path": "/albedo/home/pasili001/experiments/picontrol/catalog.duckdb",
      "owner": "pasili001",
      "registered_at": "2026-03-13T09:00:00Z"
    }
  ]
}
```

`esm-catalog register` writes this file atomically (write to `.tmp`, then
`os.rename`) and takes a file lock so concurrent registrations from multiple users
are safe.

### Complete researcher lifecycle

```
1. SLURM job runs (catalog building):
   esm-catalog scan /experiments/picontrol/outdata/
   → creates catalog.duckdb

2. Researcher publishes (one command, runs on login node):
   esm-catalog register ~/experiments/picontrol/catalog.duckdb \
       --registry /shared/paleo/registry.json \
       --name "PI Control 1000yr"
   → appends entry to registry.json atomically

3. Portal (nothing to do):
   Paleo STAC API on :23100 reads registry.json on next request
   New collections appear in STAC Browser automatically
   Health dots on portal stay green throughout
   No restarts, no admin, no port juggling
```

### Open design questions

- **Department assignment**: should `--registry` be explicit (researcher specifies
  the path) or inferred (from path conventions like `/albedo/home/<user>/` → lookup
  table, or from a `~/.esm_catalog.yaml` user config)?
- **Deregistration**: should `esm-catalog deregister` exist, or is stale-entry
  cleanup handled by health-check timeouts in the portal?
- **Portal placement**: the portal itself must be always-on.  A login-node process
  is the pragmatic first step; a systemd unit managed by IT would be the production
  answer.
- **Cross-department search**: a user wanting to search across all departments would
  need to query multiple APIs.  A global "all departments" API (one registry that
  imports all per-department registries) could be offered as a read-only view
  without changing the per-department model.

---

## Paleo Time as a Searchable Attribute (Proposal)

> **Status: design proposal — awaiting feedback from Paul Gierz.**
> Related library: [`paleodatetime`](https://github.com/pgierz/paleodatetime)

### The problem

Paleo simulations represent geological time far outside the range of standard
datetime types.  A 65 Ma run (`-65_000_000` CE) overflows DuckDB's `TIMESTAMPTZ`
column (range ≈ ±290,000 years) and is outside RFC 3339 entirely.  Researchers
need to filter items by geological age — e.g. "show me all LGM runs" or "all
Cretaceous experiments" — which is not possible with the existing `datetime` field.

### Why a plain integer works

`paleodatetime.PaleoDateTime` stores time internally as a large signed integer year.
The cleanest catalog representation is the same: a plain integer property
`paleo_year` on each item.  No special column type, no schema migration — it
participates in the existing `json_extract` query path without any changes to the
filter machinery.

```json
{
  "properties": {
    "datetime":    "1850-01-01T00:00:00Z",  ← model simulation clock (unchanged)
    "paleo_year":  -65000000,               ← geological age (year number, negative = past)
    "paleo_age_ma": 65.0                    ← human-readable display value in Ma (optional)
  }
}
```

**Why `paleo_year` and not `paleo:year`?**
The STAC extension colon-prefix convention (`paleo:year`) is valid JSON but
unreliable in DuckDB JSON path syntax — `json_extract(data, '$.properties.paleo:year')`
is ambiguous.  Using an underscore (`paleo_year`) is simpler and consistent with how
`experiment`, `variable`, etc. are stored.  Alternatively a dedicated
`paleo_year BIGINT` column in the `items` table (mirroring `datetime`) would be the
cleanest approach for efficient range queries.

### Where does the value come from at scan time?

NetCDF output files use a model calendar (e.g. year 1850 for a PI-control), not the
geological age the simulation represents.  The geological age is experiment-level
metadata.  Three candidate sources, in preference order:

1. **`finished_config.yaml`** — Paul adds a `paleo_reference_year: -65000000` field
   to the experiment config.  `integration/esm_tools.py` reads it automatically
   during the ESM-Tools tidy phase and injects it into every item in that experiment.
   This is the right long-term home.

2. **CLI flag** — `esm-catalog scan /outdata/ --paleo-year -65000000`.  A practical
   fallback for batch scanning of legacy runs where no config is available.

3. **NetCDF time coordinate auto-detection** — some paleo models encode time as
   `"years since -65000000-01-01"` with a non-standard `calendar` attribute.
   `scan/netcdf.py` could detect this and extract the reference year automatically.
   Worth checking during implementation whether FESOM/ECHAM output uses this pattern.

### Filter examples (CQL2-text)

```
paleo_year >= -70000000 AND paleo_year <= -60000000   ← Cretaceous slice
paleo_year >= -26000 AND paleo_year <= -19000          ← Last Glacial Maximum
paleo_year = -65000000                                  ← single snapshot
```

STAC Browser exposes `paleo_year` as a numeric range picker in "Additional Filters"
automatically once it is declared in `/queryables` with `"type": "integer"`.

### What needs to change

| Layer | Change needed |
|---|---|
| `stac/item.py` | Read `paleo_year` from metadata dict, include in `properties` |
| `integration/esm_tools.py` | Extract `paleo_reference_year` from `finished_config.yaml` |
| `scan/netcdf.py` | Optionally detect paleo reference from time coordinate `units` attribute |
| `storage/duckdb.py` | Add `paleo_year` to `upsert_collection_item_props`; optionally add `paleo_year BIGINT` column |
| `api/app.py` | Expose `paleo_year` in `/queryables` with `"type": "integer"`, `minimum`/`maximum` from live catalog |
| Filter machinery | **No changes needed** — `paleo_year` is handled generically like any other JSON property |

### Open question for Paul

Where is the geological age best declared in the ESM-Tools configuration?
Is `finished_config.yaml` the right place, or is there a higher-level experiment
descriptor file that already holds this kind of metadata?

---

## Universal Pathlib / fsspec Integration (Decision Note)

> **Decision: do not add as a dependency yet — but design interfaces to accommodate it.**
> Related library: [`universal_pathlib`](https://github.com/fsspec/universal_pathlib)

### What it is

`universal_pathlib` provides `UPath` — a drop-in replacement for `pathlib.Path` that
works transparently with any fsspec-backed filesystem: S3, GCS, Azure, SFTP, HTTP,
ZIP archives, and more.  Code written against the standard pathlib API (`path / "sub"`,
`path.read_bytes()`, `path.glob("**/*.nc")`) runs unchanged against any backend.

### Where it would help this project

**Asset hrefs as proper cross-system URIs.**
`_inject_item_links` currently hacks bare filesystem paths by prepending `file://` at
serve time.  If files ever live on S3, Swift object storage, or SFTP at a partner
institute, the catalog already stores the href string — it just needs to store a
proper URI from the start.  A consumer using `UPath(asset_href)` would open the file
regardless of protocol.

**Cross-site federation.**
A catalog entry with `sftp://albedo.awi.de/path/to/file.nc` or
`s3://awi-cold-storage/...` as the asset href is meaningful to any researcher with
network access — not just to processes running on the originating cluster.  This is
the long-term direction for a federated catalog across AWI, DKRZ, and other sites.

**Scanning files that are not locally mounted.**
Replacing `Path` with `UPath` in `scan/netcdf.py` and `scan/grib.py` would allow
scanning data on S3, SFTP, or HTTP without a local mount.  Both xarray and cfgrib
already accept fsspec-compatible file objects, so the scanner itself needs only a
one-line change.

### Where it does not help

**Tape/HSM state detection.**
`UPath` has no concept of HSM states (online / nearline / offline), dmattr queries,
scoutfs, or recall initiation.  The logic in `hpc/state.py` and `hpc/detect.py` must
remain custom regardless.  The two layers are orthogonal:

```
UPath          — how to open a file given a URI (transport layer)
hpc/ extension — whether the file is accessible right now (accessibility layer)
```

**Lustre/GPFS performance.**
UPath treats Lustre as plain POSIX.  No striping hints, collective I/O, or
filesystem-specific optimisations — not a regression, but not an improvement either.

**The catalog database.**
DuckDB is always local; no benefit.

### What to do now (zero-cost preparation)

Do not add `upath` as a dependency today.  Instead, keep interfaces compatible so the
swap is trivial when the time comes:

1. **Type-hint path arguments as `os.PathLike`** rather than `pathlib.Path` in
   `scan/netcdf.py`, `scan/grib.py`, and `scan/detect.py`.  `UPath` is already
   `os.PathLike`, so this is a documentation change only.

2. **Store asset hrefs as full URI strings at write time** — `file:///absolute/path`
   rather than `/absolute/path` — so the API layer does not need to fix them up.
   Currently `_inject_item_links` prepends `file://` at serve time; moving this
   earlier (to `stac/item.py` or `storage/duckdb.py`) is cleaner and makes hrefs
   valid in the stored JSON.

3. **Keep storage detection separate from path handling** in `hpc/detect.py`.
   When S3 or Swift arrives, `UPath.protocol` would be the natural complement to
   `hpc:storage_type` — the detect logic just needs to branch on protocol rather
   than path prefix.

### Trigger conditions to actually add the dependency

Add `upath` when any of these become real requirements:

- Files are migrated to S3 / Swift cold storage and the catalog needs to point there
- Cross-institute catalog federation where asset hrefs must be resolvable remotely
- Scanning data that is not locally mounted (remote S3, SFTP, HTTP collections)

---

## Dependencies

**Core:**
- `duckdb` - Storage and query
- `pystac` - STAC object model
- `pystac-client` - STAC API client (used in generated Python snippets)
- `xarray` - Read NetCDF/GRIB
- `cfgrib` / `eccodes` - GRIB support
- `ruamel.yaml` - Load `finished_config.yaml` (preserves comments; used by `integration/config.py`)
- `joblib` - Parallel file scanning in batch path

**API:**
- `stac-fastapi` - STAC API framework (custom DuckDB backend; not pgstac or sqlalchemy)
- `uvicorn` - ASGI server
- `fastapi[cors]` - CORS middleware for STAC Browser cross-origin requests

**CLI:**
- `click` / `rich-click` - CLI framework
- `loguru` - Logging

**Batch processing:**
- `snakemake` - Workflow orchestration
- `pyarrow` - Parquet I/O

**Browser (stacbrowser2 fork):**
- `highlight.js` - Python syntax highlighting in PythonCodeBox

---

## Collaboration Notes (Pavan's Work)

Pavan (siligam) built the initial proof-of-concept (`fesom_stac2`), which established:
- The stac-fastapi serving approach (carried forward into `api/server.py`)
- The Catalog → Collection → Item hierarchy with experiment × component as the grouping key
- Datacube and CF extension usage on items
- Snakemake as the batch orchestration layer

**What was incorporated:**
- stac-fastapi as the API framework
- STAC hierarchy (Catalog → Collection → Item)
- Datacube + CF extensions
- Experiment × component as the collection grouping key

**What this architecture adds beyond fesom_stac2:**
- DuckDB backend (replaces static JSON files on disk)
- `scan/context.py` for collection assignment at insert time
- HPC storage extension (`hpc/`)
- GRIB support (`scan/grib.py`)
- Federation across multiple per-experiment DuckDB files
- STAC Browser fork with PythonCodeBox and context-aware code generation

---

## Phase Plan

### Phase 1: Core (MVP) ✅ COMPLETE
- [x] Clean module structure
- [x] GRIB + NetCDF scanning (magic-byte fallback for extension-less ECHAM output; 0–360° longitude normalisation)
- [x] DuckDB storage
- [x] Basic CLI
- [x] Collection context (`scan/context.py`) — design hole identified and resolved
- [x] Pytest tests: `tests/test_hpc.py`, `tests/test_scan.py`, `tests/test_stac.py`, `tests/test_storage.py`, `tests/test_integration.py` (137 passing)
- [x] User documentation: `CLI.md` — command reference with examples for all CLI subcommands

### Phase 2: ESM-Tools Integration ✅ COMPLETE
- [x] `integration/esm_tools.py` — `add_files()` bridge for tidy phase
- [x] `integration/config.py` — `finished_config.yaml` loader (`load_config`); plus `find_finished_configs`, `get_outdata_files`, `extract_stac_metadata` helpers
- [x] Bug fix: `scan/context.py` `_find_component_for_path()` now checks `experiment_outdata_dir` (the key used in real finished_config files; `outdata_dir` is `None` in practice)
- [x] Pytest tests: `tests/test_integration.py` — 51 tests covering `load_config`, `find_finished_configs`, `get_outdata_files`, `extract_stac_metadata`, `find_file_operations_log`, `get_outdata_from_file_operations`, `add_files()` (with checksums), `add_run()` (177 total tests passing)
- [x] `integration/config.py` — `find_file_operations_log()` + `get_outdata_from_file_operations()`: primary source for catalog construction (MD5 checksums included); falls back to `finished_config.yaml` outdata_targets
- [x] `integration/esm_tools.py` — `add_files()` `checksums` param injects `file:checksum` into item assets; `add_run()` implements the priority chain (file_operations_tidy → finished_config)
- [x] User documentation: `docs/esm_tools_integration.md` — how to enable cataloging in a run script, `add_files()` API reference, all three config helpers, `finished_config.yaml` keys used, collection naming convention

### Phase 3: API ✅ COMPLETE
- [x] `api/client.py` — `DuckDBCatalogClient` (BaseCoreClient, 6 abstract methods)
- [x] `api/app.py` — `create_app()` factory; module-level `app` for `uvicorn esm_catalog.api.app:app`
- [x] Multi-catalog federation across per-experiment `catalog.duckdb` files
- [x] CORS middleware (`allow_origins=["*"]`) for STAC Browser cross-origin access
- [x] `ESM_CATALOG_DB` env var for colon-separated catalog paths
- [x] `storage/duckdb.py` — `search_items()` extended with `id` and `datetime`/`datetime_end` native column filters
- [x] CLI `serve` command wired to `create_app()` (was referencing stub `api.server`)
- [x] Pytest tests: `tests/test_api.py` — 34 tests covering landing page, conformance, collections CRUD, items CRUD, GET/POST search with datetime range, multi-catalog federation, CORS headers, client init validation (211 total tests passing)
- [x] Decision: STAC Browser is **external** — use radiantearth hosted instance; no fork required in this repo
- [x] CQL2-JSON filtering for `/search` (POST body `filter` field) and `/collections` (query param)
- [x] `GET /queryables` — JSON Schema with enum lists populated via `DISTINCT` queries on live catalog; enables STAC Browser dropdown pickers in "Additional filters"
- [x] `GET /stac-extensions/hpc/v0.1.0/schema.json` — serves HPC extension schema locally; canonical GitHub Pages URL rewired in `stac_extensions` at serve time so STAC Browser can validate
- [x] `POST /format` — OGC CQL2 format-negotiation stub; accepts raw body (plain-text CQL2 or JSON) to always return 200; silences log noise from STAC Browser probe
- [x] Absolute link injection for collections (`self`, `root`, `parent`, `items`) and items (`self`, `root`, `parent`, `collection`) — stored fragment links are not valid IRIs and break STAC validation and Browser navigation
- [x] Asset `href` normalisation — bare filesystem paths prefixed with `file://` to pass `iri-reference` format validation
- [x] Pagination for POST `/search` — `numberMatched`, `numberReturned`, and `first`/`prev`/`next` links with full body replay; token encodes integer offset
- [x] Pagination for GET `/collections/{id}/items` — `token` and `limit` read directly from `request.query_params` (stac-fastapi does not forward unknown query params via method signature)
- [x] CQL2 temporal literal unwrapping (`_cql2_value`) — STAC Browser sends `{"timestamp": "..."}` dicts; unwrapped before DuckDB binding
- [x] `GET /collections` response includes `rel=queryables` link — required for STAC Browser to load queryables and show "Additional filters" CQL2 builder in the "Search for Collections" tab (without this link the tab shows no filter controls even when `collection-search#filter` is declared)
- [x] `GET /collections/{id}/queryables` — per-collection queryables endpoint; scoped enum values for that collection; enables "Additional Filters" section in collection items view (STAC Browser silently hides the section if this returns 404)
- [x] CQL2-text parser `_parse_cql2_text()` — handles `variable = 'ssh'`, `A AND B`, `A OR B`, `NOT (A)` as generated by STAC Browser GET requests on collection items view
- [x] CQL2 NOT filter — `_CQL2_OP_INVERT` dict inverts operators under negation; `CqlNot.toText()` fixed in stac-browser fork (`logical.js`) to emit `NOT (inner)` instead of dropping the operator
- [x] CQL2 AND with duplicate fields — `_parse_cql2_json` and `_parse_cql2_text` collect multiple conditions on the same field as `[(op,v1), (op,v2)]` tuple lists; `search_items` iterates them as separate AND clauses
- [x] CQL2 OR filter — values collected as plain lists `[v1, v2]`; `search_items` detects plain vs tuple lists and emits `IN (?, ?)` SQL; `_collection_matches` uses `any()` for OR matching
- [x] Collection badge injection — `_inject_item_links` inserts the collection ID as the first keyword in item properties; STAC Browser renders keywords as colored chips, giving a visual collection indicator on item cards; also added as a Vue badge in stac-browser `Item.vue` (fork)
- [x] CLI tests — 31 CLI tests covering all four commands (`scan`, `serve`, `info`, `export`) added in `tests/test_cli.py`
- [ ] JSON-LD vocabulary links (deferred to Phase 5)
- [ ] User documentation: `docs/api_and_browser.md` — federation config, `esm-catalog serve` usage, STAC Browser URL pattern, supported filter syntax

### Phase 4: HPC Features
- [ ] Tape state detection (`hpc/state.py` — dmattr, scoutfs)
- [ ] Batch scanning with SLURM (Snakemake + `--config` context passing)
- [ ] Rate limiting
- [ ] Recall initiation
- [ ] Pytest tests: `tests/test_hpc.py` — dmattr/scoutfs mocks, rate-limiter throttle, recall initiation
- [ ] User documentation: `docs/hpc_batch_scanning.md` — Snakemake workflow, `scan-batch` + `merge-parquet` recipe, `--rate-limit`/`--checkpoint`/`--resume` flags, storage tier reference table

### Phase 5: Hardening
- [ ] Unstructured grid representation (FESOM — see Open Questions)
- [x] ECHAM GRIB support (`scan/grib.py`):
  - `_extract_dimensions_grib()` — builds `cube:dimensions` from all open hypercube datasets; handles temporal, spatial (lat/lon/vertical), spectral (`values`), and ordinal axes
  - paramId=0 expansion — ECHAM `_accw`/`_co2` files store all parameters under paramId=0; cfgrib collapses them to a single "unknown" variable; when a `.codes` table is available, that entry is expanded into one variable per codes table parameter (all share the same grid/dimensions)
  - `CollectionContextError(ValueError)` in `scan/context.py` — non-outdata paths (work/, restart/, input/, etc.) now caught at DEBUG level, not ERROR; genuine errors still log at ERROR
- [ ] `hpc-storage` extension spec document (currently undocumented custom extension)
- [ ] Checkpoint/resume for interrupted batch scans
- [ ] Pytest tests: `tests/test_scan_grib.py` — ECHAM GRIB fixtures; `tests/test_scan_unstructured.py` — FESOM mesh datacube representation
- [ ] User documentation: `docs/supported_formats.md` — NetCDF, GRIB, unstructured grid caveats; update `hpc-storage` extension spec with full field definitions

### Phase 6: Data Portal & Self-Registration (Proposed)

> Pending review and approval — see [Data Portal & Self-Registration](#data-portal--self-registration-proposal) section.

- [ ] `esm-catalog register` command — atomic append to `registry.json` with file locking; `--registry`, `--name`, `--description`, `--owner` flags
- [ ] `esm-catalog deregister` command — remove own entry from registry by path
- [ ] Registry-aware `DuckDBCatalogClient` — reads `registry.json` per request when `--registry` flag is given to `esm-catalog serve`
- [ ] Department port conventions documented — reserved ranges for Paleo / Ocean / Atmosphere / etc.
- [ ] Apptainer image (`stac-browser.sif`) built and verified — portal on :23000, STAC Browser at `/stac/`
- [ ] `run-portal.sh` / SLURM job script for always-on portal
- [ ] End-to-end test: `register` → catalog appears in STAC Browser without API restart
- [ ] User documentation: `docs/data_portal.md` — register/deregister commands, department registry paths, SSH tunnel instructions

---

## Open Questions

1. **ECHAM GRIB support** — Substantially addressed: `_extract_dimensions_grib()` populates `cube:dimensions` from all hypercube datasets; paramId=0 expansion recovers variable names for `_accw`/`_co2` files via the companion `.codes` table. Remaining gap: variables whose paramId is non-zero but not in the standard eccodes tables appear as "unknown" within mixed datasets (e.g. the `regular_gg+surface` hypercube of the main `_echam` file contains some unrecognised parameters). These residual unknowns do not block catalog construction — they simply appear as `unknown` in `cube:variables` alongside properly-named variables.

2. **Unstructured grids** - FESOM uses unstructured mesh. How to represent in datacube extension?

3. **Restart files** - Catalog them? Separate collection? Ignore?

4. **Derived data** - User-computed anomalies, regridded data. How to track provenance?

5. **Annotations** - "Don't use this run, ocean crashed" - where does this go?

---

*Document created: 2025-03-08*
*Based on architecture discussion between Paul Gierz and Claude*
