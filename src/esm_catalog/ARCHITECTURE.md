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
│   └── server.py             # stac-fastapi backed by DuckDB, CORS middleware
│
└── browser/
    ├── README.md              # Fork relationship, build steps, upstream rebase guide
    │
    ├── config/                # Tier 1: config overrides (copy-in, no source edit)
    │   ├── config.js          # apiCatalogPriority, catalog URL, feature flags
    │   ├── fields.config.js   # HPC + datacube extension field labels/formatters
    │   ├── assetActions.config.js  # Enable xarray/Jupyter, disable irrelevant viewers
    │   ├── basemaps.config.js # Map projection defaults (OSM)
    │   └── theme/
    │       └── variables.scss # ESM-Tools color scheme / branding
    │
    ├── src/                   # Tier 2: net-new additions (drop into fork's src/)
    │   ├── components/
    │   │   └── PythonCodeBox.vue   # Syntax-highlighted pystac-client code snippets
    │   └── codegen/
    │       ├── PythonGenerator.js  # Context-aware OGC filter → Python code
    │       └── templates/
    │           ├── template.py
    │           └── template_collections.py
    │
    └── patches/               # Reference diffs only (Tier 3 lives as commits in the fork)
        ├── Catalog.vue.patch   # PythonCodeBox injection into catalog view
        ├── Item.vue.patch      # PythonCodeBox injection into item view
        └── ApiSearch.vue.patch # Search tabs (Collections / Items), searchType wiring
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
# stac-fastapi with DuckDB backend

class DuckDBClient(AsyncBaseCoreClient):
    def __init__(self, catalogs: list[str]):
        # ATTACH all readable catalog files at startup; federation is transparent to callers
        for path in catalogs:
            alias = Path(path).parent.name   # e.g. "picontrol"
            self.db.execute(f"ATTACH '{path}' AS {alias} (READ_ONLY)")

    async def post_search(self, search):
        query = "SELECT data FROM items WHERE 1=1"
        if search.collections:
            query += " AND collection IN (?)"
        if search.datetime:
            query += " AND datetime BETWEEN ? AND ?"
        if search.filter:                            # custom: experiment, variable, model
            query += " AND experiment = ?"
        # DuckDB queries JSON natively
        rows = self.db.execute(query, params).fetchall()
        return {"type": "FeatureCollection", "features": [r[0] for r in rows]}

# Catalogs list comes from ~/.esm_catalog.yaml
api = StacApi(client=DuckDBClient(catalogs=config["catalogs"]))
app = api.app

# CORS required: STAC Browser runs on a different origin from the API
from fastapi.middleware.cors import CORSMiddleware
app.add_middleware(CORSMiddleware, allow_origins=["*"], allow_methods=["GET", "POST"])

# uvicorn esm_catalog.api.server:app
```

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
| `/collections/{id}/items` | GET | Items in collection |
| `/search` | GET/POST | Query items — supports CQL2 filter |
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
# api/server.py — conformance classes required for both tabs to appear
CONFORMANCE = [
    # Item search
    "https://api.stacspec.org/v1.0.0/item-search",
    "https://api.stacspec.org/v1.0.0/item-search#filter",
    # Collection search — triggers the "Search for Collections" tab
    "https://api.stacspec.org/v1.0.0/collection-search",
    "https://api.stacspec.org/v1.0.0/collection-search#filter",
    # CQL2
    "http://www.opengis.net/spec/ogcapi-features-3/1.0/conf/features-filter",
]
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
(`cql2-text` or `cql2-json`). A dedicated parser in `api/server.py` translates CQL2
expressions into the property dict consumed by the DuckDB query layer:

```
filter=experiment='basic-001' AND model='fesom'
  →  {"experiment": ("=", "basic-001"), "model": ("=", "fesom")}
```

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

## STAC Browser Configuration

### Fork relationship

The STAC Browser fork is maintained as a **separate GitHub repository** under the esm-tools
organisation:

```
https://github.com/esm-tools/stac-browser   ← fork (custom commits live here)
https://github.com/radiantearth/stac-browser ← upstream
```

The fork is separate from the `esm_tools` repo by design — it is a JavaScript application with
its own release cycle, and keeping it separate avoids mixing Python package tooling with Node/npm
tooling in a single repo.

The local working copy (`~/repos/stacbrowser2/`) should have two remotes configured:

```bash
git remote add origin   https://github.com/esm-tools/stac-browser   # fork — push target
git remote add upstream https://github.com/radiantearth/stac-browser # upstream — rebase source
```

The fork was necessary because core upstream views (`Catalog.vue`, `Item.vue`) had to be modified
to inject climate-specific UI — context-aware Python code snippets generated from the catalog
hierarchy. Configuration files alone cannot achieve this.

`browser/` in this repo tracks only the **esm_catalog-specific** content (Tier 1 and Tier 2). It
is applied on top of the fork during deployment. Tier 3 changes (view modifications) live as
commits in the fork itself, not as patch files.

### Three tiers of customization

| Tier | What | Where | How deployed |
|---|---|---|---|
| 1 — Config overrides | Runtime settings, field labels, asset actions, theme | `browser/config/` in this repo | `cp -r browser/config/* stacbrowser2/` |
| 2 — Net-new additions | PythonCodeBox component, codegen layer | `browser/src/` in this repo | `cp -r browser/src/* stacbrowser2/src/` |
| 3 — View modifications | Commits to `Catalog.vue`, `Item.vue` | Commits in `esm-tools/stac-browser` fork | Already present — clone the fork |

Tier 3 changes live as named commits in the fork, not as patch files applied at deploy time. This
makes the fork self-contained: cloning it gives a working browser without any extra steps. The
trade-off is that upstream rebases must be done carefully to preserve those commits.

### Why `browser/` is a first-class deliverable

The API (`api/server.py`) alone does not close the loop. Without browser configuration:

| Without `browser/` | With `browser/` |
|---|---|
| `hpc:state: "nearline"` rendered as raw text | "On Tape (est. 5 min recall)" with icon |
| `hpc:storage_tier` not shown | Displayed as "Hot / Warm / Cold" badge |
| `cube:dimensions` fields raw JSON | Rendered as dimension table with units |
| No Python code snippet at any catalog level | Context-aware `pystac-client` snippet at every level |
| All asset viewers shown (Cesium, Potree, ...) | Only xarray/Jupyter action relevant to climate data |
| Default blue OSM basemap | ESM-Tools branded theme |

### Tier 1 — Config files

**`config.js`** — Runtime behavior:
```javascript
apiCatalogPriority: "childs",   // Preserve experiment → model hierarchy in sidebar
catalogUrl: "http://...",        // Point to esm-catalog API
displayGeoTiffByDefault: false,
showKeywordsInItemCards: true,
```

**`fields.config.js`** — How custom extension fields are labeled and formatted:
```javascript
{ name: "hpc:state",                label: "Storage State",        formatter: hpcStateFormatter },
{ name: "hpc:storage_tier",         label: "Storage Tier" },
{ name: "hpc:recall_time_estimate", label: "Est. Recall Time (s)" },
{ name: "hpc:facility",             label: "HPC Facility" },
{ name: "hpc:system",               label: "HPC System" },
// cube:dimensions and cube:variables handled natively by STAC Browser >= 3.x
```

**`assetActions.config.js`** — Enable only relevant asset viewers:
```javascript
// Enable: xarray code snippet action, Jupyter launch
// Disable: Cesium, CopcViewer, F3D, ThreePipe, GeoJsonIo, Protomaps
```

**`basemaps.config.js`** — OSM default; no planetary data so no multi-body config needed.

**`theme/variables.scss`** — Bootstrap variable overrides for ESM-Tools branding.

### Tier 2 — Net-new additions

**`PythonCodeBox.vue`** — Renders syntax-highlighted Python code at catalog, collection, and item
level. Accepts a `searchType` prop (`"items"` or `"collections"`) wired from `ApiSearch.vue`
based on the active tab, so the generated snippet matches what the active search mode does:
- `searchType="items"` → `pystac-client catalog.search(...)` snippet
- `searchType="collections"` → `requests.get(/collections, params={filter: ...})` snippet

Uses `highlight.js` for syntax highlighting; optionally calls a `/format` endpoint for
`black`-formatted output.

**`codegen/PythonGenerator.js`** — Two generation modes driven by `searchType`:

*Item mode* — context-aware `pystac-client` calls:
- Root catalog → generic `catalog.search()` call
- Experiment catalog → filter by `experiment`
- Component collection → filter by `experiment` + `component`
- Item → filter extracted from item properties

*Collection mode* — `formatCollectionFilters` builds a CQL2 params dict:
- Uses `template_collections.py` (not `template.py`) as the code template
- Generates `requests.get(api_url + "/collections", params={"filter": ...})` — because
  `pystac-client`'s `catalog.search()` targets items, not collections

### Tier 3 — View modifications (fork commits)

`Catalog.vue` and `Item.vue` modifications are committed directly to the `esm-tools/stac-browser`
fork. Current custom commits on top of upstream:

- `8016f41e` — Add Search for Collections tab with correct Python snippet and clean card UI
  *(modifies `src/views/ApiSearch.vue` — tab structure, `searchType` prop wiring, conformance detection)*
- `f91608bc` — Strip trailing slash from api_url for cleaner URL construction
- `963d2e9e` — Add PythonCodeBox to Catalog and Item views with context-aware filters
  *(modifies `src/views/Catalog.vue`, `src/views/Item.vue`)*

Keep `browser/patches/` updated as human-readable reference diffs (not used at deploy time):

```bash
cd stacbrowser2
git diff upstream/main src/views/Catalog.vue    > ../esm_catalog/browser/patches/Catalog.vue.patch
git diff upstream/main src/views/Item.vue        > ../esm_catalog/browser/patches/Item.vue.patch
git diff upstream/main src/views/ApiSearch.vue  > ../esm_catalog/browser/patches/ApiSearch.vue.patch
```

### Upstream rebase workflow

When `radiantearth/stac-browser` releases a new version:

```bash
cd stacbrowser2
git fetch upstream
git rebase upstream/main   # replay custom commits on top of new upstream
git push origin --force-with-lease
# Regenerate patches/ reference diffs afterward
```

### Build and deploy

```bash
# Clone the fork (Tier 3 already present as commits)
git clone https://github.com/esm-tools/stac-browser stacbrowser2

# Apply Tier 1 and Tier 2 overrides from this repo
cp -r browser/config/* stacbrowser2/       # -r required: theme/ is a subdirectory
cp -r browser/src/* stacbrowser2/src/

# Build
cd stacbrowser2 && npm install && npm run build
```

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
- [x] Pytest tests: `tests/test_integration.py` — 33 tests covering `load_config`, `find_finished_configs`, `get_outdata_files`, `extract_stac_metadata`, `experiment_outdata_dir` context resolution, and end-to-end `add_files()` coverage (159 total tests passing)
- [x] User documentation: `docs/esm_tools_integration.md` — how to enable cataloging in a run script, `add_files()` API reference, all three config helpers, `finished_config.yaml` keys used, collection naming convention

### Phase 3: API + Browser
- [ ] stac-fastapi architecture defined (DuckDB backend, federation, CORS)
- [ ] STAC Browser fork (`stacbrowser2`) with PythonCodeBox and context-aware codegen
- [ ] Three-tier browser customization (config, src additions, upstream patches)
- [ ] JSON-LD vocabulary links
- [ ] Deploy and serve
- [ ] Pytest tests: `tests/test_api.py` — STAC endpoint responses, CQL2 filter parsing, CORS headers, collection search conformance
- [ ] User documentation: `docs/api_and_browser.md` — federation config (`~/.esm_catalog.yaml`), `esm-catalog serve` usage, STAC Browser deployment steps, supported CQL2 filter syntax

### Phase 4: HPC Features
- [ ] Tape state detection (`hpc/state.py` — dmattr, scoutfs)
- [ ] Batch scanning with SLURM (Snakemake + `--config` context passing)
- [ ] Rate limiting
- [ ] Recall initiation
- [ ] Pytest tests: `tests/test_hpc.py` — dmattr/scoutfs mocks, rate-limiter throttle, recall initiation
- [ ] User documentation: `docs/hpc_batch_scanning.md` — Snakemake workflow, `scan-batch` + `merge-parquet` recipe, `--rate-limit`/`--checkpoint`/`--resume` flags, storage tier reference table

### Phase 5: Hardening
- [ ] Unstructured grid representation (FESOM — see Open Questions)
- [ ] ECHAM GRIB support
- [ ] `hpc-storage` extension spec document (currently undocumented custom extension)
- [ ] Checkpoint/resume for interrupted batch scans
- [ ] Pytest tests: `tests/test_scan_grib.py` — ECHAM GRIB fixtures; `tests/test_scan_unstructured.py` — FESOM mesh datacube representation
- [ ] User documentation: `docs/supported_formats.md` — NetCDF, GRIB, unstructured grid caveats; update `hpc-storage` extension spec with full field definitions

---

## Open Questions

1. **ECHAM GRIB support** - Pavan's code only handles FESOM NetCDF. GRIB + .codes files need work.

2. **Unstructured grids** - FESOM uses unstructured mesh. How to represent in datacube extension?

3. **Restart files** - Catalog them? Separate collection? Ignore?

4. **Derived data** - User-computed anomalies, regridded data. How to track provenance?

5. **Annotations** - "Don't use this run, ocean crashed" - where does this go?

---

*Document created: 2025-03-08*
*Based on architecture discussion between Paul Gierz and Claude*
