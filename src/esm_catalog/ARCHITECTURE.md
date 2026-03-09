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
│   └── detect.py             # Auto-detect format, dispatch to scanner
│
├── stac/
│   ├── __init__.py
│   ├── item.py               # metadata dict → STAC Item dict
│   ├── collection.py         # Aggregate items → STAC Collection
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
└── api/
    ├── __init__.py
    └── server.py             # stac-fastapi backed by DuckDB
```

---

## Core Flow

### Scanning (CLI)

```python
# What happens when you run: esm-catalog scan file.grb

metadata = scan_grib(path)           # scan/grib.py - extract variables, time, bbox
item = make_item(path, metadata)     # stac/item.py - build STAC Item dict
item = add_hpc_extension(item, path) # stac/extensions/hpc.py - add tape state
db.insert(item)                      # storage/duckdb.py - store in catalog.duckdb
```

### Serving (API)

```python
# stac-fastapi with DuckDB backend

class DuckDBClient(AsyncBaseCoreClient):
    async def post_search(self, search):
        query = "SELECT data FROM items WHERE 1=1"
        if search.collections:
            query += " AND collection IN (?)"
        if search.datetime:
            query += " AND datetime BETWEEN ? AND ?"
        # DuckDB queries JSON natively
        rows = self.db.execute(query, params).fetchall()
        return {"type": "FeatureCollection", "features": [r[0] for r in rows]}

api = StacApi(client=DuckDBClient("catalog.duckdb"))
app = api.app  # uvicorn esm_catalog.api.server:app
```

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

SELECT * FROM picontrol.items
UNION ALL
SELECT * FROM historical.items
WHERE json_extract(data, '$.properties.variable') = 'tas';
```

### DuckDB Schema

```sql
CREATE TABLE items (
    id TEXT PRIMARY KEY,
    collection TEXT,
    datetime TIMESTAMP,
    bbox DOUBLE[4],
    data JSON  -- Full STAC Item, query with json_extract()
);

CREATE TABLE collections (
    id TEXT PRIMARY KEY,
    data JSON  -- Full STAC Collection
);

-- Indexes for common queries
CREATE INDEX idx_collection ON items(collection);
CREATE INDEX idx_datetime ON items(datetime);
CREATE INDEX idx_variable ON items(json_extract(data, '$.properties.variable'));
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
    input: lambda wc: get_batch_files(wc.batch_id)
    output: "staging/batch_{batch_id}.parquet"
    resources:
        runtime=10,
        mem_mb=4000
    shell:
        "esm-catalog scan-batch {input} --output {output}"

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
- `hpc:last_access` - ISO timestamp

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
| [file](https://github.com/stac-extensions/file) | File size, checksum |
| [contacts](https://github.com/stac-extensions/contacts) | Authors, ORCID |
| [scientific](https://github.com/stac-extensions/scientific) | DOI, citations |
| [hpc-storage](./hpc-storage/) | Tape state, recall time (custom) |

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
| `/` | GET | Landing page / root catalog |
| `/collections` | GET | List all collections |
| `/collections/{id}` | GET | Single collection |
| `/collections/{id}/items` | GET | Items in collection |
| `/search` | GET/POST | Query items |
| `/docs` | GET | Swagger UI |

Custom query parameters:
- `variable` - Filter by variable name
- `experiment` - Filter by experiment
- `model` - Filter by model component

---

## Dependencies

**Core:**
- `duckdb` - Storage and query
- `pystac` - STAC object model
- `xarray` - Read NetCDF/GRIB
- `cfgrib` / `eccodes` - GRIB support

**API:**
- `stac-fastapi` - STAC API framework
- `uvicorn` - ASGI server

**CLI:**
- `click` / `rich-click` - CLI framework
- `loguru` - Logging

**Batch processing:**
- `snakemake` - Workflow orchestration
- `pyarrow` - Parquet I/O

---

## Collaboration Notes (Pavan's Work)

Pavan (siligam) has built:
- Working STAC catalog generator (`build_catalog.py`)
- STAC API via stac-fastapi (`stac_api.py`)
- Snakemake integration

**What to keep:**
- stac-fastapi approach
- STAC hierarchy (Catalog → Collection → Item)
- Datacube + CF extensions

**What to add:**
- DuckDB backend (replace JSON files)
- HPC storage extension
- GRIB support
- Clean module structure

**Collaboration approach:**
1. Talk to Pavan about what he learned
2. Agree on architecture (this document)
3. Refactor together, keeping what works
4. Add missing pieces (GRIB, HPC, DuckDB)

---

## Phase Plan

### Phase 1: Core (MVP)
- [ ] Clean module structure
- [ ] GRIB + NetCDF scanning
- [ ] DuckDB storage
- [ ] Basic CLI

### Phase 2: ESM-Tools Integration
- [ ] Tidy phase hook
- [ ] Experiment config in metadata
- [ ] Auto-catalog on run completion

### Phase 3: API + Browser
- [ ] stac-fastapi with DuckDB backend
- [ ] STAC Browser integration
- [ ] JSON-LD vocabulary links

### Phase 4: HPC Features
- [ ] Tape state detection
- [ ] Batch scanning with SLURM
- [ ] Rate limiting
- [ ] Recall initiation

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
