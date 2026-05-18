# ESM Catalog CLI Reference

`esm-catalog` is a command-line tool for building and querying STAC-based catalogs of ESM-Tools
experiment output (NetCDF, GRIB).

## Installation

```bash
conda activate esm_catalog          # or whichever env has esm-tools installed
esm-catalog --help
```

## Global Options

```
esm-catalog [--verbose] COMMAND [ARGS]...
```

| Option | Description |
|--------|-------------|
| `--verbose` / `-v` (on the group, before the command) | Enable DEBUG-level logging |

---

## Commands

### `scan` — Catalog a file or directory

Scans one file or a directory tree and inserts STAC Items into a DuckDB catalog.
The experiment name and model component are resolved automatically from the path
(`…/experiments/{experiment}/outdata/{component}/…`), or from a
`finished_config.yaml` if provided.

**Intermediate directories for `--db` are created automatically.**

```
esm-catalog scan PATH --db DB [--config CONFIG]
```

| Argument / Option | Description |
|-------------------|-------------|
| `PATH` | File (`.nc`, `.grb`, …) or directory to scan recursively |
| `--db DB` | Path to `catalog.duckdb` (created with all parent dirs if absent) |
| `--config CONFIG` | ESM-Tools `finished_config.yaml` (optional; improves context resolution) |

#### Examples

**Scan a single FESOM output file:**
```bash
esm-catalog scan ssh.fesom.185001.01.nc \
    --db ~/experiments/basic-001/catalog.duckdb
```

**Scan an entire `outdata/fesom` directory:**
```bash
esm-catalog scan \
    /albedo/work/user/pgierz/SciComp/Tutorials/AWIESM_Basics/experiments/basic-001/outdata/fesom \
    --db ~/basic-001/catalog.duckdb
```

**Scan with verbose logging to see every item inserted:**
```bash
esm-catalog --verbose scan \
    /work/user/experiments/picontrol/outdata/echam \
    --db ~/picontrol/catalog.duckdb
```

**Scan using ESM-Tools config for richer context (e.g. PI name, ORCID):**
```bash
esm-catalog scan /work/user/experiments/historical/outdata/ \
    --db ~/historical/catalog.duckdb \
    --config /work/user/experiments/historical/finished_config.yaml
```

---

### `scan-batch` — Parallel scan → Parquet (for SLURM)

Scans a list of files **in parallel** using joblib and writes a Parquet staging
file instead of writing directly to DuckDB. Designed for SLURM array jobs where
multiple tasks scan different file batches concurrently.

```
esm-catalog scan-batch FILE [FILE ...] --config CONFIG --output OUTPUT [--jobs N]
```

| Option | Description |
|--------|-------------|
| `FILE …` | One or more file paths to scan |
| `--config CONFIG` | ESM-Tools `finished_config.yaml` (required for batch mode) |
| `--output OUTPUT` | Output `.parquet` staging file |
| `--jobs N` | Number of parallel workers (default: 4) |

#### Examples

**Scan a batch of files and write to Parquet:**
```bash
esm-catalog scan-batch \
    /work/user/exp/outdata/fesom/ssh.fesom.185001.01.nc \
    /work/user/exp/outdata/fesom/ssh.fesom.185002.01.nc \
    /work/user/exp/outdata/fesom/ssh.fesom.185003.01.nc \
    --config finished_config.yaml \
    --output staging/batch_001.parquet \
    --jobs 8
```

**Typical SLURM array job pattern:**

```bash
# Submit: one task per batch of files
sbatch --array=0-99 scan_array.sh
```

```bash
# scan_array.sh
#!/bin/bash
FILES=$(sed -n "${SLURM_ARRAY_TASK_ID}p" file_batches.txt)
esm-catalog scan-batch $FILES \
    --config finished_config.yaml \
    --output staging/batch_${SLURM_ARRAY_TASK_ID}.parquet \
    --jobs 4
```

---

### `merge-parquet` — Merge Parquet staging files into DuckDB

Serial final step after parallel `scan-batch` jobs. Reads all Parquet files,
creates collections, and inserts all items into a single `catalog.duckdb`.

```
esm-catalog merge-parquet FILE [FILE ...] --output DB [--config CONFIG]
```

| Option | Description |
|--------|-------------|
| `FILE …` | One or more `.parquet` staging files |
| `--output DB` | Destination `catalog.duckdb` |
| `--config CONFIG` | ESM-Tools config (optional; used for collection metadata) |

#### Examples

**Merge all staging files after a SLURM scan:**
```bash
esm-catalog merge-parquet staging/batch_*.parquet \
    --output ~/historical/catalog.duckdb \
    --config finished_config.yaml
```

---

### `serve` — Launch the STAC API server *(Phase 3)*

Starts a `stac-fastapi` HTTP server backed by one or more DuckDB catalogs.
Multiple catalogs are federated transparently (queried as one).

```
esm-catalog serve --catalog DB [--catalog DB ...] [--host HOST] [--port PORT]
```

| Option | Default | Description |
|--------|---------|-------------|
| `--catalog DB` | — | Path to `catalog.duckdb` (repeat for federation) |
| `--host HOST` | `0.0.0.0` | Bind address |
| `--port PORT` | `8000` | Bind port |

#### Examples

**Serve a single experiment catalog:**
```bash
esm-catalog serve --catalog ~/basic-001/catalog.duckdb
# API available at http://localhost:8000
```

**Federate catalogs from multiple users/experiments:**
```bash
esm-catalog serve \
    --catalog /work/ab1234/experiments/picontrol/catalog.duckdb \
    --catalog /work/cd5678/experiments/historical/catalog.duckdb \
    --port 8080
```

---

## Typical Workflows

### Single experiment, interactive scan

```bash
# 1. Scan (creates catalog.duckdb and all parent dirs automatically)
esm-catalog scan /work/user/exp/picontrol/outdata/ \
    --db ~/picontrol/catalog.duckdb

# 2. Serve
esm-catalog serve --catalog ~/picontrol/catalog.duckdb
```

### Large experiment, SLURM batch scan

```bash
# 1. Split file list into batches (e.g. 100 files per batch)
find /work/user/exp/historical/outdata/ -name "*.nc" \
    | split -l 100 - staging/batch_

# 2. Run parallel scan jobs (one per batch file)
for batch in staging/batch_*; do
    esm-catalog scan-batch $(cat $batch) \
        --config finished_config.yaml \
        --output staging/$(basename $batch).parquet \
        --jobs 4 &
done
wait

# 3. Serial merge
esm-catalog merge-parquet staging/*.parquet \
    --output ~/historical/catalog.duckdb
```

---

## Querying the catalog directly with DuckDB

While the API server provides STAC-compliant HTTP access, you can also query
`catalog.duckdb` directly with Python or the DuckDB CLI:

```python
import duckdb, json

db = duckdb.connect("~/basic-001/catalog.duckdb", read_only=True)

# List all collections
db.execute("SELECT id FROM collections").fetchall()

# List items by variable
rows = db.execute("""
    SELECT id, json_extract(data, '$.properties.datetime') AS dt
    FROM items
    WHERE json_extract(data, '$.properties.variable') = '"ssh"'
    ORDER BY dt
""").fetchall()

# List all variables available in the catalog
db.execute("""
    SELECT DISTINCT value FROM collection_item_props
    WHERE property = 'variable'
    ORDER BY value
""").fetchall()
```

```bash
# Or with the DuckDB CLI
duckdb ~/basic-001/catalog.duckdb \
    "SELECT id, collection FROM items LIMIT 10"
```

---

## Path convention for automatic context resolution

`esm-catalog` resolves the **experiment name** and **model component** from the file
path without needing a config file, as long as the path follows the ESM-Tools
output directory convention:

```
…/experiments/{experiment}/outdata/{component}/file.nc
```

Examples that are auto-resolved:

| Path | experiment | component | collection |
|------|-----------|-----------|------------|
| `.../experiments/basic-001/outdata/fesom/ssh.fesom.185001.01.nc` | `basic-001` | `fesom` | `basic-001-fesom` |
| `.../experiments/picontrol/outdata/echam/temp.echam.195001.nc` | `picontrol` | `echam` | `picontrol-echam` |

If the path does not match this convention, pass `--config finished_config.yaml`
to provide the context explicitly. If neither resolves, the scan raises an error
rather than inserting with a NULL collection (which would silently break catalog
navigation).
