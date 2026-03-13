# Quick Start: Catalog an Experiment and Browse it in STAC Browser

This guide walks through building a STAC catalog for `basic-001` and viewing it in the
radiantearth STAC Browser.

---

## Prerequisites

Activate the conda environment where `esm-catalog` is installed:

```bash
conda activate esm_catalog
esm-catalog --help   # should print the command group
```

---

## Step 1: Scan the experiment outdata

Point `esm-catalog scan` at the experiment's `outdata/` directory.
Pass the `finished_config.yaml` symlink so the scanner knows the experiment ID and
can resolve context more reliably (it will fall back to path parsing without it, but
the config is always more accurate).

```bash
EXP=/albedo/work/user/pgierz/SciComp/Tutorials/AWIESM_Basics/experiments/basic-001

esm-catalog scan $EXP/outdata \
    --db ~/basic-001.duckdb \
    --config $EXP/config/basic-001_finished_config.yaml
```

What this does:
- Recursively finds all `.nc` files (FESOM) and extension-less GRIB files (ECHAM)
- Extracts spatial/temporal metadata from each file
- Groups them into collections by `(experiment, component)`, e.g. `basic-001-fesom`
- Writes everything to `~/basic-001.duckdb`

Expected output (a few minutes, ~179 files):

```
14:05:22 | INFO     | add_files: 71/71 files cataloged for basic-001-fesom
14:06:11 | INFO     | add_files: 108/108 files cataloged for basic-001-echam
```

> **Note:** If a file fails with `UnsupportedFormatError` it is silently skipped.
> Run with `--verbose` to see which files were skipped and why.

---

## Step 2: Verify the catalog

Quick sanity check with DuckDB directly — no server needed:

```python
import duckdb

db = duckdb.connect("/albedo/home/pasili001/basic-001.duckdb", read_only=True)

# How many items total?
print(db.execute("SELECT COUNT(*) FROM items").fetchone())

# Collections created
print(db.execute("SELECT id FROM collections").fetchdf())

# First few items
print(db.execute("""
    SELECT id, collection, datetime
    FROM items
    ORDER BY datetime
    LIMIT 5
""").fetchdf())
```

You should see two collections: `basic-001-fesom` and `basic-001-echam`.

---

## Step 3: Start the API server

```bash
esm-catalog serve \
    --catalog ~/basic-001.duckdb \
    --host 0.0.0.0 \
    --port 8000
```

The server starts at `http://albedo0:8000`. You should see:

```
INFO:     Started server process [...]
INFO:     Uvicorn running on http://0.0.0.0:8000 (Press CTRL+C to quit)
```

Test it is responding:

```bash
curl -s http://localhost:8000/collections | python -m json.tool | grep '"id"'
```

---

## Step 4: Open in STAC Browser

The server is on the AWI internal network. To view it in a browser on your local
machine you need an SSH port forward:

```bash
# Run this in a new terminal on your LOCAL machine
ssh -L 8000:albedo0:8000 albedo
```

Then open this URL in your local browser (the `%2F` is a URL-encoded `/`):

```
https://radiantearth.github.io/stac-browser/#/search/external/localhost:8000
```

You should land on the landing page showing the API root. From there:

- Click **Collections** → you see `basic-001-fesom` and `basic-001-echam`
- Click a collection → browse the items (individual output files)
- Click **Search** → use the **Search for Collections** tab to filter by experiment,
  or the **Search for Items** tab to filter by datetime range

---

## Federate multiple experiments (optional)

To serve several experiments together, pass multiple `--catalog` flags:

```bash
esm-catalog serve \
    --catalog ~/basic-001.duckdb \
    --catalog ~/basic-002.duckdb \
    --port 8000
```

All collections from both catalogs appear in one API. STAC Browser navigates them as
a single federated catalog.

---

## Troubleshooting

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| `ValueError: Cannot resolve collection context` | Path doesn't contain `experiments/.../outdata/` and no `--config` given | Pass `--config` |
| File scanned but item missing | Zero-byte file or unsupported magic bytes | Run with `-v` to see skipped files |
| STAC Browser shows empty catalog | CORS blocked | Confirm `--host 0.0.0.0` is set |
| SSH tunnel connection refused | Port forward not active | Open a new terminal and run the `ssh -L` command |
