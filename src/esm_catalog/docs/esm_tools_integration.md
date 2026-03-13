# ESM-Tools Integration

`esm_catalog` integrates with ESM-Tools in two ways:

1. **Live path** — called from the tidy phase immediately after a run completes; the file list is
   known, no filesystem scan needed.
2. **Batch path** — after-the-fact scan of an existing experiment via the CLI.

---

## Live Path: Catalog During the Tidy Phase

### Enabling cataloging in a run script

Add the following snippet to your ESM-Tools run script (Python) after the tidy phase has moved
files to `outdata/`:

```python
from pathlib import Path
from esm_catalog import add_files

# experiment_config is the resolved config dict already available in the tidy phase
catalog_db = Path(experiment_config["general"]["experiment_dir"]) / "catalog.duckdb"

# Get the files that were written to outdata by this run
from esm_catalog.integration.config import get_outdata_files

for component in ["echam", "fesom", "jsbach"]:
    files = get_outdata_files(experiment_config, component)
    if files:
        add_files(catalog_db, files, experiment_config)
```

Or more concisely, catalog all components in one call:

```python
from esm_catalog import add_files

all_files = []
skip = {"general", "computer", "defaults", "recom"}
for key, block in experiment_config.items():
    if key in skip or not isinstance(block, dict):
        continue
    targets = (block.get("outdata_targets") or {})
    all_files.extend(targets.values())

add_files(catalog_db, all_files, experiment_config)
```

### `add_files()` API reference

```python
from esm_catalog.integration.esm_tools import add_files

n = add_files(
    db="/path/to/catalog.duckdb",   # created if absent
    files=[Path("output.nc"), ...], # iterable of paths
    experiment_config=config,       # dict with at minimum config["general"]["expid"]
)
# Returns: int — number of items successfully inserted
```

**Arguments:**

| Argument | Type | Description |
|---|---|---|
| `db` | `str \| Path` | Path to the catalog DuckDB file. Parent directories are created automatically. |
| `files` | `Iterable[Path \| str]` | Output files to catalog. Symlinks are resolved; duplicate real paths and zero-byte files are skipped. |
| `experiment_config` | `dict` | ESM-Tools config dict. Must contain `config["general"]["expid"]`. Component outdata directories are used to map files to collections. |

**Returns:** number of items successfully inserted (failed files are logged and skipped).

---

## Batch Path: Scan an Existing Experiment

For experiments that ran before cataloging was enabled:

```bash
# Scan all output files in an experiment directory
esm-catalog scan /work/user/experiments/basic-001/outdata/ \
    --db /work/user/experiments/basic-001/catalog.duckdb \
    --config /work/user/experiments/basic-001/config/basic-001_finished_config.yaml
```

The `--config` option provides richer collection context (experiment ID, component mapping).
Without it, context is inferred from the path structure (`.../experiments/{exp}/outdata/{comp}/...`).

---

## `finished_config.yaml` — Keys Used

ESM-Tools writes one config file per run period:

```
{expid}_finished_config.yaml_{YYYYMMDD}-{YYYYMMDD}
```

and maintains a symlink `{expid}_finished_config.yaml` → latest run.

### Keys read by `esm_catalog`

| Key path | Purpose |
|---|---|
| `config["general"]["expid"]` | Experiment ID → collection prefix (e.g. `basic-001-echam`) |
| `config["general"]["scenario"]` | Stored as `scenario` item property |
| `config["general"]["resolution"]` | Stored as `resolution` item property |
| `config["general"]["run_datestamp"]` | Identifies the run period |
| `config["general"]["lresume"]` | Whether this was a restart run |
| `config[component]["experiment_outdata_dir"]` | Used to map files to component collections |
| `config[component]["thisrun_outdata_dir"]` | Fallback outdata dir lookup |
| `config[component]["outdata_targets"]` | Dict of `{stream: absolute_path}` — the exact files produced in this run |
| `config[component]["metadata"]["Institute"]` | Component institution (e.g. MPI-Met, AWI) |
| `config[component]["metadata"]["Authors"]` | Component authors string |
| `config[component]["metadata"]["Description"]` | Model description |

> **Note:** `config[component]["outdata_dir"]` is `None` in real finished_config files —
> ESM-Tools resolves the actual paths into `experiment_outdata_dir` and `outdata_targets`.

---

## Helper Functions

### `find_finished_configs(experiment_dir)`

Find all per-run finished_config files for an experiment:

```python
from esm_catalog.integration.config import find_finished_configs

configs = find_finished_configs("/work/user/experiments/basic-001")
# Returns sorted list:
# [PosixPath('.../basic-001_finished_config.yaml_18500101-18500131'),
#  PosixPath('.../basic-001_finished_config.yaml_18500201-18500228'),
#  PosixPath('.../basic-001_finished_config.yaml_18500301-18500331')]
```

Useful for back-cataloging an entire multi-run experiment:

```python
from esm_catalog.integration.config import find_finished_configs, load_config
from esm_catalog import add_files

for config_path in find_finished_configs("/work/user/experiments/basic-001"):
    config = load_config(config_path)
    all_files = [
        p for comp in ["echam", "fesom", "jsbach"]
        for p in get_outdata_files(config, comp)
    ]
    add_files(catalog_db, all_files, config)
```

### `get_outdata_files(config, component)`

Extract the list of output files for a component from a loaded config:

```python
from esm_catalog.integration.config import get_outdata_files, load_config

config = load_config("basic-001_finished_config.yaml_18500101-18500131")
echam_files = get_outdata_files(config, "echam")
# Returns: [PosixPath('/work/.../basic-001_185001.01_echam'), ...]
```

### `extract_stac_metadata(config)`

Extract experiment and component metadata for STAC enrichment:

```python
from esm_catalog.integration.config import extract_stac_metadata, load_config

config = load_config("basic-001_finished_config.yaml")
meta = extract_stac_metadata(config)
# {
#   "expid": "basic-001",
#   "scenario": "PI-CTRL",
#   "resolution": "T63_CORE2",
#   "setup_name": "awiesm",
#   "setup_version": "2.1",
#   "run_datestamp": "18500101-18500131",
#   "lresume": False,
#   "components": {
#     "echam": {
#       "version": "6.3.05p2-awiesm-2.1",
#       "institute": "MPI-Met",
#       "authors": "Bjorn Stevens ...",
#       "description": "The ECHAM atmosphere model, major version 6",
#       "publications": "https://doi.org/10.1002/jame.20015",
#     },
#     ...
#   }
# }
```

### `load_config(path)`

Load any ESM-Tools config file (with or without `.yaml` extension):

```python
from esm_catalog.integration.config import load_config

# Symlink to latest run:
config = load_config("basic-001_finished_config.yaml")

# Specific run period (no .yaml extension — still valid YAML):
config = load_config("basic-001_finished_config.yaml_18500101-18500131")

# None-safe:
config = load_config(None)  # returns None
```

---

## Collection Naming

Collections are named `{expid}-{component}`, e.g. `basic-001-echam`.

The component name is derived from the top-level key in the config whose
`experiment_outdata_dir` is an ancestor of the file being cataloged. If no
config is provided (batch scan without `--config`), the component is inferred
from the `outdata/{component}/` path segment.
