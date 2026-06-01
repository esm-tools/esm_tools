# Changes

## esm_runscripts: exclude system components (dask) from per-model file operations

**Files:** `src/esm_runscripts/filelists.py`, `src/esm_runscripts/prepare.py`

**Problem:** Commit `009ca5f9` introduced `get_file_components()` to fix processing of `general` (a system component) in file operation loops. The function filtered out only `"general"`, so `dask` (also in `system_components`) was still included. This caused `_add_all_folders` in `prepare.py` and all file movement loops in `filelists.py` to add `experiment_*_dir`, `all_filetypes`, and `file_movements` to the `dask` config section, and also consumed/deleted `dask`'s `ignore_files`/`ignore_in_work` keys.

**Fix:** Removed `get_file_components()` entirely. All call sites now use `esm_parser.get_components(config, include_system=False)` directly, which returns only `valid_model_names` (no system components).



## esm_tests: normalize yaml key order before comparison

**File:** `src/esm_tests/output.py`

**Problem:** `finished_config.yaml` is written by `ruamel.yaml` (via `yaml_dump` in `dict_to_yaml.py`) which preserves Python dict insertion order. When the parsing code changes, key insertion order can shift, producing large diffs in `esm_tests -c` even when all values are identical.

**Fix:** Added `_sort_yaml_lines(lines)` helper that parses a yaml text (after provenance stripping) with `yaml.safe_load` and re-dumps with `sort_keys=True`. Called in `print_diff` for any `.yaml` file, on both the baseline and current file, just before the `difflib` comparison. Falls back silently to the original lines on any parse error. `yaml.safe_load` is safe here because `yaml_dump` converts all non-standard types (dates, batch systems, etc.) to plain strings before writing.

## esm_tests: add NAMELIST_PATH and RUNSCRIPT_PATH substitutions for machine-agnostic comparison

**File:** `src/esm_tests/cli.py`

**Problem:** The esm_tools installation path relative to `$HOME` differs between users and machines (e.g. `~/esm_tools` vs `~/Codes/esm_tools`). After `HOME_DIR` substitution, `config_sources` entries like `esm_namelist_dir` still differed between truth files and the current run.

**Fix:** Added `NAMELIST_PATH` and `RUNSCRIPT_PATH` entries to `rm_user_info` (sourced from `esm_tools.get_namelist_filepath()` and `esm_tools.get_runscript_filepath()`) placed **before** `HOME_DIR` in the dict so they are substituted first. Truth files in `last_tested/` must be regenerated on each machine with `esm_tests -s` to adopt the new placeholders.

## esm_tests: fix comp script generation for models with nested pushd paths

**File:** `src/esm_tests/tests.py`

**Root cause:** In check-mode compilation, `comp_test()` creates dummy empty directories to trick `esm_master` into thinking source code is present, allowing it to generate `comp-*_script.sh` files without actually compiling. The folder-collection logic filtered out any `pushd` path containing `/`:

```python
and "/" not in found_format[0]
```

For `foci-mops_lmu`, NEMO's compile step uses a nested path:

```
pushd nemo-ORCA05_LIM2_FOCI_MOPS_OASISMCT4/CONFIG/ORCA05_LIM2_FOCI_MOPS_OASISMCT4
```

Because this path contains `/`, the dummy directory was never created. When `esm_master` then ran without `-c`, it detected the missing folder and aborted — before generating any `comp-*_script.sh` files.

**Fix:**
- Removed the `"/" not in found_format[0]` condition so nested paths are collected.
- Changed `os.mkdir` to `os.makedirs(..., exist_ok=True)` so nested directories are created recursively.

Other models (oasis, echam, xios) were unaffected because their `pushd` paths are all single-level (no `/`). NEMO's FOCI-MOPS configuration requires the nested `CONFIG/` subdirectory.
