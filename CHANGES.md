# Changes

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
