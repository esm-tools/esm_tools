# Changes

## esm_parser: section validation

**Problem:** If a config file contains an unknown top-level key (e.g. `foo: bar`), the error raised is a `KeyError` with a traceback that doesn't name the offending file, making it hard to debug.

**Fix:** Added `validate_config_sections()` to `esm_parser` that checks all top-level keys are known sections (e.g. `setup`, `model`, `other`, `system`). If an unknown key is found, a coloured error is raised naming the offending file. The `yaml_file_to_dict()` function now tracks the `general.sections` list to support this validation. For this feature the following changes were made:

1. **Section validator** — `validate_config_sections()` checks all top-level config keys are known sections; raises a coloured error naming the offending file. `yaml_file_to_dict` now tracks `general.sections` to support this.
2. **`get_components()` refactor** — `include_system` bool replaced with `include=["setup","model","other","system"]`; returns a `set`. All 19 file-op call sites now use `include=["model"]` to exclude `dask` and `general`.
3. **`valid_components` → `other_components`** — renamed in `general.yaml` and `nemo.yaml`.
4. **`sim_objects.py`** — extends `system_components` with `prev_objects` so prior-chunk objects pass section validation.
5. **`esm_tests`** — YAML key-order normalisation before diff comparison; machine-agnostic `NAMELIST_PATH`/`RUNSCRIPT_PATH` substitutions in truth files.
6. **New tests + docs** — `test_yaml_section_validation.py`; "Sections" added to `yaml.rst` and `esm_variables.rst`.

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
