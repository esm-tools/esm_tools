# ESM-Tools File Operations Redesign — Filedicts

This deliverable uses as its base the old GitHub project for the refactoring of file handling https://github.com/orgs/esm-tools/projects/12, that already included many of the current requirements and design considerations reflected in this document. It is also based in existing unfinished developments in `sprint/filedicts/main` branch that belongs to that old GitHub project. The deliverable has been written using @claude -code in planning mode, using the existing resources mentioned above and new prompted design considerations.

## Overview

ESM-Tools currently handles file operations during `esm_runscripts` through a fragmented system of parallel flat dictionaries (`input_files`, `input_sources`, `input_in_work`, `forcing_files`, etc.) distributed across multiple sections of
component configs. This approach is hard to read, difficult to extend, and couples file metadata to intermediate directory structures that add unnecessary complexity and file duplication.

This document describes the new design — **filedicts** — which unifies file definitions into structured objects, removes intermediate staging directories inside `run_DATE/`, and `run_DATE` itself in favor of `work_DATE` (one work per run at the root of the experiment dir), and simplifies the operation model, where each phase declares in the backend the file operation directions.

----

## Design

### Syntax

#### `files:` block

Partly taken from https://github.com/orgs/esm-tools/projects/12/views/1?pane=issue&itemId=8130162

Discussion in: [esm-tools/esm-design#2](https://github.com/esm-tools/esm-design/issues/2)

Files are defined in a `files:` block, grouped by type. The type is inferred from the group key: no `type:` attribute needed per file. Each group may contain a `defaults:` sub-block for shared attributes. Individual file entries can be scalars (shorthand) or full objects.

```yaml
files:
    input:
        defaults:
            path_in_pool: ${echam.input_dir}
            prepare: copy
        cldoptprops:                           # null → name = label in all locations
        janspec: janspec.nc                    # scalar → name_in_pool
        jansurf:                               # object → explicit overrides only
            name_in_pool: jansurf.nc
            name_in_run: unit.24
        rrtmglw: /other/pool/path/rrtmg.nc    # has "/" → full path, ignores defaults
    forcing:
        defaults:
            path_in_pool: ${echam.forcing_dir}
            prepare: link
        sst: pisst.nc
        sic:
            name_in_pool: pisic${current_date.year}.nc
            name_in_run: unit.96
            include_years_before: 1
    outdata:
        defaults:
            tidy: copy
        histogram:                             # null → name_in_run = label
        atm_data: atmosphere_output.nc         # scalar → name_in_run
    restart:
        defaults:
            prepare: copy
            tidy: copy
        jan_restart:
            name_in_run: restart.nc
```

File types are limitted to the current existing ones, and there will be a mechanism to control that the "types" are correct.

Note about the `null` values: we might want to avoid implementing this feature. Have None as the value (or in fact, anything that might be considered `false`-y) will make anything one does with comparison logic fragile. Also, tracking the provenance of the value might be challenging, but maybe not, as `None`s also have provenance in the current implementation. Maybe, it's a matter of internally swapping the `None` of the files with the key name, while preserving the `None`'s provenance. Placeholders with `${}` could be a problem, because they might not be resolved inside the keys. All this is something to consider during the implementation.

#### File object attributes

| Attribute | Description |
|---|---|
| `name_in_pool` | Filename in the pool/source location |
| `name_in_work` | Filename in `work_DATE/` (the working directory) |
| `name_in_exp` | Filename in the experiment tree (defaults to `name_in_run`) |
| `path_in_pool` | Path to the file in the pool (excluding filename) |
| `prepare_operation` | Operation for `pool/exp → run`: `copy`, `link`, `move` |
| `tidy_operation` | Operation for `run → exp/<type>`: `copy`, `link`, `move` |
| `include_timedelta_before` | For example, years before current to include (requires `StringWithDate`) |
| `include_timdelta_after` | For example, years after current to include (requires `StringWithDate`) |
| `description` | Human-readable description [optional] |
| `allowed_to_be_missing` | If `true`, missing file does not raise an error |
| `is_reusable` | if `true` copy from exp instead of from pool, like bins and inputs (default: false) |
| filetype | filetype, like NetCDF [optional], not sure if we should implement this attribute |

#### Files with varying paths depending on dates

For files whose paths change depending on dates the syntax will be:
```yaml
files:
    <file1_label>:
        description: "some string"
        path_in_pool:
            "/path/to/first/file${year}":
                   from: <year>
                   to: <year>
            "/path/to/second/file${year}":
                   from: <year>
```

#### File selection via `choose_`

All files defined in `files:` are active by default. Scenarios override specific attributes via `choose_`. Only the differing attributes need to be specified:

```yaml
files:
    forcing:
        defaults:
            path_in_pool: ${echam.forcing_dir}
        sst:
            name_in_pool: pisst.nc      # default: PI-CTRL
            name_in_run: unit.20

choose_scenario:
    historical:
        files:
            forcing:
                sst:
                    name_in_pool: histsst.nc    # only override what changes

    ssp585:
        add_files:                              # add a file not in base config
            forcing:
                ozone:
                    name_in_pool: ozone_ssp585.nc
                    name_in_run: ozone.nc
```

A reserved `include:` list mechanism per type could be implemented in the future if explicit whitelisting is needed, or perhaps it can be implemente also as an attribute of file objects.

#### Scalar shorthand rules direction-awareness:

In order to simplify the amount of writing needed for specifying file operations we allow for shorthand file definitions, consisting of a label (key) followed by a path (value). The path can be an absolute path (starting with `/`) or a relative path. The filedicts attribute that takes the value of the shorthand-path is determined by the source of file operations for that specific phase. For example:

- For `prepare` input-like file types `input`, `forcing`, `config` and `restart` the scalar is `name_in_pool`, or `name_in_exp`.
- For `tidy` output-like file types `outdata`, `restart`, `log`, and `mon` the scalar is `name_in_run`.

Other shorthand rules are:
- For `null` values the label is used as name in all relevant locations. The label cannot have placeholders (no `${}`).
- Plain string (no `/`) → filename only; path comes from `defaults.path_in_pool`
- String with `/` → full path, overrides `defaults.path_in_pool`

#### Date-varying files

Files whose names contain date variables resolve to `StringWithDate` objects,
produced by `esm_parser` (see esm_parser dependency below). The
`include_years_before/after` attributes instruct filedicts to expand the file
for multiple years around the given date on the string (if there is only one date, if there are several dates throws and error).

```yaml
files:
    forcing:
        defaults:
            path_in_pool: ${echam.forcing_dir}
            prepare: link
        ozone:
            name_in_pool: ozon${current_year}.nc
            name_in_run: ozon${current_year}.nc
            include_years_before: 1
            include_years_after: 0
```

If `include_years_before/after` is set but the name resolves to a plain `str`
(i.e. the string contained no date variable), filedicts raises a `FileDictError`.

#### `simulation_files` (removed)

The `simulation_files` / `<type>_files` selection mechanism is replaced by the
grouped `files:` block with Option C selection (all defined files active,
scenario-specific changes via `choose_`).

---

### File Locations

Three locations replace the previous multi-level structure:

| Location | Description |
|---|---|
| `pool` | Source on the HPC system — input data, forcing data |
| `run` | `run_DATE/` — the flat working directory during simulation |
| `exp` | Experiment tree — persistent storage, structure preserved |

**`run_DATE/` is now flat.** Intermediate staging directories (`run_DATE/input/`, `run_DATE/work/`, `run_DATE/outdata/`, etc.) will be removed. All files land directly in `run_DATE/`. The `exp` tree structure won't be changed (`exp/outdata/<component>/`, `exp/restart/<component>/`, etc.).

---

### File Operations

`file_movements` special key is entirely removed, no functionality depends on this key anymore. Instead, key/values of phase/file operation can be defined as attributes in the file dictionary:

```yaml
files:
    forcing:
        ozone:
            name_in_pool: ozon${current_year}.nc
            prepare: copy
```

The direction is defined in the phase itself as a system invariant, not user-configurable. This is subject to change in the future if needed. The `FileTypes` enum declares which phases apply to each type and the `exp` subdirectory name. Default operations (`copy`/`link`/`move`) per type are defined in `per_model_defaults` in the system defaults YAML.

| Phase | Direction | Applicable types |
|---|---|---|
| `prepare` | `pool/exp → run` | `input`, `forcing`, `config`, `restart` |
| `tidy` | `run → exp/<type>` | `outdata`, `restart`, `log`, `mon` |

Restart files use both phases: `prepare` stages the previous restart into `run/`, `tidy` archives the new restart to `exp/restart/`.

---

### `StringWithDate` (esm_parser dependency)

When `esm_parser` resolves a string containing a date variable (e.g. `${current_year}`), it produces a `StringWithDate` — a `str` subclass that also carries the `Date` object and the original template. This allows filedicts to re-resolve the string for year offsets without re-running the parser:

```python
class StringWithDate(str):
    # Behaves as a normal string — resolves to current date value
    # Also exposes:
    #   .for_year_offset(n: int) → StringWithDate
    #   .date → esm_calendar.Date
```

**This is an `esm_parser` task.** Filedicts consumes `StringWithDate`; it does not create it. This dependency must be resolved before date-varying file features can be implemented in filedicts.

----

## Major Changes

- [ ] Syntax rework: grouped `files:` block with `defaults:` and scalar
  shorthand
- [ ] Remove intermediate directories inside `run_DATE/`
- [ ] Replace `file_movements` with `prepare`/`tidy` per-file attributes
- [ ] Remove `simulation_files` / `<type>_files` file selection. If it is present in the config a file operation is needed.
- [ ] `StringWithDate` in `esm_parser`

---

## Features

- [ ] Report missing values. This feature should not change, except that if a globbing has 0 files then it should report a missing value https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8312692
- [ ] File type movements per file https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8307345
- [ ] Ignore files https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8308020
- [ ] _check_fesom_missing_files https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8308173
- [ ] Remove `@YEAR@` placeholder → `StringWithDate` + `include_years_before/after` https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8308849
- [ ] Calculate absolute paths for relative paths https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8309908
- [ ] Reuse sources https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8310094
- [ ] ~Include property (switch on/off files)~ Probably to not be implemented [#209](https://github.com/esm-tools/esm_runscripts/issues/209)
- [ ] `include_years_before/after` properties https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8312260
- [ ] `FileDictError` https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8312649
- [ ] Checksums computation https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8313953
- [ ] Unit test cleanup https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=8956628
- [ ] Paths of linked files should be shown in the finished config file https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=11438417
- [ ] Make sure the config is not populated with a huge amount of repeated key/values
- [ ] Parallelization of file operations https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=11632805&issue=esm-tools%7Cesm_tools%7C811
- [ ] Better merging strategy for from/to in files https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=21671561
- [ ] Support files with `.` (config_files.namelist.config) https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=23111901
- [ ] Allow missing wildcards https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=23198986
- [ ] define category file movements (or file operations) https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=30153684
- [ ] Add intermediate step into _gather_file_movements https://github.com/orgs/esm-tools/projects/12/views/1?sortedBy%5Bdirection%5D=desc&sortedBy%5BcolumnId%5D=Status&pane=issue&itemId=30207127
- [ ] more binary info in finished_config #990 

---

## Testing

### Unit tests

One unit test per feature, developed alongside each feature following the
red/green/refactor cycle (see Development Workflow below).

### Integration tests (CI)

AWI-ESM 2 and AWICM3 are the reference cases. For each, a dry-run produces the
full resolved filedicts state (all file attributes and absolute paths). This
state is asserted against expected snapshot values. Features are built
incrementally until the full dry-run state matches the real case scenario.
Runs in CI on every PR to `sprint/filedicts/main`.

### Checksum tests (esm_tests)

To verify the refactor is behaviour-preserving — identical files end up in
identical locations:

1. Compute checksums for all files moved/copied/linked on the `release` branch
2. Compute checksums for the same run on `sprint/filedicts/main`
3. Compare checksums for both `prepare` and `tidy` phases

Any mismatch is a regression. Checksum tests run via `esm_tests`.

---

## Retrocompatibility

The new syntax will substitute to the old syntax. The old syntax won't be supported anymore from the new release version. That means that old runscripts referencing the old file lists will have to be changed to work with the new version containing this refactoring. Same goes for the configs, however, the configs' syntax will be changed to the new one as part of this release, and include test runs with esm_tests. Resuming or branching off old simulations will still be possible because the file structure in `exp` won't change.

The users will be informed of such changes.

---

## Refactoring guidelines

### General

Taken from https://github.com/orgs/esm-tools/projects/12/views/1?pane=issue&itemId=8002017

- One task, one function
- Do not change the YAML syntax excessively
  - Runscripts and config files need minimal changes
  - Changes should make configs easier to write and understand
- All tests pass (unit tests, esm_tests, red/green/refactor cycle)
- Do not refactor a feature before fully understanding its behaviour and
  dependencies

### Development workflow per feature

1. New branch: `sprint/filedicts/<descriptive_name>`
2. Unit test development
3. PR draft
4. Feature development (push early and often)
5. Pull request to `sprint/filedicts/main`
6. Review
7. Merge

### Task groups

1. **Preparation** — codebase understanding, test infrastructure
2. **New syntax / functionality** — filedicts features
3. **Back-compatibility** — migration layer for old YAML syntax
4. **Deployment** — YAML file adaptation, release preparation

### Filedicts design guidelines

Taken from https://github.com/orgs/esm-tools/projects/12/views/1?pane=issue&itemId=8306419

- Internally, use `source`/`target` terminology once directions are resolved by
  phase
- Use `pathlib` instead of string path concatenation
- Use `esm_parser.user_error/user_note` for user-facing errors and warnings
  (higher-level functions only; lower-level functions should raise exceptions
  for testability)
- Write numpy-style docstrings
- Use `.get()` for extracting file properties
- `FileDictionary` class: file-specific logic, checks, attribute completion
     - file specific
     - checks
     - autocomplete info
- `FileDictionaries` class: general functionality, initialising file objects
    - general functionalities
    - initializing the file objects

### Naming conventions 

Taken from https://github.com/orgs/esm-tools/projects/12/views/1?pane=issue&itemId=8308503

Taken from "Clean Code with Python" (https://www.amazon.de/Clean-Code-Python-maintainable-efficient/dp/1800560214/ref=asc_df_1800560214/?tag=googshopde-21&linkCode=df0&hvadid=473997534442&hvpos=&hvnetw=g&hvrand=17247415010170938050&hvpone=&hvptwo=&hvqmt=&hvdev=c&hvdvcmdl=&hvlocint=&hvlocphy=9068390&hvtargid=pla-1124354993243&psc=1&th=1&psc=1). Ask Paul if you want to borrow it

* Functions that should be used "outside" have `regular_names`
* Functions that should only be used "inside" have `_private_names`

Python does not make explicit between public and private functions, but these guidelines are used elsewhere as well. I would denote "outside" as a step to be included in the run recipe, and "inside" as some small thing you just need, but whoever is designing a run recipe does not need to know about.
