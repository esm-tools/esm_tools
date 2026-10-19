# SimulationSetup → Simulation + ConfigBuilder/RunState Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace `SimulationSetup` with `Simulation`, extracting its config-building work into `ConfigBuilder`/`SimulationConfig` (shared by both `wf_engine` values) and `RunStateBuilder`/`RunState` (pplan-only, per-invocation facts), with zero behavior change to the `legacy` path and zero regression to the already-landed pplan engine.

**Architecture:** Two new Builder/Product pairs in a new module (`ConfigBuilder`→`SimulationConfig`, `RunStateBuilder`→`RunState`), each method a thin wrapper calling one existing `config_initialization.py`/`prepare.py`/`chunky_parts.py`/`last_minute.py`/`conda_env.py`/`batch_system.py` function in today's exact order. `Simulation.from_config` replaces `SimulationSetup.__init__`'s branching; `Simulation.run` replaces `__call__`. Four call sites updated to match.

**Tech Stack:** Python 3.10, pydantic (unrelated to this plan — `plan.py` untouched), pytest.

**Spec:** `docs/superpowers/specs/2026-10-19-simulation-config-refactor-design.md`

## Global Constraints

- Zero behavior change to the `legacy` `wf_engine` path (spec Non-goals). `legacy` must still call `prepare.run_job` wholesale, unchanged, never building a `RunState`.
- Zero regression to the six already-landed pplan-engine commits' tests (`tests/test_esm_runscripts/test_plan.py`) — that file is not touched by this plan.
- `ConfigBuilder.from_runscript(path)` must work standing alone, zero arguments beyond the path, no manufactured defaults (spec Anti-goals).
- `RunState` is `pplan`-only; nothing in this plan writes `RunState` values back into the shared config dict (spec: no sync method, no dual-write). `RunStateBuilder.from_config` must pass a **copy** of the config data it reads from (`copy.deepcopy`), not the same object `SimulationConfig` holds — the spec's own flagged "sharp edge."
- Every `ConfigBuilder`/`RunStateBuilder` `.with_*()` method wraps exactly one existing function, called with the same arguments, in the same order as today's `SimulationSetup.__init__`/`prepare.run_job` recipe. No reimplementation of any wrapped function's logic in this plan.
- This machine's local venv has a broken build toolchain (duplicate `LC_RPATH` after a from-source `psutil` rebuild) that blocks `pytest` from importing `esm_runscripts` at all (triggers on `esm_runscripts/__init__.py`'s `dask_cluster` import). Every task's test step gives a `pytest` command *and* an isolated-module-exec fallback (same pattern used throughout this branch's earlier work: `importlib.util.spec_from_file_location` + `sys.modules` registration, bypassing the package `__init__.py`). Try `pytest` first; if it fails with the known `psutil`/`LC_RPATH` `ImportError`, use the fallback and say so explicitly — never silently skip verification.

## Left-to-the-builder calls made during planning

(Spec explicitly said these don't need another round with Paul.)

- **Module layout**: `ConfigBuilder`/`SimulationConfig`/`RunStateBuilder`/`RunState` all live in a new `src/esm_runscripts/simulation_config.py`, mirroring the existing `workflow.py`/`plan.py` split.
- **Job dispatch naming**: `SimulationSetup.__call__` becomes `Simulation.run` (not `.dispatch()`, not a `Job` class) — matches the spec's own placeholder, no reason found to change it.
- **Tail steps' home** (`add_submission_info`, `initialize_batch_system`, `initialize_coupler`, `set_logfile`, `check_config_for_warnings_errors`): methods on `Simulation` itself, not `RunStateBuilder`. Reasoning: they create live stateful objects (a batch-system handle, a coupler object) — `RunState` is documented as plain per-invocation *facts* (dates, paths), and stuffing live handles into it would violate that definition on its first real use.
- **Plugin resolution placement**: not implemented in this plan. `pplan`'s step execution is still a logging stub (`plan.run_steps`) with no real plugin calls to resolve; deferred until real step execution exists.
- **`ConfigBuilder.from_user_config`**: added as a second classmethod, not in the spec's original sketch. The call-site audit (Task 1) found `src/esm_master/esm_master.py:65` constructs `SimulationSetup(user_config=user_config)` — the *other* branch `SimulationSetup.__init__` already supports, which the spec's `from_runscript`-only sketch didn't cover. This completes coverage of both existing entry paths rather than opening a new design question.

## Call-site audit (done during planning, not a task)

`grep -rln "SimulationSetup" --include="*.py" .` found 8 files. Four are real references needing updates:

| File | Current call | Keeps using |
|---|---|---|
| `src/esm_runscripts/cli.py:248` | `SimulationSetup(command_line_config=command_line_config)` | `.plan`, `.workflow`, `.config`, `setup()` |
| `src/esm_master/esm_master.py:65` | `SimulationSetup(user_config=user_config)` | `.config` only |
| `src/esm_runscripts/resubmit.py:47` | `SimulationSetup(command_line_config)` (positional) | `.config`, `setup_obj(kill_after_submit=False)` |
| `tests/test_esm_catalog/fixtures/fake_experiment/resolver.py:112` | `SimulationSetup(command_line_config=command_line_config).config` | — |

Four are false positives / docs-only, not touched by this plan: `src/esm_runscripts/workflow.py:210,221,377` (the string `"SimulationSetup"` is a `batch_or_shell` tag *value*, unrelated to the class); `src/esm_master/general_stuff.py:61` and `src/esm_runscripts/prev_run.py:16` are docstring prose mentions only.

## File Structure

- Create: `src/esm_runscripts/simulation_config.py` — `ConfigBuilder`, `SimulationConfig`, `RunStateBuilder`, `RunState`.
- Create: `tests/test_esm_runscripts/test_simulation_config.py` — tests for all four classes above.
- Modify: `src/esm_runscripts/sim_objects.py` — rename `SimulationSetup` → `Simulation`; replace `__init__` with `Simulation.from_config` classmethod using `simulation_config.py`; rename `__call__` → `run`; everything else (`prepcompute`/`tidy`/`viz`/`observe`/`assembler`/`inspect`/`store_prev_objects`) untouched.
- Modify: `src/esm_runscripts/cli.py:248-253` — construct via `ConfigBuilder`/`Simulation.from_config`; `setup()` → `.run()`.
- Modify: `src/esm_master/esm_master.py:63-65` — construct via `ConfigBuilder.from_user_config`/`Simulation.from_config`.
- Modify: `src/esm_runscripts/resubmit.py:45-75,180-181` — construct via `ConfigBuilder`/`Simulation.from_config`; `setup_obj(...)` → `.run(...)`; `submission_type == "SimulationSetup"` string check stays as-is (it's a `batch_or_shell` tag value, not the class — see audit table).
- Modify: `tests/test_esm_catalog/fixtures/fake_experiment/resolver.py:96-112` — construct via `ConfigBuilder`/`Simulation.from_config`.

## Review Focus

- **`ConfigBuilder.from_runscript(path)` called with zero overrides** — must not raise `KeyError` from a missing CLI key; every downstream function it calls must tolerate a one-key `command_line_config` dict (spec Anti-goals explicitly names this).
- **`esm_master`'s `user_config=` path, which never had a runscript path or most CLI keys at all** — `ConfigBuilder.from_user_config` must not assume any CLI-sourced key exists, since `esm_master` never built one.
- **`legacy` `wf_engine` with a value *other than* `"legacy"` or `"pplan"`** (e.g. unset, or a typo) — `Simulation.from_config`'s branch must default to `legacy` behavior (today's code does: `(command_line_config or {}).get("wf_engine", "legacy")`), not silently take the `pplan` branch or raise.
- **`resubmit.py`'s positional-argument call site** (`SimulationSetup(command_line_config)`, no keyword) — the new construction path must support this call shape too, not just the keyword forms used elsewhere.
- **`RunStateBuilder` built from a `SimulationConfig` whose underlying dict gets mutated afterward by other code** — since `RunStateBuilder.from_config` must deepcopy (Global Constraints), a later mutation to the original `SimulationConfig` must not retroactively change an already-built `RunState`. Test this explicitly, since it's exactly the bug the deepcopy requirement exists to prevent.

---

### Task 1: `ConfigBuilder` and `SimulationConfig`

**Files:**
- Create: `src/esm_runscripts/simulation_config.py`
- Test: `tests/test_esm_runscripts/test_simulation_config.py`

**Interfaces:**
- Consumes: `config_initialization.get_user_config_from_command_line(command_line_config)`, `.init_interactive_info(user_config, command_line_config)`, `.init_iterative_coupling(command_line_config, user_config)`, `.get_total_config_from_user_config(user_config)`, `.add_esm_runscripts_defaults_to_config(config)`, `.check_account(config)`, `.complete_config_with_inspect(config)`, `.save_command_line_config(config, command_line_config)` — all existing, all `(dict, ...) -> dict`.
- Produces: `ConfigBuilder` with classmethods `from_runscript(path, **overrides) -> ConfigBuilder` and `from_user_config(user_config) -> ConfigBuilder`; methods `with_interactive_info()`, `with_iterative_coupling()`, `with_execution_mode_default()`, `expand_from_esm_tools_configs()`, `with_runscript_defaults()`, `validate_account()`, `with_inspect_info()`, `remember_command_line()` (all `(self) -> self`); `build() -> SimulationConfig`. `SimulationConfig(data)` with a `.data` property exposing the underlying dict (read-only by convention, not enforced — matches how `esm_parser.ConfigSetup` is used everywhere else in this codebase).

- [ ] **Step 1: Write the failing tests**

```python
# tests/test_esm_runscripts/test_simulation_config.py
import copy
from esm_runscripts.simulation_config import ConfigBuilder, SimulationConfig

def test_from_runscript_with_zero_overrides_does_not_raise(tmp_path):
    runscript = tmp_path / "test.yaml"
    runscript.write_text("general:\n  setup_name: dummy\n  expid: test\n")
    builder = ConfigBuilder.from_runscript(str(runscript))
    assert isinstance(builder, ConfigBuilder)

def test_from_runscript_passes_overrides_into_command_line_config(tmp_path):
    runscript = tmp_path / "test.yaml"
    runscript.write_text("general:\n  setup_name: dummy\n  expid: test\n")
    builder = ConfigBuilder.from_runscript(str(runscript), task="prepare")
    assert builder._command_line_config["jobtype"] == "prepare" or \
        builder._command_line_config.get("task") == "prepare"
    # exact key name depends on get_user_config_from_command_line's contract;
    # implementer confirms by reading that function, test asserts whichever
    # key cli.py's command_line_config dict actually uses for task (see cli.py)

def test_from_user_config_wraps_dict_directly():
    user_config = {"general": {"setup_name": "dummy", "expid": "test"}}
    builder = ConfigBuilder.from_user_config(user_config)
    assert isinstance(builder, ConfigBuilder)

def test_build_returns_simulation_config():
    builder = ConfigBuilder.from_user_config({"general": {"setup_name": "dummy"}})
    result = builder.build()
    assert isinstance(result, SimulationConfig)
    assert result.data["general"]["setup_name"] == "dummy"

def test_with_methods_return_self_for_chaining():
    builder = ConfigBuilder.from_user_config({"general": {"setup_name": "dummy", "expid": "t"}})
    assert builder.with_execution_mode_default() is builder
```

(Full chain test with every `.with_*()` method against a real runscript fixture — reuse `tests/test_esm_runscripts/data/` if a suitable one exists, else write a minimal one — is the implementer's call; the five tests above pin the contract, the rest is regression coverage once the body exists.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `pytest tests/test_esm_runscripts/test_simulation_config.py -v`
Expected: FAIL with `ModuleNotFoundError: No module named 'esm_runscripts.simulation_config'`
Fallback if `pytest` hits the known `psutil`/`LC_RPATH` `ImportError`: isolated exec — `importlib.util.spec_from_file_location("simulation_config", "src/esm_runscripts/simulation_config.py")`, register in `sys.modules`, run the same assertions as plain script asserts. Confirm `ImportError: No such file` since the module doesn't exist yet.

- [ ] **Step 3: Implement `ConfigBuilder` and `SimulationConfig` in `src/esm_runscripts/simulation_config.py`**

Mirror `SimulationSetup.__init__`'s steps 2-10 (`sim_objects.py:65-106`, current numbering) exactly, one `.with_*()` method per step, mutate `self._data` in place and `return self` (spec's locked decision — no new object per step). `from_runscript(path, **overrides)` builds `command_line_config = {"runscript_abspath": path, **overrides}` and calls `get_user_config_from_command_line` (spec's locked `ConfigBuilder` sketch, Anti-goals: no manufactured defaults). `from_user_config(user_config)` skips that call entirely — mirrors `SimulationSetup.__init__`'s `if not user_config:` branch (`sim_objects.py:66-69`): when `user_config` is given directly, `get_user_config_from_command_line` never runs. Store `self._command_line_config = command_line_config` (or `{}` for the `from_user_config` path, since `remember_command_line`/`with_interactive_info`/`with_iterative_coupling` all accept `command_line_config` and the original code passes `command_line_config` — possibly `None` — through unchanged; confirm each wrapped function's `None`-handling by reading it, matching today's exact behavior when `command_line_config` is `None`).

- [ ] **Step 4: Run tests to verify they pass**

Run: `pytest tests/test_esm_runscripts/test_simulation_config.py -v` (or isolated-exec fallback)
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/esm_runscripts/simulation_config.py tests/test_esm_runscripts/test_simulation_config.py
git commit -m "feat(esm_runscripts): add ConfigBuilder/SimulationConfig"
```

---

### Task 2: `RunStateBuilder` and `RunState`

**Files:**
- Modify: `src/esm_runscripts/simulation_config.py`
- Modify: `tests/test_esm_runscripts/test_simulation_config.py`

**Interfaces:**
- Consumes: `SimulationConfig` (Task 1); `prepare._read_date_file`, `.check_model_lresume`, `.resolve_some_choose_blocks`, `._initialize_calendar`, `._add_all_folders`, `.set_prev_date`, `.set_parent_info`, `.finalize_config`; `chunky_parts._update_run_in_chunk`, `.set_chunk_calendar`; `last_minute.apply_last_minute_changes`; `conda_env.set_launched_with_conda`; `batch_system.find_openmp` — all existing, all `(dict) -> dict`.
- Produces: `RunStateBuilder` with classmethod `from_config(config: SimulationConfig) -> RunStateBuilder`; methods `with_date_file()`, `with_run_in_chunk_updated()`, `with_lresume_checked()`, `with_last_minute_changes()`, `with_conda_env_recorded()`, `with_openmp_found()`, `with_choose_blocks_resolved()`, `with_calendar()`, `build() -> RunState`. `RunState(data)` with a `.data` property, same shape as `SimulationConfig`.

- [ ] **Step 1: Write the failing tests**

```python
def test_run_state_builder_from_config_does_not_mutate_original(tmp_path):
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    builder = RunStateBuilder.from_config(config)
    builder._data["general"]["expid"] = "mutated"
    assert config.data["general"]["expid"] == "test"  # the deepcopy requirement from Global Constraints

def test_run_state_builder_build_returns_run_state():
    config = ConfigBuilder.from_user_config(
        {"general": {"setup_name": "dummy", "expid": "test", "valid_model_names": []}}
    ).build()
    run_state = RunStateBuilder.from_config(config).build()
    assert isinstance(run_state, RunState)
```

(Per-step tests calling each `.with_*()` method against a representative config -- e.g. confirming `with_date_file()` populates `run_number`/`current_date`, matching `prepare._read_date_file`'s documented behavior -- are the implementer's call once the fixture data needed to exercise each wrapped function is in place; the two tests above pin the one contract Paul and the spec actually require: the deepcopy.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `pytest tests/test_esm_runscripts/test_simulation_config.py -v -k run_state` (or isolated-exec fallback)
Expected: FAIL with `ImportError`/`NameError: RunStateBuilder`

- [ ] **Step 3: Implement `RunStateBuilder` and `RunState` in `src/esm_runscripts/simulation_config.py`**

`from_config(config)` does `cls(copy.deepcopy(config.data))` — the deepcopy is non-negotiable (Global Constraints, spec's flagged sharp edge). Method order matches `prepare`'s recipe order exactly, through `finalize_config` (everything up to but not including `install_missing_plugins`, deferred per the Left-to-the-builder call above): `_read_date_file`, `_update_run_in_chunk`, `check_model_lresume`, `apply_last_minute_changes`, `set_launched_with_conda`, `find_openmp`, `resolve_some_choose_blocks` (called once here), `_initialize_calendar`, `set_chunk_calendar`, `resolve_some_choose_blocks` (called again -- `with_choose_blocks_resolved()` is called twice by the caller, matching the recipe's own double-entry, not two different methods), `_add_all_folders`, `set_prev_date`, `set_parent_info`, `finalize_config`. Each method is one line: call the wrapped function with `self._data`, assign back, `return self`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `pytest tests/test_esm_runscripts/test_simulation_config.py -v` (or isolated-exec fallback)
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/esm_runscripts/simulation_config.py tests/test_esm_runscripts/test_simulation_config.py
git commit -m "feat(esm_runscripts): add RunStateBuilder/RunState, pplan-only"
```

---

### Task 3: `Simulation` replaces `SimulationSetup`

**Files:**
- Modify: `src/esm_runscripts/sim_objects.py`
- Create: `tests/test_esm_runscripts/test_simulation.py`

**Interfaces:**
- Consumes: `ConfigBuilder`/`SimulationConfig` (Task 1), `RunStateBuilder`/`RunState` (Task 2), `plan.Workflow.resolve(config)` (existing, unchanged), `plan.SubPlan`/`plan.Step`/`plan.run_steps` — **no longer used by `Simulation`** (spec: `prepare` no longer exists as a `SubPlan`), `prepare.run_job(config)` (existing, unchanged, `legacy` only), `prev_run.PrevRunInfo(config)` (existing, unchanged).
- Produces: `Simulation` with classmethod `from_config(config: SimulationConfig) -> Simulation`, attributes `.config` (always), `.run_state`/`.plan` (only when `wf_engine == "pplan"`), method `run(kill_after_submit=True)` (renamed from `__call__`) plus all unchanged helper methods (`prepcompute`, `tidy`, `viz`, `observe`, `assembler`, `inspect`, `store_prev_objects`).

- [ ] **Step 1: Write the failing tests**

```python
# tests/test_esm_runscripts/test_simulation.py
from esm_runscripts.simulation_config import ConfigBuilder
from esm_runscripts.sim_objects import Simulation

def _minimal_config(wf_engine="legacy"):
    return ConfigBuilder.from_user_config({
        "general": {
            "setup_name": "dummy", "expid": "test", "valid_model_names": [],
            "wf_engine": wf_engine, "jobtype": "prepare",
        }
    }).build()

def test_from_config_legacy_never_builds_run_state(monkeypatch):
    import esm_runscripts.prepare as prepare
    monkeypatch.setattr(prepare, "run_job", lambda config: config)
    sim = Simulation.from_config(_minimal_config("legacy"))
    assert not hasattr(sim, "run_state")
    assert not hasattr(sim, "plan")

def test_from_config_unknown_wf_engine_defaults_to_legacy(monkeypatch):
    import esm_runscripts.prepare as prepare
    monkeypatch.setattr(prepare, "run_job", lambda config: config)
    sim = Simulation.from_config(_minimal_config(wf_engine=None))
    assert not hasattr(sim, "plan")  # must take the legacy branch, not pplan, on a missing/falsy wf_engine

def test_run_is_the_renamed_dispatch_method():
    assert hasattr(Simulation, "run")
    assert not hasattr(Simulation, "__call__")
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `pytest tests/test_esm_runscripts/test_simulation.py -v` (or isolated-exec fallback)
Expected: FAIL — `Simulation` not defined (still named `SimulationSetup`), no `from_config` classmethod.

- [ ] **Step 3: Implement `Simulation` in `src/esm_runscripts/sim_objects.py`**

Rename the class `SimulationSetup` → `Simulation`. Replace `__init__` (current lines 16-133) with:

```python
class Simulation(object):
    @classmethod
    def from_config(cls, config: "SimulationConfig") -> "Simulation":
        self = cls()
        self.config = config.data
        self.config["prev_run"] = prev_run.PrevRunInfo(self.config)
        self.store_prev_objects()

        wf_engine = self.config.get("general", {}).get("wf_engine") or "legacy"
        if wf_engine == "pplan":
            self.run_state = RunStateBuilder.from_config(config)...build()  # implementer chains the methods from Task 2, same order
            self.plan = plan.Workflow.resolve(self.config)
            self.config = self._run_pplan_tail_steps(self.config)  # see Task 4
        else:
            self.config = prepare.run_job(self.config)

        self.config["general"]["esm_tools_version"] = __version__
        return self

    def run(self, kill_after_submit=True):
        ...  # body unchanged from today's __call__, lines 135-179 current numbering
```

Note the `wf_engine` default changed from `(command_line_config or {}).get("wf_engine", "legacy")` to `self.config.get("general", {}).get("wf_engine") or "legacy"` — `command_line_config` is no longer available inside `Simulation.from_config` (it's `ConfigBuilder`'s concern now), but `ConfigBuilder.remember_command_line` already stored it at `config["general"]["command_line_config"]`, and `cli.py`/callers set `wf_engine` into the same `command_line_config` dict today (`cli.py:223`) — confirm during implementation that `wf_engine` ends up reachable at `self.config["general"]["wf_engine"]` after `ConfigBuilder`'s chain runs (trace `save_command_line_config`'s exact write path if not obvious), and adjust the lookup path in this step if it lands somewhere else. This is the one place today's exact data shape must be re-confirmed, not assumed.

Leave `prepcompute`/`tidy`/`viz`/`observe`/`assembler`/`inspect`/`store_prev_objects` (current lines 183-262) untouched — only the class name changes for them (inherited, no per-method edit needed).

- [ ] **Step 4: Run tests to verify they pass**

Run: `pytest tests/test_esm_runscripts/test_simulation.py -v` (or isolated-exec fallback)
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/esm_runscripts/sim_objects.py tests/test_esm_runscripts/test_simulation.py
git commit -m "refactor(esm_runscripts): SimulationSetup -> Simulation, builds config via ConfigBuilder/RunStateBuilder"
```

---

### Task 4: Tail steps on `Simulation` (pplan-only `add_submission_info`/`initialize_batch_system`/`initialize_coupler`/`set_logfile`/`check_config_for_warnings_errors`)

**Files:**
- Modify: `src/esm_runscripts/sim_objects.py`
- Modify: `tests/test_esm_runscripts/test_simulation.py`

**Interfaces:**
- Consumes: `prepare.add_submission_info`, `.initialize_batch_system`, `.initialize_coupler`, `.set_logfile`, `.check_config_for_warnings_errors` — all existing, all `(dict) -> dict`.
- Produces: `Simulation._run_pplan_tail_steps(config) -> dict`, called from `Simulation.from_config`'s `pplan` branch (Task 3's sketch references this method name).

- [ ] **Step 1: Write the failing test**

```python
def test_pplan_tail_steps_called_in_recipe_order(monkeypatch):
    calls = []
    import esm_runscripts.prepare as prepare
    for name in ["add_submission_info", "initialize_batch_system", "initialize_coupler",
                 "set_logfile", "check_config_for_warnings_errors"]:
        monkeypatch.setattr(prepare, name, lambda config, name=name: (calls.append(name), config)[1])
    Simulation._run_pplan_tail_steps({"general": {}})
    assert calls == ["add_submission_info", "initialize_batch_system", "initialize_coupler",
                      "set_logfile", "check_config_for_warnings_errors"]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `pytest tests/test_esm_runscripts/test_simulation.py -v -k tail_steps` (or isolated-exec fallback)
Expected: FAIL — `_run_pplan_tail_steps` not defined.

- [ ] **Step 3: Implement `Simulation._run_pplan_tail_steps(config)` in `src/esm_runscripts/sim_objects.py`**

One line per call, same five functions in the exact order they appear in `prepare`'s recipe after `assemble_workflow` (`configs/esm_software/esm_runscripts/esm_runscripts.yaml`): `add_submission_info`, `initialize_batch_system`, `initialize_coupler`, `set_logfile`, `check_config_for_warnings_errors`. `restore_protected_last_minute_changes` sits between `assemble_workflow` and `add_submission_info` in the legacy recipe — confirm during implementation whether `pplan`'s `last_minute`-changes handling (Task 2's `with_last_minute_changes`) already covers what this step restores, or whether it needs a sixth call here; read `restore_protected_last_minute_changes`'s body before deciding, since the spec doesn't resolve this and it wasn't in the original 5/7-step count discussed.

- [ ] **Step 4: Run test to verify it passes**

Run: `pytest tests/test_esm_runscripts/test_simulation.py -v` (or isolated-exec fallback)
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/esm_runscripts/sim_objects.py tests/test_esm_runscripts/test_simulation.py
git commit -m "feat(esm_runscripts): wire pplan workflow-setup tail steps into Simulation"
```

---

### Task 5: Update the four real call sites

**Files:**
- Modify: `src/esm_runscripts/cli.py:248-253`
- Modify: `src/esm_master/esm_master.py:63-65`
- Modify: `src/esm_runscripts/resubmit.py:45-75,180-181`
- Modify: `tests/test_esm_catalog/fixtures/fake_experiment/resolver.py:96-112`

**Interfaces:**
- Consumes: `simulation_config.ConfigBuilder` (Task 1), `sim_objects.Simulation` (Task 3).

- [ ] **Step 1: Update `cli.py`**

Replace `setup = SimulationSetup(command_line_config=command_line_config)` with:
```python
config = ConfigBuilder.from_runscript(
    command_line_config["runscript_abspath"], **command_line_config
).with_interactive_info().with_iterative_coupling().with_execution_mode_default() \
 .expand_from_esm_tools_configs().with_runscript_defaults().validate_account() \
 .with_inspect_info().remember_command_line().build()
setup = Simulation.from_config(config)
```
Replace `setup()` (current `cli.py:257`) with `setup.run()`. Everything reading `setup.plan`/`setup.workflow`/`setup.config` (the `drop_into_pplan_debug` call, current `cli.py:251-252`) stays as-is — `Simulation` still exposes `.plan`/`.config` (Task 3); confirm whether callers expect `.workflow` too (current code has both `setup.plan` and `setup.workflow` passed to `drop_into_pplan_debug` — Task 3's sketch only names `.plan`; re-check `cli.py`'s exact current call before editing and keep both attributes if both are read).

- [ ] **Step 2: Update `esm_master.py`**

Replace `complete_setup = SimulationSetup(user_config=user_config)` with:
```python
config = ConfigBuilder.from_user_config(user_config) \
    .with_interactive_info().with_iterative_coupling().with_execution_mode_default() \
    .expand_from_esm_tools_configs().with_runscript_defaults().validate_account() \
    .with_inspect_info().remember_command_line().build()
complete_setup = Simulation.from_config(config)
```
(`complete_config = complete_setup.config` on the next line needs no change.)

- [ ] **Step 3: Update `resubmit.py`**

Replace `setup_obj = SimulationSetup(command_line_config)` (positional) with the same `ConfigBuilder.from_runscript(...).build()` chain as `cli.py`, then `setup_obj = Simulation.from_config(config)`. Replace `setup_obj(kill_after_submit=False)` with `setup_obj.run(kill_after_submit=False)`.

- [ ] **Step 4: Update the test fixture**

Replace `config = SimulationSetup(command_line_config=command_line_config).config` with the equivalent builder chain, `.build()`, then `Simulation.from_config(...).config`.

- [ ] **Step 5: Run the full existing test suite to check for regressions**

Run: `pytest tests/ -v` (or, if the known `psutil`/`LC_RPATH` `ImportError` blocks collection entirely: isolated-exec each of `tests/test_esm_runscripts/test_plan.py`, `test_simulation_config.py`, `test_simulation.py` individually, same pattern as every earlier step, and say explicitly that the full-suite run was not possible here)
Expected: PASS, including `tests/test_esm_runscripts/test_plan.py` unchanged (Global Constraints: zero regression).

- [ ] **Step 6: Manual smoke test on a real cluster** (per spec Verification — this machine cannot run `esm_runscripts` end-to-end)

```bash
esm_runscripts <runscript> -e <expid>                      # legacy, confirm unchanged behavior
esm_runscripts <runscript> -e <expid> --wf-engine=pplan     # confirm sim.plan is the resolved Workflow, no SubPlan("prepare")
```

- [ ] **Step 7: Commit**

```bash
git add src/esm_runscripts/cli.py src/esm_master/esm_master.py src/esm_runscripts/resubmit.py tests/test_esm_catalog/fixtures/fake_experiment/resolver.py
git commit -m "refactor: update all SimulationSetup call sites to ConfigBuilder + Simulation.from_config"
```
