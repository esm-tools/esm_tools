# `SimulationSetup` → `Simulation` + `ConfigBuilder`/`SimulationConfig` refactor

Status: approved design, ready for implementation plan.

Supersedes the discussion-only sketch in
`/Users/pgierz/.claude/plans/fluttering-puzzling-abelson.md` (same session,
earlier day) — that sketch used a single fluent `Config` class with no
Builder/Product split and didn't yet account for the `wf_engine` branching
or the "`prepare` is blown-apart config-init" insight below. This document
is the canonical design going forward.

## Intent and why

`SimulationSetup` (`src/esm_runscripts/sim_objects.py:16-132`) is badly
named and does too much in one place: it both builds the experiment's
config (11 sequential free-function calls from `config_initialization.py`)
and dispatches job-type execution (`__call__`). While building the native
`--wf-engine=pplan` execution path (six commits already landed on this
branch: `plan.py`'s `Step`/`SubPlan`/`Plan`/`Workflow` models,
`Workflow.default`/`resolve`/`from_config`, `resubmit_triggers`,
`run_steps`), we found the same conflation one level down: the `prepare`
job type's 22-step recipe
(`configs/esm_software/esm_runscripts/esm_runscripts.yaml`) is ~14 steps of
pure config-init bookkeeping (dates, folders, calendar, plugin resolution)
followed by ~7 steps that are genuinely about workflow/submission setup
(`assemble_workflow`, `add_submission_info`, `initialize_batch_system`,
`initialize_coupler`, `set_logfile`, `check_config_for_warnings_errors`).
Treating `prepare` as a single `SubPlan` with 22 "steps" (today's pplan
code) dignifies config-init bookkeeping as workflow structure.

This refactor separates "building the config" from "being a running
simulation" into distinct, named classes, so that separation is visible
in the type system instead of buried in comments and a 22-item string
list.

**Independent review.** Before implementation, a fresh agent with zero
context on this codebase was briefed only on the domain problem (no-daemon
self-resubmission, multi-component join/fan-out, multi-source config,
per-invocation runtime facts, two coexisting execution strategies) and
asked for a greenfield clean-architecture opinion. It independently named
the separation this document needed and didn't yet have: static
`ResolvedConfig` (user YAML + framework defaults + CLI overrides) vs.
**`RunState`** (per-invocation facts — date, run/chunk number, output
paths, calendar — "the single most important boundary in this system"),
kept as genuinely separate types rather than smeared into one config
object. It also flagged that an execution-strategy branch (our
`wf_engine`) belongs behind one composition-time seam, not sprinkled as
conditionals through the code — not yet resolved in this document, left
open for a later pass (see Non-goals). Where the independent review's
`RunState` concept folds into this design is below, under `RunState`.

## Goals

- Replace `SimulationSetup` with `Simulation` — a class that *holds* a
  finished config and dispatches job-type execution, and does not itself
  build that config.
- Extract config-building into a `ConfigBuilder` (mutable, chainable,
  `.with_*()` methods, terminal `.build()`) that produces a `SimulationConfig`
  (the product — a real type, not a bare dict handed around).
- Extract the native engine's per-invocation runtime facts (dates,
  run/chunk number, output paths, calendar) into their own `RunState`
  type, built by its own `RunStateBuilder` from an already-built
  `SimulationConfig` — see `RunState` below for why this is `pplan`-only
  and not shared with `legacy`.
- Preserve both `wf_engine` paths (`legacy`, `pplan`) exactly as they
  behave today, just hosted on the new classes.
- No duplicated defaults: `ConfigBuilder.from_runscript` must not
  re-declare argparse's defaults from `parse_shargs()` (`cli.py`). It
  passes through whatever's given and relies on the existing config
  pipeline's own tolerance for sparse input (confirmed:
  `get_user_config_from_command_line` only requires
  `command_line_config["runscript_abspath"]`; `init_interactive_info`
  already has a `None`-safe branch).

## Non-goals

- Not changing `esm_parser.ConfigSetup` or anything in the `esm_parser`
  package. (Flagged during discussion: it's awkward that the actual
  config-holding class lives in a different package under a confusingly
  similar name; out of scope here, can't be changed as part of this pass.)
- Not changing any behavior of the `legacy` `wf_engine` path. `Simulation`
  must call `prepare.run_job` for `legacy` exactly as `SimulationSetup`
  does today — same opaque call, just on the new class.
- Not resolving the "`prepare`'s workflow-setup tail" steps
  (`assemble_workflow` onward) into `Workflow`/`Plan`/`SubPlan` model
  objects. Those five steps (`add_submission_info`,
  `initialize_batch_system`, `initialize_coupler`, `set_logfile`,
  `check_config_for_warnings_errors`) don't touch `plan.py`'s models at
  all — they're batch-system/coupler/logging setup, a separate concern
  from workflow structure. Where they land (see "Left to the builder").
- Not adding `__repr__`/`_repr_html_` notebook display support to any of
  these classes. Flagged as a good idea for later, once there's reason to
  actually use these objects interactively in Jupyter — not part of this
  pass.
- Not touching the "resolving a custom `workflow:` block natively" gap
  (`Workflow.resolve` already raises `NotImplementedError` for this,
  landed in commit `0475db6c7`) — unrelated to this refactor.
- Not building a single composition-time seam for the two `wf_engine`
  strategies (the independent review's `PhaseExecutionStrategy`-style
  port, selected once, with nothing downstream branching on which is
  active). `Simulation.from_config` still has a plain `if wf_engine ==
  "pplan":` branch in this pass. Flagged as a real improvement for later,
  not done now — this refactor is already reshaping enough at once.

## Anti-goals (would make this a failure even if it "works")

- A `ConfigBuilder`/`SimulationConfig` split that still requires every
  caller to hand-build a 21-key dict mimicking argparse's defaults to use
  `from_runscript` bare. The whole point of the split is that
  `ConfigBuilder.from_runscript(path)` works standing alone, with no CLI
  involved, no manufactured defaults.
- Silently changing what the `legacy` engine does. This refactor is a
  reshape of *where* code lives, not a behavior change to the thing most
  of the test suite and all current production runscripts exercise.
- Losing the `--wf-engine=pplan` native path's existing behavior (the six
  commits already on this branch). This refactor must integrate with that
  work, not redo or regress it.

## The design, as decided

### `ConfigBuilder`

Mutable, chainable. Every `.with_*()` method mutates `self._data` in
place and returns `self` (confirmed deliberately: no new object is
created per step; chaining is pure syntax sugar over calling the same
object's methods in sequence — this was checked against the alternative,
a "staged/typestate" builder with a new class per step enforcing call
order at the type level, and rejected as not worth 11 extra classes for a
pipeline with exactly one caller that always calls it in the right
order).

```python
class ConfigBuilder:
    def __init__(self, data, command_line_config):
        self._data = data
        self._command_line_config = command_line_config  # kept for remember_command_line

    @classmethod
    def from_runscript(cls, path, **overrides):
        command_line_config = {"runscript_abspath": path, **overrides}
        data = config_initialization.get_user_config_from_command_line(
            command_line_config
        )
        return cls(data, command_line_config)

    def with_interactive_info(self):
        self._data = config_initialization.init_interactive_info(self._data, {})
        return self

    def with_iterative_coupling(self):
        self._data = config_initialization.init_iterative_coupling({}, self._data)
        return self

    def with_execution_mode_default(self):
        mode = self._data.get("general", {}).get("execution_mode", "run")
        self._data["general"]["execution_mode"] = mode
        return self

    def expand_from_esm_tools_configs(self):
        self._data = config_initialization.get_total_config_from_user_config(self._data)
        return self

    def with_runscript_defaults(self):
        self._data = config_initialization.add_esm_runscripts_defaults_to_config(self._data)
        return self

    def validate_account(self):
        self._data = config_initialization.check_account(self._data)
        return self

    def with_inspect_info(self):
        self._data = config_initialization.complete_config_with_inspect(self._data)
        return self

    def remember_command_line(self):
        self._data = config_initialization.save_command_line_config(
            self._data, self._command_line_config
        )
        return self

    def build(self) -> "SimulationConfig":
        return SimulationConfig(self._data)
```

**`from_runscript`'s signature is the locked decision on where CLI
overrides land**, after two wrong turns worth recording so they aren't
re-litigated:

1. First wrong turn: treating it as "chain step vs. late call on the
   product" — both assumed `command_line_config` was separable from the
   runscript-loading step. It isn't: `get_user_config_from_command_line`
   needs it as part of the very first real step (it reads
   `command_line_config["runscript_abspath"]` to find the file at all).
2. Second wrong turn: `from_runscript(path, **overrides)` manufacturing a
   full dict of argparse-equivalent defaults inside `ConfigBuilder`
   itself — this duplicates `parse_shargs()`'s defaults in a second
   location.
3. Landed on: `from_runscript` builds a **minimal** dict
   (`{"runscript_abspath": path, **overrides}`), passing straight
   through to `get_user_config_from_command_line`. Confirmed that
   function and the steps that follow already tolerate a sparse dict
   (missing keys fall through to the runscript's own values or
   `esm_tools`'s real `defaults.yaml`, loaded later at
   `with_runscript_defaults`). `ConfigBuilder` owns zero default values
   of its own. The real CLI (`cli.py`) calls
   `ConfigBuilder.from_runscript(path, **vars(parsed_args))` — argparse's
   defaults remain the only copy, where they already lived.

### `SimulationConfig`

The product `.build()` returns. Thin today — mostly a named wrapper
around the resolved config data — but a real class, not a dict handed
around bare, specifically so later behavior (validation, notebook repr,
whatever comes up) has somewhere to live without another rename.

```python
class SimulationConfig:
    def __init__(self, data):
        self._data = data
```

### `RunState` / `RunStateBuilder`

Per-invocation facts — current date, run/chunk number, `thisrun_dir` and
friends, calendar — that change every resubmission, unlike
`SimulationConfig` which is stable except for CLI overrides. Today these
are computed by `prepare`'s own inner steps (`_read_date_file`,
`_initialize_calendar`, `set_chunk_calendar`, `_add_all_folders`,
`set_prev_date`, `set_parent_info`), which write their results straight
into the shared config dict.

**`RunState` is `pplan`-only.** This took a wrong turn worth recording:
the first instinct was that `RunState`, once separated from
`SimulationConfig`, would need to sync its values *back* into the config
dict so `legacy`'s downstream consumers (batch-script templates,
`filelists.py`, coupler setup) keep working — either via a dual-write
inside `RunStateBuilder` itself, or an explicit `SimulationConfig.with_runstate()`
merge step. Both were rejected: `legacy` never needs this at all, because
its downstream consumers only ever run *inside* `prepare.run_job`, which
keeps writing dates/folders/calendar into the config dict exactly as it
always has (unchanged, per the Non-goals). `RunState` only needs to serve
new, `pplan`-only code, which is written from day one to read
`run_state.thisrun_dir` instead of `config["general"]["thisrun_dir"]`.
No sync method, no bidirectional coupling between `SimulationConfig` and
`RunState` — `RunStateBuilder` depends on `SimulationConfig` (one
direction only, to read values like `time_step` it needs as input),
`SimulationConfig` never needs to know `RunState` exists.

```python
class RunStateBuilder:
    def __init__(self, data):
        self._data = data  # starts as a copy/view of the resolved SimulationConfig's data

    @classmethod
    def from_config(cls, config: "SimulationConfig") -> "RunStateBuilder":
        return cls(config._data)

    def with_dates(self):
        ...  # native equivalent of _read_date_file / _update_run_in_chunk
        return self

    def with_calendar(self):
        ...  # native equivalent of _initialize_calendar / set_chunk_calendar
        return self

    def with_folders(self):
        ...  # native equivalent of _add_all_folders
        return self

    def build(self) -> "RunState":
        return RunState(...)


class RunState:
    """Per-invocation facts for a pplan Simulation. Not shared with legacy."""
    def __init__(self, current_date, run_number, thisrun_dir, calendar, ...):
        ...
```

Whether each of `prepare`'s ~14 inner config-init steps reimplements its
logic natively or calls the existing `prepare.py`/`workflow.py` functions
directly (reuse) is not decided here — a per-step judgment call during
implementation, not a single blanket answer. `install_missing_plugins`
specifically is left unplaced (see "Left to the builder") since `pplan`'s
`run_steps` is still a pure logging stub with no real plugin calls to
resolve yet.

**Sharp edge to watch during implementation**: `RunStateBuilder.from_config`
as sketched above hands `RunStateBuilder` the *same* dict object
`SimulationConfig` holds (`config._data`, not a copy). If `with_dates()`/
`with_calendar()`/`with_folders()` end up reusing `prepare.py`'s existing
functions (which mutate their input dict in place), that would silently
write into `SimulationConfig`'s dict anyway — contradicting "no
sync-back" via a shared reference instead of an explicit method, which
is the exact thing this section exists to avoid. If implementation reuses
those functions, pass a copy (`copy.deepcopy(config._data)`) into
`RunStateBuilder`, not the same object.

### `Simulation`

Replaces `SimulationSetup`. Takes a finished `SimulationConfig`, not a
`command_line_config` dict — it never builds config itself.

```python
class Simulation:
    @classmethod
    def from_config(cls, config: "SimulationConfig") -> "Simulation":
        self = cls()
        self.config = config._data  # or however SimulationConfig exposes it
        wf_engine = self.config["general"].get("wf_engine", "legacy")
        if wf_engine == "pplan":
            self.run_state = (
                RunStateBuilder.from_config(config)
                .with_dates()
                .with_calendar()
                .with_folders()
                .build()
            )
            self.plan = plan.Workflow.resolve(self.config)
            # Workflow-setup tail steps (add_submission_info,
            # initialize_batch_system, initialize_coupler, set_logfile,
            # check_config_for_warnings_errors) still need a home --
            # see "left to the builder".
        else:
            self.config = prepare.run_job(self.config)  # legacy, unchanged; never builds a RunState
        self.config["general"]["esm_tools_version"] = __version__
        return self

    def run(self, kill_after_submit=True):
        ...  # today's __call__ body, unchanged dispatch by jobtype
```

`sim.plan` on the `pplan` path is the resolved `Workflow` object directly
— **`prepare` no longer exists as a `SubPlan` at all**. Its config-init
steps already ran as `ConfigBuilder`/`RunStateBuilder` methods before
`Simulation.from_config`'s workflow-resolution step; there is nothing
left for a `SubPlan("prepare", steps=[...])` to represent. This is a real behavior change to the pplan code landed
today (commits `2b3b955b3` through `bac6bcd49`) — in scope for this
refactor, not a side effect to avoid.

### `Workflow` / `Plan` / `SubPlan` / `Step` (`plan.py`)

Unchanged. Already exist, already correct for this refactor's purposes.

### Naming non-decisions worth recording

- **"Experiment" is not a class in this design.** In this domain,
  "Experiment" means the CMIP/DECK protocol (`piControl`, `historical`,
  etc.) — model-agnostic, defined by CMIP, not something `esm_tools`
  constructs. What `esm_runscripts`/`expid` represents today is already
  at the *Simulation* level (one model's concrete run), not the
  Experiment level. Don't name anything after "Experiment" in this
  refactor; if CMIP experiment tracking is ever needed, it's a plain
  field (e.g. `experiment_id`), not a class in this hierarchy.
- `ConfigBuilder` is the right name *because* it keeps the classic
  Builder/Product split (two classes), not a single fluent class that's
  both chain and result. If a future pass collapses them back into one
  class, this name should be reconsidered too.

## Constraints

- Must not change `legacy` `wf_engine` behavior (see Non-goals).
- Must not re-implement or regress the `pplan` native engine work already
  on this branch (`plan.py`'s models, `resubmit_triggers`, `run_steps`,
  `Workflow.default`/`resolve`) — this refactor changes *who calls* that
  code (`Simulation` instead of `SimulationSetup`), not the code itself.
- `ConfigBuilder.from_runscript` must work with zero arguments beyond a
  path (see Anti-goals).

## Left to the builder

These were raised during design and deliberately left open — a
reasonable implementer's call, not something requiring another round
with Paul:

1. **Where do `prepare`'s workflow-setup tail steps live** —
   `add_submission_info`, `initialize_batch_system`, `initialize_coupler`,
   `set_logfile`, `check_config_for_warnings_errors` (everything from the
   legacy recipe after `assemble_workflow`, which `Workflow.resolve`
   replaces)? Candidates discussed: more `RunStateBuilder`-style methods
   on a new builder, or methods on `Simulation` itself (arguably more
   "getting ready to run" than "building config", since they create
   stateful objects like the batch system handle). These are
   `pplan`-only, same reasoning as `RunState` itself — `legacy` keeps
   running them unchanged inside `prepare.run_job`. Builder's call; note
   the reasoning in the implementation.
2. **Where plugin resolution (`install_missing_plugins`) lands for
   `pplan`** — not placed in `RunStateBuilder`'s sketch above. May not
   need solving yet at all: `run_steps` is still a pure logging stub with
   no real plugin calls to resolve, so this can likely wait until actual
   step execution gets wired up. Flag if that assumption turns out wrong
   mid-implementation.
3. ~~Per-invocation resolved runtime facts~~ — **resolved**, see
   `RunState` above (own class, `pplan`-only, no sync-back into config).
4. **Job dispatch naming** — `Simulation.run()` (used above) vs.
   `.dispatch()` vs. introducing a `Job` class. No strong signal either
   way; `run()` is a placeholder, not a considered choice.
5. **Module layout** — where do `ConfigBuilder`/`SimulationConfig`/
   `RunStateBuilder`/`RunState` live? Proposal (not confirmed with Paul,
   flag if this seems wrong): a new module (e.g.
   `src/esm_runscripts/simulation_config.py`), separate from
   `sim_objects.py` (which keeps `Simulation`), mirroring the existing
   `workflow.py` (legacy engine) / `plan.py` (native models) split.
6. **Call-site audit** — `SimulationSetup` is referenced from `cli.py`
   (confirmed this session). Not yet checked: `esm_master`, any test
   files, anywhere else in the repo. This needs a grep-and-rename sweep
   as part of the implementation plan, not resolved here.

## Verification

- All six already-landed pplan-engine commits' tests
  (`tests/test_esm_runscripts/test_plan.py`) must still pass unchanged —
  this refactor doesn't touch `plan.py`'s models.
- New tests needed for `ConfigBuilder`/`SimulationConfig`/`RunStateBuilder`/
  `RunState`/`Simulation`, at minimum: `from_runscript` with zero
  overrides produces a usable config (no `KeyError`s from missing CLI
  keys); `from_runscript` with overrides merges them correctly;
  `Simulation.from_config` dispatches to `prepare.run_job` for `legacy`
  (no `RunState` built) and to `Workflow.resolve` + `RunStateBuilder` for
  `pplan`, without constructing a `SubPlan("prepare", ...)`; confirm
  `legacy`'s config dict still gets dates/folders/calendar written into
  it exactly as today, with no `RunState`-related code path touched.
- Manual smoke test on a real cluster (as done for the pplan work so
  far): `esm_runscripts <runscript> -e <expid>` (legacy, unchanged
  behavior) and `esm_runscripts <runscript> -e <expid> --wf-engine=pplan`
  (confirm `sim.plan` is the resolved `Workflow`, no `SubPlan("prepare")`
  anywhere).
- This machine's local venv has two known-broken toolchain issues
  (arch-mismatched `psutil`, fixed; a duplicate-`LC_RPATH` rebuild issue,
  not fixed) that block running `pytest` directly here — verification
  during this refactor's implementation should prefer isolated
  module-level exec (as done for all of today's `plan.py` work) or a
  real cluster run, and say so explicitly rather than skipping
  verification silently.
