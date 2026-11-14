# Running the two reference experiments with this tree

Reference simulations:

| | runscripts | coupling tree it used |
|---|---|---|
| **mono** | `/work/ba0989/a270124/PalModII/experiments/pi_ctl_1x1_icb/scripts/` | `couplings/` |
| **dual** | `/work/ab0246/a270124/esm-experiments/awiesm-2.6/spinup_20260818/scripts/` | `couplings/coupling_dual-hemisphere/` |

Adapted copies of both are in `examples/reference_mono/` and
`examples/reference_dual/`. Every line that differs from the original carries a
`# CHANGED` or `# ADDED` comment, so `diff` against the originals shows exactly
the migration.

---

## 0. Where the tree has to live

The base version is **`feat/pism_coupling_clean_up_and_merge`** in
`/home/a/a270124/_develop/esm_tools` — that is what this tree was built from.

`esm_runscripts` on your `PATH` is the editable install of
`/home/a/a270124/esm_tools`, so `${general.esm_couplings_dir}` resolves *there*,
not to `_develop`. The migrated runscripts therefore give `general.script_dir`
the absolute `_develop` path. That works as is: the coupling scripts and env
modules are loaded by path, while `configs/components/{pism,pism_nh,pism_sh}`
keep coming from the installed tree (both trees carry the same couple-directory
paths in those, checked).

If you would rather write `${general.esm_couplings_dir}/flexible_ice_domain_coupling`
in the runscripts, symlink it once:

```sh
ln -s /home/a/a270124/_develop/esm_tools/couplings/flexible_ice_domain_coupling \
      /home/a/a270124/esm_tools/couplings/flexible_ice_domain_coupling
```

The tree stays a third option next to `couplings/` and
`couplings/coupling_dual-hemisphere/`; nothing outside it is touched, and no
existing runscript changes behaviour until it opts in via `script_dir`.

**Before running either experiment, read `CHANGES-vs-REFERENCE.md`** — it lists
every change that can move numbers, and which of the two experiments each one
affects.

## 1. Mono — `pi_ctl_1x1_icb`

One ice sheet, so **do not** set `ice_domain: True`. The experiment keeps the
flat couple-directory layout (`${couple_dir}/latest_atmo_forcing_file.nc`, ...),
which is what `configs/components/pism/pism.input_and_restart_files.yaml`
expects. `ICE_DOMAINS` becomes `"."`, every domain loop runs once and every
reduction is a pass-through.

Four edits:

| file | change |
|---|---|
| `awiesm.yaml`, `pism.yaml` | `general.script_dir` → this tree (see §0) |
| `awiesm.yaml` | `oro_update_mod: 6` → `1` |
| `awiesm.yaml` | `env_preparation: env_echam.py` / `env_fesom.py` → `${general.script_dir}/envs/env_awiesm.py` |
| `pism.yaml` | `env_preparation: env_pism.py` → `.../envs/env_ice.py`, `env_pism2awiesm.py` → `.../envs/env_ice2awiesm.py` |

`awiesm_pism.yaml` is unchanged.

Iceberg discharge: one domain, so `ICE_DISCHARGE_MERGE` defaults to 1 and the
single `${couple_dir}/latest_discharge.nc` is written on the native PISM grid,
exactly as `fesom.disch_file` in this runscript expects.

## 2. Dual — `spinup_20260818`

Two ice sheets, so they *are* marked as ice domains. `configs/components/pism_nh`
and `configs/components/pism_sh` already point their `atmo_forcing`/`ocean_forcing`
at `${general.experiment_couple_dir}/pism_nh/…` and `…/pism_sh/…` — which is
precisely the `${ICE_COUPLE_DIR}` layout this tree writes, so the component
configs need no change.

| file | change |
|---|---|
| `awiesm_pism.yaml` | add `ice_domain: True` to `model2` and `model3` |
| `awiesm.yaml`, `pism_nh.yaml`, `pism_sh.yaml` | `general.script_dir` → this tree (see §0); add it to `pism_sh.yaml`, which had none and hard-coded an absolute path in its subjobs |
| `awiesm.yaml` | `script: coupling_dual-ice2echam.functions` → `coupling_ice2echam.functions` |
| `awiesm.yaml` | `oro_update_mod: 6` → `1` |
| all | `env_*.py` → `${general.script_dir}/envs/env_awiesm.py` / `env_ice.py` / `env_ice2awiesm.py` |

`coupling_dual-ice2echam.functions` has no successor file: `ice2echam` itself is
the loop over `ICE_DOMAINS` now.

The FESOM iceberg block stays **exactly as it is**:

```yaml
disch_file:
    - "${general.experiment_couple_dir}/pism_sh/latest_discharge.nc"
    - "${general.experiment_couple_dir}/pism_nh/latest_discharge.nc"
```

Those are the per-domain files this tree writes. With more than one domain
`ICE_DISCHARGE_MERGE` defaults to 0, so no combined file is produced and each
ice sheet keeps its native grid — which is what the iceberg module wants,
because `basin_file`, `domain` and `scaling_factor` are per-entry too.

`awiesm_pism.yaml` also gets the two hemisphere boxes:

```yaml
model2:
    ice_domain_box: "0,360,0,90"
model3:
    ice_domain_box: "0,360,-90,0"
```

These clip each domain's contribution before the domains are combined, which
reproduces the `sellonlatbox` clipping the dual driver did. Drop them to let a
domain's remapping halo contribute across the equator instead — see
`CHANGES-vs-REFERENCE.md` §1.2(b).

## 3. Adding a third ice sheet

One more `modelN` block with `ice_domain: True` plus its runscript, and a
`configs/components/<name>/` whose `atmo_forcing`/`ocean_forcing` point at
`${general.experiment_couple_dir}/<name>/…`. No coupling script changes.

---

## 4. `oro_update_mod: 6` → `1`

Not a behaviour change. The `feat/pism_coupling_clean_up_and_merge` cleanup
renumbered the seven modes down to two, keeping the two that are in use:

| `tmp/260821` | here | |
|---|---|---|
| `2` | `0` | legacy: `OROMEA` and `GEOSP` from the low-res input |
| `6` | `1` | masked: `OROXXX` from T511, `OROMEA` from T63 |

Verified: the mode-6 block of
`/home/a/a270124/esm_tools/couplings/echam/coupling_ice2echam.functions:504-564`
and the mode-1 block here are identical apart from one commented-out line.
Modes 3, 4, 5 and 7 do not exist here — neither reference experiment uses them.

## 4b. `oro_update_var` must now be spelled `OROMEA` or `GEOSP`

`echam_ice_select_relevant_variables` here tests

```sh
if   [[ "x${oro_update_var}" == "xGEOSP"  ]]; then ...
elif [[ "x${oro_update_var}" == "xOROMEA" ]]; then ...
else echo " You have to choose either GEOSP or OROMEA. Exit with error! "; return 42
```

while the old tree only tested `== "xgeosp"` and let *everything else* fall
into the OROMEA branch. Both reference runscripts relied on that: the mono one
has `oro_update_var: "oromea"`, the dual one `"ormoea"` (a typo). Either would
now abort `echam2ice`. Both migrated copies in `examples/` are corrected to
`"OROMEA"`; check this line in any other runscript you migrate.

## 4c. Paleo setups: set `oro_lo_res_reference_file`

If `unit.24` comes from a glacial tarfile (`tarfile_ending: LGM_glac1d`, …),
add under `echam:` the present-day `jan_surf` of the same resolution, e.g.

```yaml
oro_lo_res_reference_file: "/pool/data/ECHAM6/input/r0007/T63/T63GR15_jan_surf.nc"
```

Without it the coupling stops at the first orography update (exit 49): the old
code built `OROMEA` from the glacial `unit.24` plus the ice-sheet change since
PI and counted the paleo ice twice (RUNNING.md §3a). The old tree's mode 5 did
not have this problem, because it added the change to the restart `OROMEA`;
`oro_lo_res_source: restart` restores that path, but the high-res start check
still needs a present-day reference.

**A fresh paleo experiment also needs a paleo high-res start.** On the very
first orography update (no `echam6_new_orography_before_calnoro_high_res.nc`
in `couple/` yet) the high-res field starts from PI T511 unless told
otherwise. The restart carries the glacial ice sheets, PI T511 does not, so the
start check stops with exit 48 (140k_eq_v2 restart 3300: 317 m mean mismatch
over 7540 cells). Do one of:

* set `oro_hi_res_parent_file` to the parent's
  `echam6_new_orography_before_calnoro_high_res.nc` from the branch year.
  Copying it into `couple/` by hand (as `prepare_run_F.sh` did for
  140k_eq_smallLIS) also works, but then the start check does not see it;
* or `oro_hi_res_on_mismatch: reconstruct`: PI T511 + the restart's `OROMEA`
  anomaly, everywhere, so glacial ice on today's ocean (exposed shelves,
  Hudson Bay under the Laurentide) is kept.

Parents run with the old coupling tree carry ~130 m of `OROMEA` regridding
error (140k_eq_v2's own high-res file against its restart: 127 m), above the
100 m default; raise `oro_hi_res_tolerance` (e.g. 150) for such branches.

## 5. Things that silently do nothing now

`multi_year_mean: 0` (echam) is set in both reference runscripts. It fed
`ECHAM_TO_ISM_multiyear_mean`, which `coupling_echam2ice.functions` dropped on
`feat/pism_coupling_clean_up_and_merge` ("option removed"). Leaving it in the
runscript is harmless; it just has no effect.

`fesom_cell_area_file` was renamed to `cell_area_fesom_file`. Neither reference
runscript sets it, so nothing to do — but check it if you copy an older
runscript.
