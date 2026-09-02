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

## 5. Things that silently do nothing now

`multi_year_mean: 0` (echam) is set in both reference runscripts. It fed
`ECHAM_TO_ISM_multiyear_mean`, which `coupling_echam2ice.functions` dropped on
`feat/pism_coupling_clean_up_and_merge` ("option removed"). Leaving it in the
runscript is harmless; it just has no effect.

`fesom_cell_area_file` was renamed to `cell_area_fesom_file`. Neither reference
runscript sets it, so nothing to do — but check it if you copy an older
runscript.
