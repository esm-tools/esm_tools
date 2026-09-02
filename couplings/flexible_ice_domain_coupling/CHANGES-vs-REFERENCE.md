# What this tree changes relative to the two reference runs

Base version: **`feat/pism_coupling_clean_up_and_merge`** in
`/home/a/a270124/_develop/esm_tools` (agreed). This document lists everything in
`flexible_ice_domain_coupling/` that can move numbers, split into

* **§1** changes relative to the two *reference simulations*, and
* **§2** changes relative to the *base branch* (things the base branch got wrong
  and that this tree restores).

Reference simulations:

| | runscripts | ran against |
|---|---|---|
| **mono** | `…/pi_ctl_1x1_icb/scripts/` | `/home/a/a270124/esm_tools/couplings/` (`tmp/260821`) |
| **dual** | `…/spinup_20260818/scripts/` | `…/couplings/coupling_dual-hemisphere/` (`tmp/260821`) |

Note that the reference runs used `tmp/260821`, not the agreed base branch. The
base-branch drift alone (~1300 lines across the shared coupling files) is on top
of everything below, so neither experiment will reproduce its archived output
bit-for-bit. A clean comparison needs a fresh baseline chunk on
`feat/pism_coupling_clean_up_and_merge`.

---

## 1. Changes relative to the reference simulations

### 1.1 Affects **both** experiments

**`ISM_TO_ECHAM_reset_glac_in_ice_domain`, new, defaults to 1.**
`set_glacial_mask_echam_make_dummy_jan_surf` used to contain

```sh
if [[ ${DOMAIN_pism} == "greenland" ]]; then cdo setclonlatbox,0,0,360,0,90 … ; fi
if [[ ${DOMAIN_pism} == "nhem" ]];      then cdo setclonlatbox,0,0,360,0,90 … ; fi
```

`DOMAIN_pism` is exported by the *ice sheet* env only — no ESM-side env file in
any of the three trees exports it (checked). The branch was therefore dead in
both reference runs, and the prescribed `unit.24` `GLAC` survived inside the ice
domains. It is now driven by the reduced ice-domain mask instead of a hard coded
northern-hemisphere box, and is **on** by default: wherever any ice domain is
active the prescribed `GLAC` is cleared before the ice sheet's own mask is
applied.

*Propagates to:* `dummy_jan_surf.nc` → the JSBACH init file → land cover
fractions → the JSBACH and vegetation restarts.

*To reproduce the reference behaviour:* set `ISM_TO_ECHAM_reset_glac_in_ice_domain=0`
in the coupling environment (it is a plain shell switch read by `ice2echam`).

### 1.2 Affects the **dual** experiment

**(a) CALNORO is now fed the current chunk's merged orography.**
In `coupling_dual-hemisphere` the writers were per hemisphere

```sh
ofile=${COUPLE_DIR}/${PISM_HEMISPHERE}/echam6_new_orography_before_calnoro_high_res.nc
```

but `update_orography_prepare_calnoro_high_res` read the *global* path

```sh
ifile=${COUPLE_DIR}/echam6_new_orography_before_calnoro_high_res.nc
```

and it ran inside `ice2echam_1st`, i.e. **before** the driver's
`sellonlatbox`/`ncrcat` produced that global file. So `topodata.nc` — CALNORO's
input — was built from the *previous* chunk's merged orography, once per
hemisphere, the second call overwriting the first. `prepare_calnoro` now runs in
the global stage, after the reduction.

*Propagates to:* `OROSTD, OROSIG, OROGAM, OROTHE, OROPIC, OROVAL` in
`target_orography_echam6_*.nc`, i.e. the sub-grid orography / gravity-wave drag
parameters ECHAM runs with.

**(b) The hemispheres are combined by field arithmetic, not by cutting and
concatenating latitude bands.** Old: `sellonlatbox` each hemisphere, `ncecat`,
`ncpdq`, `ncrcat` along latitude, `ncwa`. New: sum the anomalies (orography,
discharge), take the maximum of the masks, then add the background once.

For two hemispheres that tile the globe the two agree wherever each hemisphere's
remapped field is zero outside its own half. They differ where a hemisphere's
remapping halo leaked across the equator: the old code discarded it with the
box, the new code would sum it. The migrated `awiesm_pism.yaml` therefore sets

```yaml
model2:  ice_domain_box: "0,360,0,90"
model3:  ice_domain_box: "0,360,-90,0"
```

which restores exactly that clipping. Drop those two lines to let the halos
contribute.

**(c) Orography: the background is added once, not per hemisphere.** The old
`_1st` produced `background + anomaly_hemisphere` for each hemisphere and glued
the two absolute fields together. Here the per-domain stage stops at the
anomaly, the anomalies are summed, and the background is added once. Identical
for disjoint domains; correct (rather than lossy) for overlapping ones.

**(d) Each ice domain now works on its own copy of the global exchange files.**
`atmosphere_file_for_ice.nc` and `ocean_file_for_ice.nc` used to be shared and
*mutated in place* by whichever hemisphere ran:
`ic_atm2pism_prepare_clean_missing_values` did `splitvar` + `merge` back onto the
shared file, and `iterative_coupling_ocean_pism_regrid_none` did
`mv ${COUPLE_DIR}/ocean_file_for_ice.nc`, removing it from under the second
hemisphere. `awiesm2pism` now stages copies into `${ICE_COUPLE_DIR}` first.

*Propagates to:* the forcing seen by whichever domain ran second.

### 1.3 Affects the **mono** experiment

Nothing beyond §1.1 and the base-branch drift. A single ice sheet keeps the flat
couple directory (no `ice_domain: True`), every domain loop runs once and
`ice_domain_reduce` short-circuits to a file copy, missing values included.

---

## 2. Changes relative to the base branch

These restore behaviour the reference runs already had; they are changes only if
you compare against `feat/pism_coupling_clean_up_and_merge` as it stands.

1. **`FESOM_PREP_ICEBERG_DISCHARGE` had no producer at all** in the base branch,
   so `ice2fesom` always skipped the iceberg discharge. It is now exported by
   `env_awiesm.py` from the FESOM iceberg switch (`use_icebergs` /
   `fesom_use_iceberg` / `general.with_icb`). Both reference experiments run with
   icebergs, so without this neither would work.
2. **`DOWNSCALE_PRECIP` / `DOWNSCALE_TEMP` were exported only in `couple_out`**,
   while `atmosphere2pism` reads them in `couple_in`. The mono runscript's
   `downscale_precip: 0` had no effect. Now exported by `env_ice.py` too.
3. **`PYFESOM_PATH`** was not exported; `fesom2ice` fell back to
   `${FUNCTION_PATH}/pyfesom`, which does not exist.
4. **`pism2ocean` never delivered `ice_file_at_ocean.combined.nc`** out of the
   ice sheet's work directory, so `ice2fesom` always took its "file not there,
   skipping" branch. It is now copied into `${ICE_COUPLE_DIR}`.
5. **`config["pism"]` was hard coded** for `thk_threshold` and
   `select_min_glacial_depth` while everything else used `config[setup_name]`.
   No numerical effect on either reference run (mono: the section *is* `pism`;
   dual: the values set equal the defaults), but it would silently drop those
   settings for any ice component not named `pism`.
6. **`../outdata/pism/`, `../restart/pism/`, `${COUPLE_DIR}/../`** hard coded the
   component directory name; now `${OUTPUT_DIR_pism}`, `${RESTART_DIR_pism}`,
   `${ICE_EXP_DIR}`.
7. **`CHUNK_SIZE_pism_standalone`** came from `config["model2"]["chunk_size"]`,
   i.e. from whichever model happened to be second; now the component's own
   chunk size.
8. **Hard coded `T63`** in the `oro_update_mod` and `ice_orog.nc` branches is now
   `${RES_echam}`.
9. **`ice2echam` aborts (exit 72)** when an ice domain did not deliver its
   `ice_file_for_atmosphere.nc`, instead of falling through the wait loop and
   failing later inside `cdo`.
10. **`apply_hosing_correction.py`** takes any number of
    `--pair DISCHARGE AREA` and sums the integrals; the old four-positional form
    still works.

---

## 3. Not changed

* `coupling_echam2ice.functions`, `coupling_fesom2ice.functions`,
  `general/general_helpers.functions`, `general/general_lists.functions`,
  `general/pism_helpers.functions`, `vilma/coupling_vilma2ice.functions` are
  byte-identical to the base branch.
* `oro_update_mod: 6` → `1` is a renumbering only: the mode-6 block of
  `tmp/260821` and the mode-1 block here are identical apart from one
  commented-out line.
* The FESOM iceberg block of the dual runscript is untouched — the per-domain
  `${couple_dir}/pism_nh|pism_sh/latest_discharge.nc` files it lists are exactly
  what this tree writes.
