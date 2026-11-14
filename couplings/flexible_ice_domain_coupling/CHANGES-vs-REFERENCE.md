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

**ECHAM orography: physical surface, area-mean regridding, one source.**
Three defaults change; together they take the spikes out of the T63
`OROMEA`. In the old coupling, `OROMEA` was the 5 km PISM `thk+topg`
point-sampled at the T63 cell centres: ±1000 m single-cell errors at the
Greenland margins, land below sea level wherever ice retreated onto a marine
bed, `OROMEA` outside `OROVAL`…`OROPIC`, and `GEOSP/g − OROMEA` off by several
hundred metres at those cells. Through `elev_lo` the error also reached the
lapse-rate downscaling for the ice sheet: 0.005 K/m × the `OROMEA` error.

| key | new default | legacy value | what changes |
|---|---|---|---|
| `orog_surface_definition` (ice domain) | `physical` | `thk_plus_topg` | surface handed to the atmosphere = `max(topg+thk, freeboard)`: open ocean 0 instead of the bed depth, floating ice its freeboard |
| `oro_regrid_method` (echam) | `remapcon` | `remapbil` | area mean of all ice-grid cells per ECHAM cell instead of the 4 around the centre. Anomalies are normalised by the full cell area (`destarea`), which also makes the sum over several domains exact |
| `oro_lo_res_source` (echam) | `high_res` | `restart` | `OROMEA` = ECHAM-grid reference (`unit.24`, or `oro_lo_res_reference_file` in paleo setups) + area mean (T511 → ECHAM grid) of the high-res change, where the surface changed (see below); no longer restart `OROMEA` + a separately regridded anomaly. `OROMEA`, `GEOSP` and the CALNORO parameters then come from the same field |

Checked offline on fix_oro/ssp585_1x1 data:
* The legacy values reproduce the old code exactly (all variables and values).
* One 500-year step, ssp585_1x1 PISM 2499 → 2999 started from PI: `OROMEA`
  differs from `unit.24` + the true cell-mean change by mean |.| 1.1 m and at
  most 13 m. Old chain: 21.8 m mean, at most 531 m.
* The conservative remap conserves the volume change: 99.98 % at T63, where
  bilinear gave 104.6 %.

*Propagates to:* `OROMEA` → `elev_lo` (downscaled temperature for the ice
sheet) and the SSO drag. In an experiment that already ran with the old scheme,
the first update with `oro_lo_res_source: high_res` replaces the accumulated
`OROMEA` errors in one step (up to ~1000 m at single cells; the log line
`|OROMEA - restart OROMEA|` reports the size). Outside the ice domains nothing
changes.

*To reproduce the reference behaviour:* `orog_surface_definition:
thk_plus_topg` in each ice domain runscript, and `oro_regrid_method: remapbil`
and `oro_lo_res_source: restart` under `echam:`.

**High-res orography: absolute orography lands exactly, no change applied twice.**
Two fixes in how `echam6_new_orography_before_calnoro_high_res.nc` is updated;
no option, the normal anomaly update is unchanged (checked: identical high-res
orography and `OROMEA`).

* *Absolute orography* (`ice_orog.nc`, written by `orog_reference_pism: 1` at
  the first coupling): the T511 anomaly is now taken against the high-res orography it is
  added to, not against the T63 orography remapped to T511. The high-res field
  then *is* the ice sheet surface where there is ice. Before, PI's T511 relief
  below T63 scale stayed on top of it, and CALNORO and `GEOSP` saw both:
  over a PI Greenland ice sheet that relief is 217 m on average, 627 m at the
  95th percentile, 1441 m at most. In the offline test the high-res field now
  equals the PISM surface over the ice (max difference 0.0 m) and is unchanged
  outside it. `OROMEA` (an area mean) was hardly affected. Without a high-res
  orography yet (first coupling), the starting high-res orography is now
  chosen (and checked against the restart) *before* the per-domain stage, so
  the very first absolute surface lands exactly as well (offline: max 0.000 m
  over all T511 cells with surface > 0).
* *`orog_reference_pism: 1` hands over the absolute surface at the first
  coupling of each ice domain*, not at `CHUNK_NUMBER_pism == 2`. Chunk numbers
  count all models: in a dual run `pism_nh` first couples in chunk 2 but
  `pism_sh` in chunk 3, so Antarctica never got its absolute surface. "First"
  is now: no earlier `ice_file_for_atmosphere.nc` in the domain's couple dir,
  or a repeat of the coupling recorded in `ice_orog_absolute.chunk`. Running
  experiments (which have that file) keep handing over changes. For
  `pism_nh`/mono the absolute file is bit-identical to before; mode `0` is
  unchanged.
* *Repeated `couple_in` for the same chunk* (crash, manual resubmission): the
  high-res orography accumulates, so a repeat used to add the chunk's change a
  second time (2124 m at single cells in the test). Now
  `...high_res.chunk` records the last applied chunk and
  `...high_res.before_chunk.nc` keeps the state before it; a repeat starts from
  that state again (the log says so), and a repeat of the very first update
  picks the starting orography again. Result identical to a single run.
* *OROMEA and the sub-grid parameters are updated wherever the surface
  changed*, and only there: ECHAM cells containing a T511 cell where the
  high-res orography differs from PI T511 by > 1 m
  (`mask_oro_changed_T63grid.nc`, `update_orography_changed_mask`). That is
  ice, former ice, and bedrock movement outside the ice (Lingle-Clark responds
  there too: ~-20 m within 50 km of the margin in a 2370-year NH spinup).
  Before, OROMEA used the whole ice-domain footprint, and the sub-grid
  parameters "ice now or glacier in `unit.24`", which missed former ice and
  bedrock: on fix_oro's high-res orography after 233 years, 21 cells had
  OROMEA moved by > 20 m (up to -773 m) with `unit.24` sub-grid parameters,
  11 of them with OROMEA outside OROVAL..OROPIC. The new mask covers all 21
  (245 cells instead of 192; the 13 dropped are PI glacier cells that did not
  change, |OROMEA change| <= 0.1 m). For a first update OROMEA differs from
  before by at most 0.2 m (cells whose surface moved < 1 m). Without a
  high-res orography the old sub-grid rule is kept.

**`OROMEA` reference: no double-counted paleo ice (2026-11-14).**
`update_orography_lo_res_from_high_res` (`oro_lo_res_source: high_res`) built
`OROMEA = unit.24 + remapcon(HIGH − PI T511)`, and
`update_orography_init_high_res_reference` compared `restart − unit.24` with
`candidate − PI T511`. Both assume that `unit.24` is the present-day surface
of PI T511. In paleo setups `unit.24` comes from a glacial tarfile and carries
the glacial ice sheets, which were then counted twice (140k_eq_smallLIS:
`OROMEA` 3794 m over the Laurentide against a 2016 m PISM surface, ~9 K too
warm dEBM forcing; details and numbers in RUNNING.md §3a).

New: `update_orography_lo_res_reference` picks the ECHAM-grid reference
(`echam.oro_lo_res_reference_file`, default `unit.24`) for both places and
checks it against the T511 reference on its glacier cells
(`oro_lo_res_reference_check`: `abort` (default, exit 49) / `warn` / `off`;
`oro_lo_res_reference_max_frac`, default 0.05, of the glacier cells > 500 m
apart). Paleo runscripts set the present-day `jan_surf` of their resolution.

*Affects:* neither reference experiment (both present-day): `unit.24` passes
the check and `OROMEA` is bit-identical (offline, greenland_only `unit.24`,
max |difference| 0 m). Paleo experiments with `oro_lo_res_source: high_res`
stop at the first orography update until the reference is set; with it,
`OROMEA` follows the ice-sheet surface (Laurentide 1953 m vs PISM 2016 m).
Experiments that already ran with a glacial `unit.24` carry the wrong
`OROMEA` in their restarts; the first update with the right reference
replaces it inside the changed cells (`|OROMEA - restart OROMEA|` in the log,
up to ~2500 m), but their climate since the first coupling is affected and
they should be restarted from before it.

*Same day, follow-up from review:* `oro_hi_res_on_mismatch: reconstruct`
rebuilt the high-res start only on present-day T511 land, so glacial ice on
today's ocean (Hudson Bay, exposed shelves; 813 T63 cells > 100 m in 140k,
up to ~3200 m) was missing from `GEOSP`, and from `OROMEA` wherever the
surface changes later. It now rebuilds everywhere: the anomaly is 0 where
restart and reference are both ocean, and no land mask is needed. On
140k_eq_smallLIS (restart 3301) the rebuilt `OROMEA` matches the restart
within −2 m on average on glacier cells and 4 m over the paleo ocean; on the
853 shelf-ice cells it is 851 m against 883 m (a mask of present-day plus
glacial-shelf land: −117 m, present-day land only: the ice was missing).
Present-day setups change only where `reconstruct` is used, by bilinear
spill of coastal anomalies of a few metres onto ocean cells. The exit-48
message now says what a fresh paleo run needs (`oro_hi_res_parent_file`,
which is checked, or `reconstruct`) and that branches from runs of the old
tree need `oro_hi_res_tolerance` ~150 m.

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
10. **No silent PI fallback for the high-res orography.** When
    `echam6_new_orography_before_calnoro_high_res.nc` was missing (first
    orography update, e.g. after branching from a coupled parent), the T511 PI
    orography was used without a check. That reset `GEOSP` to PI while `OROMEA`
    (from the restart) kept the parent's ice sheet. Now
    `update_orography_init_high_res_reference` starts from
    `echam.oro_hi_res_parent_file` (or PI T511) only if its anomaly matches the
    restart's `oromea` anomaly within `oro_hi_res_tolerance`, and otherwise
    stops with exit 48 unless `oro_hi_res_on_mismatch` is `reconstruct` or
    `ignore` (see RUNNING.md).
11. **`apply_hosing_correction.py`** takes any number of
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
