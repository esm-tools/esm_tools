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

**Ocean forcing for PICO / `th`: sea floor left out of the depth mean.**
FESOM writes T = S = 0 below the sea floor, and `ocean2pism` averaged these
zeros into the 0–410 m mean (FESOM levels 1–20) handed to PISM. On the
continental shelf, where PICO reads its input, mean salinity was about 22 g/kg
instead of about 32, and temperature was pulled towards 0 °C. With too fresh
water the freezing point is too high, so PICO melted far too little: in
standalone tests with the correct salinity, floating ice area dropped by about a
third and sub-shelf melt rose by about 30%. Now (`iterative_coupling_ocean_pism_depth_mean`):

* points below the sea floor are missing and left out of the mean;
* the depth range is set by `ocean_depth_top` / `ocean_depth_bottom`
  (default 0 / 410 m, the old range), and each level is weighted by the
  metres of water it stands for (before, the levels at 0, 5 and 10 m had as
  much weight as those 70 m apart further down);
* `ocean_depth_mean: bottom` uses the deepest water down to
  `ocean_depth_bottom` instead, the shelf bottom water PICO is calibrated with;
* where the sea floor is shallower than `ocean_depth_top`, the deepest water
  above it is used;
* the mean is taken on the FESOM grid **before** the regridding, so that the
  horizontal extrapolation (`fillmiss2`) does not refill the masked points
  from neighbouring columns.

*Propagates to:* `theta_ocean` / `salinity_ocean` in the PISM ocean forcing
(`ocean_ablation_method: PICO_OCEAN` or `OCEANTEMPSALT`) → sub-shelf melt →
shelf extent, discharge and the ice-to-ocean fluxes.

*To reproduce the reference behaviour:* `ocean_depth_mean: legacy` in the
ice domain's section. This is also the default when ocean anomaly forcing
(`ANOMALY_OCEAN_TEMPERATURE` / `ANOMALY_OCEAN_SALINITY`) is on: the anomaly is
taken against `BASE_STATE_OCEAN_FNAME`, which was made with the old mean, and
the shelf error only cancels if forcing and base state are made the same way.
Mixing them adds the difference of the two means (several g/kg of salinity on
the shelf) to the reference ocean. Set `ocean_depth_mean: wet` together with a
base state made with it.

**Ice sheet freshwater: the amount the ice sheet lost, on every grid.**
Before, the mass-loss flux was remapped to the ECHAM grid for the rivers
(`gfw_atmo`) and integrated there for the FESOM hosing. On grids without cell
corners in `ice.griddes` the remapping fell back to `remapbil`, which is not
conservative: in `dual_hemisphere_t02` the Antarctic (8 km) integral changed
sign (−1.64·10⁷ kg/s on the PISM grid, +2.84·10⁶ kg/s in the run's T63
file). The NH grid has corners and was remapped with `remapcon2`, 0.8 % off.
The Antarctic grid had no corners because
`make_ice_griddes.py` ran without the python3 module (`import numpy` failed).
Now:

* the ice grid no longer has to match the pool. `ice_griddes_ensure_corners`
  compares `ice.griddes` with the PISM file the domain runs on and derives a
  new one when the size differs (the old one is kept as
  `ice.griddes.wrong_size`); `ic_atm2pism_prepare_save_griddes` derives it
  when the pool has none (before: exit 3). The x/y axis for the PISM forcing
  takes only a candidate of the forcing file's size and otherwise extracts it
  from the PISM file into `${couple_dir}/<domain>/` (before: exit 44 for the
  Greenland 5 km restart of 301×561 against the pool's 337×577, and a failure
  for Antarctica 8 km, where the pool has no `_xy.nc`). Its cached
  double/float copies live in the domain's couple directory and are made anew
  when their size does not fit;
* `echam2ice` creates every domain's couple directory at the first AWI-ESM
  coupling, before any ice sheet has run;
* `ice_griddes_ensure_corners` loads the python3 module, so the corners are
  made and the remapping is `remapcon2`;
* the hosing integrates each domain on its own grid with dx·dy, the cell area
  PISM books its mass on (`utils/ice_cell_area.py`); `cdo gridarea` and its
  exit 46 for grids without corners are no longer needed there;
* the ECHAM-grid copy is corrected to the same integral after the remapping
  (`utils/conserve_remapped_flux.py`), which also removes the 1–7 % that
  `remapcon2` is off by between spherical and projected areas.

*Propagates to:* `gfw_atmo` (rivers) and `landice_yearly_mass_loss.out`
(FESOM hosing): amount and, for Antarctica, sign.

*To reproduce the reference behaviour:* not possible by a switch; this is a
bug fix.

**Ice sheet freshwater: total from PISM's own budget, real ice density.**
The mass-loss flux is ρ_ice · Δthk over the chunk, with Δthk = restart at the
end minus the chunk's input file. That also holds whatever happens to the
input when PISM starts from it: in `dual_hemisphere_t02` Antarctica gave
−518 Gt/yr against −662 Gt/yr in PISM's budget (NH: +252 against +250). The
total is now set to PISM's budget for the chunk (scalar time series:
surface + basal mass flux + discharge), keeping the Δthk pattern
(`mass_loss_total: pism_budget`). ρ_ice is read from the restart instead of
the fixed 910 kg m⁻³.

*Propagates to:* `gfw_atmo` and the FESOM hosing (amount).

*To reproduce the reference behaviour:* `mass_loss_total: thk_difference`
(the density is read in both cases).

**Constant hosing: region 2 is read.** `landice_yearly_mass_loss.out` said
1 region and the constant flux was appended below it, so FESOM never read it.
`apply_hosing_correction.py` now writes the number of regions and all fluxes
itself (`constant_landice_hosing`, see RUNNING.md).

**Ice domain footprint: where the domain may hold ice, whole ECHAM cells.**
Inside a domain's footprint the ice sheet decides ECHAM's glaciers, sub-grid
orography and albedo; outside, `unit.24` stays. The footprint was every ECHAM
cell the PISM grid touched at all. Now it is the area where the domain may
hold ice (the grid minus the cells the prescribed front retreat keeps
ice-free), and an ECHAM cell counts when at least half of it is covered: 356
instead of 442 T63 cells for the NH 5 km grid. Prescribed glaciers in
front-retreat areas (Tibet, the Canadian Arctic in a Greenland grid) and at the
grid edge are no longer removed.

*Propagates to:* `glac` in the ECHAM restart, `GLAC`/`ALB` in the JSBACH init
file, the sub-grid orography mask.

**Orography anomaly at the edge of a domain: `CDO_REMAP_NORM` now takes effect.**
The anomaly regridding was meant to normalise by the full ECHAM cell
(`destarea`), so that a cell the domain only grazes gets a share of the change.
cdo 2 accepts only `dest`/`frac` and ignored `destarea` with a warning, so the
default `fracarea` was used: such cells got the full change of the few ice
cells inside them. Fixed (also for the footprint above).

*Propagates to:* `OROMEA`, `GEOSP` and the sub-grid parameters along the edges
of the ice domains.

**JSBACH init file: only the cells whose glacier changed.** It used to be
regenerated from the pool's 1850 maps every coupling and replaced `jsbach.nc`
as a whole; in `AWIESM2.6_full01` that changed `cover_fract` on 3602 cells for
12 cells of glacier change. Now (`jsbach.jsbach_init_update: changed_cells`)
the regenerated values are taken only where the glacier mask changed. Year
and dynamic vegetation are keys (`jsbach_init_year`, `jsbach_init_dynveg`),
and the generated file is found by the name the program prints instead of the
hard-coded T63 name.

*To reproduce the reference behaviour:* `jsbach_init_update: full`.

**Maximum LAI on cells whose glacier changed.** JSBACH sets the dynamic
maximum LAI `laiMax_dyn` (veg restart) only on a cold start, to
`MaxLAI(cover_type)`, and never raises it again (`update_maximum_lai` is
commented out in `mo_phenology.f90` of echam-6.3.05p2). Glacier cells carry
`LAI_negligible` (1e-5) on every tile, so a deglaciated cell kept that value and
could never grow leaves: in the T2 deglaciation runs formerly glaciated Canada
and Scandinavia stayed about 75 % desert at 127 ka (LAI 0.1-0.3, July T2m
7-15 K too warm), and in `flextest_20261123` all 23 deglaciated cells had
`laiMax_dyn` = 0 on all tiles. The veg-restart update now sets `laiMax_dyn` on
deglaciated cells to `MaxLAI(cover_type)` from the run's `lctlib.def` (what
every other land cell holds; never lower than before) and leaves it unchanged
on new glaciers (`utils/jsbach_reset_laimax_dyn.py`; exit 79 without
`lctlib.def`). New glaciers must keep it: the coupling does not reset the LAI in
the jsbach restart, so they still carry their vegetation's LAI, and JSBACH stops
with "LAI larger than maximum LAI on input" where it exceeds `laiMax_dyn`
(`flextest_20261123` 3143, after a first version set 0 there).

*To reproduce the reference behaviour:* not switchable; the reference left
`laiMax_dyn` untouched.

**Albedo on deglaciated cells.** Where `unit.24` has a glacier and the ice
sheet has none (inside a domain), `ALB` kept the glacier value (mean 0.63 on 47
cells in `dual_hemisphere_t02`). It now takes the albedo of the nearest land
cell without a `unit.24` glacier (mean 0.34 there). Effective only with
JSBACH `use_albedo = .false.`: with `.true.` (the AWI-ESM 2.6 setups) JSBACH
does not read this field, and ECHAM does not read `ALB` from `unit.24`.

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
Rebuilding everywhere spreads coastal anomalies onto ocean points of the
T511 field (bilinear remap across the coast), and not by a few metres:
paleo (140k_eq_v2 restart 3300) 16626 open-ocean T511 points raised by
> 50 m, 461 by > 500 m, at most 1099 m; present-day (greenland_only,
`reconstruct` forced) 1668 points > 50 m, 91 > 500 m, at most 1166 m. This
is accepted on purpose: ECHAM uses T63 cell means (`GEOSP`, `OROMEA`), and
those match the restart far better than with a land mask (see above). The
raised ocean points only enter the sub-grid parameters (CALNORO) of coastal
cells; the land-sea and glacier masks do not come from this field.
Present-day setups change only where `reconstruct` is used. The exit-48
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
12. **`FESOM_DISCH_FILES` is comma separated.** ESM-Tools writes the environment
    as unquoted `export NAME=value`, so the space-separated list kept only the
    first file and the disch_file check stopped with exit 76 although both
    domains were listed (`flextest_20261123`, 3141).
13. **`latest_discharge.nc` keeps its x/y axes.** `-setgrid` with a lon/lat-only
    `ice.griddes` (from `make_ice_griddes.py`) dropped them; the iceberg plugin
    then took 1 m cells and seeded no Antarctic bergs (`flextest_20261123`
    3141/3142, about 3000 Gt/yr of calving). x/y are copied back from the PISM
    file, and a cell size that is not a PISM cell in metres stops with exit 42.
14. **A rerun of `ice2echam` starts from the model's restarts.** With
    `update_glacial_mask`, `ice2echam` edits the ECHAM, JSBACH and veg restarts
    of the previous chunk in place. A rerun of the same coupling edited the
    edited files again, and the ECHAM backup
    (`*_backup_before_glacial_mask_replacement.nc`) was overwritten with the
    edited file on every run; JSBACH/veg backups piled up with a timestamp on
    every run. Now `ice2echam_restore_restarts` keeps
    `<restart>_pre_ice2echam.nc` (the model's file) and
    `<restart>_pre_ice2echam.state` next to each edited restart and puts the
    saved copy back before a rerun edits again; a restart the model wrote anew
    is saved again (details and the one uncovered case in the function's
    header). Missing restart: exit 80. The three old backups are gone.
15. **A derived `ice.griddes` carries the projection section.** When no
    `ice.griddes` is in the couple directory yet, `ice_griddes_ensure_corners`
    (couple_in) writes one with `make_ice_griddes.py` before the pool file is
    copied. That file had the curvilinear lon/lat grid only, so remapping onto it
    wrote no x/y: the dEBM output and `latest_atmo_forcing_file.nc` had x = y = 0
    and PISM stopped with "input x coordinate has to be strictly increasing"
    (`smallLIS_core2_*_spin133`, first coupling). `make_ice_griddes.py` now adds
    gridID 2 (projection: xfirst/xinc/yfirst/yinc and the `mapping` attributes)
    as CDO's griddes of a PISM file has it; remaps then match the pool griddes.
16. **`gfw_atmo` says what it holds.** The field is water in m/s (PISM's mass
    flux divided by 1000, sign flipped), but kept `units = "kg m-2 s-1"` and the
    ice-mass `standard_name` of PISM's field. Now `units = "m s-1"`, no
    standard_name, and a long_name that gives the sign.
17. **`ice2echam` removes the five `latest_*` links ECHAM waits for.** Only
    the ice couple_out removed them (`pism2awiesm`), so a rerun of the
    coupling-in alone found last attempt's links, and they had to be removed by
    hand. Both now call `ice_domain_invalidate_echam_products`
    (`general/ice_domains.functions`), which also removes dangling links.
18. **The budget-correction warning is measured against the gross flux.**
    Against the net, a chunk where gains and losses nearly cancel warned of
    +50.8 % for a 28 Gt/yr mismatch (`flextest_20261123`, Antarctica, PISM
    run 4). `conserve_remapped_flux.py --target-integral` now compares with
    sum(|F| A) and `mass_loss_correct_to_pism_budget` warns above
    `MASS_LOSS_BUDGET_WARN_PERCENT` (default 3 %). In the test that flags the
    two runs after the fresh start (about 5 %) and not the later ones (1–2 %).
    The check after a remapping keeps the native net and 10 %.
19. **ECHAM's orography is checked against the ice sheet surface.** Only the
    change of the surface reaches ECHAM in each chunk, so an offset between
    `OROMEA` and the ice sheet at the start of the coupling stays for good
    (constant over 650 years in `ssp585_1x1_to_ssp460_fix_oro`). Nothing
    showed it. `pism2atmosphere` now also writes `ice_surface.nc` (surface
    and thickness of the chunk's restart, stamped with the chunk), and
    `update_orography_check_against_ice` compares it with the new `OROMEA` on
    thick, grounded land ice per domain. Options `oro_ice_check`
    (`warn`/`abort`/`off`), `oro_ice_check_mean_tolerance` (50 m),
    `oro_ice_check_p95_tolerance` (200 m), `oro_ice_check_min_thk` (500 m).
    In `flextest_20261123`, year 3144: Greenland mean −94 m, mean |·| 147 m,
    p95 381 m; Antarctica −51 m, 101 m, 442 m.
20. **An invalid `oro_update_mod` stops before anything is changed.** It was
    noticed only in `update_orography_generate_target_orography_file`, whose
    `return 42` nobody checked: the high-resolution orography already carried
    the chunk's change, `lupdate_orog` was switched on, and
    `latest_target_orography.nc` pointed to a file that was never written.
    `update_orography_check_options` now checks it (exit 43) at the start of
    `ice2echam`, and a failed target file stops the run (exit 42).

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
