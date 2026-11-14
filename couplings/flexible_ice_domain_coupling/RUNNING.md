# Running an experiment

How the runscripts have to look and which switches matter. The *why* is in
**README.md**; migrating the two reference experiments is in **MIGRATION.md**.

---

Two complete, ready-to-submit experiments are in `examples/ready_to_run/`:
`greenland_only/` (one ice sheet, flat layout) and `dual_hemisphere/`
(Greenland + Antarctica, one couple sub-directory each). Every path in them is
checked to exist; `cd` into either and submit its `awiesm_pism.yaml`.

## 1. What you need

```
awiesm_pism.yaml     the coupling runscript -- model1 = ESM, model2..N = one ice sheet each
awiesm.yaml          the ESM side (ECHAM/JSBACH + FESOM)
<domain>.yaml        one runscript per ice sheet, e.g. pism_nh.yaml, pism_sh.yaml
```

plus, for every ice domain, a component config
`configs/components/<domain>/` whose `atmo_forcing` / `ocean_forcing` point at

```yaml
atmo_forcing:  "${general.experiment_couple_dir}/<domain>/latest_atmo_forcing_file.nc"
ocean_forcing: "${general.experiment_couple_dir}/<domain>/latest_ocean_forcing_file.nc"
```

`pism_nh` and `pism_sh` already do. Plain `pism` points at the couple directory
without a subdirectory — that is the flat layout, see §2. A new domain needs a
new component config; copying `configs/components/pism_nh/` and replacing the
name throughout is enough.

Start it as usual, from the coupling runscript:

```sh
esm_runscripts awiesm_pism.yaml -e <expid>
```

Working set of all of these: `examples/`.

---

## 2. The coupling runscript

An ice sheet is an ice **domain** when its `modelN` block says so:

```yaml
general:
    iterative_coupling: True
    base_dir: "/work/ba0989/a270124/PalModII/experiments/"
    account: ba0989

model1:
    setup_name: awiesm
    runscript: awiesm.yaml
    chunk_size: 2
    chunk_unit: "years"

model2:
    setup_name: pism_nh              # == general.setup_name of pism_nh.yaml
    runscript: pism_nh.yaml
    chunk_size: 20
    chunk_unit: "years"
    ice_domain: True                 # <-- this is what builds ICE_DOMAINS
    ice_domain_box: "0,360,0,90"     # optional
    ice_domain_weight: 1             # optional

model3:
    setup_name: pism_sh
    runscript: pism_sh.yaml
    chunk_size: 20
    chunk_unit: "years"
    ice_domain: True
    ice_domain_box: "0,360,-90,0"
```

Adding an ice sheet = one more `modelN` block + its runscript + its component
config. Nothing in the coupling scripts changes.

| switch | where | effect |
|---|---|---|
| `ice_domain: True` | `modelN` | this model is an ice domain; its exchange files go to `${couple_dir}/<setup_name>/` |
| `ice_domain_box` | `modelN` or the domain's own section | `lon1,lon2,lat1,lat2`; clips this domain's contribution before the domains are combined. Only needed when the remapping halos of two neighbouring domains would overlap |
| `ice_domain_weight` | `modelN` or the domain's own section | scales this domain's contribution |
| `general.ice_domains` | coupling runscript | explicit list, overrides the `ice_domain: True` markers: `ice_domains: [pism_nh, pism_sh]` |

**Three layouts**, decided in this order:

1. `general.ice_domains` set → that list;
2. otherwise the `modelN` blocks with `ice_domain: True` → that list (recommended);
3. otherwise → `ICE_DOMAINS="."`, the **flat layout**: everything lands directly
   in `${couple_dir}`, every loop runs once, every reduction is a pass-through.
   This is what an existing single-ice-sheet experiment (`setup_name: pism`)
   wants — do *not* add `ice_domain: True` to it, or its component config will
   look for the forcing in the wrong place.

Single domain with `ice_domain: True` is fine too; the files then live in
`${couple_dir}/pism/` and the component config has to say so.

---

## 3. The ESM runscript (`awiesm.yaml`)

Nothing here knows how many ice sheets there are. Two things are coupling
specific — the four subjobs and the list of global files to wait for:

```yaml
general:
    setup_name: "awiesm"
    script_dir: "/home/.../couplings/flexible_ice_domain_coupling"   # absolute!
    files_to_wait_for:                # global products of ice2echam, no domain prefix
        - "latest_jsbach_init_file.nc"
        - "latest_gfw_atmo.nc"
        - "latest_jsbach_restart_file.nc"
        - "latest_veg_restart_file.nc"
        - "latest_target_orography.nc"
        - "landice_yearly_mass_loss.out"

echam:
    workflow:
        next_run_triggered_by: couple_out
        subjobs:
            couple_in:
                nproc: 1
                run_before: prepcompute
                script: coupling_ice2echam.functions
                script_dir: ${general.script_dir}/echam
                call_function: ice2echam
                env_preparation: ${general.script_dir}/envs/env_awiesm.py
                run_only: first_run_in_chunk
                skip_chunk_number: 1
            couple_out:
                nproc: 1
                run_after: tidy
                script: coupling_echam2ice.functions
                script_dir: ${general.script_dir}/echam
                call_function: echam2ice
                env_preparation: ${general.script_dir}/envs/env_awiesm.py
                run_only: last_run_in_chunk

fesom:
    workflow:                         # same shape, with
        ...                           #   coupling_ice2fesom.functions / ice2fesom
        ...                           #   coupling_fesom2ice.functions / fesom2ice
        ...                           #   and env_awiesm.py as well
```

Both components use **`env_awiesm.py`** (there is no separate `env_echam.py` /
`env_fesom.py` any more).

### Switches read from `echam:`

| key | default | effect |
|---|---|---|
| `update_ice_orography` | `True` | feed the ice sheet orography back into ECHAM |
| `update_glacial_mask` | `True` | feed the glacier mask back into ECHAM/JSBACH |
| `update_land_runoff` | `True` | feed the ice discharge into the runoff field |
| `oro_update_mod` | `1` | `0` = legacy (`OROMEA`/`GEOSP` from the low-res input), `1` = masked (`OROXXX` from T511, `OROMEA` from `${RES_echam}`). Modes 2–7 of the old tree no longer exist (see MIGRATION.md §4) |
| `oro_update_var` | `OROMEA` | orography variable that is updated. Must be **exactly** `OROMEA` or `GEOSP` — the comparison is case sensitive and anything else aborts `echam2ice` with "You have to choose either GEOSP or OROMEA" (the old tree only tested for lowercase `geosp` and silently took the OROMEA branch for everything else, so old runscripts may carry `oromea` or a typo) |
| `oro_hi_res_parent_file` | `""` | T511 file (variable `oromea`) to start the accumulated high-res orography from on the first orography update, when `${COUPLE_DIR}/echam6_new_orography_before_calnoro_high_res.nc` does not exist yet. For a branch: the parent's file **from the branch year**. Empty = PI T511. Prefer this over copying the file into `couple/` by hand: only a file given here goes through the start check |
| `oro_hi_res_tolerance` | `100` | metres. That start file is accepted only if its anomaly against PI T511 matches the restart's `oromea` anomaly against the ECHAM-grid reference (`oro_lo_res_reference_file`, default `unit.24`; mean over the changed cells). Typical: consistent branch ~35 m, parent file of the wrong year ~480 m, PI start of a branched run ~700 m |
| `oro_hi_res_on_mismatch` | `abort` | above the tolerance: `abort` (exit 48), `reconstruct` (PI T511 + bilinear T511 remap of the restart anomaly, everywhere, so glacial ice on today's shelves is kept), `ignore` (use the start file anyway; leaves `GEOSP` ≠ `OROMEA`) |
| `oro_regrid_method` | `remapcon` | how the ice sheet's orography reaches the ECHAM grids. `remapcon` = area mean of all ice-grid cells per ECHAM cell (needs cell corners in `ice.griddes`; falls back to `remapbil` with a message if they are missing). `remapbil` = legacy, samples only the 4 ice cells around each cell centre and puts single fjords or nunataks (±1000 m) into `OROMEA` |
| `oro_lo_res_source` | `high_res` | where `OROMEA` comes from when the high-res (T511) orography is updated. `high_res` = ECHAM-grid reference (`oro_lo_res_reference_file`, default `unit.24`) + area mean of the high-res change, wherever the surface changed, so `OROMEA`, `GEOSP` and the CALNORO parameters describe the same surface (needs `unit.24` and the T511 reference; stops with exit 49 otherwise). `restart` = legacy, restart `OROMEA` + its own regridded anomaly |
| `oro_lo_res_reference_file` | `${INIT_DIR_echam}/unit.24` | ECHAM-grid orography that belongs to the T511 reference (`hi_res_init_oro`, PI T511 unless `paleo_time` says otherwise). Used for `OROMEA` with `oro_lo_res_source: high_res` and for the `A_lo` anomaly of the high-res start check. **Must be a present-day surface.** In paleo setups `unit.24` comes from a glacial tarfile (`LGM_glac1d`, …) and already holds the glacial ice sheets; then set it to the present-day `jan_surf` of the same resolution, e.g. T63: `/pool/data/ECHAM6/input/r0007/T63/T63GR15_jan_surf.nc`. See §3a |
| `oro_lo_res_reference_check` | `abort` | checks that reference against the T511 reference on its glacier cells (`GLAC` > 0.5): more than `oro_lo_res_reference_max_frac` (default `0.05`) of them > 500 m apart → `abort` (exit 49), `warn` (go on), `off` (no check). Cached in `${couple_dir}/oro_lo_res_reference.checked` |
| `albedo_on_glaciers` | `0.7` | albedo prescribed on new glacier points |
| `glacial_threshold` | `0.5` | glacier fraction above which a cell counts as glaciated |
| `paleo_time` | `pi` | background state used for the orography/land surface |

### 3a. Paleo setups: the orography reference (glacial `unit.24`)

`OROMEA` is built as

```
OROMEA = REF_lo + remapcon( HIGH - REF_hi )
```

where `HIGH` is the accumulated high-resolution orography (an absolute
surface: `REF_hi` plus every ice-sheet change), `REF_hi` the T511 reference
`hi_res_init_oro` (PI T511) and `REF_lo` its ECHAM-grid partner. That is only
right if `REF_lo` and `REF_hi` are **the same surface**. `REF_lo` used to be
hard-wired to `unit.24`. With a glacial tarfile `unit.24` already contains
the paleo ice sheets, so they were counted twice:

| 140k_eq_smallLIS, after 4 legs | PISM surface | `GEOSP`/g | `OROMEA` with glacial `unit.24` | `OROMEA` with PI T63 reference |
|---|---|---|---|---|
| Laurentide (ice > 500 m) | 2016 m | 1960 m | **3794 m** | 1953 m |
| Eurasia | 1676 m | — | 1971 m | 1609 m |
| Greenland | 2280 m | — | 2443 m | 2201 m |
| N. American ice-free land | 454 m | 405 m | 958 m | 414 m |

`echam2ice` hands `OROMEA` to the ice sheet as the ECHAM height
(`oro_update_var: OROMEA`), so the downscaled air temperature for dEBM was
0.005 K/m × 1780 m ≈ **9 K too warm** over the Laurentide: JJA −14.8 → +0.3 °C
in three couplings, dEBM SMB on the North American ice +3700 → −10300 Gt/yr.
`GEOSP` and the sub-grid parameters come from `HIGH` and were right.

The same pair is used by the high-res start check (§ `oro_hi_res_*`,
`A_lo = restart OROMEA − REF_lo`). With a glacial `unit.24` that anomaly is
"restart − glacial ice" and the check meaningless; it is what stopped
140k_eq_v3 with a "925 m mismatch". With a present-day `REF_lo`, on
140k_eq_v2's 3300 restart: PI T511 as start 317 m (rejected, correctly), the
run's own high-res file 127 m (old: 753 m and 388 m). The 127 m are legacy
`OROMEA` regridding errors of runs made with the old tree; branches from such
runs may need `oro_hi_res_tolerance` above 100 m.

A **fresh** paleo experiment stops at that check on its first orography
update if it starts from PI T511 (the restart has the glacial ice, PI T511
not; exit 48, 317 m above). Give it the parent's high-res file
(`oro_hi_res_parent_file`; a copy put into `couple/` by hand skips the
check) or use `oro_hi_res_on_mismatch: reconstruct`. `reconstruct` rebuilds
the restart anomaly everywhere, so glacial ice on today's shelves survives.
140k_eq_smallLIS, restart 3301, against the restart `OROMEA`: glacier cells
−2 m on average (mean |difference| 41 m); the 853 T63 cells with > 100 m of
glacial ice on today's ocean 851 m against 883 m; paleo ocean 4 m. Rebuilding
on land only lost that shelf ice (Hudson Bay without ice in `GEOSP`), and a
land mask that included the glacial shelves still lost 117 m there.
The price: the bilinear remap spreads coastal anomalies onto ocean points of
the T511 field, by hundreds of metres next to ice-covered coasts (up to
~1100 m; numbers in CHANGES-vs-REFERENCE.md). Those points only enter the
sub-grid parameters of coastal cells; `GEOSP`, `OROMEA` and the land-sea
mask are not affected by it.

**What to set** in a paleo runscript (T63 example):

```yaml
echam:
    oro_lo_res_reference_file: "/pool/data/ECHAM6/input/r0007/T63/T63GR15_jan_surf.nc"
```

Its present-day coastline is intended: `HIGH − REF_hi` is the full change
since present day, exposed shelves included. Without the setting the check
stops the coupling (exit 49) instead of silently doubling the ice. Present-day
setups need nothing: `unit.24` passes (greenland_only: 10 of 2031 glacier cells
> 500 m off, 0.5 %; LGM_glac1d: 1305 of 3292, 40 %) and `OROMEA` is
bit-identical to before (checked).

### Switches read from `fesom:`

| key | default | effect |
|---|---|---|
| `use_icebergs` (or `fesom_use_iceberg`, or `general.with_icb`) | `False` | gates building the iceberg discharge in `ice2fesom` |
| `ice_discharge_target_grid` | `${echam.resolution}grid` | common grid used **only** when several domains have to be summed into one discharge file |
| `ice_discharge_remap` | `remapcon2` | cdo operator for that remapping |
| `iter_coup_regrid_method_ice2oce` | `INTERPOLATE` | `INTERPOLATE` / `REMAP` / `EXTRAPOLATE` / `NONE` (`NONE` = ice and ocean grid are identical) |
| `iter_coup_interact_method_oce2ice` | `OCEANTEMPSALT` | what the ocean hands to the ice sheet (see §4) |
| `cell_area_fesom_file` | `fesom.mesh.diag.nc` | used by the hosing correction (was `fesom_cell_area_file`) |
| `constant_landice_hosing_file` | — | appended to `landice_yearly_mass_loss.out` |
| `pyfesom_path` | `/pf/a/a270124/pyfesom2/` | pyfesom2 checkout |

### Iceberg discharge: one file per domain, or one combined file

Per-domain files are **always** written to `${couple_dir}/<domain>/latest_discharge.nc`.

* **several domains** → `ICE_DISCHARGE_MERGE` defaults to `0`: list the
  per-domain files in the FESOM runscript (each with its own `basin_file`,
  `domain`, `scaling_factor`), every domain keeping its native ice grid:

  ```yaml
  disch_file:
      - "${general.experiment_couple_dir}/pism_sh/latest_discharge.nc"
      - "${general.experiment_couple_dir}/pism_nh/latest_discharge.nc"
  ```

* **one domain** → `ICE_DISCHARGE_MERGE` defaults to `1`: a single
  `${couple_dir}/latest_discharge.nc` on the native grid, as before.

* To force a combined file for several domains, export
  `ICE_DISCHARGE_MERGE=1` **and** set `ice_discharge_target_grid` — otherwise
  the run stops with exit 73, because the native grids differ.

---

## 4. An ice domain runscript (`pism_nh.yaml`)

```yaml
general:
    setup_name: pism_nh              # must match model2.setup_name exactly
    script_dir: "/home/.../couplings/flexible_ice_domain_coupling"
    nyear: 10
    files_to_wait_for:               # relative to ${couple_dir} -> domain prefix
        - "pism_nh/latest_atmo_forcing_file.nc"
        - "pism_nh/latest_ocean_forcing_file.nc"

pism_nh:
    version: "github1.2.1"
    resolution: "5km"
    domain: "greenland"              # PISM's own domain name, unrelated to the ice domain
    pool_dir: "..."
    spinup_file: "..."
    ex_interval: "monthly"

    ablation_method: "DEBM"
    debm_path: "..."
    lapse_rate: -0.005
    iceberg_coupling: 1

    workflow:
        next_run_triggered_by: couple_out
        subjobs:
            couple_in:
                nproc: 1
                run_before: prepcompute
                script: coupling_awiesm2pism.functions
                script_dir: ${general.script_dir}/pism
                call_function: awiesm2pism
                env_preparation: ${general.script_dir}/envs/env_ice.py
                run_only: first_run_in_chunk
            couple_out:
                nproc: 1
                run_after: tidy
                script: coupling_pism2awiesm.functions
                script_dir: ${general.script_dir}/pism
                call_function: pism2awiesm
                env_preparation: ${general.script_dir}/envs/env_ice2awiesm.py
                run_only: last_run_in_chunk
```

`env_ice.py` / `env_ice2awiesm.py` replace the old `env_pism.py` /
`env_pism2awiesm.py`. They export the same `*_pism` variables, plus
`ICE_DOMAIN` (this domain) and `ICE_DOMAINS` (all of them).

### Switches read from the domain's own section

| key | default | effect |
|---|---|---|
| `regrid_method` | `DOWNSCALE` | atmosphere→ice: `DOWNSCALE` / `DYN_DOWNSCALE` / `INTERPOLATE` / `REMAP` |
| `ablation_method` | `PDD` | `PDD` / `DEBM` / `EBM` |
| `debm_path`, `debm_beta`, `debm_obl` | —, `999`, `23.441` | dEBM executable and parameters |
| `downscale_temp`, `downscale_precip` | `1`, `1` | downscale temperature / precipitation to the ice grid |
| `lapse_rate` | `-0.005` | lapse rate used by the downscaling |
| `reduce_temp`, `reduce_temp_by` | `0`, `1` | uniform temperature offset |
| `temp2_bias_file` | — | temperature bias correction field |
| `multi_year_mean_smb` | `1` | average the SMB over the chunk instead of using the last year |
| `use_ymonmean` | `0` | use a monthly climatology of the chunk |
| `iter_coup_interact_method_oce2ice` (or `ocean_ablation_method`) | `OCEANTEMPSALT` | `DIRECT` / `OCEANTEMP` / `OCEANTEMPSALT` / `PICO_OCEAN` / `TEMP2FLUX` / `TEMPSALT2FLUX` / `MIXED` |
| `PICO_basins_only_file` (or `basin_file`) | — | file holding the PICO `basins` field, required by `PICO_OCEAN`. Must be on the same grid as the domain's ocean forcing. Both spellings are accepted: the dual-hemisphere tree used the first, the single ice sheet tree the second |
| `iceberg_coupling` | `False` | this domain delivers ice→ocean forcing / iceberg discharge. The calving (PISM's discharge, chunk mean) then goes to FESOM as icebergs and is taken out of the mass loss sent to the rivers and the hosing; must match FESOM's `use_icebergs` (else exit 74). With `geometry.front_retreat.prescribed.file` in `kv_pairs` the discharge is 0 wherever that file allows no ice (e.g. Tibet), since PISM books cleared ice there as discharge |
| `thk_threshold` | `5.0` | ice thickness above which a cell counts as glaciated |
| `select_min_glacial_depth` | `1` | glacial mask criterion |
| `glacial_mask_from_pos_smb` | `1` | ECHAM glacier mask: `1` = ice thicker than `thk_threshold` **or** positive SMB, `0` = ice thickness only (ice-free land with positive SMB does not become glacier) |
| `orog_reference_pism` | `1` | reference the orography anomaly is taken against. **Only `0` or `1`**, checked like `oro_update_var`: anything else stops `pism2atmosphere` with exit 43. `0` = legacy (anomaly against this chunk's PISM input every chunk); `1` = at the first coupling of each ice domain hand ECHAM the absolute ice sheet surface (ECHAM's orography becomes the ice model's there), afterwards only the change over each chunk. "First" = no earlier `ice_file_for_atmosphere.nc` in the domain's couple dir (repeat-safe via `ice_orog_absolute.chunk`); not tied to a chunk number |
| `orog_surface_definition` | `physical` | surface height handed to the atmosphere. `physical` = what is actually there above sea level: `topg+thk` on grounded ice and land, the freeboard on floating ice, 0 over open ocean. `thk_plus_topg` = legacy, `topg+thk` everywhere (the bed depth over open ocean, so a retreat onto a marine bed pulled ECHAM's orography below sea level). Anything else: exit 49 |
| `sea_level_forcing` | `0` | apply a sea level forcing |
| `ANOMALY_AIR_TEMPERATURE`, `ANOMALY_PRECIPITATION` | `0` | atmospheric anomaly coupling; need `REFERENCE_ATMOS_FNAME` and `BASE_STATE_ATMOS_FNAME` |
| `ANOMALY_OCEAN_TEMPERATURE`, `ANOMALY_OCEAN_SALINITY` | `0` | ocean anomaly forcing per ice domain: `theta_ocean` / `salinity_ocean` = reference + (ocean model − base state), salinity clipped at 0. Needs `REFERENCE_OCEAN_FNAME` and `BASE_STATE_OCEAN_FNAME` in the domain's section (PISM names, domain grid); stops with exit 2 if either is missing |
| `chunk_size` (in the `modelN` block) | — | the domain's own chunk size; each domain may have its own |

---

## 5. Shell-only switches

Not in the yaml — export them in the environment if you need them:

| variable | default | effect |
|---|---|---|
| `ISM_TO_ECHAM_reset_glac_in_ice_domain` | `1` | clear the prescribed `unit.24` glacier fraction wherever any ice domain is active. `0` keeps the prescribed field |
| `ICE_DISCHARGE_MERGE` | `1` for one domain, `0` for several | write a single combined `latest_discharge.nc` |
| `WETDRY_ICE_DOMAIN` | first domain that has the file | which domain shapes the FESOM wet/dry mesh (that step handles only one; see README §6) |
| `ADD_UNCHANGED_ICE` | `0` | VILMA: add the unchanged ice outside the domains |
| `PDD_ICE_SURFACE_TEMP` | `1` | PDD path: also write `ice_surface_temp` = min(time-mean `air_temp`, 273.15) into `atmo_given_file.nc`. `0` writes the plain temp+precip file |
| `OCEAN_TEMP_LOWER_THRESHOLD` | `-260.` | floor for the ocean temperature anomaly |
| `OCEAN_SALT_LOWER_THRESHOLD` | `0.0` | floor for the ocean salinity anomaly |

---

## 6. Before you submit

* `script_dir:` and `env_preparation:` must be **absolute** (or via
  `${general.script_dir}`). A path starting with `.` is resolved against the
  runscript directory, not against `script_dir`, and would silently miss.
* `general.setup_name` of each ice runscript == `setup_name` in its `modelN`
  block == the directory name under `configs/components/`.
* `general.ice_domains`, if you use it, is comma separated — ESM-Tools writes
  the coupling environment as unquoted `export KEY=VALUE`, so no exported value
  may contain a space.
* `files_to_wait_for` on the ice side carries the domain prefix; on the ESM side
  it does not (those files are global).
* If an ice domain does not deliver, `ice2echam` now names the missing files and
  stops with exit 72 instead of failing later inside `cdo`.
* Set `PYTHONDONTWRITEBYTECODE` so the resubmission cannot fall over while
  caching the bytecode of an `env_*.py` module (seen as
  `ValueError: unmarshallable object` after a finished run):

  ```yaml
  computer:
      add_export_vars:
          PYTHONDONTWRITEBYTECODE: 1
  ```

  It belongs in every component runscript, not in the coupling runscript --
  each component writes its own `.run` script. All `examples/` runscripts
  carry it.
* If a domain's ice grid description has no cell corners (check with
  `grep -c '^xbounds' ${couple_dir}/<domain>/ice.griddes`), the masks and the
  discharge are regridded with `remapbil` instead of `remapcon2` and a warning
  is printed. That is not conservative; stage an `ice.griddes` with
  `xbounds`/`ybounds` for that domain if the discharge has to conserve mass.
* Syntax check of the shell layer:
  `bash -n general/*.functions echam/*.functions fesom/*.functions pism/*.functions vilma/*.functions`
