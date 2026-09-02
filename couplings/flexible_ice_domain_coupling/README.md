# Flexible ice domain coupling

A single coupling scheme for AWI-ESM (ECHAM6/JSBACH + FESOM) and **any number
of ice sheet domains**, replacing the two schemes that existed before:

| before | |
|---|---|
| `couplings/` | one ice sheet, everything flat in `${COUPLE_DIR}` |
| `couplings/coupling_dual-hemisphere/` | exactly two, hard coded as `pism_nh` / `pism_sh` |

Both are the same code with one difference — *namespacing* — plus one place
where they genuinely diverge: how several ice sheets are merged into the single
orography, glacial mask and runoff field that ECHAM has. This directory keeps
the namespacing as a first class concept and replaces the merge with an
operation that does not care how many domains there are or where they sit.

The code here is based on the current single-hemisphere tree (the maintained
one), not on the older dual-hemisphere copy.

---

## 1. The idea

### Ice domains

An **ice domain** is one ice sheet model instance taking part in the coupling —
`pism_nh`, `pism_sh`, `pism_gris`, ... In ESM-Tools each is its own `modelN` in
the iterative coupling, so its setup name is a natural identifier.

```
ICE_DOMAINS     "pism_nh,pism_sh,pism_gris"   -- all domains in this experiment
ICE_DOMAIN      "pism_nh"                     -- the one currently being worked on
ICE_COUPLE_DIR  ${COUPLE_DIR}/pism_nh         -- its private exchange directory
COUPLE_DIR      ${COUPLE_DIR}                 -- reserved for *global* exchange files
```

The rule that makes the whole thing work:

> Anything that belongs to exactly one ice sheet lives in `${ICE_COUPLE_DIR}`.
> `${COUPLE_DIR}` holds only what the atmosphere and the ocean produce or
> consume: `atmosphere_file_for_ice.nc`, `ocean_file_for_ice.nc`,
> `ocean.griddes`, `gfw_atmo*.nc`, `target_orography*.nc`,
> `latest_jsbach_init_file.nc`, `latest_discharge.nc`, ...

The dual-hemisphere code applied this rule inconsistently — `ice.griddes`,
`atmo_given_file.nc` and the per-domain downscaling files were sometimes
per-hemisphere and sometimes shared, so two hemispheres could race on the same
file. Here the ice sheet side *always* works in `${ICE_COUPLE_DIR}`, and the
global files it reads are **staged in** (copied into the domain directory) at
the start of `awiesm2pism`. That makes the domains independent by construction.

A single domain is not a special case, it is a list of length one. A list
containing the pseudo domain `.` maps `ICE_COUPLE_DIR` back onto `COUPLE_DIR`
and reproduces the original flat layout.

### Three stages instead of one

The ESM side used to be one function per direction. It is now three:

```
ice2echam                              ice2fesom
  ice2echam_per_domain   (per domain)    ice2fesom_regrid_domain   (per domain)
  ice2echam_reduce       (combine)       ice2fesom_reduce          (combine)
  ice2echam_global       (once)          rename / iceberg discharge (once)
```

The ice sheet side stays one function per direction — it only ever sees its own
domain.

### How the domains are combined

The dual-hemisphere scheme merged the two hemispheres by cutting a latitude band
out of each field and concatenating them along the latitude axis:

```
cdo sellonlatbox,0,360,0,90   ...   # NH half
cdo sellonlatbox,0,360,-90,0  ...   # SH half
ncecat / ncwa -a time / ncpdq -a lat,lon,record
ncrcat  nh  sh  ->  global
ncpdq -a record,lat,lon ; ncwa -a record
```

That works only for exactly two domains that tile the globe in latitude, in the
right order. It cannot express Greenland plus a Laurentide domain.

Here the merge is a **field reduction** instead (`ice_domain_reduce`): every
domain's contribution is brought onto the target grid, zero-filled outside its
own footprint, and then combined with the operator that fits the quantity:

| quantity | operator | why |
|---|---|---|
| orography anomaly (`ice_orog_difference_*grid.nc`) | `add` | anomalies against the same background add up |
| ice discharge (`ice_discharge_*grid.nc`) | `add` | mass fluxes add up |
| glacial mask, ice domain mask | `max` | a cell is glaciated as soon as one domain says so |
| ice→ocean forcing | `add` for fluxes/geometry, `max` for masks (per variable) | mixed file, see `ice_domain_reduce_by_varclass` |

No knowledge of *where* a domain sits is needed. Domains may overlap, may be
listed in any order, and may be added or removed without touching any code.

This also fixes the orography merge. The old code merged
`background + anomaly_domain` fields, which is only correct if the domains are
disjoint *and* tile the globe. Here the per-domain stage emits the **anomaly**,
the anomalies are summed, and the background is added once — which is the right
answer for overlapping domains as well.

### Optional per-domain attributes

```yaml
model2:
    setup_name: pism_nh
    ice_domain: True
    ice_domain_box: "0,360,0,90"   # clip this domain's contribution to a lon/lat box
    ice_domain_weight: 1           # scale this domain's contribution
```

`ice_domain_box` is insurance against remapping halos of two adjacent domains
overlapping. It is optional — leave it out unless you need it.

### One domain behaves exactly as before

`ice_domain_reduce` short-circuits to a plain file copy when exactly one domain
contributes and no remapping, box or weight is requested. Missing values are
preserved, no `cdo` operator touches the field. Single ice sheet experiments
therefore go through the same code path they always did.

---

## 2. Configuring it

`examples/` contains a complete, runnable-shaped set:

```
awiesm_pism_multi_domain.yaml   the coupling runscript: model1 = AWI-ESM,
                                model2..N = one ice sheet each
awiesm_pism_single_domain.yaml  the same with one ice sheet
awiesm.yaml                     the ESM side
pism_nh.yaml / pism_sh.yaml / pism_gris.yaml   one per ice domain

reference_mono/   the pi_ctl_1x1_icb runscripts, migrated to this tree
reference_dual/   the spinup_20260818 runscripts, migrated to this tree
```

`reference_mono/` and `reference_dual/` are the two production experiments this
tree has to replace `couplings/` and `couplings/coupling_dual-hemisphere/` for.
Every migrated line is marked `# CHANGED` / `# ADDED`; see **MIGRATION.md**.

Adding a fourth ice sheet is one `modelN` block plus its runscript.

The domain list is determined, in this order:

1. `general.ice_domains` — an explicit list or comma separated string;
2. the `modelN` sections carrying `ice_domain: True` (recommended);
3. neither — the experiment runs in the flat single ice sheet layout.

`env_preparation:` and `script_dir:` must be **absolute paths**. ESM-Tools
resolves a path starting with `.` against the runscript directory, not against
`script_dir`, so `../envs/env_ice.py` would silently look in the wrong place.

`ICE_DOMAINS` is comma separated on purpose: ESM-Tools writes the coupling
environment as unquoted `export KEY=VALUE` lines, so no exported value may
contain a space.

---

## 3. Layout

```
general/ice_domains.functions     the domain registry: list, select, foreach,
                                  attributes, stage-in, reduce
general/coupling_general.functions  as before, plus a domain aware read_names
general/general_helpers.functions   unchanged
general/general_lists.functions     unchanged
general/pism_helpers.functions      unchanged

echam/coupling_echam2ice.functions  unchanged (global -> one global file)
echam/coupling_ice2echam.functions  restructured into per-domain / reduce / global

fesom/coupling_fesom2ice.functions  unchanged (global -> one global file)
fesom/coupling_ice2fesom.functions  per-domain regrid + reduce; discharge and
                                    hosing aggregated over domains

pism/coupling_awiesm2pism.functions   couple_in of one domain; stages the global files in
pism/coupling_pism2awiesm.functions   couple_out of one domain
pism/coupling_atmosphere2pism.functions
pism/coupling_ocean2pism.functions
pism/coupling_pism2atmosphere.functions
pism/coupling_pism2ocean.functions
pism/coupling_pism2solidearth.functions
pism/coupling_solidearth2pism.functions   all of these now work in ${ICE_COUPLE_DIR}

vilma/coupling_ice2vilma.functions  per-domain regrid onto the VILMA grid + reduce
vilma/coupling_vilma2ice.functions  unchanged (VILMA is global)

envs/ice_domain_env.py    shared helper: builds the domain list and attributes
envs/env_awiesm.py        ESM side, now also exports ICE_DOMAINS
envs/env_ice.py           one ice domain, couple_in   (was env_pism.py)
envs/env_ice2awiesm.py    one ice domain, couple_out  (was env_pism2awiesm.py)

utils/apply_hosing_correction.py   accepts any number of (discharge, area) pairs
utils/                             otherwise unchanged
examples/                          runscripts, incl. both migrated reference experiments

MIGRATION.md            how to run the two reference experiments with this tree
CHANGES-vs-REFERENCE.md what can move numbers relative to those reference runs
```

`env_pism.py` and `env_pism2awiesm.py` became `env_ice.py` and
`env_ice2awiesm.py` because they are no longer about "the PISM component" but
about "one ice domain". They keep exporting the same `*_pism` variable names, so
none of the shell code had to be renamed.

---

## 4. What changed beyond namespacing

These are behaviour changes, not just refactoring. They are listed here because
they are the parts worth reviewing.

1. **Orography merge** (see above): the per-domain stage now emits the anomaly
   on the ECHAM grid; the background is added once, globally. For one domain the
   result is the same field as before.

2. **`GLAC` inside the ice domains.** The old code had

   ```sh
   if [[ ${DOMAIN_pism} == "greenland" ]]; then cdo setclonlatbox,0,0,360,0,90 ...
   if [[ ${DOMAIN_pism} == "nhem" ]];      then cdo setclonlatbox,0,0,360,0,90 ...
   ```

   in `set_glacial_mask_echam_make_dummy_jan_surf` — a northern hemisphere box,
   and dead code on the ESM side because `DOMAIN_pism` is not exported there.
   It is now driven by the reduced ice domain mask: the prescribed `unit.24`
   glacier fraction is cleared wherever *any* ice domain is active. Switch it off
   with `ISM_TO_ECHAM_reset_glac_in_ice_domain=0`.

3. **Hard coded `T63`** in the `oro_update_mod` branch and in the `ice_orog.nc`
   branch is now `${RES_echam}`. Those paths only worked at T63 before.

4. **Hard coded component directories.** `../outdata/pism/`,
   `../restart/pism/` and `${COUPLE_DIR}/../` became `${OUTPUT_DIR_pism}`,
   `${RESTART_DIR_pism}` and `${ICE_EXP_DIR}` — the component directory is named
   after the domain (`outdata/pism_nh`), so it cannot be spelled out.

5. **`CHUNK_SIZE_pism_standalone`** came from `config["model2"]["chunk_size"]`,
   i.e. from whichever model happened to be second. It is now the component's
   own chunk size (`general.this_chunk_size`).

6. **`ocean_file_for_ice.nc` is no longer moved away.** `regrid_none` did
   `mv ${COUPLE_DIR}/ocean_file_for_ice.nc ...`, which would have deleted the
   shared file from under the second domain. It now moves the staged per-domain
   copy.

7. **`apply_hosing_correction.py`** takes `--pair DISCHARGE AREA` any number of
   times and sums the integrals. The old four-positional-argument form still
   works.

8. **`config["pism"]` in `env_pism.py`.** Two entries (`thk_threshold`,
   `select_min_glacial_depth`) were read from a section literally named `pism`
   while everything else used `config[setup_name]` — so they silently missed
   whenever the component was called anything else. `env_ice.py` reads them
   from the component's own section.

9. **`ice2echam` aborts when an ice domain did not deliver.** The old wait loop
   fell through after ten minutes and then failed somewhere inside `cdo` with an
   unhelpful message. It now names the missing files and stops with exit 72.

10. **`pism2ocean` delivers its combined ice→ocean forcing.**
    `ice_file_at_ocean.combined.nc` was only ever written into the ice sheet's
    work directory, so `ice2fesom` always took its "file not there, skipping"
    branch. It is now copied into `${ICE_COUPLE_DIR}`, which is where
    `ice2fesom` looks.

---

## 5. Iceberg discharge: per domain or combined

FESOM can be configured either way, and both reference setups exist, so this
tree writes the per-domain files always and the combined one on request:

* **one file per ice domain** (AWI-ESM 2.6 style). The runscript lists them, and
  `basin_file`, `domain` and `scaling_factor` are lists too:

  ```yaml
  disch_file:
      - "${general.experiment_couple_dir}/pism_sh/latest_discharge.nc"
      - "${general.experiment_couple_dir}/pism_nh/latest_discharge.nc"
  ```

  Each domain keeps its native ice grid. This is the default for more than one
  domain (`ICE_DISCHARGE_MERGE=0`).

* **a single `${COUPLE_DIR}/latest_discharge.nc`** (AWI-ESM 2.1/2.5 style, the
  default of the `fesom-2.5`/`fesom-2.6` configs). This is the default for a
  single domain, where the merge is a pass-through and the native grid is kept.
  To combine several domains into one file set `ICE_DISCHARGE_MERGE=1` and
  `ICE_DISCHARGE_TARGET_GRID` (their native grids differ, so they have to be
  summed on a common one).

## 6. Known limitation

The FESOM **wet/dry mesh cutting** (`iterative_coupling_ice_fesom_wetdry_*`)
reads the ice shelf geometry from a single ice grid file, because
`utils/wetdry/wetdry_maxmesh_nodhn.py` takes one `--pism_cut_nc`. With more than
one ice domain only one of them can shape the mesh. `wetdry_resolve_cut_nc`
picks the first domain that has the file and warns; set `WETDRY_ICE_DOMAIN` to
choose explicitly. Making this multi-domain needs a change in that Python
helper, which is not part of this tree.

(This affects only the wet/dry mesh, not the iceberg discharge above.)

---

## 7. Quick check of the shell layer

```sh
bash -n general/*.functions echam/*.functions fesom/*.functions \
       pism/*.functions vilma/*.functions
```

The `#!/bin/ksh` shebangs are decorative — ESM-Tools *sources* these files into
a job script whose interpreter is `bash` (`computer.sh_interpreter`). Several of
them use `function name() { ... }`, which real `ksh` rejects.
