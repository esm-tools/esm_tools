# CMIP7 output for the AWI-ESM3 cc-LR runs

The cc-LR runscripts (for example `awiesm3-v3.5.2-cc-albedo-TCO95L91-CORE3_spinup_branch2690.yaml`) live in the experiment, not in this repository. With `cmip7_cmor_output: true` for OpenIFS and FESOM they now write the full CMIP7 set without further switches. Changes take effect from the next leg; finished legs keep their gaps.

What comes with `cmip7_cmor_output`, for every run that uses the CMIP7 templates:

- OpenIFS (`cmip7/file_def_oifs_cmip7_spinup.xml.j2`): 100 m winds (1hrPt), blh (1hr and 3hrPt), hourly maximum gust, o3 on model and pressure levels and at the lowest model level (hourly), and co2 on model levels with `general.with_co2_tracer`. `co2fire` in `atm_remapped_1m` is written for every run with CO2 coupling. Global DDH is switched off (`NAMDDH LHDGLB/LHDPRG`), because these fields change its restart buffer length; it writes no output in this setup.
- FESOM (`xios_xml_cmip7/file_def_fesom.xml.j2`): daily temp and, with REcoM, O2, pH, CO3, CO3satA, PhyChl, DiaChl, PhyC and DiaC on the two layers around 200 dbar, on meshes that set `fesom.op20bar_levels` (CORE3 and CORE3_nocav 14 15, DARS2 16 17). Other meshes get no 200 dbar files.
- For the cc versions (`v3.5.2-cc`, `develop-cc`), `awiesm3.yaml` also sets `fesom.recom_cmip7_diags: true` (export and benthic fluxes, grazing, limitation, O2 saturation and minimum, inventories). A runscript value wins: the spinup runscript still sets it to false until production piControl switches it on.

veg-HR runs that move to this branch with `cmip7_cmor_output` get the OpenIFS fields above too, and DDH goes off.

Not switched on:

- `oifs.cmip7_pextra_tendencies` (T/q physics tendencies, `tnt*`/`tnhus*`): off. With FullPos, 48r1v8 aborts in POS (`YSD_XAD%NLEVS /= KFLEVG`) until `SUXIOS_PEXTRA_FIELDS` sets `NCEXTR` before `NAMDPHY` uses it.
- The XIOS `aod550*` fields are not written: with `NAERMACC=1` the `aod550` slot is the lowest-level 550 nm extinction coefficient (`climaer_layer.F90:156-201`), not a column AOD, and the species fields are column masses. od550aer is an output gap.

## Ocean CFC-12 and SF6 (optional)

```yaml
fesom:
    with_transit_tracers: true
    # piControl and spinup: constant atmospheric values, zero for CFC-12 and SF6
    transit_anthro: false
```

For historical and scenario runs set `transit_anthro: true` and `transit_input_file` to a table with the 22 header lines `read_transit_input` (mod_transit.F90) skips. The copy on albedo, `/albedo/work/projects/p_plioMIP3_prod/tracer_forcing/Table_CO2_isoC_CFCs1112_SF6.txt`, has 15 header lines and ends in 2015, so it does not fit as it is.

Both tracers are required restart variables. Before the first leg with `with_transit_tracers: true`, add zero restart fields to the oce restart directory that leg reads, for example:

```bash
python utils/fesom_add_zero_tracer_restart.py \
    <experiment>/restart/fesom/fesom.2850-01-01.oce.restart sf6 cfc12
```

It writes `sf6.nc`, `sf6_M1.nc`, `cfc12.nc` and `cfc12_M1.nc` with the structure, time and iter of `temp.nc` / `temp_M1.nc`, and refuses to overwrite existing files.

## Tested

One month (2850-01) branched from the v3.5.2 spinup restart: every new stream exists, has the expected dimensions and time steps and plausible values, and the pycmor rules for the 96 products built from them pass.
