# CMIP7 gap-fill output for the AWI-ESM3 cc-LR spinup

The cc-LR spinup runscript (`awiesm3-v3.5.2-cc-albedo-TCO95L91-CORE3_spinup_branch2690.yaml`) lives in the experiment, not in this repository. These are the lines it needs on top of what it has. They take effect from the next leg; finished legs keep their gaps.

```yaml
fesom:
    # was false ("Match 2630-2634"): export, benthic flux, grazing, limitation, O2 diagnostics
    recom_cmip7_diags: true
    # daily temp and REcoM O2, pH, CO3, CO3satA, PhyChl, DiaChl, PhyC, DiaC around 200 dbar
    cmip7_lr_gapfill_output: true

oifs:
    # 100 m winds, hourly gust, blh, o3 (ml, plev19, lowest level hourly), pextra T/q
    # tendencies, aod550 by species, co2 on model levels
    cmip7_lr_gapfill_output: true
```

The defaults of all three switches stay false (`configs/components/fesom/fesom-2.8.yaml`, `configs/components/oifs/oifs.yaml`), so veg-HR and every other run that shares the templates writes what it wrote before. `co2fire` in `atm_remapped_1m` comes back for every run with CO2 coupling, without a switch: it was lost when the legs moved from `file_def_oifs_mon.xml.j2` to the CMIP7 template at 2840.

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

## Before the spinup picks this up

Run one test month from the current restart (design section 3.5): every new file exists, has the expected dimensions and time steps, is not constant, and is plausible (o3 1e-8 to 1e-5 kg/kg, pextra divided by the accumulation period around 1e-5 K/s, aod550 around 0.1, blh 1e2 to 1e3 m). The OpenIFS fields are checked against the develop-cc source only.
