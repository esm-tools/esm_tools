# Water Isotope (WISO) Integration for AWIESM3 - ESM-Tools Implementation

## Overview
This document summarizes the ESM-Tools configuration changes to enable water isotope (WISO) functionality in AWIESM3 with OpenIFS 48r1 and FESOM2.

## Changes Made

### 1. `/configs/setups/awiesm3/awiesm3.yaml`

#### General Configuration
- **Added**: `with_wiso: false` flag in general section (line 20)
- **Added**: `'develop-wiso'` to `available_versions` list (line 27)

#### New Version: `develop-wiso`
Created new version configuration (lines 76-87):
```yaml
develop-wiso:
    major_version: v3.4
    couplings:
        - fesom-2.7+oifs-48r1-wiso+xios-2.5+lpj_guess-4.1.2
    add_include_models:
        - xios
    oasis_with_yac: false
    xios_interp_order: 1
    with_co2_tracer: false
    with_co2_oce_coupling: false
    with_co2_veg_coupling: false
    with_wiso: true
```

#### OIFS WISO Configuration
Added `choose_general.with_wiso` block (lines 238-245):
```yaml
choose_general.with_wiso:
    true:
        branch: 'feature/wiso_implementation'
        add_namelist_changes:
            fort.4:
                NAMPHY:
                    LWISO: true
                    NWISO: 2
```

#### OASIS3-MCT Coupling Configuration
Added WISO coupling fields (lines 727-737):

**Atmosphere → Ocean (rstas.nc)**:
- `evap_o18_oce:evap_hdo_oce <--gauswgt_c-- A_Evap_O18:A_Evap_HDO`
- `prec_o18_oce:prec_hdo_oce <--gauswgt_c-- A_Precip_O18:A_Precip_HDO`
- `subl_o18_oce:subl_hdo_oce <--gauswgt_c-- A_Subl_O18:A_Subl_HDO`
- `roff_o18_oce:roff_hdo_oce <--gauswgt_c-- A_Runoff_O18:A_Runoff_HDO`

**Ocean → Atmosphere (rstos.nc)**:
- `A_SST_O18:A_SST_HDO <--gauswgt_i-- sst_o18_feom:sst_hdo_feom`
- `A_Ice_O18:A_Ice_HDO <--gauswgt_i-- ice_o18_feom:ice_hdo_feom`

### 2. `/configs/components/oifs/oifs.yaml`

#### Added WISO Version
- **Added**: `48r1-wiso` to `available_versions` list (line 121)
- **Added**: Version configuration block (lines 356-365):
```yaml
48r1-wiso:
    branch: 'feature/wiso_implementation'
    comp_command: "./openifs-bundle create; ./openifs-bundle build --threads=32 --openifs-only --without-double-precision --install; chmod -R ${source_code_permissions} .; git config core.fileMode false; mv install/bin/ifsMASTER.SP install/bin/OpenIFS"
    clean_command: rm -rf build install
    executable: OpenIFS
    git-repository: https://git.smhi.se/jan.streffing/oifs48r1.git
    install_bins: install/bin/OpenIFS
    destination: oifs-48r1
    with_xios: false
    major_version: 48r1
```

## How to Use

### Running with WISO
To use the water isotope functionality:

```bash
# In your runscript, set:
version: "develop-wiso"
```

This will:
1. Use the `feature/wiso_implementation` branch of OpenIFS 48r1
2. Enable WISO physics in OpenIFS (`LWISO=.true.`, `NWISO=2`)
3. Configure OASIS3-MCT coupling for isotope fields between atmosphere and ocean
4. Set up proper remapping and interpolation for isotope tracers

### Field Names
The implementation uses these exact field names (verified from source code):

**OpenIFS (Atmosphere)**:
- Send: `A_Evap_O18`, `A_Evap_HDO`, `A_Precip_O18`, `A_Precip_HDO`, `A_Subl_O18`, `A_Subl_HDO`, `A_Runoff_O18`, `A_Runoff_HDO`
- Receive: `A_SST_O18`, `A_SST_HDO`, `A_Ice_O18`, `A_Ice_HDO`

**FESOM2 (Ocean)** - Expected names:
- Send: `sst_o18_feom`, `sst_hdo_feom`, `ice_o18_feom`, `ice_hdo_feom`
- Receive: `evap_o18_oce`, `evap_hdo_oce`, `prec_o18_oce`, `prec_hdo_oce`, `subl_o18_oce`, `subl_hdo_oce`, `roff_o18_oce`, `roff_hdo_oce`

## Coupling Methods

All isotope fields use appropriate OASIS3-MCT coupling methods:
- **`gauswgt_c`**: Conservative Gaussian weight interpolation for fluxes (A→O)
- **`gauswgt_i`**: Instantaneous Gaussian weight interpolation for state variables (O→A)

## Integration with Existing Features

The WISO configuration is designed to be:
- **Mutually exclusive with CO2 coupling**: `develop-wiso` sets CO2 flags to false
- **Compatible with XIOS**: Uses standard XIOS 2.5
- **Compatible with LPJ-GUESS**: No interference with vegetation coupling

## Next Steps (for FESOM2 Implementation)

The FESOM2 model needs to:
1. Implement isotope tracers (O18 and HDO/D)
2. Configure isotope tracer names to match expected OASIS fields
3. Implement isotope fractionation in ocean/ice processes
4. Add isotope fields to FESOM2 output

## Validation

- ✅ YAML syntax validated for both configuration files
- ✅ Field names verified against OpenIFS source code implementation
- ✅ Coupling structure follows existing CO2 coupling pattern
- ✅ Branch references match actual repository structure

## References

- Design Document: `/work/ab0246/a270092/model_codes/stable_water_isotopes/wiso_esm_tools_design.md`
- OpenIFS WISO Branch: `feature/wiso_implementation` in `https://git.smhi.se/jan.streffing/oifs48r1.git`
- OpenIFS Coupling Implementation: `ifs-source/arpifs/ecearth/module/ecearth.F90`
