# PISM-FESOM Iterative Coupling System

## Overview

This coupling system enables dynamic interaction between the PISM ice sheet model and FESOM ocean model within the ESM-Tools framework. The ocean mesh adaptively responds to changing ice sheet geometry, including ice shelf cavities.

## New Components (7 files)

### Bash/Shell Scripts

- **`coupling_pism2esm.functions`** - Main PISM→ESM coupling orchestrator
- **`coupling_ice2fesomUKK.functions`** - Ice→FESOM coupling with dynamic submesh generation
- **`test.sh`** - Test harness for coupling workflow
- **`env_pism2awiesm.txt`** - Runtime environment snapshot (atmosphere coupling)
- **`env_pism2ocean.txt`** - Runtime environment snapshot (ocean coupling)

### Python Script

- **`mesh_flags.py`** - Generates mesh masks from PISM ice geometry
  - Reads PISM NetCDF output (ice mask, ice shelf base depth)
  - Reads FESOM maximum mesh (ASCII: `nod2d.out`, `aux3d.out`)
  - Outputs: `topo_raw.txt`, `cavity_raw.txt`, `mask.txt`

### Fortran Program

- **`fesom_submesh_UKK.F90`** - Mesh reduction algorithm
  - Creates ocean submesh from maximum mesh based on ice extent
  - Handles ice shelf cavity geometry
  - Tracks node/element mappings
  - Outputs reduced mesh files

## Call Tree

```
test.sh (Test Entry Point)
│
├─── pism2esm()  [coupling_pism2esm.functions]
│    │
│    ├─── cdo: Extract ice geometry variables
│    │         (ice_subNN, shelf_msk, ocean_msk, etc.)
│    │
│    ├─── pism2atmosphere()  [coupling_pism2atmosphere.functions]
│    │    └─── (atmosphere forcing generation)
│    │
│    └─── pism2ocean()  [coupling_pism2ocean.functions]
│         └─── (ocean forcing preparation)
│
└─── ice2fesom()  [coupling_ice2fesomUKK.functions]
     │
     ├─── IF (CHANGE_OCEAN == 1):
     │    │
     │    └─── build_submesh()
     │         │
     │         ├─── cdo: Regrid ice geometry to ocean grid
     │         │         (remapbil → T512, remapnn → core2)
     │         │
     │         ├─── python3 mesh_flags.py
     │         │    │
     │         │    ├─── Read: PISM geometry NetCDF
     │         │    │         (mask, ice_subNN)
     │         │    │
     │         │    ├─── Read: FESOM max mesh
     │         │    │         (nod2d.out, aux3d.out)
     │         │    │
     │         │    └─── Write: topo_raw.txt
     │         │              cavity_raw.txt
     │         │              mask.txt
     │         │
     │         └─── gfortran + ./fesom_submesh.x
     │              │
     │              ├─── Read: topo_raw.txt
     │              │         cavity_raw.txt
     │              │         mask.txt
     │              │         MAXMESH files
     │              │
     │              ├─── mesh_reduce_byflag()
     │              │    └─── Filter nodes/elements
     │              │
     │              └─── Write: Reduced mesh to SUBMESH_DIR/
     │                         (nod2d.out, elem2d.out, aux3d.out, etc.)
     │
     ├─── iterative_coupling_ice_fesom1x_write_names()
     │    └─── Generate variable name mappings
     │         (ice_names_for_fesom.dat)
     │
     ├─── REGRID METHOD (ice2oce):
     │    │
     │    ├─── INTERPOLATE/REMAP:
     │    │    └─── iterative_coupling_ice_ocean_regrid_interpolate()
     │    │
     │    ├─── EXTRAPOLATE:
     │    │    ├─── iterative_coupling_ice_ocean_regrid_interpolate()
     │    │    └─── iterative_coupling_ice_ocean_regrid_extrapolate()
     │    │
     │    └─── NONE:
     │         └─── iterative_coupling_ice_ocean_regrid_none()
     │
     ├─── iterative_coupling_ice_fesom_rename_vars()
     │    └─── ncrename: Set FESOM variable names
     │
     └─── iterative_coupling_pism_ocean_prepare_ocean_icebergmodel_forcing()
          └─── cdo: Process iceberg discharge data
```

## Data Flow

```
PISM Output                    Python Processing              Fortran Processing
─────────────────              ──────────────────             ──────────────────
latest_ex_file_pism.nc    →    mesh_flags.py          →      fesom_submesh.x
  ├─ mask                       ├─ Read PISM mask             ├─ Read mask.txt
  ├─ usurf                      ├─ Read ice_subNN             ├─ Read topo_raw.txt
  ├─ thk                        └─ Generate flags             ├─ Read cavity_raw.txt
  └─ ...                                                      └─ Create submesh
                                      ↓                              ↓
                                topo_raw.txt              Reduced Mesh Files
                                cavity_raw.txt            ─────────────────
                                mask.txt                  nod2d.out
                                                         elem2d.out
                                                         aux3d.out
                                                         cavity_depth.out
                                                              ↓
                                                    FESOM Ocean Model
```

## Key PISM Mask Values

| Mask | Description | Treatment |
|------|-------------|-----------|
| 1 | Ice-free land | Excluded (topo=9999) |
| 2 | Grounded ice | Excluded (topo=9999) |
| 3 | Floating ice shelf | **Included** (cavity) |
| 4 | Open ocean | **Included** |
| 5 | Special case | Handled separately |

## Workflow Execution

1. **PISM runs** → generates `latest_ex_file_pism.nc`
2. **`pism2esm()`** extracts ice geometry variables using CDO
3. **`ice2fesom()`** triggered:
   - If ocean mesh change needed (`CHANGE_OCEAN=1`):
     - Regrids ice geometry to ocean resolution
     - Python generates mesh masks from ice state
     - Fortran builds reduced ocean mesh
   - Prepares iceberg discharge forcing
   - Regrids/interpolates to final ocean grid
   - Writes forcing file for FESOM

## Environment

- **HPC System**: Levante (DKRZ)
- **Compilers**: Intel oneAPI 2022.0.1, GCC 11.2.0
- **MPI**: OpenMPI 4.1.2
- **Libraries**: NetCDF-C/Fortran 4.8.1/4.5.3, HDF5 1.12.1
- **Tools**: CDO 2.0.5, NCO 5.0.6