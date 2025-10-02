# ICON-FESOM within ESM-Tools
## Setup
ESM-Tools can now run ICON-FESOM simulations. The setup is still in development. It is based on ICON v2024.10 and FESOM v2.6.2. Compilation and running has been tested on Levante. Both gcc and intel work. Intel is currently the default as (according to Claudia Frauen from DKRZ) runs up to 50% faster. To get started follow these steps:
1. Install ESM-Tools following https://esm-tools.readthedocs.io/en/latest/installation.html
2. Switch branch and install: 
```bash
cd esm_tools
git checkout feature/icon-fesom
./install.sh
```
3. Get and compile source code:
```bash
cd /path/to/where/source/code/should/go
esm_master install-icon-fesom-v1.0
```
4. Run a test simulation: Open and edit yaml runscript `icon-fesom-pictl.yaml`:
```bash
vim ~/esm/esm_tools/runscripts/icon-fesom/icon-fesom-pictl.yaml
```
Submit test run (monthly restarts, runs for 3 months):
```bash
esm_runscripts -e test_icon_fesom icon-fesom-pictl.yaml

To do an "operational" with run yearly restarts, please see comments in `icon-fesom-pictl.yaml`.

## Namelist changes
To change namelists settings, you can
1. directly edit the namelists in the ESM-Tools source code tree.
    * ICON namelists can be found at `namelists/icon/2024.10-public/`
    * FESOM namelists can be found at `namelists/fesom2/2.6.2-yac/`
2. use the yaml runscript `icon-fesom-pictl.yaml` to change settings. Example: modify `tune_entrorg`:
```yaml
add_namelist_changes:
  NAMELIST_atm:
    nwp_tuning_nml:
      tune_entrorg: 2.2e-3
```

## Limitations
* Currently only levante is supported. Easy switching between gnu and intel will be implemented if it is actually needed.
* Environments are loaded from ICON's and FESOM's compile scripts which is not very flexible and against the ESM-Tools philosophy where environments are defined in the machine yaml files and adapted in the setup yaml file (in our case `configs/setups/icon-fesom/icon-fesom.yaml`).
* Only R2B4 is supported, once R2B5 is available it will be added.
* Input data is taken from `/work/bb0519/foci_input2/ICON-FESOM/`. For the directory structure initially used by Sveta was kept. May change in the future.

## Contact
For questions on the ICON-FESOM within ESM-Tools, please contact Sebastian Wahl (swahl@geomar.de).