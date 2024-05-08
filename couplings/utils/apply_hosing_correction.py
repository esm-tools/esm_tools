import pandas as pd
import xarray as xr
import sys
import os.path

rho_water = 1000 #kg/m3

discharge_file = sys.argv[1]
cell_area_echam_file = sys.argv[2]
cell_area_fesom_file = sys.argv[3]
hosing_dir = sys.argv[4]

if os.path.exists(cell_area_fesom_file):
    print(" * Loading total FESOM grid area from ", cell_area_fesom_file)
    fl = xr.open_dataset(cell_area_fesom_file, engine="netcdf4")
    print(" * Summing up FESOM grid areas")
    if "nod_area" in fl.variables:
        total_FESOM_cell_area = fl.nod_area[0, :].sum().squeeze().values
    elif "cell_area" in fl.variables:
        total_FESOM_cell_area = fl.cell_area[:].sum().squeeze().values
    print(" * Total cell area = ", total_FESOM_cell_area)
else:
    print(" * File does not exist!")
    exit

if os.path.exists(discharge_file):
    print(" * Loading ice discharge from ", discharge_file)
    fl1 = xr.open_dataset(discharge_file, engine="netcdf4")
else:
    print(" * File does not exist!")
    exit

if os.path.exists(discharge_file):
    print(" * Loading total ECHAM grid area from ", cell_area_echam_file)
    fl2 = xr.open_dataset(cell_area_echam_file, engine="netcdf4")
    print(" * Integrate discharge over ECHAM grid")
    # Convert kg/s into m3/s
    discharge_tot = fl1.total_ice_mass_loss_flux.weighted(fl2.cell_area.squeeze()).sum() / rho_water
    print(" * Distribute over FESOM domain evenly")
    discharge = -discharge_tot / total_FESOM_cell_area
else:
    print(" * File does not exist!")
    exit

if not os.path.isfile(os.path.join(hosing_dir, "landice_nodes_in_region_1.out")):
    df = pd.read_csv(os.path.join(os.path.dirname(cell_area_fesom_file), "nod2d.out"), sep="\s+", skiprows=[0], header=None)
    n=df.iloc[:, 0]
    n.to_csv(os.path.join(hosing_dir, "landice_nodes_in_region_1.out"), header=[str(len(n.values))], index=False)

mass_loss = discharge.squeeze().values
with open(os.path.join(hosing_dir, "landice_yearly_mass_loss.out"), 'w') as f:
    f.write(str(1) + "\n" + str(mass_loss))

