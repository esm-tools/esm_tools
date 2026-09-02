"""Distribute the total land ice mass loss evenly over the FESOM ocean.

Any number of ice domains may contribute. Each contributes one
(discharge file, cell area file) pair; the pairs are integrated separately --
they usually live on different ice sheet grids -- and the resulting volume
fluxes are summed before they are spread over the FESOM surface.

Usage (multi domain):

    apply_hosing_correction.py --fesom_cell_area FILE --hosing_dir DIR \
        --pair discharge_1.nc area_1.nc [--pair discharge_2.nc area_2.nc ...]

Usage (legacy single domain, kept so that older call sites keep working):

    apply_hosing_correction.py DISCHARGE AREA FESOM_CELL_AREA HOSING_DIR
"""

import argparse
import os.path
import sys

import pandas as pd
import xarray as xr

RHO_WATER = 1000  # kg/m3
DISCHARGE_VARNAME = "total_ice_mass_loss_flux"


def parse_arguments(argv):
    """Accept both the pair based and the legacy positional interface."""
    if argv and argv[0].startswith("-"):
        parser = argparse.ArgumentParser(description=__doc__)
        parser.add_argument("--fesom_cell_area", required=True)
        parser.add_argument("--hosing_dir", required=True)
        parser.add_argument(
            "--pair",
            nargs=2,
            action="append",
            metavar=("DISCHARGE", "CELL_AREA"),
            required=True,
            help="one ice domain's discharge file and its cell area file",
        )
        parser.add_argument("--discharge_varname", default=DISCHARGE_VARNAME)
        args = parser.parse_args(argv)
        return args.fesom_cell_area, args.hosing_dir, args.pair, args.discharge_varname

    if len(argv) != 4:
        raise SystemExit(__doc__)
    discharge_file, cell_area_file, fesom_cell_area_file, hosing_dir = argv
    return (
        fesom_cell_area_file,
        hosing_dir,
        [[discharge_file, cell_area_file]],
        DISCHARGE_VARNAME,
    )


def total_fesom_cell_area(cell_area_fesom_file):
    print(" * Loading total FESOM grid area from ", cell_area_fesom_file)
    fl = xr.open_dataset(cell_area_fesom_file, engine="netcdf4")
    print(" * Summing up FESOM grid areas")
    if "nod_area" in fl.variables:
        return fl.nod_area[0, :].sum().squeeze().values
    if "cell_area" in fl.variables:
        return fl.cell_area[:].sum().squeeze().values
    raise SystemExit(
        f" * Neither nod_area nor cell_area found in {cell_area_fesom_file}"
    )


def integrate_discharge(discharge_file, cell_area_file, varname):
    """Volume flux (m3/s) of one ice domain."""
    print(" * Loading ice discharge from ", discharge_file)
    discharge = xr.open_dataset(discharge_file, engine="netcdf4")
    print(" * Loading the matching grid cell area from ", cell_area_file)
    area = xr.open_dataset(cell_area_file, engine="netcdf4")
    print(" * Integrate discharge over the ice grid")
    # Convert kg/s into m3/s
    return (
        discharge[varname].weighted(area.cell_area.squeeze()).sum() / RHO_WATER
    )


def main(argv):
    fesom_cell_area_file, hosing_dir, pairs, varname = parse_arguments(argv)

    for path in [fesom_cell_area_file] + [f for pair in pairs for f in pair]:
        if not os.path.exists(path):
            raise SystemExit(f" * File does not exist: {path}")

    fesom_area = total_fesom_cell_area(fesom_cell_area_file)
    print(" * Total cell area = ", fesom_area)

    discharge_tot = None
    for discharge_file, cell_area_file in pairs:
        contribution = integrate_discharge(discharge_file, cell_area_file, varname)
        print(f" * {discharge_file}: {float(contribution.squeeze().values)} m3/s")
        discharge_tot = (
            contribution if discharge_tot is None else discharge_tot + contribution
        )

    print(f" * Total over {len(pairs)} ice domain(s), distribute over FESOM evenly")
    discharge = -discharge_tot / fesom_area

    region_file = os.path.join(hosing_dir, "landice_nodes_in_region_1.out")
    if not os.path.isfile(region_file):
        nod2d = pd.read_csv(
            os.path.join(os.path.dirname(fesom_cell_area_file), "nod2d.out"),
            sep=r"\s+",
            skiprows=[0],
            header=None,
        )
        nodes = nod2d.iloc[:, 0]
        nodes.to_csv(region_file, header=[str(len(nodes.values))], index=False)

    mass_loss = discharge.squeeze().values
    with open(os.path.join(hosing_dir, "landice_yearly_mass_loss.out"), "w") as f:
        f.write(str(1) + "\n" + str(mass_loss))


if __name__ == "__main__":
    main(sys.argv[1:])
