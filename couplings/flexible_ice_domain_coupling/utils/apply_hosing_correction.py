"""Distribute the total land ice mass loss evenly over the FESOM ocean.

Any number of ice domains may contribute. Each contributes one
(discharge file, cell area file) pair; the pairs are integrated separately --
they usually live on different ice sheet grids -- and the resulting volume
fluxes are summed before they are spread over the FESOM surface.

Usage (multi domain):

    apply_hosing_correction.py --fesom_cell_area FILE --hosing_dir DIR \
        --pair discharge_1.nc area_1.nc [--pair discharge_2.nc area_2.nc ...]

Constant hosing on top (optional, repeatable, at most 8 extra regions):

    ... --constant_region NODES VALUE UNIT

    NODES  FESOM node list (first line the number of nodes, then 1-based node
           numbers), copied to HOSING_DIR/landice_nodes_in_region_<i>.out
    VALUE  the constant freshwater flux into that region
    UNIT   Sv (spread evenly over the region's node area) or m/s

FESOM (oce_landice_water.F90) gives every node the flux of the *last* region
that lists it, and region 1 is the whole ocean. So the value written for an
extra region is the ice sheet hosing plus the constant, and the ice sheet
hosing still reaches the nodes of that region.

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
        parser.add_argument(
            "--constant_region",
            nargs=3,
            action="append",
            default=[],
            metavar=("NODES", "VALUE", "UNIT"),
            help="constant hosing into a region of FESOM nodes; UNIT is Sv or m/s",
        )
        args = parser.parse_args(argv)
        return (args.fesom_cell_area, args.hosing_dir, args.pair,
                args.discharge_varname, args.constant_region)

    if len(argv) != 4:
        raise SystemExit(__doc__)
    discharge_file, cell_area_file, fesom_cell_area_file, hosing_dir = argv
    return (
        fesom_cell_area_file,
        hosing_dir,
        [[discharge_file, cell_area_file]],
        DISCHARGE_VARNAME,
        [],
    )


def fesom_node_area(cell_area_fesom_file):
    """Surface area (m2) of every FESOM node, in node order."""
    print(" * Loading FESOM node areas from ", cell_area_fesom_file)
    fl = xr.open_dataset(cell_area_fesom_file, engine="netcdf4")
    if "nod_area" in fl.variables:
        area = fl.nod_area
        return (area[0, :] if area.ndim > 1 else area).values
    if "cell_area" in fl.variables:
        return fl.cell_area.squeeze().values
    raise SystemExit(
        f" * Neither nod_area nor cell_area found in {cell_area_fesom_file}"
    )


def read_nodes(nodes_file):
    """1-based FESOM node numbers from a landice_nodes_in_region file."""
    with open(nodes_file) as f:
        values = f.read().split()
    if not values:
        raise SystemExit(f" * Empty node list: {nodes_file}")
    count, nodes = int(values[0]), [int(v) for v in values[1:]]
    if count != len(nodes):
        raise SystemExit(
            f" * {nodes_file}: header says {count} nodes, the file lists {len(nodes)}"
        )
    return nodes


def write_nodes(nodes, path):
    with open(path, "w") as f:
        f.write(f"{len(nodes)}\n")
        f.write("\n".join(str(n) for n in nodes) + "\n")


def constant_flux(value, unit, nodes, node_area):
    """The constant hosing of one region as a surface flux in m/s."""
    if unit == "m/s":
        return value
    if unit.lower() == "sv":
        region_area = float(node_area[[n - 1 for n in nodes]].sum())
        return value * 1.0e6 / region_area
    raise SystemExit(f" * Unknown unit {unit!r} for a constant region (Sv or m/s)")


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
    fesom_cell_area_file, hosing_dir, pairs, varname, constant_regions = parse_arguments(argv)

    for path in ([fesom_cell_area_file] + [f for pair in pairs for f in pair]
                 + [region[0] for region in constant_regions]):
        if not os.path.exists(path):
            raise SystemExit(f" * File does not exist: {path}")
    if len(constant_regions) > 8:
        raise SystemExit(" * FESOM reads at most 9 regions (1 + 8 constant ones)")

    node_area = fesom_node_area(fesom_cell_area_file)
    fesom_area = node_area.sum()
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

    mass_loss = float(discharge.squeeze().values)
    fluxes = [mass_loss]
    for i, (nodes_file, value, unit) in enumerate(constant_regions, start=2):
        nodes = read_nodes(nodes_file)
        target = os.path.join(hosing_dir, f"landice_nodes_in_region_{i}.out")
        if os.path.abspath(nodes_file) != os.path.abspath(target):
            write_nodes(nodes, target)
        extra = constant_flux(float(value), unit, nodes, node_area)
        print(f" * region {i}: {len(nodes)} nodes, constant {value} {unit} = {extra} m/s,"
              f" plus the ice sheet hosing {mass_loss} m/s")
        # the last region listing a node wins in FESOM, so add the hosing here
        fluxes.append(mass_loss + extra)

    with open(os.path.join(hosing_dir, "landice_yearly_mass_loss.out"), "w") as f:
        f.write(str(len(fluxes)) + "\n" + " ".join(str(v) for v in fluxes) + "\n")


if __name__ == "__main__":
    main(sys.argv[1:])
