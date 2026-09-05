#!/usr/bin/env python3
"""
fesom_mesh_to_cdo.py -- native replacement for the meshtools-creator container's
`mesh-griddes` app (which used spheRlab's sl.grid.readFESOM + sl.grid.writeCDO).

pyfesom2 ships a direct port of exactly that spheRlab conversion in
pyfesom2.ascii_to_netcdf: read_fesom_ascii_grid() + write_mesh_to_netcdf().
This produces the CDO unstructured grid-description NetCDF (lon/lat + bounds,
cell_area, node_node_links, triag_nodes, coast, depth) that build_submesh feeds
to cdo to attach OCE_flag/GLAC_flag.

Usage:
    python fesom_mesh_to_cdo.py <submesh_dir> <ofile.nc>

Requires: the submesh_dir must already contain the ASCII mesh + the partitioner
outputs (nod2d.out, elem2d.out, aux3d.out, nlvls.out, cavity_depth@node.out,
cavity_nlvls.out, cavity_elvls.out) -- i.e. run fesom_meshpart first.
Runs with the ocp-tool python (pyfesom2 available), same env as remap_oasis_restart.
"""
import sys, os


def _load_ascii_to_netcdf():
    """Load pyfesom2.ascii_to_netcdf WITHOUT executing pyfesom2/__init__.py.

    The package __init__ eagerly imports .plotting (cartopy/matplotlib),
    .fesom2GeoFormat (osgeo/gdal), .regridding, .diagnostics, ... which costs
    ~50 s (cold: >120 s) -- paid on every mesh-change leg here. ascii_to_netcdf
    has no intra-package imports (only stdlib+numpy+netCDF4+numba), so we locate
    the package dir via find_spec (which does NOT run __init__) and exec just
    that module file. Cuts the griddes import from ~50 s to ~8 s (numba only).
    """
    import importlib.util
    spec_pkg = importlib.util.find_spec("pyfesom2")
    mod_path = os.path.join(os.path.dirname(spec_pkg.origin), "ascii_to_netcdf.py")
    spec = importlib.util.spec_from_file_location("_pf2_ascii_to_netcdf", mod_path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.read_fesom_ascii_grid, mod.write_mesh_to_netcdf


def main(griddir, ofile):
    read_fesom_ascii_grid, write_mesh_to_netcdf = _load_ascii_to_netcdf()
    if not griddir.endswith(os.sep):
        griddir += os.sep
    # rot=False: submeshes are already in geographical coordinates (FESOM issue
    # #83: force_rotation=.FALSE., rotated_grid=.true.). threeD=True for depth.
    grid = read_fesom_ascii_grid(griddir=griddir, rot=False, threeD=True)
    write_mesh_to_netcdf(
        grid, ofile=ofile, netcdf=True,
        cell_area=True, node_node_links=True, triag_nodes=True,
        coast=True, depth=True, overwrite=True,
    )
    print(f"     - wrote CDO grid description -> {ofile}")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(__doc__)
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])
