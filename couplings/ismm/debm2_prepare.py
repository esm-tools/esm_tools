#!/usr/bin/env python3
"""
Prepare the ISM-mapper's dEBM2 for one leg (ismm preprocess, general.debm_coupled).

Writes into the work directory what libdebm2.so reads at debm2_init:

    param   the dEBM2 parameter file (a Python file, dEBM2's own format)
    static  geometry, orbit and corrections, rebuilt every leg from the ice
            sheet's current geometry, so the lapse-rate correction and the
            masks follow PISM
    restart the initial surface state, only with --init-restart (first leg):
            a uniform firn column at atm_firnmax, dEBM2's 'AA' spin-up

The reference orography in the static file is only a placeholder: the mapper
replaces it every month with the atmosphere's own (<prefix>_Orog).

    debm2_prepare.py --debm2-dir DIR --geometry latest_ice_geometry.nc \
        [--surface-from pism_state.nc] --param debm2_param.py --static debm2_static.nc \
        --year Y [--init-restart R] \
        [--check-grid grids.nc --grid-name ismp] [dEBM2 settings]
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

import numpy as np
import netCDF4 as nc

# PISM mask values
ICE_FREE_LAND, GROUNDED, FLOATING, OCEAN = 0, 2, 3, 4

PARAM_TEMPLATE = '''\
# dEBM2 parameters for the ISM-mapper, written by esm_tools debm2_prepare.py
# for {exp}. Only the physics and orbit settings are used in a coupled run;
# the file-path fields are required by dEBM2's loader and are not read.
years          = [{year}]
forcing_dir    = "."
domain_file_hi = "."
domain_file_lo = "."
expdir         = "."
exp            = "{exp}"

hem            = "{hem}"
biascorr       = "{biascorr}"
precision      = "float64"
full_output    = False
Afac           = {afac!r}

# sublimation: 0 off, 1 losses only, 2 net (losses and deposition)
atm_subl       = {subl}
atm_firnmax    = {firnmax!r}

# The orbit the atmosphere runs with (OpenIFS NAMORB). dEBM2 uses the
# monthly declination and irradiance from it for the melt period; the TOA
# shortwave itself comes from the atmosphere.
orbit_ecc      = {ecc!r}
orbit_obl      = {obl!r}
orbit_lonp     = {lonp!r}
orbit_S0       = {s0!r}
'''


def write_param(path: Path, a: argparse.Namespace, afac: float) -> None:
    path.write_text(PARAM_TEMPLATE.format(
        exp=a.exp, year=a.year, hem=a.hem, biascorr=a.biascorr, afac=afac,
        subl=a.subl, firnmax=a.firnmax, ecc=a.orbit_ecc, obl=a.orbit_obl,
        lonp=a.orbit_lonp, s0=a.orbit_s0))


def _last(ds, name):
    v = np.ma.filled(np.asarray(ds[name][:], dtype=np.float64), np.nan)
    return v[-1] if v.ndim == 3 else v


def usurf_from_state(path: Path) -> np.ndarray:
    """
    Ice surface from thickness and bed, as PISM computes it: the bed plus the
    ice where it is grounded, the freeboard where it floats (sea level 0).
    """
    rho_ice, rho_sea = 910.0, 1028.0       # PISM's defaults
    with nc.Dataset(path) as ds:
        thk = _last(ds, "thk")
        topg = _last(ds, "topg")
    grounded_surface = topg + thk
    floating_surface = (1.0 - rho_ice / rho_sea) * thk
    return np.maximum(grounded_surface, floating_surface)


def read_geometry(path: Path, surface_from: Path | None = None):
    """
    lat, lon, usurf and mask of the last record, and the cell area [km2].

    pism2esm's geometry carries usurf; the chunk-1 bundles made before it did
    do not, and then the surface comes from the PISM state the ice sheet
    starts from (surface_from).
    """
    with nc.Dataset(path) as ds:
        lat = np.asarray(ds["lat"][:], dtype=np.float64)
        lon = np.asarray(ds["lon"][:], dtype=np.float64)
        mask = _last(ds, "mask")
        usurf = _last(ds, "usurf") if "usurf" in ds.variables else None
        x = np.asarray(ds["x"][:], dtype=np.float64)
        y = np.asarray(ds["y"][:], dtype=np.float64)
    if usurf is None:
        if surface_from is None:
            raise SystemExit(f"debm2_prepare: {path} has no usurf; pass --surface-from "
                             f"with the PISM state (thk, topg) the ice sheet starts from")
        usurf = usurf_from_state(surface_from)
        if usurf.shape != mask.shape:
            raise SystemExit(f"debm2_prepare: {surface_from} is {usurf.shape}, "
                             f"{path} is {mask.shape}")
        print(f"debm2_prepare: usurf from thk and topg of {surface_from}")
    dx = float(np.abs(np.diff(x)).mean())
    dy = float(np.abs(np.diff(y)).mean())
    return lat, lon, usurf, mask, dx * dy / 1e6


def build_static(lat, lon, usurf, mask, afac, cfg):
    from debm_orbit import compute_orbit
    from debm_static import StaticFields

    if str(cfg.biascorr).lower() != "none":
        raise SystemExit(f"debm2_prepare: biascorr '{cfg.biascorr}' needs an ERA5 and a model "
                         f"climatology on the ice-sheet grid, which the coupled set-up does "
                         f"not provide yet; use biascorr none")
    # The physics runs on land and ice (dEBM2's AIS set-up uses the land-sea
    # mask, so ice-free ground gets an SMB too); the totals are over the ice.
    land = np.isfinite(mask) & (mask <= FLOATING)
    ice = np.isfinite(mask) & ((mask == GROUNDED) | (mask == FLOATING))
    z_surf = np.where(np.isfinite(usurf), usurf, 0.0)
    orb = compute_orbit(lat, cfg)
    return StaticFields(
        lon=lon, lat=lat, lsm=land.astype(float), imsk=land.astype(float),
        gmsk=ice.astype(float), z_surf=z_surf,
        z_ref=z_surf.copy(),           # placeholder, the mapper hands over the atmosphere's
        af=afac, decli=orb.decli, irrad=orb.irrad,
        dTdz=cfg.atm.dTdz, hydr_start=(3 if cfg.hem.upper() == "AIS" else 9),
    )


def check_grid(static, grids: Path, name: str) -> None:
    """The static file must be on the grid OASIS delivers to the mapper."""
    with nc.Dataset(grids) as ds:
        olat = np.asarray(ds[f"{name}.lat"][:], dtype=np.float64)
    if olat.shape != static.lat.shape:
        raise SystemExit(f"debm2_prepare: {grids} {name}.lat is {olat.shape}, the ice-sheet "
                         f"geometry is {static.lat.shape}")
    err = float(np.nanmax(np.abs(olat - static.lat)))
    if err > 1e-3:
        raise SystemExit(f"debm2_prepare: ice-sheet geometry and OASIS grid {name} disagree "
                         f"by {err:.3g} deg in latitude (flipped or transposed?)")
    print(f"debm2_prepare: geometry matches OASIS grid {name} (max |dlat| {err:.2e} deg)")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--debm2-dir", required=True, help="dEBM2 source tree (the debm2 component)")
    ap.add_argument("--geometry", required=True, type=Path)
    ap.add_argument("--surface-from", default="",
                    help="PISM state with thk and topg, for a geometry file without usurf")
    ap.add_argument("--param", required=True, type=Path)
    ap.add_argument("--static", required=True, type=Path)
    ap.add_argument("--init-restart", type=Path)
    ap.add_argument("--year", type=int, required=True, help="first year of this leg")
    ap.add_argument("--check-grid", type=Path)
    ap.add_argument("--grid-name", default="ismp")
    ap.add_argument("--exp", default="awiesm3")
    ap.add_argument("--hem", default="AIS")
    ap.add_argument("--biascorr", default="none")
    ap.add_argument("--subl", type=int, default=2)
    ap.add_argument("--firnmax", type=float, default=10000.0)
    ap.add_argument("--orbit-ecc", type=float, required=True)
    ap.add_argument("--orbit-obl", type=float, required=True)
    ap.add_argument("--orbit-lonp", type=float, required=True)
    ap.add_argument("--orbit-s0", type=float, default=1361.0)
    a = ap.parse_args()

    sys.path[:0] = [a.debm2_dir]
    from debm_config import load_config
    from debm_state import SurfaceState

    lat, lon, usurf, mask, afac = read_geometry(
        a.geometry, Path(a.surface_from) if a.surface_from else None)
    write_param(a.param, a, afac)
    cfg = load_config(a.param)

    static = build_static(lat, lon, usurf, mask, afac, cfg)
    if a.check_grid is not None:
        if a.check_grid.exists():
            check_grid(static, a.check_grid, a.grid_name)
        else:
            print(f"debm2_prepare: {a.check_grid} not there yet, grid not checked")
    static.save(a.static, exp=a.exp, geometry=str(a.geometry))
    print(f"debm2_prepare: {a.static}  {static.summary()}  (from {a.geometry})")

    if a.init_restart is not None:
        state = SurfaceState.uniform(static.shape, cfg.atm.firnmax, year=a.year, month=0)
        state.save(a.init_restart, exp=a.exp, do_spinup="AA")
        print(f"debm2_prepare: initial state {a.init_restart}  {state.summary()}")


if __name__ == "__main__":
    main()
