#!/usr/bin/env python
"""Make the ocean->ice handoff use the cavity that FESOM already knows about.

WHY THIS EXISTS
---------------
FESOM writes `fw`, the freshwater flux, at EVERY ocean node.  Under an ice
shelf it is basal melt; in the open ocean it is precipitation minus
evaporation; along the coast it is dominated by sea-ice formation, which
REMOVES freshwater and therefore carries the opposite sign.  The coupling
remaps the whole field and hands it to PISM as `shelfbmassflux`, so the coastal
signal lands on the shelf cells next to the calving front and cancels part of
the melt.  Measured on one chunk of ism41_v5g:

    FESOM cavity melt                         774 Gt/yr
    of which arrives on PISM's shelves        779   (101 %, so the remap is fine)
    added by non-cavity nodes                -355
    ------------------------------------------------
    what PISM receives                        424

In the ring 0-50 km outside the cavity, fw runs at +5.2 to +5.7e-5 kg/m2/s with
70-80 % of nodes positive: read the way PISM reads the variable that is
REFREEZING at 1.8-2.0 m/yr, against a cavity melt of 0.83.

`shelfbtemp` has the mirror-image fault.  It comes from `Tsurf`, the temperature
of level 1, and under a cavity level 1 is inside the ice: Tsurf is missing at
all 7509 cavity nodes.  PISM therefore gets extrapolated open-ocean surface
temperature, -1.07 degC, where the freezing point at the ice draft is -2.11.

The masking this restores is not new: coupling_fesom2ice.functions already
extracts `cavity_flag_extended` and carries a branch that masks a flux to the
shelf region.  That branch cannot run -- its test is `x[q,w]net` while the loop
variables are `Tsurf Ssurf fh fw`, and the file it writes is never passed to the
extraction script, which reads the raw FESOM directory by variable and year.
Repairing it there would mean reworking that interface, so the same intent is
applied here instead, at the two points where everything needed is in hand.

WHAT EACH STAGE DOES
--------------------
mask  (on the FESOM node file, before remapping)
      sets shelfbmassflux to missing outside the cavity, and records the exact
      cavity total so the next stage can conserve it.

fix   (on the PISM grid, after remapping and hole-filling)
      1. renormalises shelfbmassflux so its integral over PISM's floating cells
         equals the recorded FESOM cavity total.  This is not cosmetic: a
         distance-weighted average of a RATE does not conserve a FLUX when the
         target area exceeds the source area, and PISM's floating domain is
         about 11 % larger than FESOM's cavity, so the masked field integrates
         to ~1065 Gt/yr against a truth of 774 even with no hole-filling.
      2. replaces shelfbtemp by the freezing point at PISM's own ice draft,
         which is what the variable means and what FESOM's own three-equation
         scheme uses:  tf = a*S + b + c*z  with a = -0.0575, b = 0.0901,
         c = 7.61e-4 (cavity_param.F90).  The water at the cavity top, -1.72
         degC, sits 0.39 degC above that -- correct for water that is melting
         ice, wrong for the temperature OF the ice base.

The renormalisation factor is computed against PISM's mask 3 at the start of the
leg (see pism_floating_mask for where that comes from and why it is not a
flotation test).  That mask MOVES while the leg runs, and by more than an earlier
estimate here claimed: over ism43 chunk 2 it went 1176 -> 1208 x10^3 km2, +2.7 %,
and the melt reaching it went from 0.985 to 1.027 of the conserved total, so the
same forcing over-delivers by about 4 % by the end of a ten-year leg.  (The figure
previously given here, 0.68 % of shelf area and ~5 Gt/yr on 774, was too small.)

So the factor must be recomputed every coupling step and never carried over -- and
note that this within-leg drift is now the LARGER of the two conservation errors,
bigger than the mask mismatch that pism_floating_mask exists to remove.  Closing
it needs a shorter coupling step, not a better mask.

usage:
  cavity_consistency.py mask --node-file F --submesh-root DIR --total-file T
                             [--submesh DIR] [--var V] [--temp-var V]
  cavity_consistency.py fix  --ice-file F --pism-file P --total-file T
                             [--melt-var V] [--temp-var V] [--salinity S]
                             [--pism-mask-file EX]
"""
import argparse
import os
import sys

import numpy as np
import netCDF4

R_EARTH = 6371000.0
SECONDS_PER_YEAR = 365 * 86400.0
# freezing point of sea water at the ice-ocean interface, as FESOM's
# cavity_param.F90 writes it:  tf = a*sal + (b + c*zice), zice negative downward
TF_A, TF_B, TF_C = -0.0575, 0.0901, 7.61e-4
RHO_ICE, RHO_SEAWATER = 910.0, 1028.0


def node_areas(submesh):
    """Cluster area per node: a third of each triangle it belongs to."""
    nod = np.loadtxt(os.path.join(submesh, "nod2d.out"), skiprows=1)
    elem = np.loadtxt(os.path.join(submesh, "elem2d.out"), skiprows=1, dtype=int) - 1
    lon, lat = np.radians(nod[:, 1]), np.radians(nod[:, 2])
    xyz = np.stack([np.cos(lat) * np.cos(lon),
                    np.cos(lat) * np.sin(lon),
                    np.sin(lat)], axis=1)
    a, b, c = xyz[elem[:, 0]], xyz[elem[:, 1]], xyz[elem[:, 2]]
    tri = 0.5 * np.linalg.norm(np.cross(b - a, c - a), axis=1) * R_EARTH ** 2
    area = np.zeros(len(nod))
    for k in range(3):
        np.add.at(area, elem[:, k], tri / 3.0)
    return area


def cavity_nodes(submesh):
    """Nodes that carry a cavity, as FESOM's own submesh records them.

    cavity_nlvls.out names the first wet level: 1 where the sea surface is free,
    greater where the top levels are buried in the shelf above.  That is the
    file to trust.  cavity_depth@node.out, which the first version of this
    helper read, is NOT equivalent to it -- it leaves the ice-base depth at
    exactly 0.0 on some nodes that cavity_nlvls puts 4 to 13 levels under ice,
    and reading it would mask those nodes away as open ocean and throw their
    melt out:

        submesh                       nlvls>1   depth<0   thrown away
        ism41_v5g submesh_1916           7509      7288           221
        orog6     submesh_1901           5619      5444           175

    The nlvls>1 set is also, node for node on both meshes, exactly the set where
    the forcing file itself carries missing Tsurf/Ssurf -- FESOM has no sea
    surface under an ice shelf.  check_against_field() holds it to that.
    """
    nlvls = np.loadtxt(os.path.join(submesh, "cavity_nlvls.out"), dtype=int)
    if nlvls.ndim > 1:
        nlvls = nlvls[:, -1]
    return nlvls > 1


def submesh_node_count(submesh):
    """The first number of nod2d.out is the node count."""
    try:
        with open(os.path.join(submesh, "nod2d.out")) as handle:
            return int(handle.readline().split()[0])
    except (OSError, ValueError, IndexError):
        return -1


def pick_submesh(root, nodes, explicit=None):
    """Find the submesh the field was written on, by NODE COUNT.

    The cavity is recarved every chunk and the node count moves with it -- over
    the eighteen chunks of ism41_v5g it ran from 217004 to 217424 and back down
    -- so the mesh belonging to a given field is the one whose count matches,
    and nothing else identifies it safely.  Names do not: `latest_submesh` is
    the mesh carved for the NEXT leg, `previous_submesh` the one before it, and
    the newest directory is whichever was carved last.  Masking with the wrong
    mesh would silently mask the wrong nodes, so a mismatch stops the step
    instead of falling back to something plausible.  check_against_field() then
    confirms the winner against the field's own witness of the cavity, because a
    matching count alone does not identify the mesh.

    ``root`` may name SEVERAL directories, os.pathsep-separated, because the mesh
    a given forcing was written on does not always live in the couple dir:

        chunk 1   the forcing comes from a harvest, and the mesh with it
        chunk 2   the forcing was written during fesom's FIRST leg, which ran on
                  the INITIAL mesh -- that one is in the fesom mesh dir, and the
                  couple dir holds no submesh_* of that vintage at all
        chunk 3+  couple/submesh_* , carved by the interactive-mesh step

    Roots are searched in the order given and the first match wins.
    """
    if explicit:
        got = submesh_node_count(explicit)
        if got == nodes:
            return explicit
        sys.exit(f"cavity_consistency: {explicit} has {got} nodes, the field has "
                 f"{nodes} -- wrong submesh for this chunk")

    roots = [r for r in root.split(os.pathsep) if r and os.path.isdir(r)]
    if not roots:
        sys.exit(f"cavity_consistency: none of the submesh roots exist: {root}")

    candidates = []
    for one in roots:
        # a root may BE a mesh (the fesom mesh dir is not a directory of them)
        if submesh_node_count(one) > 0:
            candidates.append(one)
        candidates += [os.path.join(one, n)
                       for n in ("previous_submesh", "latest_submesh")
                       if os.path.isdir(os.path.join(one, n))]
        candidates += sorted(
            (os.path.join(one, d) for d in os.listdir(one)
             if d.startswith("submesh_") and os.path.isdir(os.path.join(one, d))),
            reverse=True)

    for path in candidates:
        if submesh_node_count(path) == nodes:
            return path
    sys.exit(f"cavity_consistency: no submesh with {nodes} nodes under any of "
             f"{', '.join(roots)} (looked at {len(candidates)} candidates)")


def check_against_field(ds, cav, temp_var):
    """Hold the submesh's cavity against the forcing file's own witness of it.

    Tsurf and Ssurf (here already renamed to shelfbtemp) are level-1 values, and
    under an ice shelf level 1 is inside the ice, so FESOM writes them missing on
    exactly the cavity nodes.  The file being masked therefore carries an
    independent copy of the answer, and it agreed with cavity_nlvls.out node for
    node on both meshes this was measured on (7509 and 5619 nodes, zero
    disagreement).

    A matching node count is necessary but NOT sufficient to identify the right
    submesh: the cavity is recarved every chunk and the count wanders back over
    values it has held before, so two different meshes can both match.  Masking
    with the wrong nodes leaves no trace afterwards, hence this stops the step
    rather than warning.

    If the file carries no missing values at all the attribute was lost on the
    way (cdo drops _FillValue in some conversions) and there is nothing to check
    against -- that is reported, not treated as a disagreement.
    """
    if temp_var not in ds.variables:
        print(f"     - {temp_var} not in the file: cavity cross-check skipped")
        return
    missing = np.ma.getmaskarray(ds.variables[temp_var][:])
    missing = missing.reshape(-1, missing.shape[-1])[0]
    if missing.shape != cav.shape:
        sys.exit(f"cavity_consistency: {temp_var} has {missing.shape[0]} nodes, "
                 f"the submesh has {cav.shape[0]}")
    if not missing.any():
        print(f"     - {temp_var} carries no missing values: cavity "
              f"cross-check skipped")
        return
    disagree = int((missing != cav).sum())
    if disagree:
        sys.exit(f"cavity_consistency: the submesh says {int(cav.sum())} cavity "
                 f"nodes, the missing {temp_var} in the field says "
                 f"{int(missing.sum())}, and they disagree on {disagree} of them "
                 f"-- this submesh does not belong to this field")
    print(f"     - cavity agrees with the missing {temp_var} on all "
          f"{int(cav.sum())} nodes")


def stage_mask(args):
    ds = netCDF4.Dataset(args.node_file, "a")
    if args.var not in ds.variables:
        ds.close()
        sys.exit(f"cavity_consistency: {args.var} not in {args.node_file}")
    var = ds.variables[args.var]
    values = np.ma.filled(var[:], 0.0).astype("f8")

    submesh = pick_submesh(args.submesh_root, values.shape[-1], args.submesh)
    print(f"     - submesh {os.path.basename(os.path.realpath(submesh))} "
          f"({values.shape[-1]} nodes)")
    cav = cavity_nodes(submesh)
    check_against_field(ds, cav, args.temp_var)
    area = node_areas(submesh)
    args.submesh = submesh

    # FESOM's fw is negative where freshwater enters the ocean, i.e. where the ice
    # melts -- but this stage runs on the file AFTER cdo setpartabn, whose partable
    # carries `factor = -1` on fw -> shelfbmassflux (esm_tools 33f71c607).  So the
    # field is already positive-for-melting here and must not be negated again.
    flat = values.reshape(-1, values.shape[-1])
    cavity_total = (flat[0][cav] * area[cav]).sum() * SECONDS_PER_YEAR / 1e12
    if cavity_total < 0.0:
        print(f"     ! WARNING: the cavity integrates to {cavity_total:+.0f} Gt/yr, "
              f"i.e. net refreezing over the whole cavity.  Expect melt here: "
              f"check that the cdo partable still applies factor = -1 to fw.")

    fill = var.getncattr("_FillValue") if "_FillValue" in var.ncattrs() else -9.0e33
    values[..., ~cav] = fill
    var[:] = values
    var.setncattr("missing_value", np.array(fill, dtype=var.dtype))
    ds.setncattr("cavity_masked_by", "cavity_consistency.py")
    ds.close()

    with open(args.total_file, "w") as handle:
        handle.write(f"{cavity_total:.6f}\n")
    print(f"     - cavity nodes {int(cav.sum())} of {len(cav)}, "
          f"area {area[cav].sum()/1e9:.0f} x10^3 km2")
    print(f"     - FESOM cavity melt {cavity_total:+.0f} Gt/yr -> {args.total_file}")


PISM_MASK_FLOATING = 3


def pism_floating_mask(path, shape):
    """PISM's own mask 3, from the last step of the previous leg's ex-file.

    Returns None only when NOTHING WAS ASKED FOR -- no path, or a path that does
    not exist, which is the normal case on chunk 1 of a fresh tree and makes the
    caller fall back to a flotation test.

    A file that IS there and cannot supply a usable mask is a CONFIGURATION ERROR
    and stops the step, rather than falling back: the caller has already announced
    in the log that it is using this file, so a quiet fallback would leave the log
    claiming one thing and the arithmetic doing another.  That applies both to a
    file with no `mask` in it (wrong kind of file -- a FESOM node file, say) and to
    one whose mask is on a different grid.

    PISM restarts do not carry `mask` (it is diagnostic), which is why this reads
    an ex-file rather than the file thk and topg come from.
    """
    if not path or not os.path.isfile(path):
        return None
    ds = netCDF4.Dataset(path)
    try:
        if "mask" not in ds.variables:
            sys.exit(f"cavity_consistency: {path} was given as the PISM mask file "
                     f"but carries no `mask` variable -- wrong kind of file. A PISM "
                     f"ex-file has one; a restart and a FESOM node file do not.")
        mask = np.ma.filled(ds.variables["mask"][:], 0)
        mask = np.squeeze(mask)
        if mask.ndim == 3:                     # (time, y, x) -> last step
            mask = mask[-1]
        if mask.shape != shape:
            sys.exit(f"cavity_consistency: {path} has mask{mask.shape}, the ice "
                     f"geometry is {shape} -- wrong grid, refusing to guess")
        floating = mask == PISM_MASK_FLOATING
        print(f"     - floating set: PISM's own mask 3 from "
              f"{os.path.basename(path)} ({int(floating.sum())} cells)")
        return floating
    finally:
        ds.close()


def stage_fix(args):
    with open(args.total_file) as handle:
        cavity_total = float(handle.read().strip())

    pism = netCDF4.Dataset(args.pism_file)
    thk = np.ma.filled(np.squeeze(pism.variables["thk"][:]), 0.0)
    topg = np.ma.filled(np.squeeze(pism.variables["topg"][:]), 0.0)
    dx = abs(float(pism.variables["x"][1]) - float(pism.variables["x"][0]))
    dy = abs(float(pism.variables["y"][1]) - float(pism.variables["y"][0]))
    pism.close()
    if thk.ndim == 3:                      # a restart may carry a time axis
        thk, topg = thk[-1], topg[-1]
    cell_area = dx * dy

    # The set the renormalisation conserves over MUST be the set PISM will apply
    # shelfbmassflux to, which is mask 3 and nothing else.  A flotation test is
    # not that set, and the difference is not small when the geometry did not come
    # out of PISM: measured on ism43 chunk 1, whose input is the raw BEDMAP2
    # bootstrap, the test held 1797 cells more than mask 3, carrying 112 of
    # 783 Gt/yr, so PISM received 0.79 of the conserved total.  85 % of that was
    # 1600 cells PISM calls mask 4 -- sub-grid calving front, sitting 261 m clear
    # of flotation, so no threshold tweak reaches them -- and the rest 197 cells
    # PISM calls grounded, within 10 m of flotation.
    #
    # So PISM is asked instead of guessed: mask 3 is read from the last step of the
    # previous leg's ex-file, which is PISM's own classification of the state it is
    # about to restart from.  Reproducing PISM's part_grid bookkeeping here was
    # tried on paper and abandoned -- ice_area_specific_volume, the obvious
    # discriminant, is identically zero in the bootstrap file.
    #
    # Chunk 1 has no previous leg, so it falls back to the flotation test and says
    # so.  That is the one chunk where the gap is large, and it is also the chunk
    # whose forcing comes from a foreign harvest, so it is not a chunk to draw
    # conclusions from either way.
    floating = pism_floating_mask(args.pism_mask_file, thk.shape)
    if floating is None:
        floating = (thk > 0.0) & (thk * RHO_ICE / RHO_SEAWATER <= -topg)
        print("     - floating set: flotation test on thk/topg "
              "(no previous-leg mask available)")
    draft = -thk * RHO_ICE / RHO_SEAWATER          # negative downward

    ds = netCDF4.Dataset(args.ice_file, "a")
    melt = ds.variables[args.melt_var]
    values = np.ma.filled(melt[:], 0.0).astype("f8")
    shaped = values.reshape(-1, *floating.shape)

    # shelfbmassflux is positive for melt, fw is negative for it; the rename
    # between them flips the sign, so take the magnitude from the field itself
    # rather than assuming which convention has arrived here.
    current = (shaped[0][floating] * cell_area).sum() * SECONDS_PER_YEAR / 1e12
    if abs(current) < 1e-6:
        ds.close()
        sys.exit("cavity_consistency: remapped melt integrates to zero over the "
                 "floating domain -- refusing to scale")
    # The rename upstream flips the sign: fw is negative where the ice melts,
    # shelfbmassflux positive.  Scale by a positive factor so whatever arrived
    # is preserved, then check that what arrived actually melts.  A field that
    # mostly refreezes means the conversion did not happen, and letting it
    # through would have PISM freeze mass onto every shelf without a word.
    melting = float((shaped[0][floating] > 0).mean())
    if melting < 0.5:
        ds.close()
        sys.exit(f"cavity_consistency: only {100*melting:.0f} % of floating cells "
                 f"melt -- shelfbmassflux looks sign-inverted (fw convention?), "
                 f"refusing to scale it")
    scale = abs(cavity_total) / abs(current)
    values *= scale
    melt[:] = values
    melt.setncattr("renormalised_to_Gt_per_year", np.float64(cavity_total))
    melt.setncattr("renormalisation_factor", np.float64(scale))
    print(f"     - floating cells {int(floating.sum())}, "
          f"area {floating.sum()*cell_area/1e9:.0f} x10^3 km2")
    print(f"     - melt {abs(current):.0f} -> {abs(cavity_total):.0f} Gt/yr "
          f"(factor {scale:.3f}, {100*melting:.0f} % of cells melting)")

    if args.temp_var in ds.variables:
        temp = ds.variables[args.temp_var]
        tf = TF_A * args.salinity + TF_B + TF_C * draft
        old = np.ma.filled(temp[:], np.nan).astype("f8")
        old_mean = np.nanmean(old.reshape(-1, *floating.shape)[0][floating])
        temp[:] = np.broadcast_to(tf, old.shape).copy()
        temp.setncattr("long_name", "ice shelf basal temperature (freezing point "
                                    "at the ice draft)")
        temp.setncattr("computed_by", "cavity_consistency.py")
        print(f"     - shelfbtemp {old_mean:+.2f} -> {tf[floating].mean():+.2f} degC "
              f"(freezing point, S = {args.salinity})")
    ds.close()


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="stage", required=True)

    m = sub.add_parser("mask", help="mask the melt flux to the cavity (FESOM grid)")
    m.add_argument("--node-file", required=True)
    # give either the directory holding the submeshes, and let the node count
    # pick, or one explicit submesh, which is then checked against that count
    m.add_argument("--submesh-root", default=".")
    m.add_argument("--submesh", default=None)
    m.add_argument("--total-file", required=True)
    m.add_argument("--var", default="shelfbmassflux")
    # the variable whose missing values witness the cavity independently of the
    # submesh (Tsurf, by this point renamed); see check_against_field
    m.add_argument("--temp-var", default="shelfbtemp")
    m.set_defaults(func=stage_mask)

    f = sub.add_parser("fix", help="conserve the melt and set the basal temperature")
    f.add_argument("--ice-file", required=True)
    f.add_argument("--pism-file", required=True)
    f.add_argument("--total-file", required=True)
    f.add_argument("--melt-var", default="shelfbmassflux")
    f.add_argument("--temp-var", default="shelfbtemp")
    # the previous leg's ex-file, read ONLY for its `mask`: PISM's own answer to
    # which cells it will apply the melt on.  Omitted or absent -> flotation test.
    f.add_argument("--pism-mask-file", default=None)
    # the cavity-top salinity varies little enough that remapping it changes the
    # freezing point by 0.005 degC, so a constant keeps the field out of the
    # coupling for no measurable cost
    f.add_argument("--salinity", type=float, default=34.4)
    f.set_defaults(func=stage_fix)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
