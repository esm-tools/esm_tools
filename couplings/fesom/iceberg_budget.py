#!/usr/bin/env python3
"""Antarctic icebergs from the model's own mass budget, without an ice-sheet model.

The atmosphere sheds all Antarctic snow above 10 m water equivalent as "calving" and the ocean
melts the ice-shelf bases in its cavities. With no ice sheet in between, what is left for icebergs
is the difference:

    icebergs = calving - cavity basal melt        (accumulation = basal melt + icebergs)

This tool keeps that budget from leg to leg and writes the iceberg seed files FESOM reads. It has two
steps, because the amount is known at the END of a leg and the seed files depend on the length of
the NEXT one, which may change between restarts.

  budget   after a leg: measure calving and basal melt of that leg from its output, move the
           running rate [Gt/yr] towards it (memory --rate-memory-years, so one noisy year of a short
           leg does not set it) and add the leg to the ledger of mass owed against mass seeded.
           Independent of the length of any other leg.
  seed     before a leg, when its dates are known: turn the rate into icb_*.dat for exactly that
           leg. Bergs are released at the rate, evenly from the first to the last day; a ledger
           imbalance (from a changed rate or a changed leg length) is paid back over --relax-years,
           so that seeded mass follows owed mass in the long run whatever the leg lengths were.

State lives in <couple_dir>/icb_ledger.json. First leg: the rate comes from --init-rate-file if that
exists (a JSON with "rate_gt_yr", e.g. from the pool), else from --default-rate (1100 Gt/yr).

Calving  = snowfall + evaporation - runoff on land south of 60S, from the remapped monthly OpenIFS
           output (sf, e, ro in m of water per hour). This closes against the runoff mapper's
           Antarctic calving to 1 %, and needs no extra coupling field.
Basal melt = -fw at cavity nodes (FESOM monthly output) times the node area at the top wet level.

Where the bergs go: release sites are the open-ocean elements next to the ice front or coast, in 17
sectors; a sector's share of the total is its observed present-day ice-front flux (Rignot et al.
2013, Science, Table 1, "Ice front" column, summed over the shelves of the sector). The model only
supplies the total. Sizes follow the ice-sheet coupling (fesom_icb_pism): areas from a power law
with exponent 1.52 between 0.01 and 400 km2 (Tournadre et al. 2015), one model berg standing for
several real ones in the small size classes. Mass of a model berg = scaling * L^2 * H * 850 kg/m3.

Usage:
  iceberg_budget.py budget --couple-dir D --oifs-outdata O --fesom-outdata F --mesh-dir M \
                           --start YYYY-MM-DD --end YYYY-MM-DD
  iceberg_budget.py seed   --couple-dir D --mesh-dir M --start YYYY-MM-DD --end YYYY-MM-DD \
                           [--restart-ism FILE] [--init-rate-file FILE] [--default-rate 1100] \
                           [--oifs-outdata O --fesom-outdata F]

Both steps are safe to repeat: a budget already in the ledger is skipped, a leg that is seeded again
replaces its earlier entry, and 'seed' takes the previous leg's budget itself if the end-of-leg job
has not done so (given the two outdata directories).
"""
import argparse
import datetime as dt
import glob
import json
import os
import sys

import numpy as np

RHO_ICE = 850.0            # kg m-3, FESOM's rho_icb and the ice-sheet coupling's value
BERG_HEIGHT = 218.75       # m, written to icb_height.dat; 7/8 of 250 m as in fesom_icb_pism
BERG_THICK = 250.0         # m, thickness that converts discharge volume to berg area
ALPHA, A_MIN, A_MAX = 1.52, 0.01, 400.0          # power law of berg area [km2]
BINS = [0.1, 1.0, 10.0, 100.0, 1000.0]           # km2, upper edges of the size classes
SCALING = [100, 50, 10, 1, 1, 1]                 # real bergs per model berg, per class
GT = 1.0e12                                      # kg

# Sector, longitude range [E], latitude range, observed ice-front flux [Gt/yr].
# Rignot et al. (2013) Table 1, column "Ice front", summed over the listed shelves. Total 1088.4.
SECTORS = [
    ("Larsen",                -63.0,  -58.0, -74.0, -65.0,  49.3),   # Larsen B-G
    ("Ronne",                 -62.0,  -50.0, -90.0, -74.0, 149.2),
    ("Filchner",              -44.0,  -36.0, -90.0, -74.0,  82.8),
    ("Brunt/Riiser-Larsen",   -27.0,  -12.0, -90.0, -64.5,  40.2),   # Brunt/Stancomb, Riiser-Larsen
    ("Dronning Maud west",    -12.0,    5.0, -90.0, -64.5,  30.9),   # Quar, Ekstrom, Atka, Jelbart, Fimbul
    ("Dronning Maud east",      5.0,   38.0, -90.0, -64.5,  40.7),   # Vigrid, Nivl, Lazarev, Borchgrevink, Baudouin, Prince Harald
    ("Enderby/Kemp",           38.0,   60.0, -90.0, -64.5,  18.5),   # Shirase, Rayner/Thyer, Edward VIII, Wilma/Robert/Downer
    ("Amery",                  68.0,   78.0, -90.0, -64.5,  55.6),   # Amery, Publications
    ("West/Shackleton",        80.0,  104.0, -90.0, -64.5,  64.2),   # West, Shackleton, Tracy/Tremenchus, Conger/Glenzer
    ("Wilkes Land",           108.0,  130.0, -90.0, -64.5,  89.1),   # Vincennes, Totten, Moscow Univ., Holmes
    ("Adelie/George V",       132.0,  155.0, -90.0, -64.5,  72.4),   # Dibble, Mertz, Ninnis, Cook
    ("Victoria Land",         160.0,  168.0, -76.0, -69.0,   5.3),   # Rennick, Lillie, Mariner, Aviator, Nansen, Drygalski
    ("Ross",                  165.0,  202.0, -90.0, -76.0, 146.3),   # Ross East + Ross West (202 E = 158 W)
    ("Marie Byrd coast",     -158.0, -136.0, -90.0, -64.5,  20.2),   # Withrow, Swinburne, Sulzberger, Nickerson, Land
    ("Getz",                 -136.0, -114.0, -90.0, -64.5,  53.5),
    ("Amundsen",             -114.0,  -99.0, -90.0, -64.5, 135.3),   # Dotson, Crosson, Thwaites, Pine Island, Cosgrove
    ("Bellingshausen",        -99.0,  -66.0, -90.0, -64.5,  34.9),   # Abbot, Venable, Ferrigno, Stange, George VI, Bach, Wilkins, Wordie
]


# ------------------------------------------------------------------------------------------------
def _date(s):
    return dt.date(*[int(x) for x in s.split("-")[:3]])


def _ledger_path(couple_dir):
    return os.path.join(couple_dir, "icb_ledger.json")


def _load_ledger(couple_dir):
    p = _ledger_path(couple_dir)
    if os.path.isfile(p):
        with open(p) as fh:
            return json.load(fh)
    return None


def _save_ledger(couple_dir, led):
    os.makedirs(couple_dir, exist_ok=True)
    tmp = _ledger_path(couple_dir) + ".tmp"
    with open(tmp, "w") as fh:
        json.dump(led, fh, indent=1)
    os.replace(tmp, _ledger_path(couple_dir))


# ------------------------------------------------------------------------------------------------
def measure_calving(oifs_dir, y0, y1):
    """Antarctic snow-cap shedding [Gt/yr]: sf + e - ro on land south of 60S, mean over y0..y1."""
    import xarray as xr

    def one(var, y):
        f = glob.glob(os.path.join(oifs_dir, f"atm_remapped_1m_{var}_{y}-{y}.nc")) + \
            glob.glob(os.path.join(oifs_dir, f"atm_remapped_1m_{var}_1m_{y}-{y}.nc"))
        if not f:
            raise FileNotFoundError(f"no atm_remapped_1m_{var} for {y} in {oifs_dir}")
        with xr.open_dataset(f[0], decode_times=False) as ds:
            x = ds[var]
            return x.mean([d for d in x.dims if d not in ("lat", "lon")]).load()

    lsm = one("lsm", y1)
    lat = np.deg2rad(lsm["lat"].values)
    lon = lsm["lon"].values
    dlat = np.abs(np.gradient(lat))
    dlon = np.deg2rad(np.abs(np.gradient(lon)))
    area = (6.371e6 ** 2) * np.outer(np.cos(lat) * dlat, dlon)            # m2
    land = (lsm.values > 0.5) & (lsm["lat"].values[:, None] < -60.0)
    tot = []
    for y in range(y0, y1 + 1):
        net = one("sf", y).values + one("e", y).values - one("ro", y).values     # m water per hour
        tot.append(np.sum(net[land] * area[land]) * 1000.0 * 24 * 365.25 / GT)
    return float(np.mean(tot))


def measure_basal_melt(fesom_dir, mesh_dir, y0, y1):
    """Cavity basal melt [Gt/yr]: -fw at cavity nodes times the node area at the top wet level."""
    import xarray as xr

    with xr.open_dataset(os.path.join(mesh_dir, "fesom.mesh.diag.nc")) as md:
        ul = md["ulevels_nod2D"].values.astype(int)
        na = md["nod_area"].values
    cav = ul > 1
    idx = np.where(cav)[0]
    a = na[ul[idx] - 1, idx]
    tot = []
    for y in range(y0, y1 + 1):
        f = os.path.join(fesom_dir, f"fw.fesom.{y}.nc")
        if not os.path.isfile(f):
            raise FileNotFoundError(f)
        with xr.open_dataset(f, decode_times=False) as ds:
            fw = ds["fw"].mean("time").values[idx]
        tot.append(-np.nansum(fw * a) * 1000.0 * 86400 * 365.25 / GT)
    return float(np.mean(tot))


class _Lock:
    """Serialise the two steps: the end-of-leg job and the preparation of the next leg may overlap."""

    def __init__(self, couple_dir):
        os.makedirs(couple_dir, exist_ok=True)
        self.path = os.path.join(couple_dir, "icb_ledger.lock")

    def __enter__(self):
        import fcntl
        self.fh = open(self.path, "w")
        fcntl.flock(self.fh, fcntl.LOCK_EX)
        return self

    def __exit__(self, *exc):
        import fcntl
        fcntl.flock(self.fh, fcntl.LOCK_UN)
        self.fh.close()


def _has(led, step, start, end):
    return any(l["step"] == step and l["start"] == start and l["end"] == end for l in led.get("legs", []))


def cmd_budget(a):
    with _Lock(a.couple_dir):
        try:
            _budget(a)
        except FileNotFoundError as err:
            # the leg's output may still be on its way to outdata; the next leg's preparation catches up
            print(f" *   iceberg budget {a.start}..{a.end}: output not there yet ({err}); left to the next leg")


def _budget(a):
    led = _load_ledger(a.couple_dir)
    if led is None:
        sys.exit("iceberg_budget budget: no ledger yet; 'seed' has to run before the first leg")
    if _has(led, "budget", a.start, a.end):
        print(f" *   iceberg budget {a.start}..{a.end}: already in the ledger, nothing to do")
        return
    s, e = _date(a.start), _date(a.end)
    years = ((e - s).days + 1) / 365.25
    y0, y1 = s.year, e.year
    calv = measure_calving(a.oifs_outdata, y0, y1)
    melt = measure_basal_melt(a.fesom_outdata, a.mesh_dir, y0, y1)
    target = max(calv - melt, 0.0)
    # the rate follows the measured target with a memory, so that a single noisy year of a short leg
    # does not set it; the ledger makes up the difference
    led["rate_gt_yr"] += (target - led["rate_gt_yr"]) * min(1.0, years / a.rate_memory_years)
    led["owed_gt"] = led.get("owed_gt", 0.0) + target * years
    led.setdefault("legs", []).append(dict(step="budget", start=a.start, end=a.end, years=round(years, 4),
                                           calving_gt_yr=round(calv, 2), basal_melt_gt_yr=round(melt, 2),
                                           target_gt_yr=round(target, 2), owed_gt=round(led["owed_gt"], 2),
                                           seeded_gt=round(led.get("seeded_gt", 0.0), 2)))
    _save_ledger(a.couple_dir, led)
    print(f" *   iceberg budget {a.start}..{a.end}: calving {calv:.0f}, basal melt {melt:.0f}, "
          f"icebergs {target:.0f} Gt/yr; owed {led['owed_gt']:.0f} Gt, seeded {led.get('seeded_gt', 0.0):.0f} Gt")


# ------------------------------------------------------------------------------------------------
def _unit(lon, lat):
    lo, la = np.radians(lon), np.radians(lat)
    return np.stack([np.cos(la) * np.cos(lo), np.cos(la) * np.sin(lo), np.sin(la)], axis=-1)


def release_elements(mesh_dir):
    """Per sector, the elements a berg may be released in: every node open ocean and off the mesh
    boundary (so FESOM's own placement test passes with margin), and touching a node that is next to
    the ice front or the coast."""
    nod = np.loadtxt(os.path.join(mesh_dir, "nod2d.out"), skiprows=1)
    elem = np.loadtxt(os.path.join(mesh_dir, "elem2d.out"), skiprows=1, dtype=np.int64) - 1
    lon, lat = nod[:, 1], nod[:, 2]
    cavf = os.path.join(mesh_dir, "cavity_depth@node.out")
    cavity = np.loadtxt(cavf, ndmin=1) != 0.0 if os.path.isfile(cavf) else np.zeros(len(lon), bool)
    bad = cavity | (nod[:, 3] != 0.0)
    near = np.zeros(len(lon), bool)                 # nodes sharing an element with a bad node
    touch = bad[elem].any(axis=1)
    near[elem[touch].ravel()] = True
    ok = (~bad[elem]).all(axis=1) & near[elem].any(axis=1)
    v = _unit(lon, lat)
    c = v[elem].mean(axis=1)
    c /= np.linalg.norm(c, axis=1, keepdims=True)
    clat = np.degrees(np.arcsin(c[:, 2]))
    clon = np.degrees(np.arctan2(c[:, 1], c[:, 0]))
    out = []
    for name, l0, l1, b0, b1, flux in SECTORS:
        cl = np.where(clon < l0, clon + 360.0, clon) if l1 > 180.0 else clon
        m = ok & (cl >= l0) & (cl < l1) & (clat >= b0) & (clat < b1)
        out.append(np.where(m)[0])
    return lon, lat, elem, out


def draw_bergs(volume_km3, rng):
    """Model bergs for an ice volume: areas from the truncated power law, grouped by size class.
    Returns arrays of length [m] and scaling; the summed volume is exact."""
    area_tot = volume_km3 / (BERG_THICK / 1000.0)                       # km2
    if area_tot <= 0:
        return np.zeros(0), np.zeros(0, int)
    b = 1.0 - ALPHA
    lo, hi = A_MIN ** b, A_MAX ** b
    mean = ((A_MAX ** (b + 1) - A_MIN ** (b + 1)) / (b + 1)) / ((hi - lo) / b)
    areas = np.zeros(0)
    while areas.sum() < area_tot:
        n = max(int(1.2 * (area_tot - areas.sum()) / mean), 8)
        areas = np.concatenate([areas, (lo + rng.random(n) * (hi - lo)) ** (1.0 / b)])
    keep = np.searchsorted(np.cumsum(areas), area_tot) + 1
    areas = areas[:keep] * (area_tot / areas[:keep].sum())              # exact total
    cls = np.digitize(areas, BINS, right=True)
    L, S = [], []
    for k in range(len(SCALING)):
        ak = areas[cls == k]
        s = SCALING[k]
        for i in range(0, len(ak), s):
            grp = ak[i:i + s]
            L.append(np.sqrt(grp.mean()) * 1000.0)                      # one model berg of the mean size
            S.append(len(grp))
    return np.array(L), np.array(S, int)


def cmd_seed(a):
    with _Lock(a.couple_dir):
        _seed(a)


def _seed(a):
    os.makedirs(a.couple_dir, exist_ok=True)
    led = _load_ledger(a.couple_dir)
    if led is not None:
        # a resubmitted leg: take its earlier seeding back out before redoing it
        for l in [l for l in led["legs"] if l["step"] == "seed" and l["start"] == a.start and l["end"] == a.end]:
            led["seeded_gt"] -= l["seeded_gt"]
            led["legs"].remove(l)
        # the leg before this one, if its end-of-leg budget has not been taken yet and its output is there
        prev = [l for l in led["legs"] if l["step"] == "seed" and l["end"] < a.start]
        if prev and a.oifs_outdata and a.fesom_outdata and not _has(led, "budget", prev[-1]["start"], prev[-1]["end"]):
            _save_ledger(a.couple_dir, led)
            b = argparse.Namespace(couple_dir=a.couple_dir, mesh_dir=a.mesh_dir, start=prev[-1]["start"], end=prev[-1]["end"],
                                   oifs_outdata=a.oifs_outdata, fesom_outdata=a.fesom_outdata,
                                   rate_memory_years=a.rate_memory_years)
            try:
                _budget(b)
            except FileNotFoundError as err:
                print(f" *   note: no budget for {b.start}..{b.end} ({err}); keeping the previous rate")
            led = _load_ledger(a.couple_dir)
    if led is None:
        rate, src = a.default_rate, f"default {a.default_rate:g} Gt/yr"
        if a.init_rate_file and os.path.isfile(a.init_rate_file):
            with open(a.init_rate_file) as fh:
                rate = float(json.load(fh)["rate_gt_yr"])
            src = a.init_rate_file
        led = dict(rate_gt_yr=rate, owed_gt=0.0, seeded_gt=0.0, relax_years=a.relax_years, init=src, legs=[])
    s, e = _date(a.start), _date(a.end)
    days = (e - s).days + 1
    years = days / 365.25
    if days < 360:
        sys.exit("iceberg_budget seed: legs shorter than a year are not supported (the rate is an annual mean)")
    debt = led["owed_gt"] - led["seeded_gt"]
    # the first leg has nothing owed yet: no correction before the first budget step has run
    corr = debt / max(led.get("relax_years", a.relax_years), years) if any(l["step"] == "budget" for l in led["legs"]) else 0.0
    rate = max(led["rate_gt_yr"] + corr, 0.0)
    mass = rate * years                                                  # Gt for this leg

    lon, lat, elem, sect = release_elements(a.mesh_dir)
    flux = np.array([x[5] for x in SECTORS])
    have = np.array([len(x) > 0 for x in sect])
    if not have.any():
        sys.exit("iceberg_budget seed: no release elements found on this mesh")
    w = np.where(have, flux, 0.0)
    w = w / w.sum()                                                      # sectors without a site hand their share on
    rng = np.random.default_rng(int(s.strftime("%Y%m%d")))
    LON, LAT, LEN, SCA, FEL = [], [], [], [], []
    per_sector = []
    for k, (name, *_) in enumerate(SECTORS):
        if not have[k]:
            per_sector.append((name, 0, 0.0))
            continue
        L, S = draw_bergs(mass * w[k] * GT / RHO_ICE / 1e9, rng)       # ice volume of the sector's share [km3]
        # the size draw uses BERG_THICK; rescale lengths so that S*L^2*BERG_HEIGHT*RHO is exactly the mass
        if len(L):
            m_now = np.sum(S * L * L * BERG_HEIGHT * RHO_ICE) / GT
            L = L * np.sqrt(mass * w[k] / m_now)
        els = rng.choice(sect[k], size=len(L))
        r1 = 0.25 + 0.5 * rng.random(len(L)); r2 = 0.25 + 0.5 * rng.random(len(L))
        n1, n2, n3 = elem[els, 0], elem[els, 1], elem[els, 2]
        p = (1 - np.sqrt(r1))[:, None] * _unit(lon[n1], lat[n1]) + (np.sqrt(r1) * (1 - r2))[:, None] * _unit(lon[n2], lat[n2]) \
            + (r2 * np.sqrt(r1))[:, None] * _unit(lon[n3], lat[n3])
        p /= np.linalg.norm(p, axis=1, keepdims=True)
        LON += list(np.degrees(np.arctan2(p[:, 1], p[:, 0]))); LAT += list(np.degrees(np.arcsin(p[:, 2])))
        LEN += list(L); SCA += list(S); FEL += list(els + 1)
        per_sector.append((name, len(L), float(np.sum(S * L * L * BERG_HEIGHT * RHO_ICE) / GT)))
    n = len(LON)
    order = rng.permutation(n)                                           # sizes and sectors mixed over the leg
    LON, LAT, LEN, SCA, FEL = (np.array(x)[order] for x in (LON, LAT, LEN, SCA, FEL))
    # release days counted from the start of the leg; the last day is left free (FESOM releases on
    # istep > step_per_day*calving_day, and a berg that never leaves the front corrupts the restart)
    day = 1.0 + (days - 2.0) * (np.arange(n) + 0.5) / max(n, 1)
    seeded = float(np.sum(SCA * LEN * LEN * BERG_HEIGHT * RHO_ICE) / GT)

    def w_(name, arr, fmt):
        np.savetxt(os.path.join(a.couple_dir, f"icb_{name}.dat"), arr, fmt=fmt)

    w_("longitude", LON, "%.6f"); w_("latitude", LAT, "%.6f"); w_("length", LEN, "%.3f")
    w_("height", np.full(n, BERG_HEIGHT), "%.3f"); w_("scaling", SCA, "%d"); w_("felem", FEL, "%d")
    w_("calving_day", day, "%.4f")
    carried = 0
    if a.restart_ism and os.path.isfile(a.restart_ism):
        with open(a.restart_ism) as fh:
            carried = sum(1 for line in fh if line.strip())
    with open(os.path.join(a.couple_dir, "num_non_melted_icb_file"), "w") as fh:
        fh.write(f"{carried}\n")

    led["seeded_gt"] += seeded
    led["legs"].append(dict(step="seed", start=a.start, end=a.end, years=round(years, 4), rate_gt_yr=round(rate, 2),
                            base_rate_gt_yr=round(led["rate_gt_yr"], 2), correction_gt_yr=round(corr, 2),
                            seeded_gt=round(seeded, 2), model_bergs=int(n), real_bergs=int(np.sum(SCA)), carried=carried,
                            sectors={nm: [cnt, round(m, 2)] for nm, cnt, m in per_sector}))
    _save_ledger(a.couple_dir, led)
    print(f" *   iceberg seeding {a.start}..{a.end} ({years:.2f} yr): {rate:.0f} Gt/yr "
          f"(base {led['rate_gt_yr']:.0f}, correction {corr:+.0f}) -> {seeded:.1f} Gt in {n} model bergs "
          f"({int(np.sum(SCA))} real), {carried} carried over")
    miss = [nm for (nm, cnt, m), h in zip(per_sector, have) if not h]
    if miss:
        print(f" *   note: no release site on this mesh for {', '.join(miss)}; their share went to the other sectors")


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    b = sub.add_parser("budget"); s = sub.add_parser("seed")
    for q in (b, s):
        q.add_argument("--couple-dir", required=True); q.add_argument("--mesh-dir", required=True)
        q.add_argument("--start", required=True); q.add_argument("--end", required=True)
    b.add_argument("--oifs-outdata", required=True); b.add_argument("--fesom-outdata", required=True)
    b.add_argument("--rate-memory-years", type=float, default=10.0)
    s.add_argument("--restart-ism", default=""); s.add_argument("--init-rate-file", default="")
    s.add_argument("--default-rate", type=float, default=1100.0); s.add_argument("--relax-years", type=float, default=20.0)
    s.add_argument("--oifs-outdata", default="", help="with --fesom-outdata: take the previous leg's budget first if it is missing")
    s.add_argument("--fesom-outdata", default=""); s.add_argument("--rate-memory-years", type=float, default=10.0)
    a = p.parse_args(argv)
    (cmd_budget if a.cmd == "budget" else cmd_seed)(a)


if __name__ == "__main__":
    main()
