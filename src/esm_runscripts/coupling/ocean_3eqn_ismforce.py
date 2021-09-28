#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Convert prepared ocean hydrographic output to basal ice shelf melting rates
#
# For the coupling of ocean models, the original model output of the
# hydrographic fields temp and salt are modified as part of the ESM
# tools. The modfied hydrographic data is used here to computed via
# the three equation system of Holland and Jenkins (JPO, 1999) the
# boundary conditions and the basal melting rates.
#
# (C) Christian Rodehacke, DMI Copenhagen,   2017-07-12 (initial)
#     Paul Gierz,          AWI Bremerhaven,  2018-05-02 (parameter arguments)
#     Christian Rodehacke, AWI Bremerhaven,  2019-01-14 (general overhaul:
#                                                        xarray, logging,
#                                                        improved parameter
#                                                        arguments,
#                                                        vectorization)
#
# Example:
# python3 ./ocean_3eqn_ismforce.py  --OCEAN_INPUT_FILE ocean.tyx.nc --ISM_INPUT_FILE test53_pismr_extra_0001-0010.nc --OUT_BASAL_TEMP_VARIABLE tempbase
#


#
# Import further packages
#
import sys

sys.path.append("./")
import argparse

#
# -----------
#
def parse_arguments():
    parser = argparse.ArgumentParser()
    #
    # Input file names (Required)
    #
    parser.add_argument(
        "--OCEAN_INPUT_FILE",
        "-O",
        "-Oce",
        nargs="?",
        required=True,
        help="Ocean input file (REQUIRED)",
    )
    parser.add_argument(
        "--ISM_INPUT_FILE",
        "-I",
        "-Ice",
        nargs="?",
        required=True,
        help="ice sheet input file (REQUIRED)",
    )

    #
    # Ocean input: variables names
    # Note: temperature are in degC, salinities in psu~=g/kg, depth in "meter"
    #       velocity is the absolute velocity in meter/sec
    #
    parser.add_argument(
        "--OCEAN_TEMP_VARIABLE",
        "-T",
        nargs="?",
        default="theta_ocean",
        help="ocean temperature variable name",
    )
    parser.add_argument(
        "--OCEAN_SALT_VARIABLE",
        "-S",
        nargs="?",
        default="salinity_ocean",
        help="ocean salinity variable name",
    )
    parser.add_argument(
        "--OCEAN_DEPTH_VARIABLE",
        "-D",
        nargs="?",
        default="depth",
        help="ocean depth variable name",
    )
    parser.add_argument(
        "--OCEAN_VELOCITY_VARIABLE",
        "-V",
        nargs="?",
        default="Skip_Use_Of_Absolute_Velocity",
        help="ocean absolute velocity variable name (skip => do not consider velocity)",
    )
    parser.add_argument(
        "--OCEAN_TIME_VARIABLE",
        "-ot",
        nargs="?",
        default="time",
        help="ocean time variable name",
    )

    #
    # Ice sheet input: variables
    # Note: depth, thickness, elevations are in "meter"
    #
    parser.add_argument(
        "--ISM_BED_VARIABLE",
        "-b",
        nargs="?",
        default="topg",
        help="ice model bedrock topography variable name",
    )
    parser.add_argument(
        "--ISM_MASK_VARIABLE",
        "-m",
        nargs="?",
        default="mask",
        help="ice model mask variable name",
    )
    parser.add_argument(
        "--ISM_THICK_VARIABLE",
        "-t",
        nargs="?",
        default="thk",
        help="ice model ice thickness variable name",
    )
    parser.add_argument(
        "--ISM_ICE_TEMP_VARIABLE",
        "-s",
        nargs="?",
        default="ice_surface_temp",
        help="ice model upper ice temperature variable name",
    )
    parser.add_argument(
        "--ISM_TIME_VARIABLE",
        "-it",
        nargs="?",
        default="time",
        help="ice model time variable name",
    )

    #
    # Ice sheet input: values, where length and height are in "meter"
    #
    parser.add_argument(
        "--ISM_MASK_FLOAT_VALUE",
        nargs="?",
        type=int,
        default=3,
        help="mask value of floating ice in ice model",
    )
    parser.add_argument(
        "--ISM_GRID_LENGTH",
        nargs="?",
        type=float,
        default=16000.0,
        help="horizontal grid size (meter) in ice model. If ISM_GRID_LENGTH>0 (meter) use this length else exploit the distance of the first two rows/columns to compute the related lenghts",
    )
    parser.add_argument(
        "--ISM_GRID_HEIGHT",
        nargs="?",
        type=float,
        default=40.0,
        help="vertical grid size (meter) in ice model, between base and upper ice temperature (see input '-s', '--ISM_ICE_TEMP_VARIABLE'). If ISM_GRID_HEIGHT>0 use this value, if ISM_GRID_HEIGHT=0 use the ice thickness distribution, else use the difference between the two lowest layers",
    )  # basal grid layer
    parser.add_argument(
        "--ISM_GRID_Z_AXIS_VARIABLE",
        nargs="?",
        default="z",
        help="variable name of vertical z-axis",
    )  # z-axis name

    #
    # Output file name
    #
    parser.add_argument(
        "--OUTPUT_FILE",
        nargs="?",
        default="Shelfmelt_forcing.nc4",
        help="name of ice sheet forcing output file",
    )
    parser.add_argument(
        "--LOGGING_OUTPUT_FILE",
        "-l",
        nargs="?",
        default="ocean_hydrography2ism_forcing.log",
        help="name of logging output file",
    )
    parser.add_argument(
        "--LOGGING_OUTPUT_LEVEL",
        "-v",
        nargs="?",
        default="INFO",
        help="logging level",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL", "NONE"],
    )

    #
    # Output file name: variables
    #
    parser.add_argument(
        "--OUT_BASAL_TEMP_VARIABLE",
        nargs="?",
        default="shelfbtemp",
        help="output file, basal temperature variable name",
    )
    parser.add_argument(
        "--OUT_BASAL_MASSFLUX_VARIABLE",
        nargs="?",
        default="shelfbmassflux",
        help="output file, basal mass flux variable name",
    )
    parser.add_argument(
        "--OUT_BASAL_HEATFLUX_VARIABLE",
        nargs="?",
        default="shelfbheatflux",
        help="output file, basal heat flux variable name",
    )

    #
    # Output: What shall be computed?
    #
    parser.add_argument(
        "--OUT_CALC_BASAL_MELT",
        nargs="?",
        type=bool,
        default=True,
        help="Shall we compute the basal melting rate?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_CALC_BASAL_TEMP",
        nargs="?",
        type=bool,
        default=True,
        help="Shall we compute the basal ice temperature rate?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_CALC_FRONTAL_MELT",
        nargs="?",
        type=bool,
        default=False,
        help="Shall we compute the frontal melting rate?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_CALC_FRONTAL_MELT_AT_ICE_POSITION",
        nargs="?",
        type=bool,
        default=True,
        help="Frontal melting shall be relased on the ice shelf side (True) or in the neihboring open ocean (False)?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_CALC_MERGE_BASAL_FRONTAL_MELT",
        nargs="?",
        type=bool,
        default=False,
        help="Shall we merge basal and vertical integrated frontal melt into the basal melting rate (bmelt=bmelt+fmelt)?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_CALC_LESS_BASAL_LOW_CAVITY",
        nargs="?",
        type=bool,
        default=False,
        help="Shall we reduce the basal melting rate for low vertical space between bedrock and basal ice interface?",
        choices=[True, False],
    )

    #
    # Output: What shall be written?
    #
    parser.add_argument(
        "--OUT_WRITE_2DFIELDS",
        "-2",
        nargs="?",
        type=bool,
        default=True,
        help="Shall we write 2Dim output fields?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_WRITE_3DFIELDS",
        "-3",
        nargs="?",
        type=bool,
        default=False,
        help="Shall we write 3Dim output fields?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_WRITE_TEMP_EXCESS",
        "-E",
        nargs="?",
        type=bool,
        default=False,
        help="Shall we compute and write the temperature excess above local melting temperature (CMIP6 thermal forcing)?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_WRITE_AUXVAR",
        "-A",
        nargs="?",
        type=bool,
        default=False,
        help="Shall we write auxillary output fields?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_WRITE_MASS_FLUX_KG_BASED",
        "-K",
        nargs="?",
        type=bool,
        default=True,
        help="Shall mass flux be based on the unit kg/m2/s?",
        choices=[True, False],
    )
    parser.add_argument(
        "--OUT_WRITE_MACHINE_INFORMATION",
        nargs="?",
        type=bool,
        default=True,
        help="Shall we write some basic information about the use machine and user",
        choices=[True, False],
    )

    #
    # Misc switches
    # Note densities are in "kg m^-3" and pressure uses "Pa"
    #
    parser.add_argument(
        "--LOGGING_WRITE_TIME",
        nargs="?",
        type=bool,
        default=True,
        help="Shall we write time to standard output?",
        choices=[True, False],
    )
    parser.add_argument(
        "--DENSITY_ICE", nargs="?", type=float, default=910.0, help="density of ice"
    )
    parser.add_argument(
        "--DENSITY_OCEAN",
        nargs="?",
        type=float,
        default=1026.0,
        help="density of ocean water",
    )
    parser.add_argument(
        "--DENSITY_FRESHWATER",
        nargs="?",
        type=float,
        default=1000.0,
        help="density of fresh water",
    )
    parser.add_argument(
        "--SEALEVEL_PRESSURE",
        nargs="?",
        type=float,
        default=101325.0,
        help="sealevel pressure",
    )
    parser.add_argument(
        "--AIR_TEMPERATURE",
        nargs="?",
        type=float,
        default=-20.0,
        help="air temperature",
    )

    return parser.parse_args()


args = parse_arguments()


#
# -- Logging
#

print("---------------------------------------------------------------")
print("  Ice shelf melting after Hollard and Jenkins (1999), JPO")
print("---------------------------------------------------------------")

import logging

loglevel = getattr(logging, args.LOGGING_OUTPUT_LEVEL.upper(), "Info")
if args.LOGGING_OUTPUT_FILE == None:
    args.LOGGING_OUTPUT_FILE = "ocean_hydrography2ism_forcing.log"
logging.basicConfig(
    filename=args.LOGGING_OUTPUT_FILE,
    filemode="w",
    format="%(asctime)s:%(levelname)-8s:%(message)s",
    level=loglevel,
)
print(
    "Start logging for current job into file '"
    + args.LOGGING_OUTPUT_FILE
    + "' with level:"
    + str(loglevel)
)
logging.debug(
    "**** Start logging for current job into file '%s' with level '%s' ****",
    args.LOGGING_OUTPUT_FILE,
    loglevel,
)
logging.debug("Imported already essential module 'sys'")
logging.debug("Imported already essential module 'argparse'")
logging.debug("Imported module 'logging'")

logging.info("---------------------------------------------------------------")
logging.info("  Ice shelf melting after Hollard and Jenkins (1999), JPO")
logging.info("---------------------------------------------------------------")

#
# Some checks (1/2) about command line combinations
#
logging.debug("Consitency checks of argument list")

#
# If some BOOL command line arguments are set without value, we assume
# that these should be activated.
#
if args.OUT_CALC_BASAL_MELT == None:
    args.OUT_CALC_BASAL_MELT = True
if args.OUT_CALC_BASAL_TEMP == None:
    args.OUT_CALC_BASAL_TEMP = True
if args.OUT_CALC_FRONTAL_MELT == None:
    args.OUT_CALC_FRONTAL_MELT = True
if args.OUT_CALC_FRONTAL_MELT_AT_ICE_POSITION == None:
    args.OUT_CALC_FRONTAL_MELT_AT_ICE_POSITION = True
if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT == None:
    args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT = True
if args.OUT_CALC_LESS_BASAL_LOW_CAVITY == None:
    args.OUT_CALC_LESS_BASAL_LOW_CAVITY = True
if args.OUT_WRITE_2DFIELDS == None:
    args.OUT_WRITE_2DFIELDS = True
if args.OUT_WRITE_3DFIELDS == None:
    args.OUT_WRITE_3DFIELDS = True
if args.OUT_WRITE_TEMP_EXCESS == None:
    args.OUT_WRITE_TEMP_EXCESS = True
if args.OUT_WRITE_AUXVAR == None:
    args.OUT_WRITE_AUXVAR = True
if args.OUT_WRITE_MASS_FLUX_KG_BASED == None:
    args.OUT_WRITE_MASS_FLUX_KG_BASED = True
if args.OUT_WRITE_MACHINE_INFORMATION == None:
    args.OUT_WRITE_MACHINE_INFORMATION = True
if args.LOGGING_WRITE_TIME == None:
    args.LOGGING_WRITE_TIME = True

if (
    args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT == True
    and args.OUT_CALC_FRONTAL_MELT == False
):
    logging.warning(
        "Merging basal and frontal melting is TRUE while calculation of frontal melt is FALSE"
    )
if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT == True and args.OUT_CALC_BASAL_MELT == False:
    logging.warning(
        "Merging basal and frontal melting is TRUE while calculation of basal melt is FALSE"
    )

if args.OUT_CALC_FRONTAL_MELT == False and args.OUT_CALC_BASAL_MELT == False:
    logging.warning("Both basal and frontal melting is FALSE")

if args.OUT_CALC_BASAL_TEMP == False and args.OUT_CALC_BASAL_MELT == False:
    logging.warning("Both basal temperature and basal melting is FALSE")

if (
    args.OUT_CALC_FRONTAL_MELT == False
    and args.OUT_CALC_BASAL_MELT == False
    and args.OUT_CALC_BASAL_MELT == False
):
    logging.error(
        "Basal temperature, basal and frontral melting is FALSE => Nothing to do??"
    )
    sys.exit(3)


if args.DENSITY_ICE <= 0.0:
    logging.error("Unphysical density of ice, DENSITY_ICE=%f", args.DENSITY_ICE)
if args.DENSITY_OCEAN <= 0.0:
    logging.error(
        "Unphysical density of ocean water, DENSITY_OCEAN=%f", args.DENSITY_OCEAN
    )
if args.DENSITY_FRESHWATER <= 0.0:
    logging.error(
        "Unphysical density of fresh water, DENSITY_FRESHWATER=%f",
        args.DENSITY_FRESHWATER,
    )
if args.SEALEVEL_PRESSURE <= 0.0:
    logging.error(
        "Unphysical sea level pressure, SEALEVEL_PRESSURE=%f", args.SEALEVEL_PRESSURE
    )


#
# Information about used settings / parameter arguments
#
for single_arg in sorted(vars(args)):
    ##print(" %-30s =  "% (single_arg)+str(getattr(args, single_arg)) )
    logging.info("  %-30s =  " % (single_arg) + str(getattr(args, single_arg)))

#
# Check 2/2 : Empty arguments lead to a stop
#
ierror = 0
if args.OCEAN_INPUT_FILE == None:
    ierror = ierror + 1
    logging.error("--OCEAN_INPUT_FILE is set without value")
if args.ISM_INPUT_FILE == None:
    ierror = ierror + 1
    logging.error("--ISM_INPUT_FILE is set without value")
if args.OCEAN_TEMP_VARIABLE == None:
    ierror = ierror + 1
    logging.error("--OCEAN_TEMP_VARIABLE is set without value")
if args.OCEAN_SALT_VARIABLE == None:
    ierror = ierror + 1
    logging.error("--OCEAN_SALT_VARIABLE is set without value")
if args.OCEAN_DEPTH_VARIABLE == None:
    ierror = ierror + 1
    logging.error("--OCEAN_DEPTH_VARIABLE is set without value")
if args.OCEAN_VELOCITY_VARIABLE == None:
    ierror = ierror + 1
    logging.error("--OCEAN_VELOCITY_VARIABLE is set without value")
if args.OCEAN_TIME_VARIABLE == None:
    ierror = ierror + 1
    logging.error("--OCEAN_TIME_VARIABLE is set without value")
if args.ISM_BED_VARIABLE == None:
    ierror = ierror + 1
if args.ISM_THICK_VARIABLE == None:
    ierror = ierror + 1
if args.ISM_MASK_VARIABLE == None:
    ierror = ierror + 1
if args.ISM_ICE_TEMP_VARIABLE == None:
    ierror = ierror + 1
if args.ISM_TIME_VARIABLE == None:
    ierror = ierror + 1
if args.ISM_MASK_FLOAT_VALUE == None:
    ierror = ierror + 1
if args.ISM_GRID_LENGTH == None:
    ierror = ierror + 1
if args.ISM_GRID_HEIGHT == None:
    ierror = ierror + 1
if args.ISM_GRID_Z_AXIS_VARIABLE == None:
    ierror = ierror + 1
if args.OUTPUT_FILE == None:
    ierror = ierror + 1
    logging.error("--OUTPUT_FILE is set without value")
if args.LOGGING_OUTPUT_FILE == None:
    ierror = ierror + 1
if args.LOGGING_OUTPUT_LEVEL == None:
    ierror = ierror + 1
if args.OUT_BASAL_TEMP_VARIABLE == None:
    ierror = ierror + 1
if args.OUT_BASAL_MASSFLUX_VARIABLE == None:
    ierror = ierror + 1
if args.OUT_BASAL_HEATFLUX_VARIABLE == None:
    ierror = ierror + 1
if args.DENSITY_ICE == None:
    ierror = ierror + 1
if args.DENSITY_OCEAN == None:
    ierror = ierror + 1
if args.DENSITY_FRESHWATER == None:
    ierror = ierror + 1
if args.SEALEVEL_PRESSURE == None:
    ierror = ierror + 1
if args.AIR_TEMPERATURE == None:
    ierror = ierror + 1

if ierror > 0:
    print(" Found " + str(ierror) + " in the command land arguments")
    print(" Please inspect logging file " + args.LOGGING_OUTPUT_FILE)
    # logging.CRITICAL("Found errors due to wrongly set command line arguments")
    logging.error("Found errors due to wrongly set command line arguments")
    sys.exit(1)


#
# Import additional packages, depending of command line arguments
#
try:
    import numpy as np
except:
    logging.error("Failed to import the required module 'numpy'")
    # sys.exit(4)
logging.debug("Imported module 'numpy'")

# from netCDF4 import Dataset, MFDataset
try:
    from netCDF4 import Dataset, MFDataset

    logging.debug("Imported from module 'netCDF4' 'Dataset, MFDataset'")
except:
    logging.error("Failed to import the required module 'netCDF4'")
#    sys.exit(5)

try:
    import xarray

    flag_use_xarray = True
    logging.debug("Imported module 'xarray'")
except:
    flag_use_xarray = False
    logging.warning("Failed to import the recommanded module 'xarray'")
    # sys.exit(6)
    logging.warning("==> Fall back to generic netCDF4 construction")

if args.LOGGING_WRITE_TIME:
    try:
        import time as time

        logging.debug("Imported module 'time'")
        time_date_start = time.ctime(time.time())
    except:
        logging.warning("Failed to import the recommanded module 'time'")
        args.LOGGING_WRITE_TIME = False

if args.OUT_WRITE_MACHINE_INFORMATION:
    try:
        import os as os

        logging.debug("Imported module 'os'")
    except:
        logging.warning("Failed to import the recommanded module 'os'")
        args.OUT_WRITE_MACHINE_INFORMATION = False

flag_write_platform_information = True
if flag_write_platform_information:
    try:
        import platform as platform

        logging.debug("Imported module 'platform'")
    except:
        logging.warning("Failed to import the recommanded module 'platform'")
        flag_write_platform_information = False

if flag_write_platform_information:
    logging.info(" Python version  %s", platform.python_version())
    logging.debug(" Python compiler %s", platform.python_compiler())
    logging.debug(" Computer system %s", platform.system())
    logging.debug(" Computer node   %s", platform.node())

# --------------------------------------------------------------
#
#
# General variables
#
eps = 1.0e-6  # a tiny number


#
#  Physical parameter
#
# densities
dens_ice = args.DENSITY_ICE
dens_ocean = args.DENSITY_OCEAN
dens_water = args.DENSITY_FRESHWATER

pressure_sealevel = args.SEALEVEL_PRESSURE  # sea level presssure (Pa)
gravitional_acceleration = 9.81  # m/s2
Depth2Press = (
    dens_ocean * gravitional_acceleration
)  # water depth in water pressure (Pa/m)
# assume constant density and incompressibility
Kelvin2DegC = -273.15  # K->DegC


# =============================================
#
# -------------- sub routines ----------------
#
# def cdo_preparation(in_ice,in_ocean,temp,salt,no_layers):
#    status=0
##test if file exists to run the CDO commands
##if test ==1 run CDO commands
#    if status != 0 :
#        raise XXXXError,'CDO script does not exists'
#    return status


def find_kindex(depths, basaldepth):
    # Find the ocean layer below the basal shelf interface
    # Input
    # depth     : depths array of ocean layers
    # basaldepth: depth of the basal shelf interface
    # Output
    # kindex         : index of the layer with a depth below the basal
    #                  shelf interface
    # fraction_below : fractional distance of the depth below = 1.0

    # FIXME: Assume increasing depth with raising level
    try:
        kindex = np.min(np.where(depth > basaldepth))
    except:
        # If max(depth)<basaldepth, we set kindex to the lowers depth
        # aka the highest index number
        kindex = np.size(depths) - 1

    dz_below_above = depths[kindex] - depths[max(kindex - 1, 0)]
    dz_below_interface = depths[kindex] - basaldepth
    if abs(dz_below_above) < 1e-4:
        fraction_below = 1.0
    else:
        # For cases where basaldepth>max(depth), we restrict
        # fraction_below to 1.0. This avoids unrealistic fraction
        # greater than 1.0
        fraction_below = min(1.0 - (dz_below_interface / dz_below_above), 1.0)

    return kindex, fraction_below


def depth2dz(depth):  # , depth_length):
    # Caculates from a given depth profile the according grid box heigths.
    # FIXME: Assumption: depths are located in the middle between floor
    # and ceiling and the depth increases with raising index number
    #
    # Input
    # depth :  array of depths (eg. m)
    # depth_length  : length of the array "depth", number of elements
    #
    # Output
    # dz_oce    : vertical thickness of each grid cell (eg. m)
    #
    kl = np.size(depth)
    dz_oce[0] = 2 * depth[0]
    for k in range(1, kl):
        # The thickness of grid box [k] is difference between grid points
        # minus half of the thickness of the box[k-1] above.
        dz_oce[k] = (depth[k] - depth[k - 1]) - (dz_oce[k - 1] * 0.5)
        # 4test print k, depth, dz_oce

    return dz_oce


def depth2press(depth, ocean_density, pressure0):
    # calculate the pressure from the water depth
    # Input
    # depth         : depths array of ocean layers (m)
    # ocean_density : ocean density (kg/m3)
    # pressure0     : pressure at atmosphere-ocean interface (Pa)
    # Output
    # pressure      : water pressure (Pa)

    # FIXME: Assume constant density in calculation of relation between water depth and pressure
    pressure = pressure0 + gravitional_acceleration * ocean_density * depth
    return pressure


def Tfreeze_lin(S, dpres):
    # Linear pressure dedendent freezing point temperature
    # Input
    # S     : salinity (psu)
    # dpres : pressure (Pa); pressure diff of 1Pa equals X meter water depth diff
    # Output
    # Tf    : Freezing point temperature (DegC)

    # Holland, D.M. and Jenkins, A (1999). Modeling Thermodynamic Ice-Ocean
    # Interactions at the Base of an Ice Shelf. Journal of Physical Oceanography,
    # 29(8), 1787-1800. doi:10.1175/1520-0485(1999)029<1787:MTIOIA>2.0.CO;2
    a = -5.73e-2  # degC/psu
    b = +9.39e-2  # degC
    c = -7.53e-8  # degC/Pa

    Tf = a * S + b + c * dpres
    return Tf


def boundaryFluxSaltTempW3(
    dz0_ice,
    Tice,
    dpres,
    T,
    S,
    gammaS=5.05e-7,
    gammaT=1.0e-4,
    rho_oce=1026.0,
    rho_ice=910.0,
):
    # 3Eqn basal melting rates through ocean-ice shelf interaction
    # (Holland and Jenkins, 1999)
    #
    # Input
    # dz0_ice: basal layer thickness in ice model (m)
    # Tice   : ice (surface) temperature (degC)
    #        : temperature gradient in basal layer: (Tice-Tb)/dz0_ice
    # T      : ocean temperature (beyond boundary layer) (degC)
    # S      : ocean salinity (beyond boundary layer) (psu)
    # dpres  : pressure (Pa); pressure diff of 1Pa equals X meter
    #          water depth diff
    # gammaS : Salinity exchange velocity (m/s)
    # gammaT : Thermal exchange velocity (m/s)
    # rho_oce: ocean density (kg/m3)
    # rho_ice: ice density (kg/m3)
    #
    # Output
    # bmelt : basal melting rate (m/s) where
    #         ????bmelt>0 = ice loss = fresh water gain????
    # FluxS : salt flux (psu/m2/s)
    # FluxT : temperature flux (degC/m2/s)
    # Tb    : boundary layer temperature (degC)
    # Sb    : boundary layer salinity (degC)
    # dT_dZ : Accounted for temperature gradient
    #
    # Holland, D.M. and Jenkins, A (1999)."Modeling Thermodynamic
    # Ice-Ocean Interactions at the Base of an Ice Shelf". Journal of
    # Physical Oceanography, 29(8), 1787-1800.
    # doi:10.1175/1520-0485(1999)029<1787:MTIOIA>2.0.CO;2

    # rho_oce = 1026.0 #kg/m3
    # rho_ice =  910.0 #kg/m3
    # gammaT  = 1.0E-4 #m/s
    # gammaS  = 5.05E-7#m/s
    cp_oce = 3974.0  # J/kg/K
    cp_ice = 2009.0  # J/kg/K
    LatHeat = 3.34e5  # J/kg
    kappa = 1.14e-6  # m2/s
    Smelt0 = 5.0  # fall back value (p/kg ~= psu)
    _eps = 1.0e-7  # a tiny number

    a = -5.73e-2  # degC/psu
    b = +9.39e-2  # degC
    c = -7.53e-8  # degC/Pa

    eta1 = rho_oce * cp_oce * gammaT
    eta2 = rho_oce * LatHeat * gammaS
    eta3 = rho_ice * cp_ice * kappa / (dz0_ice + _eps)

    eta4 = b + c * dpres
    # Using here c<0 => +c*dpres
    Tb = Tfreeze_lin(S, dpres)  # in Degree Celsius

    # Tice: temperature of ice above the ice-ocean boundary, which
    #       is not the boundary temperature.
    eta5 = (eta1 * T - eta1 * eta4 + eta2 + eta3 * Tice - eta3 * eta4) / (
        a * (eta1 + eta3)
    )

    dT_dz = (Tice - Tb) / (dz0_ice + _eps)

    eta6 = eta2 * S / (a * (eta1 + eta3))

    # Sb   = eta5*0.5 +- sqrt( eta5*eta5*0.25 - eta6 );
    # ==> Vectorized version:
    Sb = np.full_like(T, Smelt0)
    flag = np.zeros_like(T, dtype="bool")

    TmpVar = eta5 * eta5 * 0.25 - eta6
    if False:
        flag = np.where(TmpVar < 0, True, False)
        TmpVar = np.where(flag == False, np.sqrt(TmpVar), 0.0)
        Sb1 = eta5 * 0.5 - TmpVar
        Sb2 = eta5 * 0.5 + TmpVar
        Sb = np.where(flag == True, Smelt0, np.where(Sb1 <= 0, Sb2, (Sb1 + Sb2) * 0.5))
    else:
        Sb1 = np.zeros_like(S)
        Sb2 = np.full_like(S, Smelt0)

        indx = np.where(TmpVar >= 0)
        TmpVar[indx] = np.sqrt(TmpVar[indx])
        Sb1[indx] = eta5[indx] * 0.5 - TmpVar[indx]
        Sb2[indx] = eta5[indx] * 0.5 + TmpVar[indx]
        Sb = np.where(Sb1 <= 0, Sb2, (Sb1 + Sb2) * 0.5)

    bmelt = ((Sb - S) / Sb) * rho_oce / rho_ice * gammaS
    FluxS = (Sb - S) * rho_oce * gammaS
    FluxT = (Tb - T) * rho_oce * cp_oce * gammaT

    return bmelt, FluxS, FluxT, Sb, Tb, dT_dz


# =============================================
#
# -------------- main program ----------------
#


# =============================================
#
# -------------- main program ----------------
#

# Write logging information
#
if args.LOGGING_WRITE_TIME:
    print("Task list    (" + time_date_start + ")")
else:
    print("Task list")
print("  do_basaltemp         : " + str(args.OUT_CALC_BASAL_TEMP))
print("  do_basalmelt         : " + str(args.OUT_CALC_BASAL_MELT))
print("  do_reduce_bmelt      : " + str(args.OUT_CALC_LESS_BASAL_LOW_CAVITY))
print("  do_frontalmelt       : " + str(args.OUT_CALC_FRONTAL_MELT))
print("  do_mergefrontal&basal: " + str(args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT))
#
print("  write_2d_auxvar      : " + str(args.OUT_WRITE_AUXVAR))
print("  write_2d_hist        : " + str(args.OUT_WRITE_2DFIELDS))
print("  write_3d_hist        : " + str(args.OUT_WRITE_3DFIELDS))

logging.debug("  do_basaltemp         : %s", args.OUT_CALC_BASAL_TEMP)
logging.debug("  do_basalmelt         : %s", args.OUT_CALC_BASAL_MELT)
logging.debug("  do_reduce_bmelt      : %s", args.OUT_CALC_LESS_BASAL_LOW_CAVITY)
logging.debug("  do_frontalmelt       : %s", args.OUT_CALC_FRONTAL_MELT)
logging.debug("  do_mergefrontal&basal: %s", args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT)
logging.debug("  do_write_time        : %s", args.LOGGING_WRITE_TIME)
logging.debug("  write_2d_auxvar      : %s", args.OUT_WRITE_AUXVAR)
logging.debug("  write_2d_hist        : %s", args.OUT_WRITE_2DFIELDS)
logging.debug("  write_3d_hist        : %s", args.OUT_WRITE_3DFIELDS)


print(" ")

# --------------
#
# Open the files and access the data
#
if args.LOGGING_WRITE_TIME:
    print("Load data    (" + time.strftime("%H:%M:%S [%Z]") + ")")
else:
    print("Load data")

# -----------
#
# ice sheet/shelf data
#
print(" Read ice model data file  : " + args.ISM_INPUT_FILE)
logging.info('Read ice   model data file  : "%s"', args.ISM_INPUT_FILE)

if flag_use_xarray:
    fileice = xarray.open_dataset(args.ISM_INPUT_FILE, decode_times="false")
    try:
        time_ice = fileice.get(args.ISM_TIME_VARIABLE).data
        time_ice0 = fileice.time.size - 1
    except:
        time_ice = [1234.5678]
        time_ice0 = [1234.5678]
    topg = fileice.get(args.ISM_BED_VARIABLE).isel(time=-1).data
    try:
        mask = fileice.get(args.ISM_MASK_VARIABLE).isel(time=-1).data
    except:
        mask = np.full_like(topg, args.ISM_MASK_FLOAT_VALUE)
        logging.warning(
            "MISSING mask variable '%s' use instead commen value of %i",
            args.ISM_MASK_VARIABLE,
            args.ISM_MASK_FLOAT_VALUE,
        )
    try:
        thk = fileice.get(args.ISM_THICK_VARIABLE).isel(time=-1).data
    except:
        thk = np.full_like(topg, 1000.0)
        logging.warning(
            "MISSING thickness variable '%s' use instead commen value of 1000.0",
            args.ISM_THICK_VARIABLE,
        )
    try:
        icetemp_top = fileice.get(args.ISM_ICE_TEMP_VARIABLE).isel(time=-1).data
    except:
        icetemp_top = np.full_like(topg, args.AIR_TEMPERATURE)
        logging.warning(
            "MISSING ice temperature variable '%s' use instead commen value of %f",
            args.ISM_TEMP_VARIABLE,
            args.AIR_TEMPERATURE,
        )
    lat = fileice.lat.data
    lon = fileice.lon.data
    xpism = fileice.x.data
    ypism = fileice.y.data
    # Tice3D = fileice.get('temp_pa').isel(time=-1).data
else:
    fileice = MFDataset(args.ISM_INPUT_FILE)
    try:
        time_ice = fileice.variables[args.ISM_TIME_VARIABLE]
        time_ice0 = np.size(time_ice) - 1
    except:
        time_ice = [1234.5678]
        time_ice0 = [1234.5678]
    topg = fileice.variables[args.ISM_BED_VARIABLE][time_ice0, :, :]
    try:
        mask = fileice.variables[args.ISM_MASK_VARIABLE][time_ice0, :, :]
    except:
        mask = np.full_like(topg, args.ISM_MASK_FLOAT_VALUE)
        logging.warning(
            "MISSING mask variable '%s' use instead commen value of %i",
            args.ISM_MASK_VARIABLE,
            args.ISM_MASK_FLOAT_VALUE,
        )
    try:
        thk = fileice.variables[args.ISM_THICK_VARIABLE][time_ice0, :, :]
    except:
        thk = np.full_like(topg, 1000.0)
        logging.warning(
            "MISSING thickness variable '%s' use instead commen value of 1000.0",
            args.ISM_THICK_VARIABLE,
        )
    try:
        icetemp_top = fileice.variables[args.ISM_ICE_TEMP_VARIABLE][time_ice0, :, :]
    except:
        icetemp_top = np.full_like(topg, args.AIR_TEMPERATURE)
        logging.warning(
            "MISSING ice temperature variable '%s' use instead commen value of %f",
            args.ISM_TEMP_VARIABLE,
            args.AIR_TEMPERATURE,
        )
    lat = fileice.variables["lat"]
    lon = fileice.variables["lon"]
    xpism = fileice.variables["x"]
    ypism = fileice.variables["y"]
    # Tice3D   = fileice.variables['temp_pa'].getValue()[time_ice0,:,:,:] #(t,x,y,z)

logging.debug("  Completed read of ice model")


#
# Geometrical parameters (Ice sheet model specific values)
#
if args.ISM_GRID_HEIGHT > 0:
    dz0_ice = args.ISM_GRID_HEIGHT  # lowest vertical grid size in ice model (meter)
    logging.info(
        "  * dz (height diff.) = %s m : command line parameter <= 'ISM_GRID_HEIGHT>0'",
        "{:7.2f}".format(dz0_ice),
    )
elif args.ISM_GRID_HEIGHT == 0:
    dz0_ice = thk
    logging.info(
        "  * dz (height diff.) = %s m : ice thickness data <='ISM_GRID_HEIGHT=0'",
        "{:7.2f}".format(dz0_ice.max()),
    )
else:
    if flag_use_xarray:
        height = fileice.get(args.ISM_GRID_Z_AXIS_VARIABLE).data
    else:
        height = fileice.variables[args.ISM_GRID_Z_AXIS_VARIABLE]
    dz0_ice = abs(height[1] - height[0])  # dz0_ice = abs(z[1] - z[0])
    logging.info(
        "  * dz (height diff.) = %s m : from vertical axis %s <= 'ISM_GRID_HEIGHT<0'",
        "{:7.2f}".format(dz0_ice),
        args.ISM_GRID_Z_AXIS_VARIABLE,
    )

if args.ISM_GRID_LENGTH > 0:
    dx0_ice = args.ISM_GRID_LENGTH  # x-direction (meter)
    dy0_ice = args.ISM_GRID_LENGTH  # y-direction (meter)
    logging.info(
        "  * dx&dy (horizontal size) = %s m/%s m : command line argument <= 'ISM_GRID_LENGTH>0'",
        "{:8.1f}".format(dx0_ice),
        "{:8.1f}".format(dy0_ice),
    )
else:
    dx0_ice = abs(xpism[1] - xpism[0])
    dy0_ice = abs(ypism[1] - ypism[0])
    logging.info(
        "  * dx&dy (horizontal size) = %s m/%s m : vertical axis %s and %s <= 'ISM_GRID_LENGTH<0'",
        "{:8.1f}".format(dx0_ice),
        "{:8.1f}".format(dy0_ice),
        "x",
        "y",
    )

area_const = dx0_ice * dy0_ice  # (m2) horizontal area of the ice sheet grid
logging.info("    => grid area %s km", "{:9.2f}".format(area_const * 1e-6))

# -----------
#
# ocean data
#
print(" Read ocean model data file: " + args.OCEAN_INPUT_FILE)
logging.info('Read ocean model data file  : "%s"', args.OCEAN_INPUT_FILE)
flag_use_uabs = False
flag_expand_dim = False
if flag_use_xarray:
    fileoce = xarray.open_dataset(args.OCEAN_INPUT_FILE, decode_times="false")
    try:
        time_oce = fileoce.get(args.OCEAN_TIME_VARIABLE).data
    except:
        time_oce = [1234.5678]
        flag_expand_dim = True
        logging.debug(
            "  - Time does not exists, use dummy fime %f", "{:12.4f}".format(time_oce)
        )
    temp = fileoce.get(args.OCEAN_TEMP_VARIABLE).data
    salt = fileoce.get(args.OCEAN_SALT_VARIABLE).data
    depth = fileoce.get(args.OCEAN_DEPTH_VARIABLE).data

    if args.OCEAN_VELOCITY_VARIABLE != "Skip_Use_Of_Absolute_Velocity":
        uabs = fileoce.get(args.OCEAN_VELOCITY_VARIABLE).data
        flag_use_uabs = True
else:
    fileoce = MFDataset(args.OCEAN_INPUT_FILE)
    try:
        time_oce = fileoce.variables[args.OCEAN_TIME_VARIABLE]
    except:
        time_oce = [1234.5678]
        flag_expand_dim = True
        logging.debug(
            "  - Time does not exists, use dummy fime %f", "{:12.4f}".format(time_oce)
        )
    temp = fileoce.variables[args.OCEAN_TEMP_VARIABLE]
    salt = fileoce.variables[args.OCEAN_SALT_VARIABLE]
    depth = fileoce.variables[args.OCEAN_DEPTH_VARIABLE]
    if args.OCEAN_VELOCITY_VARIABLE != "Skip_Use_Of_Absolute_Velocity":
        uabs = fileoce.variables[args.OCEAN_VELOCITY_VARIABLE]
        flag_use_uabs = True

if flag_expand_dim:
    temp = np.expand_dims(temp, axis=0)
    salt = np.expand_dims(salt, axis=0)
    if flag_use_uabs:
        uabs = np.expand_dims(uabs, axis=0)
        logging.info("  -> Expanded the dimensions of 'temp', 'salt', 'uabs'")
    else:
        logging.info("  -> Expanded the dimensions of 'temp', 'salt'")

logging.debug("  Completed read of ocean model")

if flag_use_uabs:
    logging.warning(
        "Use of the ocean velocity '%s' is not yet implemented",
        args.OCEAN_VELOCITY_VARIABLE,
    )

# =============================================
#
#
# Deduced variables
#
ilen = np.size(xpism)  # ilen = np.size(temp[0,0,:,0])
jlen = np.size(ypism)  # jlen = np.size(temp[0,0,0,:])
klen = np.size(depth)

#
# FIXME: Assumption about the NEMO/ocean data file
#   1. time is first index in the
#   2. depth is the second index
#   3. lat/lon are the third and fourth index
#
# itime=0;   idepth=1;   idx=2;  jdx=3       # 0-based (zero-based)
tlen = np.size(time_oce)  # np.size(temp[:, 1, 1, 1])

size2D = (ilen, jlen)  # np.shape(isfloating)
size2Dtime = (tlen, ilen, jlen)  # np.shape(temp[:, 1, :, :])
size3D = (klen, ilen, jlen)  # np.shape(temp[:, :, :, :])
size3Dtime = (tlen, klen, ilen, jlen)  # np.shape(temp[:, :, :, :])

# --
# From ice model data
#

# logical fields: floating ice, ice-free ocean
#   The following requires numpy: == on arrays
isice = thk > eps
isfloating = np.logical_and(isice, mask == args.ISM_MASK_FLOAT_VALUE)  # floating ice
isocean = (
    mask >= 3
)  # ocean ; #isocean     = np.logical_or(mask == 7, mask == 4,  mask == 3)
isocean_free = np.logical_and(np.abs(thk) < eps, isocean)  # ice free ocean
isfrontalmelt = np.zeros(size2D, dtype="bool")

# Melting under the ice shelf :: where the neigboring cell is ocean
for i0 in range(1, ilen - 1):
    for j0 in range(1, jlen - 1):
        if isocean_free[i0, j0]:
            for ii in [i0 - 1, i0 + 1]:
                # links, rechts
                if isfloating[ii, j0]:
                    # do melt
                    isfrontalmelt[ii, j0] = True
            for jj in [j0 - 1, j0 + 1]:
                # unten, oben
                if isfloating[i0, jj]:
                    # do melt
                    isfrontalmelt[i0, jj] = True
# -----------
#
# geometery under the ice
#
floating_thk = np.where(isfloating, thk, 0.0)
floating_basedepth = floating_thk * dens_ice / dens_ocean
floating_space_grounded = 1.23  # meter
floating_space_grounded = 1.01  # meter
# floating_free      = floating_basedepth - topg
# Where we have floating ice shelves we compute the actual free space
# between bedgord and the ice she lf base, and else we take the depth
# bedrock topography for ocean areas (topography/ocean depth) and
# while for land has assume free vertical space of `floating_space_grounded`
floating_free = np.where(
    isfloating,
    floating_basedepth - topg,
    np.where(isocean, -topg, floating_space_grounded),
)

#
# No basal melting for free cavity height of less than depth_free_start,
# linear increasing fraction until depth_free_end, afterwards full melting
#
depth_free_start = 1.0  # (m)
# depth_free_start   = 0.5   # (m)
depth_free_end = 5.0  # (m)
df_a = 1.0 / (depth_free_end - depth_free_start)  # (1/m)
df_b = -depth_free_start * df_a  # (1)

# Full basal melting in open ocean and fractional melting under floating ice, no melting of grounded ice
if args.OUT_CALC_LESS_BASAL_LOW_CAVITY:
    bmb_frac = np.where(
        isice, np.minimum(1.0, np.maximum(0.0, df_a * floating_free + df_b)), 1.0
    )
    # Full basal melting in open ocean and of grounded ice, fractional melting of floating ice.
    # bmb_frac = np.where(isfloating, np.minimum(1.0, np.maximum(0.0,  df_a * floating_free + df_b)), 1.0)
else:
    bmb_frac = np.ones(size2D)


# -----------
#
# Depth of layer at/below the basal ice-ocean interface in the ocean grid
#
kbelow = np.zeros(size2D, dtype="i")
press3 = np.zeros(size3D)
press2 = np.zeros(size2D) + pressure_sealevel
frac_below = np.ones(size2D)

press1 = np.array(depth2press(depth, dens_ocean, pressure_sealevel))
for i0 in range(ilen):
    for j0 in range(jlen):
        # sigma-coordiantes: press1 = np.array(depth2press(depth[:, i0, j0],  dens_ocean ,  pressure_sealevel))
        press3[:, i0, j0] = press1

for i0 in range(ilen):
    for j0 in range(jlen):
        frac_below[i0, j0] = 1.0
        # Take as reference depth for the temperature and salinity the
        # box as deep or deeper than the depth of the base.
        if isfloating[i0, j0]:
            [kbelow[i0, j0], frac_below[i0, j0]] = find_kindex(
                depth, floating_basedepth[i0, j0]
            )
            press2[i0, j0] = press3[kbelow[i0, j0], i0, j0]
        elif isice[i0, j0]:
            [kbelow[i0, j0], frac_below[i0, j0]] = find_kindex(depth, -topg[i0, j0])
            press2[i0, j0] = press3[kbelow[i0, j0], i0, j0]

kabove = np.where(kbelow > 0, kbelow - 1, 0)

#
# Thickness of each oceanic
#
# old dz_oce = np.zeros(np.shape(depth))
dz_oce = np.zeros_like(depth)
dz_oce = depth2dz(depth)

# -----------
#
# Basal melting under the ice
#
print("")

#
# Basal ice temperature
#
# 2D at ocean-ice interface
TempIceBase = np.zeros(size2Dtime)
bmelt = np.zeros(size2Dtime)
SFlux = np.zeros(size2Dtime)
TFlux = np.zeros(size2Dtime)
TempBound = np.zeros(size2Dtime)
SaltBound = np.zeros(size2Dtime)
dTdz = np.zeros(size2Dtime)

# 3D through out the entire ocean columns
TempIceBase3 = np.zeros(size3Dtime)
bmelt3 = np.zeros(size3Dtime)
SFlux3 = np.zeros(size3Dtime)
TFlux3 = np.zeros(size3Dtime)
TempBound3 = np.zeros(size3Dtime)
SaltBound3 = np.zeros(size3Dtime)
dTdz3 = np.zeros(size3Dtime)

# frontal melting
fmelt = np.zeros(size2Dtime)
if args.OUT_CALC_FRONTAL_MELT:
    fmelt3 = np.zeros(size3Dtime)

#
# Melting rates
#
bmelt_rate = np.zeros(size2Dtime)  # m3/s
fmelt_rate = np.zeros(size2Dtime)
bmelt3_rate = np.zeros(size2Dtime)  # m3/s
if args.OUT_CALC_FRONTAL_MELT:
    fmelt3_rate = np.zeros(size3Dtime)

#
#
#
Tinterface = np.zeros(size2D)
Sinterface = np.zeros(size2D)
Tabove = np.zeros(size2D)
Sabove = np.zeros(size2D)
Tbelow = np.zeros(size2D)
Sbelow = np.zeros(size2D)

#
# Freezing temperatures for all ocean grid points at any time
#
if args.LOGGING_WRITE_TIME:
    print(
        "FREEZING temperature through the column     ("
        + time.strftime("%H:%M:%S [%Z]")
        + ")"
    )
else:
    print("FREEZING temperature through the column")
TempIceBase3 = Tfreeze_lin(salt, press3)


if args.OUT_WRITE_TEMP_EXCESS:
    if args.LOGGING_WRITE_TIME:
        print(
            "TEMPERATURE above melting in the column     ("
            + time.strftime("%H:%M:%S [%Z]")
            + ")"
        )
    else:
        print("TEMPERATURE above melting in the column")
    # TempIceAboveMelt = np.zeros_like(TempIceBase3)
    TempIceAboveMelt = temp - TempIceBase3


#
# The loop
#
logging.debug("Loop of %i time step(s)", tlen)
print("Loop of all " + str(tlen) + " time step(s)")

returnall = np.zeros((6, ilen, jlen))
for it in range(tlen):
    print("  Time step " + str(it) + ", ocean time :" + str(time_oce[it]))
    logging.info("  Time step %5i with time stamp %s", it, str(time_oce[it]))

    #
    # Fields below/at ocean-ice interface
    #
    for i0 in range(ilen):
        for j0 in range(jlen):
            Sabove[i0, j0] = salt[it, kabove[i0, j0], i0, j0]
            Tabove[i0, j0] = temp[it, kabove[i0, j0], i0, j0]

            Sbelow[i0, j0] = salt[it, kbelow[i0, j0], i0, j0]
            Tbelow[i0, j0] = temp[it, kbelow[i0, j0], i0, j0]

    Sinterface = Sbelow * frac_below + Sabove * (1.0 - frac_below)
    Tinterface = Tbelow * frac_below + Tabove * (1.0 - frac_below)

    #
    # Basal freezing temperatures
    #
    if args.OUT_CALC_BASAL_TEMP or args.OUT_CALC_BASAL_MELT:
        if args.LOGGING_WRITE_TIME:
            print(
                "     BASAL freezing temperature    ("
                + time.strftime("%H:%M:%S [%Z]")
                + ")"
            )
        else:
            print("     BASAL freezing temperature")

        # Basal ice shelf temperature in the ice
        # MemoryError!! TempIceBase[it, :,:] = TempIceBase3[it, kbelow, :,:]
        for i0 in range(ilen):
            for j0 in range(jlen):
                TempIceBase[it, i0, j0] = TempIceBase3[
                    it, kbelow[i0, j0], i0, j0
                ] * frac_below[i0, j0] + TempIceBase3[it, kabove[i0, j0], i0, j0] * (
                    1.0 - frac_below[i0, j0]
                )

    if np.logical_or(args.OUT_CALC_BASAL_MELT, args.OUT_CALC_FRONTAL_MELT):
        if args.LOGGING_WRITE_TIME:
            print(
                "     MELTING through the column    ("
                + time.strftime("%H:%M:%S [%Z]")
                + ")"
            )
        else:
            print("     MELTING through the column")

        for k0 in range(klen):
            print(
                "      depth layer ("
                + str(k0)
                + " / "
                + str(klen - 1)
                + ") = "
                + str(depth[k0])
                + "m"
            )
            returnall = np.array(
                boundaryFluxSaltTempW3(
                    dz0_ice,
                    icetemp_top[:, :],
                    press3[k0, :, :],
                    temp[it, k0, :, :],
                    salt[it, k0, :, :],
                )
            )
            bmelt3[it, k0, :, :] = returnall[0, :, :]  # 0-based
            SFlux3[it, k0, :, :] = returnall[1, :, :]
            TFlux3[it, k0, :, :] = returnall[2, :, :]
            SaltBound3[it, k0, :, :] = returnall[3, :, :]
            TempBound3[it, k0, :, :] = returnall[4, :, :]
            dTdz3[it, k0, :, :] = returnall[5, :, :]

        bmelt3_rate = bmelt3 * dx0_ice * dy0_ice

    if args.OUT_CALC_BASAL_MELT:
        if args.LOGGING_WRITE_TIME:
            print("     BASAL melting      (" + time.strftime("%H:%M:%S [%Z]") + ")")
        else:
            print("     BASAL melting")
        for i0 in range(ilen):
            procent = 100.0 * float(i0 / ilen)
            string2show = str("{:5.1f}".format(procent)) + "%"
            print("{0}\r".format(string2show), end="")

            for j0 in range(jlen):
                if isfloating[i0, j0]:
                    bmelt[it, i0, j0] = bmelt3[it, kbelow[i0, j0], i0, j0] * frac_below[
                        i0, j0
                    ] + bmelt3[it, kabove[i0, j0], i0, j0] * (1.0 - frac_below[i0, j0])
                    SFlux[it, i0, j0] = SFlux3[it, kbelow[i0, j0], i0, j0] * frac_below[
                        i0, j0
                    ] + SFlux3[it, kabove[i0, j0], i0, j0] * (1.0 - frac_below[i0, j0])
                    TFlux[it, i0, j0] = TFlux3[it, kbelow[i0, j0], i0, j0] * frac_below[
                        i0, j0
                    ] + TFlux3[it, kabove[i0, j0], i0, j0] * (1.0 - frac_below[i0, j0])
                    SaltBound[it, i0, j0] = SaltBound3[
                        it, kbelow[i0, j0], i0, j0
                    ] * frac_below[i0, j0] + SaltBound3[it, kabove[i0, j0], i0, j0] * (
                        1.0 - frac_below[i0, j0]
                    )
                    TempBound[it, i0, j0] = TempBound3[
                        it, kbelow[i0, j0], i0, j0
                    ] * frac_below[i0, j0] + TempBound3[it, kabove[i0, j0], i0, j0] * (
                        1.0 - frac_below[i0, j0]
                    )
                    dTdz[it, i0, j0] = dTdz3[it, kabove[i0, j0], i0, j0]
        print("{0}\r".format("100.0%"))

        if args.OUT_CALC_LESS_BASAL_LOW_CAVITY:
            print("       Reduction for low cavities")
            #
            # Reduced melting for cavities with small height
            # -- Reduce only fluxes (indexes 0, 1, 2) => range(3)
            #
            [i0, j0] = np.where(floating_free <= depth_free_end)
            bmelt[it, i0, j0] = np.multiply(bmelt[it, i0, j0], bmb_frac[i0, j0])
            SFlux[it, i0, j0] = np.multiply(SFlux[it, i0, j0], bmb_frac[i0, j0])
            TFlux[it, i0, j0] = np.multiply(TFlux[it, i0, j0], bmb_frac[i0, j0])

        bmelt_rate = bmelt * dx0_ice * dy0_ice
    else:
        print("     SKIP Basal melting")

    #
    # Frontal Melting
    #
    if args.OUT_CALC_FRONTAL_MELT:
        if args.LOGGING_WRITE_TIME:
            print("     FRONTAL melting    (" + time.strftime("%H:%M:%S [%Z]") + ")")
        else:
            print("     FRONTAL melting")
            if args.OUT_CALC_FRONTAL_MELT_AT_ICE_POSITION:
                print("     + release on ice shelf side")
            else:
                print("     + release on open ocean side")
        if np.any(isfrontalmelt):

            kmax = np.max(kbelow)  # np.max(kabove+1):
            fmelt[it, :, :] = 0.0
            fmelt3[it, :, :, :] = 0.0
            fmelt_rate[it, :, :] = 0.0
            fmelt3_rate[it, :, :, :] = 0.0

            for k0 in range(kmax):
                procent = 100.0 * float(k0 / kmax)
                string2show = str("{:5.1f}".format(procent)) + "%"
                print("{0}\r".format(string2show), end="")

                for i0 in range(1, ilen - 1):
                    for j0 in range(1, jlen - 1):
                        if isfrontalmelt[i0, j0]:
                            for ii in [
                                i0 - 1,
                                i0 + 1,
                            ]:  # left and right: ii=i0+-1, jj=j0
                                if isfloating[ii, j0]:  # do melt
                                    ##iice=ii ; jice=j0 # At the floating ice position
                                    if args.OUT_CALC_FRONTAL_MELT_AT_ICE_POSITION:
                                        iice = ii
                                        jice = j0  # At the floating ice position
                                    else:
                                        iice = i0
                                        jice = j0  # At the ocean/ice front position

                                    fmelt3[it, k0, iice, jice] = (
                                        fmelt3[it, k0, iice, jice]
                                        + bmelt3[it, k0, i0, j0]
                                    )
                                    fmelt3_rate[it, k0, iice, jice] = (
                                        fmelt3_rate[it, k0, iice, jice]
                                        + bmelt3[it, k0, i0, j0] * dz_oce[k0] * dy0_ice
                                    )
                            for jj in [
                                j0 - 1,
                                j0 + 1,
                            ]:  # above and below : ii=i0, jj=j0+-1
                                if isfloating[i0, jj]:  # do melt
                                    ##iice=i0 ; jice=jj # At the floating ice position
                                    if args.OUT_CALC_FRONTAL_MELT_AT_ICE_POSITION:
                                        iice = i0
                                        jice = jj  # At the floating ice position
                                    else:
                                        iice = i0
                                        jice = j0  # At the ocean/ice front position

                                    fmelt3[it, k0, iice, jice] = (
                                        fmelt3[it, k0, iice, jice]
                                        + bmelt3[it, k0, i0, j0]
                                    )
                                    fmelt3_rate[it, k0, iice, jice] = (
                                        fmelt3_rate[it, k0, iice, jice]
                                        + bmelt3[it, k0, i0, j0] * dz_oce[k0] * dx0_ice
                                    )
                        fmelt[it, i0, j0] = fmelt[it, i0, j0] + fmelt3[it, k0, i0, j0]
                        fmelt_rate[it, i0, j0] = (
                            fmelt_rate[it, i0, j0] + fmelt3_rate[it, k0, i0, j0]
                        )

            print("{0}\r".format("100.0%"))

        else:
            print("     => NO ice fronts")
    else:
        print("     SKIP Frontal melting")
    if args.LOGGING_WRITE_TIME:
        print("  .... next             (" + time.strftime("%H:%M:%S [%Z]") + ")")
    else:
        print("  .... next")

print("")

# =============================================
#
# -------------- Output file ----------------
#
if args.LOGGING_WRITE_TIME:
    print("Save data               (" + time.strftime("%H:%M:%S [%Z]") + ")")
else:
    print("Save data")
# -----------
#
# ice sheet/shelf data
#
print(" Create ice-ocean forcing output data file  : " + args.OUTPUT_FILE)
logging.info("Create ice-ocean forcing output file : '%s'", args.OUTPUT_FILE)
filebmb = Dataset(args.OUTPUT_FILE, mode="w")  # Open output

#
# Global NetCDF attributes
#
logging.debug("  Define structure")
logging.debug("    Global attributes")

#
# Model data specific information
#
filebmb.title = "Ocean-driven ablation of ice shelves and sheet"
filebmb.type = "Ocean forcing for ice sheet model"
filebmb.icesheet_input_file = args.ISM_INPUT_FILE
filebmb.ocean_input_file = args.OCEAN_INPUT_FILE
filebmb.forcing_output_file = args.OUTPUT_FILE
if args.LOGGING_WRITE_TIME:
    filebmb.creation_date = "Created on " + time.ctime(time.time())

#
# Please put here your information
#
filebmb.authors = "Christian Rodehacke"
filebmb.insitution = "Alfred Wegener Institute for Polar and Marine Research (AWI)"
filebmb.devision = "Climate"
filebmb.department = "Paleoclimate"
filebmb.address = "Handelshafen, D-27570 Bremerhaven, Germany"
filebmb.web = "http://www.awi.de"

#
# Computer related information
#
if args.OUT_WRITE_MACHINE_INFORMATION:
    try:
        filebmb.working_directory = os.getcwd()
    except:
        filebmb.working_directory = "unknown"

    try:
        filebmb.username = os.environ["USER"]
    except:
        filebmb.username = "unknown"

    try:
        filebmb.uname_node = os.uname().nodename
    except:
        filebmb.uname_node = "unknown"

    try:
        filebmb.uname_system = os.uname().sysname
    except:
        filebmb.uname_system = "unknown"

    try:
        filebmb.uname_machine = os.uname().machine
    except:
        filebmb.uname_machine = "unknown"
    # filebmb.uname_os = os.popen('uname -o').read()


if flag_write_platform_information:
    try:
        filebmb.python_version = platform.python_version()
    except:
        filebmb.python_version = "unknown"

    try:
        filebmb.python_compiler = platform.python_compiler()
    except:
        filebmb.python_compiler = "unknown"


#
# Definition of the dimensions
#
logging.debug("    Define dimensions")
filebmb.createDimension("time", tlen)
filebmb.createDimension("x", ilen)
filebmb.createDimension("y", jlen)
filebmb.createDimension("z", klen)
filebmb.createDimension("one", (1))


#
# Define output variables and specify its attributes
#

logging.debug("    Define static variables")
rhoi_var = filebmb.createVariable("rho_ice", "d", ("one",))
rhoi_var.long_name = "density of ice"
rhoi_var.units = "kg/m3"

rhoo_var = filebmb.createVariable("rho_ocean", "d", ("one",))
rhoo_var.long_name = "density of sea water"
rhoo_var.units = "kg/m3"

# x-, y-arrays
x_var = filebmb.createVariable("x", "d", ("x",))
x_var.standard_name = "projection_x_coordinate"
x_var.long_name = "X-coordinate in Cartesian system"
x_var.unit = "m"
y_var = filebmb.createVariable("y", "d", ("y",))
y_var.standard_name = "projection_y_coordinate"
y_var.long_name = "Y-coordinate in Cartesian system"
y_var.unit = "m"

# i-, j-, k-arrays
i_var = filebmb.createVariable("ic", "i", ("x",))
i_var.long_name = "counter of X-coordinate"
j_var = filebmb.createVariable("jc", "i", ("y",))
j_var.long_name = "counter of Y-coordinate"
k_var = filebmb.createVariable("kc", "i", ("z",))
k_var.long_name = "counter of Z-coordinate"

# FIXME: Assume that the oceanic depth is a 1D array
depth_var = filebmb.createVariable("depth1D", "d", ("z",))
# depth_var           = filebmb.createVariable('depth2D', 'd', ('z', 'x','y',))
depth_var.long_name = "depth"
depth_var.units = "m"
depth_var.positive = "downward"
depth_var.valid_range = [-12000.0, 12000.0]

if args.OUT_WRITE_AUXVAR:
    dz_var = filebmb.createVariable("dz1D", "d", ("z",))
    # dz_var           = filebmb.createVariable('dz2D', 'd', ('z', 'x','y',))
    dz_var.long_name = "grid_box_height"
    dz_var.units = "m"
    dz_var.valid_range = [0.0, 1000.0]


# lon, lat
lon_var = filebmb.createVariable(
    "longitude",
    "d",
    (
        "x",
        "y",
    ),
)
lon_var.long_name = "longitude"
lon_var.units = "degree"
lon_var.valid_range = [-180.0, 360.0]
lat_var = filebmb.createVariable(
    "latitude",
    "d",
    (
        "x",
        "y",
    ),
)
lat_var.long_name = "latitude"
lat_var.units = "degree"
lat_var.valid_range = [-90.0, 90.0]

area_var = filebmb.createVariable(
    "area",
    "d",
    (
        "x",
        "y",
    ),
)
area_var.long_name = "horizontal area"
area_var.units = "m2"
area_var.valid_im = 0.0

# Ocean data grid
# kbelow
kbelow_var = filebmb.createVariable(
    "kbelow",
    "i",
    (
        "x",
        "y",
    ),
)
kbelow_var.long_name = "ocean grid, layer at/below the interface ice base-ocean"
kbelow_var.coordinates = "latitude longitude"
kbelow_var.positive = "downward"

# basal interface
basedepth_var = filebmb.createVariable(
    "basedepth",
    "d",
    (
        "x",
        "y",
    ),
)
basedepth_var.long_name = "floating ice basal interface depth"
basedepth_var.units = "m"
basedepth_var.coordinates = "latitude longitude"
basedepth_var.valid_min = 0.0
basedepth_var.reference_density_ice = dens_ice
basedepth_var.reference_density_ocean = dens_ocean
basedepth_var.positive = "downward"

# bmelt fraction
bmelt_frac_var = filebmb.createVariable(
    "bmb_frac",
    "d",
    (
        "x",
        "y",
    ),
)
bmelt_frac_var.long_name = "fraction_bmb"
bmelt_frac_var.coordinates = "latitude longitude"
# bmelt_frac_var.units       ='m'
bmelt_frac_var.valid_range = [0.0, 1.0]

if args.OUT_CALC_LESS_BASAL_LOW_CAVITY:
    bmelt_frac_var.use_fractional_basal_melting = "Yes, True"
    bmelt_frac_var.use_fractional_basal_melting_doc = (
        "Since OUT_CALC_LESS_BASAL_LOW_CAVITY="
        + str(args.OUT_CALC_LESS_BASAL_LOW_CAVITY)
        + ", we apply fractional basal melting for low vertical space between bedrock and ice shelf base. Pleases inspect also variable 'floating_free'"
    )
    bmelt_frac_var.depth_free_start = depth_free_start
    bmelt_frac_var.depth_free_start_doc = (
        "At and below this free vertical start depth="
        + str(depth_free_start)
        + "m, the fraction is zero, Between start and end depth the fraction is linearly interpolated"
    )
    bmelt_frac_var.depth_free_end = depth_free_end
    bmelt_frac_var.depth_free_end_doc = (
        "At and below this free vertical end depth="
        + str(depth_free_end)
        + "m, the fraction is one. Between start and end depth the fraction is linearly interpolated"
    )
else:
    bmelt_frac_var.use_fractional_basal_melting = "No, False"
    bmelt_frac_var.use_fractional_basal_melting_doc = (
        "Since OUT_CALC_LESS_BASAL_LOW_CAVITY="
        + str(args.OUT_CALC_LESS_BASAL_LOW_CAVITY)
        + ", we do not use fractional basal melting for low vertical space between bedrock and ice shelf base"
    )

bmelt_frac_var.floating_space_of_grounded = floating_space_grounded
bmelt_frac_var.floating_space_of_grounded_doc = (
    "Grounded ice has an assumed vertical free space of "
    + str(floating_space_grounded)
    + " meter"
)


if args.OUT_WRITE_AUXVAR:
    # isocean ice mask
    isocean_var = filebmb.createVariable(
        "isocean",
        "i",
        (
            "x",
            "y",
        ),
    )
    isocean_var.long_name = "isocean_mask (1: receives frontal melt, 0: else)"
    isocean_var.coordinates = "latitude longitude"

    # isfloating ice mask
    isfloat_var = filebmb.createVariable(
        "isfloating",
        "i",
        (
            "x",
            "y",
        ),
    )
    isfloat_var.long_name = "isfloating_mask (1: floating ice shelf, 0: else)"
    isfloat_var.coordinates = "latitude longitude"

    # isfrontalmelt ice mask
    isfrontalmelt_var = filebmb.createVariable(
        "isfrontalmelting",
        "i",
        (
            "x",
            "y",
        ),
    )
    isfrontalmelt_var.long_name = (
        "isfrontalmelting_mask (1: receives frontal melt, 0: else)"
    )
    isfrontalmelt_var.coordinates = "latitude longitude"

    # floating_free
    float_free_var = filebmb.createVariable(
        "floating_free",
        "d",
        (
            "x",
            "y",
        ),
    )
    float_free_var.long_name = "free_floating_depth"
    float_free_var.coordinates = "latitude longitude"
    float_free_var.units = "m"
    float_free_var.valid_min = -0.0
    float_free_var.valid_max = np.max(-topg)
    float_free_var.floating_space_of_grounded = floating_space_grounded

    # depthbelow
    depthbelow_var = filebmb.createVariable(
        "depthbelow",
        "d",
        (
            "x",
            "y",
        ),
    )
    depthbelow_var.long_name = "depth of ocean layer at/below the interface"
    depthbelow_var.units = "m"
    depthbelow_var.coordinates = "latitude longitude"
    depthbelow_var.valid_range = [np.min(depth), np.max(depth)]


logging.debug("    Define temporal variables")
# time (taken from ocean output)
time_var = filebmb.createVariable("time", "d", ("time",))
time_var.long_name = "time"
time_var.units = "seconds since 1850-01-01 00:00:00"
time_var.calendar = "gregorian"

#
# --- Required rates for the coupling: Basal ice temperature
#
bit_var = filebmb.createVariable(
    args.OUT_BASAL_TEMP_VARIABLE,
    "d",
    (
        "time",
        "x",
        "y",
    ),
)
bit_var.long_name = "ice temperature at ice base interface"
bit_var.units = "degree_Celsius"
bit_var.coordinates = "latitude longitude"

#
# --- Required rates for the coupling: fluxes
#
# basal melting
bmbR_var = filebmb.createVariable(
    args.OUT_BASAL_MASSFLUX_VARIABLE,
    "d",
    (
        "time",
        "x",
        "y",
    ),
)
if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
    bmbR_var.long_name = "basal and frontal melting rate combined"
else:
    bmbR_var.long_name = "basal_melting_rate"
if args.OUT_WRITE_MASS_FLUX_KG_BASED:
    bmbR_var.units = "kg/m2/s"
else:
    bmbR_var.units = "m3/s"
bmbR_var.coordinates = "latitude longitude"
bmbR_var.comment = "positive values correspond to ice loss"
bmbR_var.reference_density = dens_ice

### basal heat transfer rate
##bhbR_var = filebmb.createVariable(args.OUT_BASAL_HEATFLUX_VARIABLE,'d',('time', 'x','y', ))
##bhbR_var.long_name   ='basal_heat_transfer_rate'
##bhbR_var.units       ='degC/m2/s'
##bhbR_var.coordinates ='latitude longitude'
## #??# fhbR_var.comment     = "positive values correspond to ice warming" ;
##bhbR_var.reference_density=dens_ice

# frontal melting
fmbR_var = filebmb.createVariable(
    "fmbI",
    "d",
    (
        "time",
        "x",
        "y",
    ),
)
fmbR_var.long_name = "frontal_melting_rate, vertical integrated"
if args.OUT_WRITE_MASS_FLUX_KG_BASED:
    fmbR_var.units = "kg/m2/s"
else:
    fmbR_var.units = "m3/s"
fmbR_var.coordinates = "latitude longitude"
fmbR_var.comment = "positive values correspond to ice loss"
fmbR_var.reference_density = dens_ice

if args.OUT_CALC_FRONTAL_MELT:
    fmb3R_var = filebmb.createVariable(
        "fmb",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    fmb3R_var.long_name = "frontal_melting_rate"
    if args.OUT_WRITE_MASS_FLUX_KG_BASED:
        fmb3R_var.units = "kg/m2/s"
    else:
        fmb3R_var.units = "m3/s"
        fmb3R_var.coordinates = "latitude longitude"
        fmb3R_var.comment = "positive values correspond to ice loss"
        fmb3R_var.reference_density = dens_ice

# temperature gradient in the ice body
dTdz_var = filebmb.createVariable(
    "dTdz",
    "d",
    (
        "time",
        "x",
        "y",
    ),
)
dTdz_var.long_name = "temperature gradient in ice shelf"
dTdz_var.units = "Degree Celsius/Meter"
dTdz_var.coordinates = "latitude longitude"
dTdz_var.reference_density = dens_ice


if args.OUT_WRITE_2DFIELDS:
    #
    # --- Grid area independent melting quantities
    #
    #
    # -- basal melt
    #
    # basal melting
    bmb_var = filebmb.createVariable(
        "bmb_unit_area",
        "d",
        (
            "time",
            "x",
            "y",
        ),
    )
    if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
        bmb_var.long_name = "basal and frontal melting combined per unit area"
    else:
        bmb_var.long_name = "basal melting per unit area"
    bmb_var.units = "m/s"
    bmb_var.coordinates = "latitude longitude"
    bmb_var.comment = "positive values correspond to ice loss"
    bmb_var.reference_density = dens_ice

    # salt flux
    sflux_var = filebmb.createVariable(
        "Sflux_unit_area",
        "d",
        (
            "time",
            "x",
            "y",
        ),
    )
    sflux_var.long_name = "salinity flux per unit area at ice base"
    # sflux_var .units     ='psu/s'
    sflux_var.units = "psu/m2/s"
    sflux_var.coordinates = "latitude longitude"

    # temperature flux
    tflux_var = filebmb.createVariable(
        "Tflux_unit_area",
        "d",
        (
            "time",
            "x",
            "y",
        ),
    )
    tflux_var.long_name = "ocean temperature flux per unit area at ice base"
    # tflux_var .units       ='degC/s'
    tflux_var.units = "degC/m2/s"
    tflux_var.coordinates = "latitude longitude"

    # boundary layer salinity
    sbound_var = filebmb.createVariable(
        "basal_salt_boundary",
        "d",
        (
            "time",
            "x",
            "y",
        ),
    )
    sbound_var.long_name = "salinity boundary in layer at ice shelf base"
    sbound_var.units = "psu"
    sbound_var.coordinates = "latitude longitude"
    sbound_var.reference_density = dens_ice
    sbound_var.valid_range = [-0.0, 50.0]

    # boundary layer temperature
    tbound_var = filebmb.createVariable(
        "basal_temp_boundary",
        "d",
        (
            "time",
            "x",
            "y",
        ),
    )
    tbound_var.long_name = "ocean temperature in boundary layer at ice shelf base"
    tbound_var.units = "Degree Celsius"
    tbound_var.coordinates = "latitude longitude"
    tbound_var.reference_density = dens_ice
    tbound_var.valid_range = [-11.0, 30.0]

    #
    # -- frontal melt
    #
    # frontal melting
    fmb_var = filebmb.createVariable(
        "fmbI_unit_area",
        "d",
        (
            "time",
            "x",
            "y",
        ),
    )
    fmb_var.long_name = "frontal_melting_per_unit_area, vertical integrated"
    fmb_var.units = "m/s"
    fmb_var.coordinates = "latitude longitude"
    fmb_var.comment = "positive values correspond to ice loss"
    fmb_var.reference_density = dens_ice

if args.OUT_WRITE_3DFIELDS:
    # basal melting
    bmb3R_var = filebmb.createVariable(
        args.OUT_BASAL_MASSFLUX_VARIABLE + "3D",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
        bmb3R_var.long_name = (
            "basal and frontal melting rate combined at each ocean level "
        )
    else:
        bmb3R_var.long_name = "basal melting rate at each ocean level "
    if args.OUT_WRITE_MASS_FLUX_KG_BASED:
        bmb3R_var.units = "kg/m2/s"
    else:
        bmb3R_var.units = "m3/s"
    bmb3R_var.coordinates = "latitude longitude"
    bmb3R_var.comment = "positive values correspond to ice loss"
    bmb3R_var.reference_density = dens_ice

    bmb3_var = filebmb.createVariable(
        args.OUT_BASAL_MASSFLUX_VARIABLE + "3D_unit_area",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
        bmb3_var.long_name = (
            "basal and frontal melting rate combined at each ocean level per unit area"
        )
    else:
        bmb3_var.long_name = "basal melting rate at each ocean level per unit area"
    bmb3_var.units = "m/s"
    bmb3_var.coordinates = "latitude longitude"
    bmb3_var.comment = "positive values correspond to ice loss"
    bmb3_var.reference_density = dens_ice

    fmb3_var = filebmb.createVariable(
        "fmb_unit_area",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    fmb3_var.long_name = "frontal_melting_per_unit_area"
    fmb3_var.units = "m/s"
    fmb3_var.coordinates = "latitude longitude"
    fmb3_var.comment = "positive values correspond to ice loss"
    fmb3_var.reference_density = dens_ice

    # salt flux
    sflux3_var = filebmb.createVariable(
        "Sflux_unit_area3D",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    sflux3_var.long_name = "salinity flux per unit area"
    # sflux3_var.units     ='psu/s'
    sflux3_var.units = "psu/m2/s"
    sflux3_var.coordinates = "latitude longitude"

    # temperature flux
    tflux3_var = filebmb.createVariable(
        "Tflux_unit_area3D",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    tflux3_var.long_name = "ocean temperature flux per unit area"
    # tflux3_var.units       ='degC/s'
    tflux3_var.units = "degC/m2/s"
    tflux3_var.coordinates = "latitude longitude"

    # boundary layer salinity
    sbound3_var = filebmb.createVariable(
        "salt_boundary3D",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    sbound3_var.long_name = "salinity in boundary layer through water column"
    sbound3_var.units = "psu"
    sbound3_var.coordinates = "latitude longitude"
    sbound3_var.reference_density = dens_ice
    sbound3_var.valid_range = [-0.0, 50.0]

    # boundary layer temperature
    tbound3_var = filebmb.createVariable(
        "temp_boundary3D",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    tbound3_var.long_name = "ocean temperature in boundary layer through water column"
    tbound3_var.units = "Degree Celsius"
    tbound3_var.coordinates = "latitude longitude"
    tbound3_var.reference_density = dens_ice
    tbound3_var.valid_range = [-11.0, 30.0]

if args.OUT_WRITE_TEMP_EXCESS:
    tabove3_var = filebmb.createVariable(
        "temp_above_melting3D",
        "d",
        (
            "time",
            "z",
            "x",
            "y",
        ),
    )
    tabove3_var.long_name = (
        "ocean temperature excess above in local refreezing point through water column"
    )
    tabove3_var.units = "Degree Celsius"
    tabove3_var.coordinates = "latitude longitude"
    tabove3_var.valid_range = [-11.0, 35.0]
    tabove3_var.comment = "positive values correspond to an ocean temperatures above the local freezing temperature"


logging.debug("    Sync")
filebmb.sync()

#
# Assign the values
#
logging.debug("  Assign values")

logging.debug("    Skalars")
rhoi_var[:] = dens_ice
rhoo_var[:] = dens_ocean

# time line
logging.debug("    Time")
time_var[:] = time_oce[:]

logging.debug("    Static fields")
# x-, y-arrarys
x_var[:] = xpism[:]
y_var[:] = ypism[:]
# i-, j-, k-arrarys
i_var[:] = range(ilen)
j_var[:] = range(jlen)
k_var[:] = range(klen)
depth_var[:] = depth[:]
if args.OUT_WRITE_AUXVAR:
    dz_var[:] = dz_oce
# 2dim, temporal invariant
lon_var[:] = lon[:]
lat_var[:] = lat[:]
area_var[:] = area_const + np.zeros(np.shape(lat))

kbelow_var[:] = kbelow
bmelt_frac_var[:] = bmb_frac
basedepth_var[:] = floating_basedepth

dTdz_var[:] = dTdz


if args.OUT_WRITE_AUXVAR:
    logging.debug("    Auxillary fields")
    isocean_var[:] = isocean
    isfloat_var[:] = isfloating
    isfrontalmelt_var[:] = isfrontalmelt
    float_free_var[:] = floating_free
    depthbelow_var[:] = depth[kbelow]
else:
    logging.debug("    Skip auxillary fields")

#
# temporal evolution
#
logging.debug("    Evolving fields")

#
# basal ice temperature
#
bit_var[:] = TempIceBase

#
# Rates (grid area dependent)
#
if args.OUT_WRITE_MASS_FLUX_KG_BASED:
    # bmbR_var[:] = bmelt_rate * args.DENSITY_FRESHWATER
    if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
        bmbR_var[:] = (bmelt_rate + fmelt_rate) * args.DENSITY_ICE
    else:
        bmbR_var[:] = bmelt_rate * args.DENSITY_ICE
    fmbR_var[:] = fmelt_rate * args.DENSITY_ICE
else:
    if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
        bmbR_var[:] = bmelt_rate + fmelt_rate
    else:
        bmbR_var[:] = bmelt_rate
    fmbR_var[:] = fmelt_rate


if args.OUT_CALC_FRONTAL_MELT:
    if args.OUT_WRITE_MASS_FLUX_KG_BASED:
        fmb3R_var[:] = fmelt3_rate * args.DENSITY_ICE
    else:
        fmb3R_var[:] = fmelt3_rate

#
# grid area indendent quantities
#
if args.OUT_WRITE_2DFIELDS:
    logging.debug("      Extra 2d fields")
    # basal conditions
    if args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT:
        bmb_var[:] = bmelt + fmelt
    else:
        bmb_var[:] = bmelt
    sflux_var[:] = SFlux
    tflux_var[:] = TFlux
    sbound_var[:] = SaltBound
    tbound_var[:] = TempBound

    # frontal conditions
    fmb_var[:] = fmelt
else:
    logging.debug("      Skip extra 2d fields")


if args.OUT_WRITE_3DFIELDS:
    logging.debug("      Extra 3d fields")

    if np.logical_and(
        args.OUT_CALC_FRONTAL_MELT, args.OUT_CALC_MERGE_BASAL_FRONTAL_MELT
    ):
        if args.OUT_WRITE_MASS_FLUX_KG_BASED:
            bmb3R_var[:] = (bmelt3_rate + fmelt3_rate) * args.DENSITY_ICE
        else:
            bmb3R_var[:] = bmelt3_rate + fmelt3_rate
        bmb3_var[:] = bmelt3 + fmelt3
    else:
        if args.OUT_WRITE_MASS_FLUX_KG_BASED:
            bmb3R_var[:] = bmelt3_rate * args.DENSITY_ICE
        else:
            bmb3R_var[:] = bmelt3_rate
        bmb3_var[:] = bmelt3

    if args.OUT_CALC_FRONTAL_MELT:
        fmb3_var[:] = fmelt3
    sflux3_var[:] = SFlux3
    tflux3_var[:] = TFlux3
    sbound3_var[:] = SaltBound3
    tbound3_var[:] = TempBound3
else:
    logging.debug("      Skip extra 3d fields")


if args.OUT_WRITE_TEMP_EXCESS:
    logging.debug("      Melting temperature excess 3d fields")
    tabove3_var[:] = TempIceAboveMelt
else:
    logging.debug("      Skip melting temperature excess 3d fields")

#
# Close the files
#
if args.LOGGING_WRITE_TIME:
    print(
        "Close all open files and flush data buffer(s)    ("
        + time.strftime("%H:%M:%S [%Z]")
        + ")"
    )
else:
    print("Close all open files and flush data buffer(s)")
logging.info("Close all open files and flush data buffer(s)")
fileoce.close()
fileice.close()
filebmb.close()
logging.debug("  All files closed")
# -----------
#
# Some information
#
print("")

if args.LOGGING_WRITE_TIME:
    print("Started at " + time_date_start)
    print("Ending  at " + time.ctime(time.time()))
print("")
print("                  E N D ")
print("-------------------------------------------")
logging.info("-- END --------------------------------------------------------")
# -- last line
