#!/usr/bin/env python
"""
This program basal and frontal melt from PISM to MPIOM

Usage: transform_ice_to_ocean.py [options] GRID_FILE ICE_FILE OCEAN_FILE

Options:
-i, --do-fortran-boundIJ
-l, --write-latlon
-t, --write-time

"""
import numpy as np
from scipy.io import netcdf
import docopt

#
# TODO: Remove heat stuff, it is WRONG
#

def load_data(file_grid, file_ice, file_ocean):
    with netcdf.netcdf_file(file_grid, "r") as filegrid, netcdf.netcdf_file(file_ice, "r") as fileice:
        i_index_of_closest_ocean_on_ice_grid = filegrid.variables["J_1to2"].data.copy()
        j_index_of_closest_ocean_on_ice_grid = filegrid.variables["I_1to2"].data.copy()
        isocean = filegrid.variables["isocean"].data.copy()
        time_ice = fileice.variables["time"].data.copy()
        time_ice0 = np.size(time_ice) - 1
        mass_flux_due_to_basal_melting_ice = fileice.variables["bmelt"].data.squeeze().copy()
        total_mass_flux_due_to_all_melting_ice = fileice.variables["delta_thk"].data.squeeze().copy() # NOTE: Thickness HAS BEEN TRANSFORMED!
        # The thickness variable is assumed to be pre-transformed to be the difference between the first and last timestep divided by number of seconds!

        #
        # FIXME: This next line doesn't make any sense...why would we turn dh/dt into a 1/0 mask???
        # total_mass_flux_due_to_all_melting_ice = np.where(total_mass_flux_due_to_all_melting_ice>0., 1., 0.)
        #

        return i_index_of_closest_ocean_on_ice_grid, j_index_of_closest_ocean_on_ice_grid, isocean, time_ice, time_ice0, mass_flux_due_to_basal_melting_ice, total_mass_flux_due_to_all_melting_ice




def main():
    pass

if __name__ == '__main__':
    args = docopt.docopt(__doc__)
    print(args)
    file_grid = args["GRID_FILE"]
    file_ice = args["ICE_FILE"]
    file_ocean = args["OCEAN_FILE"]
    # Program Flow Switches
    do_fortran_boundIJ = args["--do-fortran-boundIJ"] or False
    write_latlon = args["--write-latlon"] or False
    do_write_time = args["--write-time"] or False

    # Physical Parameters
    latent_heat = 333700.       # J/kg, PG: Of liquid-solid water phase change
    rho_ice = 910.               # kg/m3
    rho_oce = 1026.0
    salinity_seaice = 5.

    # Load the data
    i_index_of_closest_ocean_on_ice_grid, j_index_of_closest_ocean_on_ice_grid, isocean, time_ice, time_ice0, mass_flux_due_to_basal_melting_ice, total_mass_flux_due_to_all_melting_ice = load_data(file_grid, file_ice, file_ocean)
    ilen1, jlen1 = np.shape(mass_flux_due_to_basal_melting_ice)
    print("mass_flux_due_to_basal_melting_ice has:", ilen1, jlen1)
    ilen2, jlen2 = np.shape(isocean)
    print("isocean has:", ilen2, jlen2)
    tlen = np.size(time_ice)

    if tlen > 1:
        sizeIce2Oce = (tlen, ilen2, jlen2)
    else:
        sizeIce2Oce = (ilen2, jlen2)

    if do_fortran_boundIJ:
        i_index_of_closest_ocean_on_ice_grid -= 1
        j_index_of_closest_ocean_on_ice_grid -= 1
    mass_flux_due_to_basal_melting_ice_on_ocean_grid = np.zeros(sizeIce2Oce)
    total_mass_flux_due_to_all_melting_ice_on_ocean_grid = np.zeros(sizeIce2Oce)

    bhb_oce = np.zeros(sizeIce2Oce)
    fhb_oce = np.zeros(sizeIce2Oce)

    if tlen > 1:
        for it in range(tlen):
            for i in range(ilen1):
                for j in range(jlen1):
                    i_index_ocean = i_index_of_closest_ocean_on_ice_grid[i, j]
                    j_index_ocean = j_index_of_closest_ocean_on_ice_grid[i, j]
                    mass_flux_due_to_basal_melting_ice_on_ocean_grid[it, i_index_ocean, j_index_ocean] += mass_flux_due_to_basal_melting_ice[it, i, j]
                    total_mass_flux_due_to_all_melting_ice_on_ocean_grid[it, i_index_ocean, j_index_ocean] += total_mass_flux_due_to_all_melting_ice[it, i, j]
    else:
        for i in range(ilen1):
            for j in range(jlen1):
                i_index_ocean = int(i_index_of_closest_ocean_on_ice_grid[i, j])
                j_index_ocean = int(j_index_of_closest_ocean_on_ice_grid[i, j])
                mass_flux_due_to_basal_melting_ice_on_ocean_grid[i_index_ocean, j_index_ocean] += mass_flux_due_to_basal_melting_ice[i, j]
                total_mass_flux_due_to_all_melting_ice_on_ocean_grid[i_index_ocean, j_index_ocean] += total_mass_flux_due_to_all_melting_ice[i, j]
    # Convert latent heat to a heat flux by assuming everything must melt completely:
    conversion_factor = latent_heat * rho_ice
    bhb_oce = mass_flux_due_to_basal_melting_ice_on_ocean_grid * conversion_factor
    thb_oce = total_mass_flux_due_to_all_melting_ice_on_ocean_grid * conversion_factor

    ### Save the data
    print("Saving output...")
    fout = netcdf.netcdf_file(file_ocean, mode="w")

    #
    # Global NetCDF attributes
    #
    print('    Global attributes')
    fout.authors = 'Paul Gierz'
    fout.instiution = "Alfred Wegener Institute for Polar and Marine Research"
    fout.address = "Bussesstrasse 24, 27570 Bremerhaven, Germany"
    fout.web = "http://www.awi.de"
    import os   as os
    fout.working_directory = os.getcwd()
    fout.username = os.environ["USER"]
    #fout.hostname = os.environ["HOST"]
    #fout.hosttype = os.environ["HOSTTYPE"]
    fout.uname = os.popen('uname -a').read()
    #fout.gridfile1=fileice
    #fout.gridfile2=fileoce


    #fout.gridfile1=fileice
    #fout.gridfile2=fileoce

    #
    # Definition of the dimensions
    #
    print('    Create dimension')
    if ( tlen >= 1 ):
        fout.createDimension('time',tlen)
    else :
        fout.createDimension('time',tlen)
    fout.createDimension('x1',ilen1)
    fout.createDimension('y1',jlen1)
    fout.createDimension('x2',ilen2)
    fout.createDimension('y2',jlen2)
    #fout.createDimension('z',klen)
    fout.createDimension('one',(1))


    #
    # Define output variables and specify its attributes
    #
    print('    Define variables')

    ## x-, y-arrays
    x1_var       = fout.createVariable('x1', 'd', ('x1', ))
    x1_var.standard_name = 'projection_x_coordinate'
    x1_var.long_name     = 'X-coordinate in Cartesian system' ;
    #x1_var.unit          = 'm'
    y1_var       = fout.createVariable('y1', 'd', ('y1', ))
    y1_var.standard_name = 'projection_y_coordinate'
    y1_var.long_name     = 'Y-coordinate in Cartesian system' ;
    #y1_var.unit          = 'm'

    x2_var       = fout.createVariable('x2', 'd', ('x2', ))
    x2_var.standard_name = 'projection_x_coordinate'
    x2_var.long_name     = 'X-coordinate in Cartesian system' ;
    #x2_var.unit          = 'm'
    y2_var       = fout.createVariable('y2', 'd', ('y2', ))
    y2_var.standard_name = 'projection_y_coordinate'
    y2_var.long_name     = 'Y-coordinate in Cartesian system' ;
    #y2_var.unit          = 'm'


    if ( write_latlon ):
        lat1_var       = fout.createVariable('lat1', 'd', ('x1', 'y1', ))
        lat1_var.standard_name = 'latitude'
        lat1_var.long_name     = 'latitude,  grid 1'
        lat1_var.unit          = 'degree'
        lon1_var       = fout.createVariable('lon1', 'd', ('x1', 'y1', ))
        lon1_var.standard_name = 'longitude'
        lon1_var.long_name     = 'longitude, grid 1' ;
        lon1_var.unit          = 'degree'


        lat2_var       = fout.createVariable('lat2', 'd', ('x2', 'y2', ))
        lat2_var.standard_name = 'latitude'
        lat2_var.long_name     = 'latitude,  grid 2'
        lat2_var.unit          = 'degree'
        lon2_var       = fout.createVariable('lon2', 'd', ('x2', 'y2', ))
        lon2_var.standard_name = 'longitude'
        lon2_var.long_name     = 'longitude, grid 2' ;
        lon2_var.unit          = 'degree'


    isocean_var       = fout.createVariable('isocean', 'd', ('x2', 'y2', ))
    isocean_var.long_name   = 'ocean mask (1:ocean, 0:else)' ;
    isocean_var.coordinates ='lat2 lon2'

    #
    if ( tlen >= 1 ):
        bmr_var       = fout.createVariable('bmr', 'd', ('time', 'x2', 'y2', ))
    else :
        bmr_var       = fout.createVariable('bmr', 'd', ('x2', 'y2', ))
    bmr_var.long_name   ='basal_melting_rate'
    bmr_var.units       ='m3/s'
    bmr_var.coordinates ='lat2 lon2'
    #bmr_var.reference_density=dens_ice

    if ( tlen >= 1 ):
        tmr_var       = fout.createVariable('tmr', 'd', ('time', 'x2', 'y2', ))
    else :
        tmr_var       = fout.createVariable('tmr', 'd', ('x2', 'y2', ))
    tmr_var.long_name   ='total_melting_rate (basal melting + frontal melting)'
    tmr_var.units       ='m3/s'
    tmr_var.coordinates ='lat2 lon2'

    #
    #
    #
    print('    Populate variables')

    # x-, y-arrarys
    x1_var[:]   = range(ilen1); y1_var[:]   = range(jlen1)
    x2_var[:]   = range(ilen2); y2_var[:]   = range(jlen2)

    if ( write_latlon ):
        lat1_var[:] = lat1
        lon1_var[:] = lon1

        lat2_var[:] = lat2
        lon2_var[:] = lon2

    isocean_var[:]=isocean

    bmr_var[:] = mass_flux_due_to_basal_melting_ice_on_ocean_grid
    tmr_var[:] = total_mass_flux_due_to_all_melting_ice_on_ocean_grid

    ################
    #
    # Close the files
    #
    print("Close all open files")


    fout.close()

    # -----------
    #
    # Some information
    #
    print("")
    if ( do_write_time ) :
        print(' Done at '+time.ctime(time.time()))
    else :
        print(' Done')
