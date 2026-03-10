#!/usr/bin/env  python3

import argparse, os, sys
import netCDF4   as nc
import numpy as np

import pyfesom2 as pf
from pyfesom2 import load_mesh, get_data

def read_data(finname, var):
    with nc.Dataset(finname,'r') as fin:
        data = fin.variables[var]
        # print(data.dimensions)
        # print("nz1" in data.dimensions)
        # print(len(data.dimensions) == 3)
        # print(data.dimensions != ('time', 'nod2', 'nz1'))
        ndim = len(data.dimensions)
        # check
        if ndim == 3 :
            print("Read var >>",var,"<< with the dims:")
            if data.dimensions == ('time', 'nod2', 'nz1'):
                print(data.dimensions)
                levdim = 2
            elif data.dimensions == ('time', 'nz1', 'nod2'):
                print(data.dimensions)
                levdim = 1
            else:
                raise ValueError('dimension is not (time nod2 nz1) or (time nz1 nod2)')

        elif ndim == 2:
            print("Read var:",var," with the dims:")
            print(data.dimensions)
            if data.dimensions != ('time', 'nod2'):
                raise ValueError('dimension is not (time nod2)')

            levdim = -1

        data1 = fin.variables[var][:]
    return data1, ndim, levdim


def function_FESOM2ice_regular_grid(
            version_FESOM=2,
            meshpath = 'path', abg=[50, 15, -90],
            fin='./',
            fout='FESOM_for_ice.nc',
            depthmax=500., depthmin=300.,):

    if (version_FESOM >= 2) and (version_FESOM < 3) :
        print("version:", version_FESOM)
    else:
        raise ValueError('!!!!! only support for Fesom 2x now, stop !!!!!')

    #######################################
    # predefined var names and time info
    varnames = ['temp', 'salt' ]
    varnamesout = ['temp', 'salt' ]  # fixed names for output file , do not edit
    units = ['degC', 'psu']
    unitsout = ['degC', 'g/kg']   # fixed names for output file , do not edit
    missingvalue = [0,33.]
    nvar = len(varnames)
    #

    finname = fin
    foutname = fout
    depth_range = [depthmin, depthmax]
    nlon = 1440
    nlat = 720
    #######################################

    mesh = pf.load_mesh(meshpath,abg=abg, usepickle=False)

    # get the depth information
    if depth_range is None:
        depths = mesh.zlev[:-1]
    else:
        idx = (np.abs(mesh.zlev) >= depth_range[0]) & (np.abs(mesh.zlev) <= depth_range[1])
        depths = mesh.zlev[idx]

    nlev = len(depths)

    # the new regular grids:
    lon = np.linspace(-180, 180, nlon)
    lat = np.linspace(-90, 90, nlat)
    lons, lats = np.meshgrid(lon,lat)


    # read the time dim:
    fin = nc.Dataset(finname,'r')
    timein = fin.variables['time']
    ntime = len(timein)


    # write data
    with nc.Dataset(foutname, 'w') as fout:
        fout.createDimension('lat', nlat)
        latout = fout.createVariable('lat',np.float32, ('lat',))
        latout.units = 'degree_north'
        latout.standard_name = 'latitude'
        latout[:] = lat

        fout.createDimension('lon', nlon)
        lonout = fout.createVariable('lon',np.float32,('lon',))
        lonout.units = 'degree_east'
        lonout.standard_name = 'longitude'
        lonout[:] = lon

        fout.createDimension('time', None)
        timeout = fout.createVariable('time',np.float32,('time',))
        # timeout.setncatts(timein.__dict__)
        timeout.standard_name = "time"
        timeout.units = "years since 01-01-01 00:00:00"
        timeout.calendar = "365_day"
        timeout.axis = "T"


        fout.createDimension('level', None)
        levout = fout.createVariable('level',np.float32,('level',))
        levout.units = 'm'


        fout.description =  'interpolated fesom2.0 data'

        for i in range(nvar):
            thevar = varnames[i]
            print(">>>>>>>>>> processing >>> ",thevar)
            # read data
            data, ndim, levdim = read_data(finname, thevar)
            print("data.shape, ndim, levdim:")
            print(data.shape, ndim, levdim)

            varout = fout.createVariable(thevar,np.float32,('time','level','lat','lon'))
            varout.units = unitsout[i]

            for it in range(0,ntime):
                print("> time step ", it)
                if ndim == 3:
                    levout[:] = depths
                    for iz in range(0,nlev):
                        ilev = pf.ind_for_depth(depths[iz], mesh)
                        print("> level:", depths[iz])
                        if levdim == 2:
                            level_data = data[it, :, ilev]
                        elif levdim == 1:
                            level_data = data[it, ilev, :]
                        else:
                            raise ValueError('Something wrong with data!')

                        level_data[level_data==0] = np.nan
                        #idist_fast = pf.fesom2regular(level_data, mesh, lons, lats, how='idist', k=20)
                        idist_fast = pf.fesom2regular(level_data, mesh, lons, lats )
                        new = np.where(np.isnan(idist_fast), missingvalue[i], idist_fast)
                        varout[it,iz,:,:] = new

                elif ndim == 2:
                    levout[:] = 0. # meter
                    print("> surface")
                    level_data = data[it, :]
                    idist_fast = pf.fesom2regular(level_data, mesh, lons, lats )
                    new = np.where(np.isnan(idist_fast), missingvalue[i], idist_fast)
                    varout[it,0,:,:] = new

    # close input data
    fin.close()
    return

#### defination:
parser = argparse.ArgumentParser(description='Input options')
parser.add_argument("-meshpath", type=str, help='FESOM mesh path')
parser.add_argument("-datain", type=str, help='FESOM data path')
parser.add_argument("-dataout", type=str, help='FESOM output data path')



arguments = parser.parse_args()
print(arguments)

meshpath = arguments.meshpath
datain = arguments.datain
dataout = arguments.dataout


depthmax = os.getenv('OCEAN_depthmax', 300.)
print('... OCEAN depthmax:', depthmax )

depthmin = os.getenv('OCEAN_depthmin', 150.)
print('... OCEAN depthmin:', depthmin )

a = os.getenv('OCEAN_meshalpha', 0.)
print('... OCEAN meshalpha:', a )

b = os.getenv('OCEAN_meshbeta', 0.)
print('... OCEAN meshbeta:', b)

g = os.getenv('OCEAN_meshgamma', 0.)
print('... OCEAN meshgamma:', g )

version = 2.


function_FESOM2ice_regular_grid(
            version_FESOM=version,
            meshpath = meshpath, abg=[a, b, g],
            fin=datain,
            fout=dataout,
            depthmax=depthmax, depthmin=depthmin,)
