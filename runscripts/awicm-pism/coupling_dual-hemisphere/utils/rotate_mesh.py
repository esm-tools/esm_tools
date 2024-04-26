import random
import numpy as np
import xarray as xr
import pandas as pd
import sys
import math
import os
from tqdm import tqdm
from datetime import datetime
from numpy.linalg import inv

mesh_path = sys.argv[1]
opath = sys.argv[2]
nod2d_file = os.path.join(mesh_path, "nod2d.out")

def _generate_rotation_matrix():
    alphaEuler=50
    betaEuler=15
    gammaEuler=-90
    
    al = np.radians(alphaEuler)
    be = np.radians(betaEuler)
    ga = np.radians(gammaEuler)
   
    rotate_matrix = np.zeros((3,3))
    rotate_matrix[0,0]=np.cos(ga)*np.cos(al)-np.sin(ga)*np.cos(be)*np.sin(al)
    rotate_matrix[0,1]=np.cos(ga)*np.sin(al)+np.sin(ga)*np.cos(be)*np.cos(al)
    rotate_matrix[0,2]=np.sin(ga)*np.sin(be)
    rotate_matrix[1,0]=-np.sin(ga)*np.cos(al)-np.cos(ga)*np.cos(be)*np.sin(al)
    rotate_matrix[1,1]=-np.sin(ga)*np.sin(al)+np.cos(ga)*np.cos(be)*np.cos(al)
    rotate_matrix[1,2]=np.cos(ga)*np.sin(be)
    rotate_matrix[2,0]=np.sin(be)*np.sin(al) 
    rotate_matrix[2,1]=-np.sin(be)*np.cos(al)  
    rotate_matrix[2,2]=np.cos(be)
    
    return rotate_matrix

def _r2g(lon_r, lat_r):
    A = _generate_rotation_matrix()
    A_ = inv(A)

    lon_ = np.radians(lon_r)
    lat_ = np.radians(lat_r)

    v_ = np.zeros((3,1))
    v_[0]=np.cos(lat_)*np.cos(lon_)
    v_[1]=np.cos(lat_)*np.sin(lon_)
    v_[2]=np.sin(lat_) 
    vr = np.dot(A_, v_)

    lon_g = np.degrees(math.atan2(vr[1], vr[0]))
    lat_g = np.degrees(math.asin(vr[2]))
    return  lon_g, lat_g

def _back_rotate():
    r2g_v = np.vectorize(_r2g, excluded="A")

    with open(nod2d_file, 'r') as csvfile:
        nodes = pd.read_csv(csvfile, header=None, sep=r'\s* \s*', skiprows=1, engine='python')
        #nodes.drop(nodes.index[0], inplace=True)
        nodes.columns = ['index', 'lon', 'lat', 'mask']
        [lon_tmp, lat_tmp] = r2g_v(np.array(nodes['lon'].values[:], dtype=float).transpose(), 
                                   np.array(nodes['lat'].values[:], dtype=float).transpose())
        nodes['lon'] = lon_tmp
        nodes['lat'] = lat_tmp
        nodes.to_csv(os.path.join(opath, "nod2d.out"), sep=' ', header=[str(len(nodes)), "", "", ""], index=False)

_back_rotate()
