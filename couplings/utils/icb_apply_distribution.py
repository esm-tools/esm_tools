import random
import numpy as np
import numexpr as ne
import xarray as xr
import pandas as pd
import sys
import os
from tqdm import tqdm
from datetime import datetime

RES_pism = 5
rho_ice = 910
height_icb = 250

N_min = 0
N_max = 10000

#Nlength = np.array([5000, 500, 100, 50, 20, 10, 5, 2, 1]) #icb_scaled20
Nlength = np.array([5000, 500, 100, 50, 20, 10, 1, 1, 1]) #icb_scaled21
#Nlength = np.array([1, 1, 1, 1, 1, 1, 1, 1, 1])
#sdepth = np.array([40, 67, 133, 175, 250, 250, 250, 250, 250]) * 7/8
sdepth = np.array([height_icb]*9)
print("sdepth = ", sdepth)
slength_bins = np.array([0, 100, 200, 350, 500, 700, 900, 1200, 1600, 2200])
svol_bins = np.multiply(slength_bins, slength_bins) * height_icb
print("svol_bins = ", svol_bins)

mu_length = 250
trunc_length=0

vol_trunc=10e6 #trunc_length**3*16/21
pism_trunc_fkt=-vol_trunc/(RES_pism*RES_pism*10e6/rho_ice)

xs = []
ys = []
length = []

random.seed()

ifile = sys.argv[1]
mesh_path = sys.argv[2]
icb_path = sys.argv[3]

nod2d_file = os.path.join(mesh_path, "nod2d.out")
elem2d_file = os.path.join(mesh_path, "elem2d.out")

#read PISM file
def read_pism_file(ifile):
    fl = xr.open_dataset(ifile).squeeze()

    data = fl.tendency_of_ice_amount_due_to_discharge.where(fl.tendency_of_ice_amount_due_to_discharge < pism_trunc_fkt*N_min)
    data.to_netcdf(os.path.join(icb_path, "icb_mask.nc"))
    #data = fl.discharge_flux_cumulative.where(fl.discharge_flux_cumulative < 0, drop=True)
    data = np.array(data).reshape(1, len(data.x)*len(data.y))
    data = data[~np.isnan(data)]

    lon_bnds = fl.lon_bnds.where(fl.tendency_of_ice_amount_due_to_discharge < pism_trunc_fkt*N_min, drop=True)
    #lon_bnds = fl.lon_bnds.where(fl.discharge_flux_cumulative < 0, drop=True)
    lon_bnds = np.array(lon_bnds).reshape(len(lon_bnds.x)*len(lon_bnds.y), 4)
    lon_bnds = lon_bnds[~np.isnan(lon_bnds)]
    lon_bnds = lon_bnds.reshape(int(len(lon_bnds)/4), 4)

    lat_bnds = fl.lat_bnds.where(fl.tendency_of_ice_amount_due_to_discharge < pism_trunc_fkt*N_min, drop=True)
    #lat_bnds = fl.lat_bnds.where(fl.discharge_flux_cumulative < 0, drop=True)
    lat_bnds = np.array(lat_bnds).reshape(len(lat_bnds.x)*len(lat_bnds.y), 4)
    lat_bnds = lat_bnds[~np.isnan(lat_bnds)]
    lat_bnds = lat_bnds.reshape(int(len(lat_bnds)/4), 4)

    lons = []
    lats = []

    for x, y in zip(lon_bnds, lat_bnds):
        lons.append(np.mean(x))
        lats.append(np.mean(y))

    return [data, lons, lats]


class point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def to_dict(self):
        return {'x': self.x, 'y': self.y}


# https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
def sign(p1, p2, p3):
    return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)

def PointInTriangle(pt, v1, v2, v3):
    d1 = sign(pt, v1, v2)
    d2 = sign(pt, v2, v3)
    d3 = sign(pt, v3, v1)

    has_neg = (d1 < 0) or (d2 < 0) or (d3 < 0)
    has_pos = (d1 > 0) or (d2 > 0) or (d3 > 0)

    return not (has_neg and has_pos)




#generate icebergs
def icb_generator(vals, ps1, ps2, ps3, icb_path, rho_ice=910, res_pism=5):
    #mu, sigma = mu_length**3*16/21/10e6, 1.2
    mu, sigma = mu_length**2*height_icb/10e6, 1.2

    points = []
    #depth=length/1.5
    height = [] #height=depth*8/7=length*8/7*2/3=length*16/21
    scaling_tmp = 1

    with tqdm(total=len(vals), file=sys.stdout, desc='create icebergs') as pbar:
        for val, p1, p2, p3 in zip(vals, ps1, ps2, ps3):
            disch_cum = val*res_pism*res_pism*10e6/rho_ice
            #N = max(int(np.absolute(disch_cum)/(10e6)/10), 1)
            N = int(np.absolute(disch_cum)/(10e6))
            if N < N_min:
                N = 0
            elif N > N_max:
                scaling_tmp = N/N_max
                N = N_max

            s = np.random.lognormal(mu, sigma, N)*10e6

            f_scale = sum(s)/np.absolute(disch_cum)
            s_ = s / f_scale / scaling_tmp
            
            length_tmp = ne.evaluate('(s_/height_icb)**(1/2)')

            #ll = np.digitize(length_tmp, slength_bins)
            ll = np.digitize(s_, svol_bins)
            for i, (s, d) in enumerate(zip(Nlength, sdepth)):
                sbin = np.where(ll==i+1)[0]
                if list(sbin):
                    if len(sbin) <= s:
                        chunk = [sbin]
                    else:
                        chunk = [sbin[k:k + s] for k in range(0, len(sbin), s)]
                    l = np.array([np.mean(s_[c]) for c in chunk])
                    s = np.array([len(c) for c in chunk])
                    if not 'length' in locals():
                        length = ne.evaluate('(l/height_icb)**(1/2)')
                        scaling = np.array([s])
                        depth = np.array([d]*len(l))
                    else:
                        length = np.append(length, ne.evaluate('(l/height_icb)**(1/2)'))
                        scaling = np.append(scaling, np.array([s]))
                        depth = np.append(depth, np.array([d]*len(l)))

                    for i in range(len(l)):
                        r1 = random.random()
                        r2 = random.random()
                        
                        lower_bound = 0.1
                        upper_bound = 0.9

                        r1 = r1 * (upper_bound - lower_bound) + lower_bound
                        r2 = r2 * (upper_bound - lower_bound) + lower_bound
                        #https://math.stackexchange.com/questions/18686/uniform-random-point-in-triangle
                        tmp_x = (1-np.sqrt(r1))*p1.x + (np.sqrt(r1)*(1-r2))*p2.x + (r2*np.sqrt(r1))*p3.x
                        tmp_y = (1-np.sqrt(r1))*p1.y + (np.sqrt(r1)*(1-r2))*p2.y + (r2*np.sqrt(r1))*p3.y
                        points.append(point(tmp_x, tmp_y))
                        #length_tmp = (np.absolute(s_[i])/height_icb)**(1/2)
                        #length.append(length_tmp)
                        ##length**2*length*16/21=s_--> length=(s_*21/16)**(1/3)
                        #length.append(np.absolute(s_[i]*21/16)**(1/3))
                        #height.append(height_icb)
                        #height.append(length[-1]*16/21)
                        #scaling.append(scaling_tmp)
            pbar.update(1)

    points_ = pd.DataFrame.from_records([p.to_dict() for p in points])
    np.savetxt(os.path.join(icb_path, "LON.dat"), points_.x.values)
    np.savetxt(os.path.join(icb_path, "LAT.dat"), points_.y.values)
    np.savetxt(os.path.join(icb_path, "LENGTH.dat"), length)
    np.savetxt(os.path.join(icb_path, "HEIGHT.dat"), depth)
    np.savetxt(os.path.join(icb_path, "SCALING.dat"), scaling)
    return [points, length]

def PointTriangle_distance(lon0, lat0, lon1, lat1, lon2, lat2, lon3, lat3):
    d1 = ne.evaluate('(lon1 - lon0)**2 + (lat1 - lat0)**2')
    d2 = ne.evaluate('(lon2 - lon0)**2 + (lat2 - lat0)**2')
    d3 = ne.evaluate('(lon3 - lon0)**2 + (lat3 - lat0)**2')
    
    dis = d1+d2+d3
    ind = np.where(dis == np.amin(dis))
   
    p1 = point(lon1[ind], lat1[ind])
    p2 = point(lon2[ind], lat2[ind])
    p3 = point(lon3[ind], lat3[ind])

    return [p1, p2, p3], ind

def find_FESOM_elem(nod2d_file, elem2d_file, lons, lats):
    points = []
    nod2d = pd.read_csv(nod2d_file, header=0, names=["lon", "lat", "coastal"], sep='\s+', index_col=0)
    elem2d = pd.read_csv(elem2d_file, header=0, names=["nod1", "nod2", "nod3"], sep='\s+')

    lon1=[] 
    lat1=[]
    lon2=[]
    lat2=[]
    lon3=[]
    lat3=[]

    lon1 = nod2d.lon[elem2d.nod1]
    lat1 = nod2d.lat[elem2d.nod1]
    lon2 = nod2d.lon[elem2d.nod2]
    lat2 = nod2d.lat[elem2d.nod2]
    lon3 = nod2d.lon[elem2d.nod3]
    lat3 = nod2d.lat[elem2d.nod3]
    
    with tqdm(total=len(lons), file=sys.stdout, desc='find FESOM elements') as pbar:
        for lon, lat in zip(lons, lats):
            tmp, ind = PointTriangle_distance(lon, lat, np.array(lon1), np.array(lat1), np.array(lon2), np.array(lat2), 
                                                    np.array(lon3), np.array(lat3))
            
            points.append(tmp[:3])
            #print("point in triangle?: ", PointInTriangle(point(lon, lat), tmp[0], tmp[1], tmp[2]))
            #print("point: [" + str(lon) + ", " + str(lat) + "]")
            #print("triangle: [", str(tmp[0].x) + ", " + str(tmp[0].y) + "], " + "[", str(tmp[1].x) + ", " + str(tmp[1].y) + "], " + "[", str(tmp[2].x) + ", " + str(tmp[2].y) + "]")
            pbar.update(1)
    return points












data, lons, lats = read_pism_file(ifile)
points = find_FESOM_elem(nod2d_file, elem2d_file, lons, lats)

ps1, ps2, ps3 = np.transpose(points)
icb_generator(data, ps1, ps2, ps3, icb_path)

#pos = find_FESOM_elem(nod2d_file, elem2d_file, xs, ys)
#
#np.savetxt("pos.csv", np.array(pos))






































def _generate_rotation_matrix(self):
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
    
    self.rotate_matrix = rotate_matrix

def _g2r(self):
    lon1 = np.radians(self.box[0])
    lon2 = np.radians(self.box[1])
    lat1 = np.radians(self.box[2])
    lat2 = np.radians(self.box[3])

    v1_ = np.zeros((3,1))
    v1_[0]=np.cos(lat1)*np.cos(lon1)
    v1_[1]=np.cos(lat1)*np.sin(lon1)
    v1_[2]=np.sin(lat1) 
    vr1 = np.dot(self.rotate_matrix, v1_)

    v2_ = np.zeros((3,1))
    v2_[0]=np.cos(lat2)*np.cos(lon2)
    v2_[1]=np.cos(lat2)*np.sin(lon2)
    v2_[2]=np.sin(lat2) 
    vr2 = np.dot(self.rotate_matrix, v2_)
    
    self.box[0] = np.degrees(math.atan2(vr1[1], vr1[0]))
    self.box[2] = np.degrees(math.asin(vr1[2]))
    self.box[1] = np.degrees(math.atan2(vr2[1], vr2[0]))
    self.box[3] = np.degrees(math.asin(vr2[2]))

def _r2g(self, lon_r, lat_r):

    A = inv(self.rotate_matrix)

    lon_ = np.radians(lon_r)
    lat_ = np.radians(lat_r)

    v_ = np.zeros((3,1))
    v_[0]=np.cos(lat_)*np.cos(lon_)
    v_[1]=np.cos(lat_)*np.sin(lon_)
    v_[2]=np.sin(lat_) 
    vr = np.dot(A, v_)

    lon_g = np.degrees(math.atan2(vr[1], vr[0]))
    lat_g = np.degrees(math.asin(vr[2]))
    return  lon_g, lat_g

def _back_rotate(self):
    r2g_v = np.vectorize(self._r2g)

    with open(self.nod_file, 'r') as csvfile:
        nodes = pd.read_csv(csvfile, header=None, sep=r'\s* \s*', skiprows=1, engine='python')
        nodes.drop(nodes.index[0], inplace=True)
        nodes.columns = ['index', 'lon', 'lat', 'mask']
        [lon_tmp, lat_tmp] = r2g_v(np.array(nodes['lon'].values[:], dtype=float).transpose(), 
                                   np.array(nodes['lat'].values[:], dtype=float).transpose())
        nodes['lon'] = lon_tmp
        nodes['lat'] = lat_tmp
        nodes.to_csv('nodes_back_rotated.csv', sep=' ', header=True, index=False)
        self.nod_file = 'nodes_back_rotated.csv'
