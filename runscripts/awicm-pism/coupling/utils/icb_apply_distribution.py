import matplotlib.pyplot as plt
import seaborn as sns
import random
import math
import numpy as np
import numexpr as ne
import xarray as xr
import pandas as pd
import sys
import os
from tqdm import tqdm
from datetime import datetime
import powerlaw

# resolution of PISM grid [km]
RES_PISM = 16

# density of ice [kg m-3]
RHO_ICE = 920

# mean iceberg height [km]
HEIGHT_ICB = 0.25

N_min = 1
N_max = 100000

bins = [0.01, 0.1, 1, 10, 100, 1000]
#WEIGHTS_A = [0.009, 0.025, 0.074, 0.222, 0.671]
WEIGHTS_A = [0.0005, 0.0005, 0.008, 0.025, 0.074, 0.893]
#WEIGHTS_N = [0.75, 0.175, 0.05, 0.02, 0.005]
WEIGHTS_N = [0.4, 0.2, 0.15, 0.175, 0.05, 0.025]

# mean iceberg surface area for smalles class [km2]
SMEAN = [0.001, 0.01, 0.1, 1, 10, 100]
#SMEAN = 10

# minimum iceberg surface area [km2]
SMIN = 0

# maximum iceberg surface area [km2]
SMAX = 2000

#Nlength = np.array([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) #icb_scaled21
#Nlength = np.array([2000, 200, 50, 20, 10, 5, 2, 1, 1, 1, 1, 1])
#vlength = np.array([0, 60, 100, 200, 350, 500, 700, 900, 1200, 1600, 2200, 10000]) #gladstone et al., 2001
#vthick = np.array([0, 40, 67, 133, 175, 250, 250, 250, 250, 250, 250, 250])

#Nlength = np.array([1000, 100, 10, 1, 1])
Nlength = np.array([1, 1, 1, 1, 1, 1])
vlength = np.array(bins)
vthick = np.array([0.25, 0.25, 0.25, 0.25, 0.25, 0.25])
sdepth = vthick #* 7/8
varea = np.array(bins) #np.multiply(vlength, vlength)
vvol = np.multiply(varea, vthick)

slength_bins = vlength
SVOL_BINS = vvol# / 1e9

mu_length = 250
trunc_length=0

vol_trunc=1e9 #trunc_length**3*16/21
area_trunc=1e6
pism_trunc_fkt=-area_trunc * HEIGHT_ICB * 1e3 * RHO_ICE / (RES_PISM**2 * 1e6)

xs = []
ys = []
length = []

random.seed()

ifile = sys.argv[1]
mesh_path = sys.argv[2]
icb_path = sys.argv[3]
basin_path = sys.argv[4]

nod2d_file = os.path.join(mesh_path, "nod2d.out")
elem2d_file = os.path.join(mesh_path, "elem2d.out")

def read_basins_file(ifile):
    fl = xr.open_dataset(ifile).squeeze()
    return fl.basin


def get_nearest_lon_lat(ds, lon, lat):
    #https://stackoverflow.com/questions/58758480/xarray-select-nearest-lat-lon-with-multi-dimension-coordinates
    abslat = np.abs(ds.lat-lat)
    abslon = np.abs(ds.lon-lon)
    c = np.maximum(abslon, abslat)

    ([xloc], [yloc]) = np.where(c == np.min(c))

    # Now I can use that index location to get the values at the x/y diminsion
    point_ds = ds.isel(x=xloc, y=yloc)
    return point_ds
    

#read PISM file
def read_pism_file(ifile):
    fl = xr.open_dataset(ifile).squeeze()

    data = fl.tendency_of_ice_amount_due_to_discharge.where(fl.tendency_of_ice_amount_due_to_discharge < pism_trunc_fkt*N_min, drop=True)
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

def create_icebergs_within_basin(df):
######################################
# input:    data frame for one basin: discharge and FESOM cell corners (p1, p2, p3)
# output:   iceberg volume array
######################################
    # mu and sigma for lognormal distribution after Tournadre et al. (2011)
    mu, sigma = 12.3, 1.55**0.5

    # alpha for powerlaw after Tournadre et al. (2015)
    a, xmin = 1.52, 0.1

    # get values within basin
    vals = df.disch.values

    # remove nan's of values
    vals[np.isnan(vals)] = 0

    # get total discharge within basin in [km3 year-1]
    disch_tot = abs(sum(vals)) * RES_PISM**2 / RHO_ICE / 1e3

    # get total iceberg area within basin in [km2 year-1]
    # assuming constant iceberg height
    area_tot = disch_tot / HEIGHT_ICB

    # create iceberg areas according to Tournadre et al. (2015)
    # divide icebergs into classes of different area sizes (0.1-1, 1-10, 10-100, ... [km2])
    # and draw from powerlaw distribution with alpha=1.52 except for the icebergs from
    # smallest class. Get total number of icebergs with share of smallest class (WEIGHTS_N)
    # and mean size within smalles class (SMEAN_1). Get number of icebergs of each other class
    # with corresponding share. 
   
    if area_tot > SMAX:
        vrs = np.random.uniform(bins[-1], SMAX)
        area_tot_new = area_tot - vrs
    else:
        area_tot_new = area_tot

    for i, (wa, wn) in enumerate(zip(WEIGHTS_A, WEIGHTS_N)):
        if i==0:
            # get amount of icebergs of smalles class
            N = abs(int(area_tot_new * wa / SMEAN[i]))
            # get total amount of icebergs
            N_tot = int(N/wn)
            xmin = bins[i]
            xmax = bins[i+1]
            # draw area sizes from lognormal distribution for smalles iceberg class
            tmp = np.random.lognormal(mu, sigma, N)
            # get scaling factor to sum iceberg areas up to actual share of discharge (wa * area_tot)
            if N == N_tot:
                f = area_tot_new / sum(tmp)
            else:
                f = wa * area_tot_new / sum(tmp)
            vrs = tmp * f
        elif i==len(bins)-1:
            # get amount of icebergs of class
            #N = int(N_tot * wn)
            N = abs(int(area_tot_new * wa / SMEAN[i]))
            if N == 0:
                break
            xmin = bins[0]
            xmax = bins[-1]
            # draw area sizes from power-law distribution
            #tmp = powerlaw.Power_Law(xmin=xmin, xmax=xmax, parameters=[a]).generate_random(N, estimate_discrete=True)
            tmp = np.random.lognormal(mu, sigma, N)
            f = wa * area_tot / sum(tmp)
            vrs = np.concatenate((vrs, tmp*f), axis=0)
        else:
            # get amount of icebergs of class
            #N = int(N_tot * wn)
            N = abs(int(area_tot_new * wa / SMEAN[i]))
            if N == 0:
                break
            xmin = bins[i]
            xmax = bins[i+1]
            # draw area sizes from power-law distribution
            #tmp = powerlaw.Power_Law(xmin=xmin, xmax=xmax, parameters=[a]).generate_random(N, estimate_discrete=True)
            tmp = np.random.lognormal(mu, sigma, N)
            f = wa * area_tot / sum(tmp)
            vrs = np.concatenate((vrs, tmp*f), axis=0)

    # break down all iceberg with surface area greater than SMAX [km2] into smaller icebergs
    while len(vrs[vrs>SMAX] > 0):
        tmp = np.where(vrs>SMAX)
        vrs[tmp] = vrs[tmp]/2
        vrs = np.append(vrs, vrs[tmp])

    # create data frame with iceberg elements: area, volume, bin
    ib_elems = pd.DataFrame({"area": vrs, 
                            "volume": vrs*HEIGHT_ICB,
                            "bin": np.digitize(vrs*HEIGHT_ICB, SVOL_BINS, right=True)})
   
    ib_elems_ = ib_elems #.where(ib_elems.area>=SMIN).dropna()
        
    #sns.histplot(vrs, log_scale=True, stat="probability", bins=25)
    #plt.xscale("log")
    #plt.show()

    print("*** Check for validity:")
    print("***      assumed iceberg height [km]:         ", HEIGHT_ICB)
    print("***      total discharge [km3 year-1]:        ", disch_tot)
    print("***      summed iceberg volume [km3]:         ", sum(ib_elems.volume))
    print("***      summed iceberg volume [km3]:         ", sum(ib_elems_.volume))
    print("***      total iceberg area [km2 year-1]:     ", area_tot)
    print("***      summed area (of generated ib) [km2]: ", sum(ib_elems.area)) 
    print("***      summed area (of generated ib) [km2]: ", sum(ib_elems_.area)) 
    print("***      total number of icebergs:            ", len(ib_elems))
    print("***      total number of icebergs:            ", len(ib_elems_))
    print(ib_elems_)
    return ib_elems_



def scale_icebergs(df):
######################################
# input:    data frame: area, volume, bin
# output:   data frame: length, scaling, depth
######################################
    # loop over all bins
    with tqdm(total=len(Nlength), file=sys.stdout, desc='go through all bins') as pbar:
        for i, (s, d) in enumerate(zip(Nlength, sdepth)):
            
            # get icb elements of particular size class
            ib_bin = df.where(df.bin==i).dropna()
   
            if not ib_bin.empty:
                # split iceberg array of size class into chunks with length s
                chunks = np.array_split(ib_bin, math.ceil(len(ib_bin)/s))
                #chunks = [ib_bin[k:k + s] for k in range(0, len(ib_bin), s)]
                # get mean of each chunk
                chunks_mean_area = np.array([chunk.area.mean(axis=0) for chunk in chunks])
                #print("*** chunks_mean_area = ", chunks_mean_area)

                # check if arrays are initialized
                if not 'length' in locals():
                    # get mean length of icebergs for each chunk
                    length = ne.evaluate('chunks_mean_area**(1/2)')
                    #print("*** length = \n", length)
                    # get scaling factor (length of each chunk)
                    scaling = np.array([len(chunk) for chunk in chunks])
                    # get depth
                    depth = np.array([d] * len(chunks))

                else:
                    # get mean length of icebergs for each chunk
                    length = np.append(length, ne.evaluate('chunks_mean_area**(1/2)'))
                    #print("*** length = \n", length)
                    # get scaling factor (length of each chunk)
                    scaling = np.append(scaling, np.array([len(chunk) for chunk in chunks]))
                    # get depth
                    depth = np.append(depth, np.array([d] * len(chunks)))
                pbar.update(1)
            else:
                print("*** bin is empty")
                pbar.update(1)
    
    # create data frame with scaled iceberg elements: length, scaling, depth
    df_out = pd.DataFrame({"length": length,
                            "scaling": scaling,
                            "depth": depth})
    
    print("*** Check for validity:")
    print("***      BEFORE SCALING:")
    print("***      total iceberg area [km2]:   ", df.sum(axis=0).area)
    print("***      total iceberg volume [km3]: ", df.sum(axis=0).volume)
    print("***      total amount of icebergs:   ", len(df))
    print("***      AFTER SCALING:")
    print("***      total iceberg area [km2]:   ", np.sum(df_out.length * df_out.length * df_out.scaling))
    print("***      total iceberg volume [km2]: ", np.sum(df_out.length * df_out.length * df_out.scaling * df_out.depth))
    print("***      total amount of icebergs:   ", np.sum(df_out.scaling))
    print("***      total am. of sim. icebergs: ", len(df_out))
    return df_out
    


def create_iceberg_coordinates(df1, df2):
    pass 







#generate icebergs
def icb_generator(df, icb_path, rho_ice=920, res_pism=16):
    ###############################
    # bisher verwendet!
    #mu, sigma = mu_length**2*height_icb/(10e6), 1.2
    mu, sigma = 12.3, 1.55**0.5     #Tournadre et al. 2011
    a = 1.52

    ib_elems_scaled = pd.DataFrame()
    ib_elems_loc = pd.DataFrame()

    points = []
    #depth=length/1.5
    height = [] #height=depth*8/7=length*8/7*2/3=length*16/21
    scaling_tmp = 1

    df.sort_values("basin", inplace=True)
    ubasins = df.basin.unique()
    #ubasins = [1]
    with tqdm(total=len(ubasins), file=sys.stdout, desc='go through basins') as pbar:
        for b in ubasins:
            print("*****************************")
            print("*** BASIN = ", b)
            # create icebergs for basin [m3]
            ib_elems = create_icebergs_within_basin(df.where(df.basin==b).dropna())
            ib_elems.sort_values("bin", inplace=True)

            # loop over FESOM cells within this basin
            #with tqdm(total=len(ubasins), file=sys.stdout, desc='go through FESOM cells') as pbar:
            #    for row in ib_elems.groupby(["p1", "p2", "p3"]).size().reset_index().rename(columns={0: "count"}).iterrows():
            #        ib_elems_FESOM_cell = ib_elems.where(ib_elems.p1==row[1]["p1"]).where(ib_elems.p2==row[1]["p2"]).where(ib_elems.p3==row[1]["p3"]).dropna()

            # scale iceberg elements
            if not ib_elems_scaled.empty:
                ib_elems_scaled = pd.concat([ib_elems_scaled, scale_icebergs(ib_elems)])
            else:
                ib_elems_scaled = scale_icebergs(ib_elems)

            ib_elems_grouped = df.where(df.basin==b).groupby(["x1", "y1", "x2", "y2", "x3", "y3"]).size().reset_index().rename(columns={0: "count"})
            
            with tqdm(total=len(ib_elems_grouped), file=sys.stdout, desc='go through FESOM cells') as pbar:
                for row in ib_elems_grouped.iterrows():
                    #ib_elems_FESOM_cell = df.where(df.p1==row[1]["p1"]).where(df.p2==row[1]["p2"]).where(df.p3==row[1]["p3"]).dropna()
                    ib_elems_FESOM_cell = df.loc[(df["x1"]==row[1]["x1"]) & (df["y1"]==row[1]["y1"]) & \
                                                (df["x2"]==row[1]["x2"]) & (df["y2"]==row[1]["y2"]) & \
                                                (df["x3"]==row[1]["x3"]) & (df["y3"]==row[1]["y3"])]
                    
                    tmp = ib_elems_FESOM_cell.disch.sum(axis=0)
                    disch_FESOM_cell_tot = tmp * RES_PISM**2 / RHO_ICE / 1e3

                    disch_FESOM_cell = 0
                    while abs(disch_FESOM_cell) < abs(disch_FESOM_cell_tot):
                        if ib_elems_scaled.empty:
                            break

                        #print("*** len(ib_elems_scaled) = ", len(ib_elems_scaled))
                        ib_elem = ib_elems_scaled.sample()
                        #print("ib_elem = ", ib_elem)
                        
                        disch_FESOM_cell += ib_elem.length.values * ib_elem.length.values * ib_elem.depth.values * ib_elem.scaling.values

                        r1 = random.random()
                        r2 = random.random()
                        
                        lower_bound = 0.25
                        upper_bound = 0.75

                        r1 = r1 * (upper_bound - lower_bound) + lower_bound
                        r2 = r2 * (upper_bound - lower_bound) + lower_bound
                        #https://math.stackexchange.com/questions/18686/uniform-random-point-in-triangle
                        try:
                            lon = (1-np.sqrt(r1))*ib_elems_FESOM_cell.iloc[0].x1 + (np.sqrt(r1)*(1-r2))*ib_elems_FESOM_cell.iloc[0].x2 + (r2*np.sqrt(r1))*ib_elems_FESOM_cell.iloc[0].x3
                            lat = (1-np.sqrt(r1))*ib_elems_FESOM_cell.iloc[0].y1 + (np.sqrt(r1)*(1-r2))*ib_elems_FESOM_cell.iloc[0].y2 + (r2*np.sqrt(r1))*ib_elems_FESOM_cell.iloc[0].y3
                            #points.append(point(tmp_x, tmp_y))
                        except:
                            break
                       
                        if ib_elems_loc.empty:
                            print("*** HIER NUR EINMAL HIN!")
                            ib_elems_loc = pd.DataFrame({"length": ib_elem.length, 
                                                        "depth": ib_elem.depth,
                                                        "scaling": ib_elem.scaling,
                                                        "lon": lon, "lat": lat})
                        else:
                            ib_elems_loc = pd.concat([ib_elems_loc, pd.DataFrame({"length": ib_elem.length, 
                                                                                "depth": ib_elem.depth,
                                                                                "scaling": ib_elem.scaling,
                                                                                "lon": lon, "lat": lat})])
                        ib_elems_scaled = ib_elems_scaled.drop(ib_elem.index.values)

                    #print("*** disch_FESOM_cell = ", disch_FESOM_cell)
                    #print("*** disch_FESOM_cell_tot = ", disch_FESOM_cell_tot)
                    pbar.update(1)
            pbar.update(1)

    #points_ = pd.DataFrame.from_records([p.to_dict() for p in points])
    np.savetxt(os.path.join(icb_path, "LON.dat"), ib_elems_loc.lon.values)
    np.savetxt(os.path.join(icb_path, "LAT.dat"), ib_elems_loc.lat.values)
    #np.savetxt(os.path.join(icb_path, "LON.dat"), points_.x.values)
    #np.savetxt(os.path.join(icb_path, "LAT.dat"), points_.y.values)
    np.savetxt(os.path.join(icb_path, "LENGTH.dat"), ib_elems_loc.length.values * 1e3)
    #np.savetxt(os.path.join(icb_path, "LENGTH.dat"), length)
    np.savetxt(os.path.join(icb_path, "HEIGHT.dat"), ib_elems_loc.depth.values * 1e3)
    np.savetxt(os.path.join(icb_path, "SCALING.dat"), ib_elems_loc.scaling.values)
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

#print("max = ", min(data)*16*16*10e6/920)

lons = [lon if lon<180 else lon-360 for lon in lons]

basins = read_basins_file(basin_path)
b_arr = []

with tqdm(total=len(lons), file=sys.stdout, desc="find basins") as pbar:
    for lon, lat in zip(lons, lats):
        tmp = get_nearest_lon_lat(basins, lon, lat)
        b_arr.append(tmp.squeeze().values)
        pbar.update(1)

#print("b_arr = ", b_arr)
#print("lon = ", lons)
#print("lat = ", lats)
print("size(b_arr) = ", len(b_arr))
print("size(data) = ", len(data))
print("sum(data) = ", sum(data))
print("sum(data) = ", sum(data)*RES_PISM**2/910/1e3)


points = find_FESOM_elem(nod2d_file, elem2d_file, lons, lats)
ps1, ps2, ps3 = np.transpose(points)
print("size(ps1) = ", len(ps1))

array_sum = np.sum(data)
print("data has nan = ", np.argwhere(np.isnan(data)))
print("size = ", len(data))
df = pd.DataFrame({"disch": data, 
        "basin": np.array(b_arr),
        "x1": [i.x[0] for i in ps1], "y1": [i.y[0] for i in ps1],
        "x2": [i.x[0] for i in ps2], "y2": [i.y[0] for i in ps2],
        "x3": [i.x[0] for i in ps3], "y3": [i.y[0] for i in ps3]})
df.dropna(inplace=True)
array_sum = np.sum(df.disch.values)
#print("df = ", df)
#print("data has nan = ", np.argwhere(np.isnan(df.disch.values)))
##icb_generator(data, ps1, ps2, ps3, icb_path)
icb_generator(df, icb_path)

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
