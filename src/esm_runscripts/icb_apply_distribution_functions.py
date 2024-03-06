import numpy as np
import xarray as xr
import pandas as pd
import numexpr as ne
import random
import powerlaw
import sys
import os
import math
from tqdm import tqdm
import pyfesom2 as pf
import time

class IcebergCalving:
    def __init__(self, ifile, mesh_path, icb_path, basin_file, 
                latest_restart_file="", abg=[0,0,0],
                scaling_factor=[1, 1, 1, 1, 1, 1]):
        # PISM section
        self.ifile = ifile
        self.icb_path = icb_path
        self.basin_file = basin_file
        self.mesh_path = mesh_path
        self.nod2d_file = os.path.join(mesh_path, "nod2d.out")
        self.elem2d_file = os.path.join(mesh_path, "elem2d.out")
        self.latest_restart_file = latest_restart_file
        self.abg = abg
        self.name_of_discharge = "tendency_of_ice_amount_due_to_discharge" #"tendency_of_ice_amount_due_to_calving"
        self.rho_ice = 850
        #self.bins = [0.01, 0.1, 1, 10, 100, 1000]
        #self.weights_area = [0.0005, 0.0005, 0.008, 0.025, 0.074, 0.893]
        #self.weights_dist = [0.4, 0.2, 0.15, 0.175, 0.05, 0.025]
        #self.area_mean = [0.01, 0.1, 1, 10, 100, 1000]
        self.bins = [0.1, 1, 10, 100, 1000]
        self.weights_area = [0.0005, 0.008, 0.025, 0.074, 0.893]
        self.weights_dist = [0.75, 0.175, 0.05, 0.02, 0.005]
        self.area_mean = [0.01, 0.1, 1, 10, 100]
        self.area_min = 0.01             #[km2]
        self.area_max = 400           #[km2]
        self.min_disch_in_cell = 0.0   #[kg m-2 year-1]
        #self.scaling_factor = np.array([1000, 100, 10, 1, 1, 1])
        self.scaling_factor = np.array(scaling_factor)
        self.thick = np.array([0.25, 0.25, 0.25, 0.25, 0.25, 0.25])
        self.thick_max = 0.25
        self.depth = self.thick * 7/8
        self.height = self.thick - self.depth

        self._read_pism_file()
        self._get_pism_resolution()
        self._convert_to_kg_m2_year1()
        self._get_coords()
        self._read_basins_file()

        # FESOM section
        self._read_mesh()
        self._read_nod2d_file()
        self._read_elem2d_file()
        if not self.latest_restart_file=="":
            print("LA DEUBG: Latest_restart_file found. Check for full FESOM cells")
            self._get_full_cells()
        else:
            self.full_elems = []

    def create_dataframe(self):
        self._get_data()
        self._write_icb_mask()
        self._find_basins()
        self._find_FESOM_elem()

        self.df = pd.DataFrame({
                "disch": self.data * self.res * self.res / self.rho_ice, #[m3/year] 
                "elems": self.indices1D,
                "basin": self.basins1D,
                })
        self.df.dropna(inplace=True)
        self.df_agg = self.df.get(["disch", "basin"]).groupby("basin").sum()

        elem_tmp = []
        neigh_tmp = []
        
        # iterate over basins
        for b in self.df.groupby("basin"):

            # get all FESOM elements nearest to discharge location within this basin 
            # every element shall occur only once
            elem_tmp.append(b[1]["elems"].unique())
            n=[]
            for x in elem_tmp[-1]:
                
                # get all neighbouring elements for each FESOM element
                # every above found FESOM element is associated with a list of neighbouring elements
                n.append(self._get_FESOM_neighbours(x))
                
                ################################################   
                #tmp = self._get_FESOM_neighbours(x)
                #m=[]
                #for y in tmp:
                #    m = m + self._get_FESOM_neighbours(y)
                #n.append(np.unique(m))
                ################################################
            neigh_tmp.append(n)
        
        self.df_agg["elems"] = elem_tmp
        self.df_agg["neigh."] = neigh_tmp

    def _set_min_disch_in_cell(self, min_disch_in_cell):
        self.min_disch_in_cell = min_disch_in_cell

    def _read_mesh(self):
        self.mesh = pf.load_mesh(self.mesh_path, self.abg, usepickle=False)

    def _read_pism_file(self):
        print(" * open latest discharge file: ", self.ifile)
        self.fl = xr.open_dataset(self.ifile, decode_times=False).squeeze()
        self.disch_field = self.fl.get(self.name_of_discharge)
        self.disch_units = self.disch_field.units

    def _get_pism_resolution(self):
        res_x = (self.fl.x[-1] - self.fl.x[0]) / (len(self.fl.x) - 1) 
        res_y = (self.fl.y[-1] - self.fl.y[0]) / (len(self.fl.y) - 1) 
        if res_x != res_y:
            print(" * non-quadratic grid cells. Not sure what to do ...")
            exit
        else:
            self.res = res_x.values #[m]
            print("self.res = ", self.res)

    def _convert_to_kg_m2_year1(self):
        if self.disch_units == "Gt year-1":
            self.disch_field = self.disch_field * 1e12 / ( self.res * self.res )

    def _get_data(self):
        data = self.disch_field.where(self.disch_field < self.min_disch_in_cell, drop=True)
        data = np.array(data).reshape(1, len(data.x) * len(data.y))
        self.data = data[~np.isnan(data)]

    def _write_icb_mask(self): 
        tmp = self.disch_field.where(self.disch_field < self.min_disch_in_cell).fillna(0) 
        #tmp = xr.DataArray(self.disch_field.where(self.disch_field < self.min_disch_in_cell).squeeze(), 
        #        coords={
        #        "x": self.fl.x.values, 
        #        "y": self.fl.y.values}, 
        #        dims=["x", "y"], name=self.name_of_discharge)
        tmp.to_netcdf(os.path.join(self.icb_path, "icb_mask.nc"))

    def _get_coords(self):
        lon_bnds = self.fl.lon_bnds.where(self.disch_field < self.min_disch_in_cell, drop=True)
        lon_bnds = np.array(lon_bnds).reshape(len(lon_bnds.x)*len(lon_bnds.y), 4)
        lon_bnds = lon_bnds[~np.isnan(lon_bnds)]
        lon_bnds = lon_bnds.reshape(int(len(lon_bnds)/4), 4)

        lat_bnds = self.fl.lat_bnds.where(self.disch_field < self.min_disch_in_cell, drop=True)
        lat_bnds = np.array(lat_bnds).reshape(len(lat_bnds.x)*len(lat_bnds.y), 4)
        lat_bnds = lat_bnds[~np.isnan(lat_bnds)]
        lat_bnds = lat_bnds.reshape(int(len(lat_bnds)/4), 4)

        lons = []
        lats = []

        for x, y in zip(lon_bnds, lat_bnds):
            lons.append(np.mean(x))
            lats.append(np.mean(y))

        lons = [lon if lon<180 else lon-360 for lon in lons]
        self.lons = lons
        self.lats = lats

    def _read_basins_file(self):
        fl = xr.open_dataset(self.basin_file, decode_times=False)
        if "basin" in fl:
            self.basins = fl.squeeze().basin
        elif "basins" in fl:
            self.basins = fl.squeeze().basins
        else:
            print("No basins in basin file")
            return -1
    
    def _get_nearest_lon_lat(self, ds, lon, lat):
        #https://stackoverflow.com/questions/58758480/xarray-select-nearest-lat-lon-with-multi-dimension-coordinates
        abslat = np.abs(ds.lat-lat)
        abslon = np.abs(ds.lon-lon)
        c = np.maximum(abslon, abslat)
    
        ([yloc], [xloc]) = np.where(c == np.min(c))
        point_ds = ds.isel(x=xloc, y=yloc)
        return point_ds

    def _find_basins(self):
        basins = []
        with tqdm(total=len(self.lons), file=sys.stdout, desc="find basins") as pbar:
            for lon, lat in zip(self.lons, self.lats):
                tmp = self._get_nearest_lon_lat(self.basins, lon, lat)
                basins.append(int(tmp.squeeze().values))
                pbar.update(1)
        self.basins1D = basins

    def _read_nod2d_file(self):
        self.nod2d = pd.read_csv(self.nod2d_file, header=0, names=["lon", "lat", "coastal"], sep='\s+', index_col=0)
        
    def _read_elem2d_file(self):
        self.elem2d = pd.read_csv(self.elem2d_file, header=0, names=["nod1", "nod2", "nod3"], sep='\s+')

    def _get_full_cells(self):
        df = pd.read_csv(self.latest_restart_file, header=None, delim_whitespace=True)
        df_group  = df[[1,2,18,24]].groupby(18)
        full_elems_tmp = []

        for felem in df_group:
            if felem[0] == 0.0:
                continue
            ai = felem[1][1] * felem[1][2] * felem[1][24]
            af = self.mesh.voltri[int(felem[0])-1]
            if ai.sum() >= af:
                print("*** FESOM element is full: ", felem[0])
                print(" element area = ", af)
                print(" iceberg area = ", ai)
                full_elems_tmp.append(felem[0])
        self.full_elems = full_elems_tmp

    def _find_FESOM_elem(self):
        points = []
        indices = []
        neighbours = []

        lon1 = self.nod2d.lon[self.elem2d.nod1]
        lat1 = self.nod2d.lat[self.elem2d.nod1]
        lon2 = self.nod2d.lon[self.elem2d.nod2]
        lat2 = self.nod2d.lat[self.elem2d.nod2]
        lon3 = self.nod2d.lon[self.elem2d.nod3]
        lat3 = self.nod2d.lat[self.elem2d.nod3]
        
        with tqdm(total=len(self.lons), file=sys.stdout, desc='find FESOM elements') as pbar:
            for lon, lat in zip(self.lons, self.lats):
                tmp, ind = PointTriangle_distance(lon, lat, 
                                                np.array(lon1), np.array(lat1), 
                                                np.array(lon2), np.array(lat2), 
                                                np.array(lon3), np.array(lat3))
                
                points.append(tmp[:3])
                indices.append(ind)
                pbar.update(1)
        
        self.points1D = points
        self.indices1D = indices

    def _get_FESOM_neighbours(self, ind):
        points = []
        indices = []

        tmp = self.elem2d.loc[((self.elem2d["nod1"].isin(self.elem2d.values[ind])) & (self.elem2d["nod2"].isin(self.elem2d.values[ind]))) | \
                        ((self.elem2d["nod1"].isin(self.elem2d.values[ind])) & (self.elem2d["nod3"].isin(self.elem2d.values[ind]))) | \
                        ((self.elem2d["nod2"].isin(self.elem2d.values[ind])) & (self.elem2d["nod3"].isin(self.elem2d.values[ind])))]
   
        indices = list(tmp[tmp.index != ind].index)
        return indices


    def _create_icebergs_within_basin(self, df):
    ######################################
    # input:    data frame for one basin: discharge and FESOM cell corners (p1, p2, p3)
    # output:   iceberg volume array
    ######################################
        # maximal time in seconds to wait for iceberg generation to finish (for one basin)
        dtime_MAX = 30
        
        # mu and sigma for lognormal distribution after Tournadre et al. (2011)
        mu, sigma = 12.3, 1.55**0.5
   
        # alpha for powerlaw after Tournadre et al. (2015)
        a, xmin = 1.52, 0.01
        median = 2**(1/(a-1))*xmin

        # get values within basin
        vals = abs(df.disch)
    
        # get total discharge within basin in [km3 year-1]
        disch_tot = vals / 1e9
    
        # get total iceberg area within basin in [km2 year-1]
        # assuming constant iceberg height
        area_tot = disch_tot / self.thick_max
    
        # create iceberg areas according to Tournadre et al. (2015)
        # divide icebergs into classes of different area sizes (0.1-1, 1-10, 10-100, ... [km2])
        # and draw from powerlaw distribution with alpha=1.52 except for the icebergs from
        # smallest class. Get total number of icebergs with share of smallest class (WEIGHTS_N)
        # and mean size within smalles class (SMEAN_1). Get number of icebergs of each other class
        # with corresponding share. 
        N = int(area_tot / median)

        # generates random variates of power law distribution
        vrs = powerlaw.Power_Law(xmin=xmin, xmax=self.area_max, parameters=[a]).generate_random(N)

        x = vrs
        corr = area_tot / sum(x)
        
        x = x * corr

        # correction with respect to iceberg volume and not iceberg area
        thick = x**(1/2)
        thick[thick>self.thick_max] = self.thick_max
        vol = x * thick
        corr = disch_tot / sum(vol) 
        #corr = area_tot / sum(x)
        
        x = x * corr
        
        # correction with respect to iceberg volume and not iceberg area
        thick = x**(1/2)
        thick[thick>self.thick_max] = self.thick_max
        vol = x * thick
        vol_sum_0 = sum(vol)
        #x_sum_0 = sum(x)
        
        x = x[x>=xmin]
        x = x[x<=self.area_max]
        if sum(x) == 0:
            print(" * no icebegs")
            return pd.DataFrame()
        
        # correction with respect to iceberg volume and not iceberg area
        thick = x**(1/2)
        thick[thick>self.thick_max] = self.thick_max
        vol = x * thick
        vol_sum_1 = sum(vol)
        #x_sum_1 = sum(x)
        corr = vol_sum_0 / vol_sum_1
        #corr = x_sum_0 / x_sum_1
        x = x * corr

        x_tot = x
        tstart = time.time()
        while N > 0:
            N = int((vol_sum_0 - vol_sum_1) / vol_sum_0 * N)
            print(" N = ", str(N))
            if N==0:
                break
            vrs = powerlaw.Power_Law(xmin=xmin, xmax=self.area_max, parameters=[a]).generate_random(N)
            x = vrs
            x_too_small = x[x<xmin]
            x_too_large = x[x>self.area_max]
            x = x[x>=xmin]
            x = x[x<=self.area_max]
            x_tot = np.concatenate([x_tot, x])
            if sum(x_tot) == 0:
                print(" * no icebegs")
                return pd.DataFrame()
        
            # correction with respect to iceberg volume and not iceberg area
            thick = x_tot**(1/2)
            thick[thick>self.thick_max] = self.thick_max
            vol = x_tot * thick
            corr = disch_tot / sum(vol)
            #corr = area_tot / sum(x_tot)
            x_tot = x_tot * corr
            tend = time.time()
            dtime = tend - tstart
            if dtime >= dtime_MAX:
                print("elapsed time = ", str(tend - tstart))
                print("start iceberg generation again for this basin")
                return -1
            
        # correction with respect to iceberg volume and not iceberg area
        vol = x_tot * thick

        area = x_tot
        bins = np.digitize(area, self.bins, right=True)

        #for a, b in zip(area, bins):
        #    vol = np.concatenate([vol, [a * self.thick[b]]])

        # create data frame with iceberg elements: area, volume, bin
        ib_elems = pd.DataFrame({"area": area, 
                                "volume": vol,
                                "bin": np.digitize(area, self.bins, right=True)})
       
        ib_elems_ = ib_elems.where(ib_elems.area >= self.area_min).dropna()
    
        print("*** Check for validity:")
        print("***      assumed iceberg thickness [km]:         ", self.thick)
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


    def __create_icebergs_within_basin(self, df):
    ######################################
    # input:    data frame for one basin: discharge and FESOM cell corners (p1, p2, p3)
    # output:   iceberg volume array
    ######################################
        # mu and sigma for lognormal distribution after Tournadre et al. (2011)
        mu, sigma = 12.3, 1.55**0.5
   
        # alpha for powerlaw after Tournadre et al. (2015)
        a, xmin = 1.52, 0.1
        C = (a - 1) / xmin * xmin ** (a - 1)

        # get values within basin
        vals = abs(df.disch)
    
        # get total discharge within basin in [km3 year-1]
        disch_tot = vals / 1e9
    
        # get total iceberg area within basin in [km2 year-1]
        # assuming constant iceberg height
        area_tot = disch_tot / self.thick.mean()
    
        # create iceberg areas according to Tournadre et al. (2015)
        # divide icebergs into classes of different area sizes (0.1-1, 1-10, 10-100, ... [km2])
        # and draw from powerlaw distribution with alpha=1.52 except for the icebergs from
        # smallest class. Get total number of icebergs with share of smallest class (WEIGHTS_N)
        # and mean size within smalles class (SMEAN_1). Get number of icebergs of each other class
        # with corresponding share. 
       
        print("*** Total area [km2] = ", area_tot)
        if area_tot > self.area_max:
            area = np.array([np.random.uniform(self.bins[-1], self.area_max)])
            vol = area * self.thick[-1]
            print("*** Total area too big. Calving of giant iceberg with area [km2] = ", area)
            area_tot_new = area_tot - area
        else:
            area = 0
            print("*** Total area okay")
            area_tot_new = area_tot

        stop_iceberg_generation = False
        for i, (wa, wn) in enumerate(zip(self.weights_area, self.weights_dist)):
            print("*** Bin Number ", str(i))
            if i==0:
                # get amount of icebergs of smalles class
                N = int(area_tot_new * wa / self.area_mean[i])
                # get total amount of icebergs
                N_tot = int(N / wn)
                print("N_tot = ", N_tot)
                # draw area sizes from lognormal distribution for smalles iceberg class
                #tmp = np.random.lognormal(mu, sigma, N)
                tmp = C * powerlaw.Power_Law(xmin=xmin, parameters=[a]).generate_random(N)
                # get scaling factor to sum iceberg areas up to actual share of discharge (wa * area_tot)
                if N == N_tot:
                    f = area_tot_new / sum(tmp)
                else:
                    f = wa * area_tot_new / sum(tmp)
                area_new = tmp * f
                vol_new = area_new * self.thick[i]
                
                if area == 0:
                    area = area_new
                    vol = vol_new
                else:
                    print(" *** AREA = ", area)
                    area = np.concatenate((area, area_new), axis=0)
                    vol = np.concatenate((vol, vol_new), axis=0)
            
            else:
                # get amount of icebergs of class
                N = math.ceil(area_tot_new * wa / self.area_mean[i])
                if N == 0:
                    continue
                    #N = 1
                    stop_iceberg_generation = True
                
                #tmp = np.random.lognormal(mu, sigma, N)
                # generates random variates of power law distribution
                tmp = C * powerlaw.Power_Law(xmin=xmin, parameters=[a]).generate_random(N)

                if stop_iceberg_generation:
                    f = sum(self.weights_area[i:]) * area_tot_new / sum(tmp)
                else:
                    f = wa * area_tot_new / sum(tmp)
                area_new = tmp * f
                vol_new = area_new * self.thick[i]
                area = np.concatenate((area, area_new), axis=0)
                vol = np.concatenate((vol, vol_new), axis=0)
   
            if stop_iceberg_generation:
                print("*** No further icebergs are generated")
                continue

        ## break down all iceberg with surface area greater than SMAX [km2] into smaller icebergs
        #while len(area[area > self.area_max] > 0):
        #    tmp = np.where(area > self.area_max)
        #    area[tmp] = area[tmp]/2
        #    area = np.append(area, area[tmp])
    
        # create data frame with iceberg elements: area, volume, bin
        ib_elems = pd.DataFrame({"area": area, 
                                "volume": vol,
                                "bin": np.digitize(area, self.bins, right=True)})
       
        ib_elems_ = ib_elems.where(ib_elems.area >= self.area_min).dropna()
    
        print("*** Check for validity:")
        print("***      assumed iceberg thickness [km]:         ", self.thick)
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

    def _scale_icebergs(self, df):
    ######################################
    # input:    data frame: area, volume, bin
    # output:   data frame: length, scaling, depth
    ######################################
        # loop over all bins
        with tqdm(total=len(self.scaling_factor), file=sys.stdout, desc='go through all bins') as pbar:
            for i, (s, d) in enumerate(zip(self.scaling_factor, self.thick)):
                
                # get icb elements of particular size class
                ib_bin = df.where(df.bin==i).dropna()
       
                if not ib_bin.empty:
                    # split iceberg array of size class into chunks with length s
                    chunks = np.array_split(ib_bin, math.ceil(len(ib_bin)/s))
                    # get mean of each chunk
                    chunks_mean_area = np.array([chunk.area.mean(axis=0) for chunk in chunks])
                    chunks_mean_volume = np.array([chunk.volume.mean(axis=0) for chunk in chunks])
    
                    # check if arrays are initialized
                    if not 'length' in locals():
                        # get mean length of icebergs for each chunk
                        length = ne.evaluate('chunks_mean_area**(1/2)')
                        # get scaling factor (length of each chunk)
                        scaling = np.array([len(chunk) for chunk in chunks])
                        # get mean height of icebergs for each chunk
                        depth = ne.evaluate('chunks_mean_volume / chunks_mean_area')
                        ## get depth
                        #depth = np.array([d] * len(chunks))
    
                    else:
                        # get mean length of icebergs for each chunk
                        length = np.append(length, ne.evaluate('chunks_mean_area**(1/2)'))
                        # get scaling factor (length of each chunk)
                        scaling = np.append(scaling, np.array([len(chunk) for chunk in chunks]))
                        # get mean height of icebergs for each chunk
                        depth = np.append(depth, ne.evaluate('chunks_mean_volume / chunks_mean_area'))
                        ## get depth
                        #depth = np.append(depth, np.array([d] * len(chunks)))
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
        print("***      total iceberg volume [km3]: ", np.sum(df_out.length * df_out.length * df_out.scaling * df_out.depth))
        print("***      total amount of icebergs:   ", np.sum(df_out.scaling))
        print("***      total am. of sim. icebergs: ", len(df_out))
        return df_out
    
    #generate icebergs
    def _icb_generator(self):
        ###############################
        # bisher verwendet!
        mu, sigma = 12.3, 1.55**0.5     #Tournadre et al. 2011
        a = 1.52
    
        ib_elems_scaled = pd.DataFrame()
        ib_elems_loc = pd.DataFrame()
    
        points = []
        height = [] #height=depth*8/7=length*8/7*2/3=length*16/21
    
        with tqdm(total=len(self.df_agg), file=sys.stdout, desc='go through basins') as pbar:
            for index in self.df_agg.index:
                # inner loop to enable redo if generation of icebergs takes too long
                # https://stackoverflow.com/questions/36573486/redo-for-loop-iteration-in-python
                while True:
                    b = self.df_agg.loc[index]
                    print("*****************************")
                    print("*** BASIN = ", index)

                    # create icebergs for basin [m3]
                    ib_tmp = self._create_icebergs_within_basin(b)
                    if isinstance(ib_tmp, int):
                        if ib_tmp == -1:
                            continue            # equivalent to redo
                    elif ib_tmp.empty:
                        break                   # now equivalent to continue
                    ib_elems = self._scale_icebergs(ib_tmp)

                    # make list of fesom elements and it's neighbours
                    felems = list(b.elems)
                    for n in b["neigh."]:
                        felems = felems + list(n)
                    felems = list(set(felems))
  
                    ##############################################################
                    # exclude coastal nodes (and full cells)
                    elems_to_drop = self.full_elems
                
                    for felem in felems:
                        nodes = self.elem2d.iloc[felem].values
                        coastal = False
                        for node in nodes:
                            lon, lat, tmp = self.nod2d.loc[node]
                            if (tmp == 1 or coastal == 1):
                                coastal = True
                    
                        if coastal == 1:
                            elems_to_drop.append(felem)

                    print(" * drop these elements: ", elems_to_drop) 
                    new_felems = [elem for elem in felems if elem not in elems_to_drop]
                    felems = new_felems
                    ##############################################################

                    if len(felems) != 0:
                        tmp = felems * int(len(ib_elems) / len(felems)) + felems[:len(ib_elems)%len(felems)]
                        felems = tmp
   
                        with tqdm(total=len(self.df_agg), file=sys.stdout, desc='initialize icebergs') as pbar:
                            for felem, index in zip(felems, ib_elems.index):
                                ib_elem = ib_elems.loc[index]
                                
                                nod1, nod2, nod3 = self.elem2d.iloc[felem].values
                                lon1, lat1, tmp = self.nod2d.loc[nod1].values
                                lon2, lat2, tmp = self.nod2d.loc[nod2].values
                                lon3, lat3, tmp = self.nod2d.loc[nod3].values

                                r1 = random.random()
                                r2 = random.random()
                                
                                lower_bound = 0.25
                                upper_bound = 0.75
    
                                r1 = r1 * (upper_bound - lower_bound) + lower_bound
                                r2 = r2 * (upper_bound - lower_bound) + lower_bound
                                #https://math.stackexchange.com/questions/18686/uniform-random-point-in-triangle
                                try:
                                    lon = (1-np.sqrt(r1))*lon1 + (np.sqrt(r1)*(1-r2))*lon2 + (r2*np.sqrt(r1))*lon3
                                    lat = (1-np.sqrt(r1))*lat1 + (np.sqrt(r1)*(1-r2))*lat2 + (r2*np.sqrt(r1))*lat3
                                except:
                                    continue
                               
                                if ib_elems_loc.empty:
                                    ib_elems_loc = pd.DataFrame({"length": [ib_elem.length], 
                                                                "depth": [ib_elem.depth],
                                                                "scaling": [ib_elem.scaling],
                                                                "lon": [lon], "lat": [lat],
                                                                "felem": [felem]})
                                else:
                                    ib_elems_loc = pd.concat([ib_elems_loc, pd.DataFrame({"length": [ib_elem.length], 
                                                                                        "depth": [ib_elem.depth],
                                                                                        "scaling": [ib_elem.scaling],
                                                                                        "lon": [lon], "lat": [lat],
                                                                                        "felem": [felem]})])
                                pbar.update(1)
                    pbar.update(1)
                    break

        if not ib_elems_loc.empty:
            np.savetxt(os.path.join(self.icb_path, "icb_longitude.dat"), ib_elems_loc.lon.values)
            np.savetxt(os.path.join(self.icb_path, "icb_latitude.dat"), ib_elems_loc.lat.values)
            np.savetxt(os.path.join(self.icb_path, "icb_length.dat"), ib_elems_loc.length.values * 1e3)
            np.savetxt(os.path.join(self.icb_path, "icb_height.dat"), ib_elems_loc.depth.values * 1e3)
            np.savetxt(os.path.join(self.icb_path, "icb_scaling.dat"), ib_elems_loc.scaling.values)
            np.savetxt(os.path.join(self.icb_path, "icb_felem.dat"), ib_elems_loc.felem.values)




















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

def PointTriangle_distance(lon0, lat0, lon1, lat1, lon2, lat2, lon3, lat3):
    d1 = ne.evaluate('(lon1 - lon0)**2 + (lat1 - lat0)**2')
    d2 = ne.evaluate('(lon2 - lon0)**2 + (lat2 - lat0)**2')
    d3 = ne.evaluate('(lon3 - lon0)**2 + (lat3 - lat0)**2')
    
    dis = d1+d2+d3
    ind = np.where(dis == np.amin(dis))
   
    p1 = point(lon1[ind], lat1[ind])
    p2 = point(lon2[ind], lat2[ind])
    p3 = point(lon3[ind], lat3[ind])

    return [p1, p2, p3], ind[0][0]
