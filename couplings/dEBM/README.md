The diurnal Energy Balance Model (dEBM)
============
The surface mass balance scheme dEBM (diurnal Energy Balance Model) provides a novel interface between the atmosphere and land ice for Earth System modeling, which is based on the energy balance of glaciated surfaces. In contrast to empirical schemes, dEBM accounts for changes in the Earth’s orbit and atmospheric composition. The scheme only requires monthly atmospheric forcing (precipitation, temperature, shortwave and longwave radiation, and cloud cover). It is also computationally inexpensive, which makes it particularly suitable to investigate the ice sheets' response to long-term climate change.
A full model description is found in https://doi.org/10.5194/tc-2020-247

Getting Started with dEBM
=============

1. download the code :

   git clone https://github.com/ukrebska/dEBM/tree/update/Fortran-v1.0 dEBM
   cd dEBM

2. prepare the input:

   EXAMPLE input files: test.nc (include air_temp precipitation cc swd TOAswd emiss tau)
   test.nc is already downscaled and interpolated from a T63 atmospheric resolution to a 20km equidistant grid.

   edit namelist of dEBM:
      vi namelist.dEBM:
          & dEBM : filename_in='test.nc'

   path for the results:
   ./

3. compile the code:

   make comp

4. run the model:

   make run

5. investigate the results:
   output: surface_mass_balance.nc
   restart: restart_debm.nc
   
Technical Support  
============
If you have some questions, please contact Shan Xu <shan.xu@awi.de> or Uta Krebs-Kanzow <ukrebska@awi.de>.
