The diurnal Energy Balance Model (dEBM)
======

The diurnal Energy Balance Model (dEBM) is a surface mass balance model of glaciated surfaces. It provides a novel interface between atmosphere and land ice for Earth System modelling. In contrast to empirical schemes, dEBM accounts for changes in the Earth's orbit and atmospheric composition. The scheme only requires monthly atmospheric forcing (precipitation, temperature, shortwave and longwave radiation and cloud cover) and calculates surface mass balance (melt, refreeze, net surface mass balance) on a glacier surface. The dEBM is computationally inexpensive, which makes it particularly suitable to investigate the response of ice sheets to long-term climate change.

If you have some questions, please contact Uta Krebs-Kanzow <ukrebska@awi.de> or Shan Xu <shan.xu@awi.de>

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

References
==========

1. Krebs-Kanzow, U., Gierz, P., and Lohmann, G.: Brief communication: An ice surface melt scheme including the diurnal cycle of solar radiation, The Cryosphere, 12, 3923–3930, https://doi.org/10.5194/tc-12-3923-2018, 2018.

2. Krebs-Kanzow, U., Gierz, P., Rodehacke, C. B., Xu, S., Yang, H., and Lohmann, G.: The diurnal Energy Balance Model (dEBM): A convenient surface mass balance solution for ice sheets in Earth System modeling, The Cryosphere Discuss., https://doi.org/10.5194/tc-2020-247, in review, 2020.
