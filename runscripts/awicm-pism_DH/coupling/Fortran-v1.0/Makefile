comp:
	ifort -I$(NETCDF_INC) -L$(NETCDF_LIB) -lnetcdf -lnetcdff MOD_PRE.f90 MOD_DATA.f90 MOD_MAIN.f90 MOD_OUTPUT.f90 MOD_RESTART.f90 dEBMmain.f90 -o dEBMmain
run:
	./dEBMmain
clean:
	rm dEBMmain 
test: 
	rm surface_mass_balance.nc
	rm dEBMmain 
	ifort -I$(NETCDF_INC) -L$(NETCDF_LIB) -lnetcdf -lnetcdff MOD_PRE.f90 MOD_DATA.f90 MOD_MAIN.f90 MOD_OUTPUT.f90 MOD_RESTART.f90 dEBMmain.f90 -o dEBMmain
	./dEBMmain
