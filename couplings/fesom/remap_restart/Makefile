FC      ?= gfortran
FFLAGS  ?= -O2 -g
NETCDF_F = $(shell nf-config --prefix)
NETCDF_C = $(shell nc-config --prefix)

NCFLAGS = -I$(NETCDF_F)/include
NCLIBS  = -L$(NETCDF_F)/lib -lnetcdff \
          -L$(NETCDF_C)/lib -lnetcdf \
          -Wl,-rpath,$(NETCDF_F)/lib \
          -Wl,-rpath,$(NETCDF_C)/lib
#NETCDF  = $(shell nf-config --prefix)
#NCFLAGS = $(shell nf-config --fflags)
#NCLIBS = $(shell nf-config --flibs | sed 's/-lMPI::MPI_Fortran//g' | sed 's/-lnetcdff_c//g')

SRCS = mo_remap_mesh.F90 \
       mo_remap_fields.F90 \
       mo_restart_io.F90 \
       remap_restart.F90

OBJS = mo_remap_mesh.o \
       mo_remap_fields.o \
       mo_restart_io.o \
       remap_restart.o

TARGET = remap_restart

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(NCLIBS)

mo_remap_mesh.o: mo_remap_mesh.F90
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

mo_restart_io.o: mo_restart_io.F90 
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

mo_remap_fields.o: mo_remap_fields.F90 mo_remap_mesh.o mo_restart_io.o
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

remap_restart.o: remap_restart.F90 mo_remap_fields.o mo_remap_mesh.o mo_restart_io.o
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

clean:
	rm -f *.o *.mod $(TARGET)
