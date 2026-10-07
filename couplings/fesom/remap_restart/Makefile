FC      ?= gfortran
FFLAGS  ?= -O3 -march=native -fopenmp -g
# netCDF: the RUN environment's netcdf-fortran, not whatever module happens to
# be loaded at build time, and pinned so it cannot be swapped at runtime.
#
# esm_tools exports NETCDFFROOT/NETCDFROOT in the experiment's env.sh and puts
# $NETCDFFROOT/lib at the FRONT of LD_LIBRARY_PATH.  build.sh, by contrast,
# sources FESOM's env/<machine>/shell, which module-loads a DIFFERENT
# netcdf-fortran: 4.5.4 against the run env's 4.6.1 on albedo.  Both have soname
# libnetcdff.so.7, so a binary compiled against one netcdf.mod loads the other
# .so without complaint -- and then the nf90_inquire_variable call sequence
# disagrees and the process dies with a nil dereference inside netcdf, naming a
# perfectly intact restart file.  It cost two experiments (antftest8 died on the
# fourth restart file, antftest9 on the first; which file depends only on the
# heap layout) and reproduces exactly by swapping nothing but the library.
#
# So the prefix comes from the environment when it is set, which makes the build
# match the run env; build.sh keeps esm_tools' value alive across the FESOM env
# it sources (that env's module file does `pushenv NETCDFFROOT`, overwriting it).
#
# Pinning the library with DT_RPATH instead of DT_RUNPATH would also close it --
# DT_RPATH outranks LD_LIBRARY_PATH -- but it is not available here: `mpiifort
# -v` shows the Intel driver appending its own --enable-new-dtags AFTER whatever
# we pass, and the last of the two wins, so -Wl,--disable-new-dtags has no
# effect from inside this Makefile. Hence the startup banner below instead: if
# the two ever drift apart again, the netcdf-fortran the binary was built
# against is in the log, next to the run's own env.
NETCDF_F = $(if $(NETCDFFROOT),$(NETCDFFROOT),$(shell nf-config --prefix))
NETCDF_C = $(if $(NETCDFROOT),$(NETCDFROOT),$(shell nc-config --prefix))

NCFLAGS = -I$(NETCDF_F)/include
NCLIBS  = -L$(NETCDF_F)/lib -lnetcdff \
          -L$(NETCDF_C)/lib -lnetcdf \
          -Wl,-rpath,$(NETCDF_F)/lib \
          -Wl,-rpath,$(NETCDF_C)/lib

# Compiled into the binary and echoed at startup, so the netcdf-fortran a run
# actually used is in the log instead of having to be inferred from a backtrace.
VERFLAG = -DNETCDF_F_PREFIX=\"$(NETCDF_F)\"
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
	@echo "[Makefile] netcdf-fortran: $(NETCDF_F)"
	@echo "[Makefile] netcdf-c      : $(NETCDF_C)"
	$(FC) $(FFLAGS) -o $@ $^ $(NCLIBS)

mo_remap_mesh.o: mo_remap_mesh.F90
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

mo_restart_io.o: mo_restart_io.F90 
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

mo_remap_fields.o: mo_remap_fields.F90 mo_remap_mesh.o mo_restart_io.o
	$(FC) $(FFLAGS) $(NCFLAGS) -c $<

remap_restart.o: remap_restart.F90 mo_remap_fields.o mo_remap_mesh.o mo_restart_io.o
	$(FC) $(FFLAGS) $(NCFLAGS) $(VERFLAG) -c $<

clean:
	rm -f *.o *.mod $(TARGET)
