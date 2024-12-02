!------------------------------------------------------------------------------
!
! program reading a packed array and a corresponding 2d-mask and writes the unpacked array 
!
! Compilation
!
! linux-x64:
! module load nag
! nagfor -colour -nan -gline  -O0 -C=all -w=uep unpack.f90 -o unpack  -I/sw/jessie-x64/netcdf_fortran-4.4.2-static-nag60/include  -L/sw/jessie-x64/netcdf_fortran-4.4.2-static-nag60/lib -lnetcdff  -L/sw/jessie-x64/netcdf-4.3.3.1-static-gccsys/lib/ -lnetcdf  -L/sw/jessie-x64/hdf5-1.8.16-static-gccsys/lib -lhdf5_hl -lhdf5 -ldl  -L/sw/jessie-x64/szip-2.1-static-gccsys/lib -lsz -lz
!
! mistral:
! module load nag
! export LD_LIBRARY_PATH="/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib:/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-gcc48/lib:/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib"
!
! nagfor -colour '-maxcontin=100' '-wmismatch=define_var_new,nf_get_var_double,nf_copy_att,nf_get_var_real,nf_put_var_double,nf_def_var,nf_put_vara_double,dgemm,nfmpi_def_dim,nfmpi_put_vara_double,nfmpi_def_var' '-C=all' -g -gline -nan '-w=uep' -o unpack ./unpack.f90 -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-static-nag60/include -L/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-static-nag60/lib -lnetcdff -L/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-gcc48/lib -lnetcdf -L/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib -lhdf5_hl -lhdf5 -ldl -L/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib -lsz -L/usr/lib64/lib -lz -ldl -lm -lnuma -Wl,--export-dynamic -lrt -lnsl -lutil -lm -ldl
!------------------------------------------------------------------------------
PROGRAM unpack_array

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307)
  REAL(dp), PARAMETER :: missval=NF_FILL_DOUBLE
  INTEGER :: i, j, nlon, nlat, ntiles, nland, nsoil
  INTEGER :: stat, ncid, ncin, varid, lonid, latid, varndims, idlon, idlat, idtile, idsoil
  INTEGER, ALLOCATABLE :: vardimids(:)

  REAL(dp), ALLOCATABLE :: packed1d(:)
  REAL(dp), ALLOCATABLE :: packed2d(:,:)
  REAL(dp), ALLOCATABLE :: packed3d(:,:,:)
  REAL(dp), ALLOCATABLE :: unpacked2d(:,:)
  REAL(dp), ALLOCATABLE :: unpacked3d(:,:,:)
  REAL(dp), ALLOCATABLE :: unpacked4d(:,:,:,:)
  REAL(dp), ALLOCATABLE :: rmask(:,:)
  REAL(dp), ALLOCATABLE :: lon(:)
  REAL(dp), ALLOCATABLE :: lat(:)
  LOGICAL,  ALLOCATABLE :: lmask(:,:)

  CHARACTER*100 :: array_name, mask_name, tile_name, soil_name

  !PRINT*, 'enter array file name (without .nc)'
  READ*, array_name
  !PRINT*, 'enter mask file name (without .nc)'
  READ*, mask_name
  !PRINT*, 'Got:', array_name, 'and', 'mask_name'
  !-- read land sea mask from mask file (array and file have the same names)

  stat = nf_open(TRIM(mask_name)//'.nc',NF_NOWRITE, ncin)
  CALL hdlerr(stat)
  stat = nf_inq_varid(ncin,mask_name,varid)
  CALL hdlerr(stat)

  stat = nf_inq_varndims(ncin,varid,varndims)
  CALL hdlerr(stat)
  ALLOCATE(vardimids(varndims))
  stat = nf_inq_vardimid(ncin,varid,vardimids)
  CALL hdlerr(stat)
  stat = nf_inq_dimlen(ncin,vardimids(1),nlon)
  CALL hdlerr(stat)
  stat = nf_inq_dimlen(ncin,vardimids(2),nlat)
  CALL hdlerr(stat)
  DEALLOCATE(vardimids)
  ALLOCATE(rmask(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,rmask)
  CALL hdlerr(stat)

  !-- read packed array from file (array and file have the same names)

  stat = nf_open(TRIM(array_name)//'.nc',NF_NOWRITE,ncid)
  CALL hdlerr(stat)
  stat = nf_inq_varid(ncid,array_name,varid)
  CALL hdlerr(stat)

  stat = nf_inq_varndims(ncid,varid,varndims)
  CALL hdlerr(stat)
  ALLOCATE(vardimids(varndims))
  stat = nf_inq_vardimid(ncid,varid,vardimids)
  CALL hdlerr(stat)
  stat = nf_inq_dimlen(ncid,vardimids(1),nland)
  CALL hdlerr(stat)
  IF (varndims > 1) THEN
     stat = nf_inq_dimlen(ncid,vardimids(2),ntiles)
     CALL hdlerr(stat)
     stat = nf_inq_dimname(ncid,vardimids(2),tile_name)
     CALL hdlerr(stat)
  END IF
  IF (varndims > 2) THEN
     stat = nf_inq_dimlen(ncid,vardimids(3),nsoil)
     CALL hdlerr(stat)
     stat = nf_inq_dimname(ncid,vardimids(3),soil_name)
     CALL hdlerr(stat)
  END IF
  DEALLOCATE(vardimids)

  IF (varndims == 1) THEN
     ALLOCATE(packed1d(nland))
     stat = nf_get_var_double(ncid,varid,packed1d)
     CALL hdlerr(stat)
  ELSE IF (varndims == 2) THEN
     ALLOCATE(packed2d(nland,ntiles))
     stat = nf_get_var_double(ncid,varid,packed2d)
     CALL hdlerr(stat)
  ELSE
     ALLOCATE(packed3d(nland,ntiles,nsoil))
     stat = nf_get_var_double(ncid,varid,packed3d)
     CALL hdlerr(stat)
  END IF
  stat = nf_close(ncid)
  CALL hdlerr(stat)

  !-- unpack the array

  ALLOCATE(lmask(nlon,nlat))
  WHERE (rmask == 1._dp)
     lmask = .TRUE.
  ELSEWHERE
     lmask = .FALSE.
  ENDWHERE

  IF (varndims == 1) THEN
     ALLOCATE(unpacked2d(nlon,nlat))
     unpacked2d(:,:) = UNPACK(packed1d(:), MASK=lmask(:,:), FIELD=missval)
  ELSE IF (varndims == 2) THEN
     ALLOCATE(unpacked3d(nlon,nlat,ntiles))
     DO i = 1, ntiles
        unpacked3d(:,:,i) = UNPACK(packed2d(:,i), MASK=lmask(:,:), FIELD=missval)
     END DO
  ELSE
     ALLOCATE(unpacked4d(nlon,nlat,ntiles,nsoil))
     DO j = 1, nsoil
        DO i = 1, ntiles
           unpacked4d(:,:,i,j) = UNPACK(packed3d(:,i,j), MASK=lmask(:,:), FIELD=missval)
        END DO
     END DO
   END IF

  !-- write the output file

  stat = nf_inq_varid(ncin,'lon',lonid)
  CALL hdlerr(stat)
  ALLOCATE(lon(nlon))
  stat = nf_get_var_double(ncin,lonid,lon)
  CALL hdlerr(stat)
  stat = nf_inq_varid(ncin,'lat',latid)
  CALL hdlerr(stat)
  ALLOCATE(lat(nlat))
  stat = nf_get_var_double(ncin,latid,lat)
  CALL hdlerr(stat)

  stat = nf_create('2d_'//TRIM(array_name)//'.nc',NF_CLOBBER,ncid)
  CALL hdlerr(stat)
  stat = nf_def_dim(ncid, 'lon', nlon, idlon)
  CALL hdlerr(stat)
  stat = nf_def_dim(ncid, 'lat', nlat, idlat)
  CALL hdlerr(stat)
  IF (varndims > 1) THEN
     stat = nf_def_dim(ncid, tile_name, ntiles, idtile)
     CALL hdlerr(stat)
  END IF
  IF (varndims > 2) THEN
     stat = nf_def_dim(ncid, soil_name, nsoil, idsoil)
     CALL hdlerr(stat)
  END IF
  stat = nf_def_var(ncid, 'lon', NF_DOUBLE, 1, (/idlon/), lonid)
  CALL hdlerr(stat)
  stat = nf_def_var(ncid, 'lat', NF_DOUBLE, 1, (/idlat/), latid)
  CALL hdlerr(stat)
  IF (varndims == 1) THEN
     stat = nf_def_var(ncid, array_name, NF_DOUBLE, 2, (/idlon,idlat/), varid)
     CALL hdlerr(stat)
  ELSE IF (varndims == 2) THEN
     stat = nf_def_var(ncid, array_name, NF_DOUBLE, 3, (/idlon,idlat,idtile/), varid)
     CALL hdlerr(stat)
  ELSE
     stat = nf_def_var(ncid, array_name, NF_DOUBLE, 4, (/idlon,idlat,idtile,idsoil/), varid)
     CALL hdlerr(stat)
  END IF
  stat = nf_put_att_double(ncid, varid, '_FillValue', NF_DOUBLE, 1, missval)
  CALL hdlerr(stat)

  stat = nf_enddef(ncid)
  CALL hdlerr(stat)

  stat = nf_put_var_double(ncid,lonid,lon)
  CALL hdlerr(stat)
  stat = nf_put_var_double(ncid,latid,lat)
  CALL hdlerr(stat)
  IF (varndims == 1) THEN
     stat = nf_put_var_double(ncid,varid,unpacked2d)
     CALL hdlerr(stat)
  ELSE IF (varndims == 2) THEN
     stat = nf_put_var_double(ncid,varid,unpacked3d)
     CALL hdlerr(stat)
  ELSE
     stat = nf_put_var_double(ncid,varid,unpacked4d)
     CALL hdlerr(stat)     
  END IF
  stat = nf_close(ncid)
  CALL hdlerr(stat)
  stat = nf_close(ncin)
  CALL hdlerr(stat)

  DEALLOCATE(rmask, lmask, lon, lat)
  IF (varndims == 1) THEN
     DEALLOCATE(packed1d, unpacked2d)
  ELSE IF (varndims == 2) THEN
     DEALLOCATE(packed2d, unpacked3d)
  ELSE
    DEALLOCATE(packed3d, unpacked4d)
  END IF

END PROGRAM unpack_array

!------------------------------------------------------------------------------
!
!  Routine to handle netcdf errors
!
!------------------------------------------------------------------------------
SUBROUTINE hdlerr(stat)

  IMPLICIT NONE

  include 'netcdf.inc'

! INTENT(in)
  INTEGER,       INTENT(in) :: stat

!------------------------------------------------------------------------------

  IF (stat /= NF_NOERR) THEN
     WRITE (6,*) '--------'
     WRITE (6,*) ' ERROR:  ', nf_strerror(stat)
     WRITE (6,*) '--------'
     STOP
  END IF

END SUBROUTINE hdlerr
!------------------------------------------------------------------------------
