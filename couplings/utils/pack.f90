!------------------------------------------------------------------------------
!
! program reading an unpacked array and a corresponding 2d-mask and writes the
! packed array
!
! Compilation:
!
!linux-x64:
! module load nag
! nagfor -colour -nan -gline  -O0 -C=all -w=uep pack.f90 -o pack  -I/sw/jessie-x64/netcdf_fortran-4.4.2-static-nag60/include  -L/sw/jessie-x64/netcdf_fortran-4.4.2-static-nag60/lib -lnetcdff  -L/sw/jessie-x64/netcdf-4.3.3.1-static-gccsys/lib/ -lnetcdf  -L/sw/jessie-x64/hdf5-1.8.16-static-gccsys/lib -lhdf5_hl -lhdf5 -ldl  -L/sw/jessie-x64/szip-2.1-static-gccsys/lib -lsz -lz

!mistral:
! module load nag
! export LD_LIBRARY_PATH="/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib:/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-gcc48/lib:/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib"
!
! nagfor -colour '-maxcontin=100' '-wmismatch=define_var_new,nf_get_var_double,nf_copy_att,nf_get_var_real,nf_put_var_double,nf_def_var,nf_put_vara_double,dgemm,nfmpi_def_dim,nfmpi_put_vara_double,nfmpi_def_var' '-C=all' -g -gline -nan '-w=uep' -o pack ./pack.f90 -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-static-nag60/include -L/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-static-nag60/lib -lnetcdff -L/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-gcc48/lib -lnetcdf -L/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib -lhdf5_hl -lhdf5 -ldl -L/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib -lsz -L/usr/lib64/lib -lz -ldl -lm -lnuma -Wl,--export-dynamic -lrt -lnsl -lutil -lm -ldl
!------------------------------------------------------------------------------
PROGRAM pack_array

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307)
  INTEGER :: i, j, nlon, nlat, ntiles, nland, nsoil, nnlon, nnlat
  INTEGER :: stat, ncid, ncin, varid, landid, tileid, soilid, varndims, idtile, idland, idsoil
  INTEGER, ALLOCATABLE :: vardimids(:)

  REAL(dp), ALLOCATABLE :: packed1d(:)
  REAL(dp), ALLOCATABLE :: packed2d(:,:)
  REAL(dp), ALLOCATABLE :: packed3d(:,:,:)
  REAL(dp), ALLOCATABLE :: unpacked2d(:,:)
  REAL(dp), ALLOCATABLE :: unpacked3d(:,:,:)
  REAL(dp), ALLOCATABLE :: unpacked4d(:,:,:,:)
  REAL(dp), ALLOCATABLE :: landpoints(:)
  REAL(dp), ALLOCATABLE :: tiles(:)
  REAL(dp), ALLOCATABLE :: rmask(:,:)
  LOGICAL,  ALLOCATABLE :: lmask(:,:)

  CHARACTER*100 :: array_name, mask_name, tile_name,  soil_name

  PRINT*, 'enter array file name (without .nc)'
  READ*, array_name
  PRINT*, 'enter mask file name (without .nc)'
  READ*, mask_name

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

  !-- read unpacked array from file (array and file have the same names)

  stat = nf_open(TRIM(array_name)//'.nc',NF_NOWRITE,ncid)
  CALL hdlerr(stat)
  stat = nf_inq_varid(ncid,array_name,varid)
  CALL hdlerr(stat)

  stat = nf_inq_varndims(ncid,varid,varndims)
  CALL hdlerr(stat)
  ALLOCATE(vardimids(varndims))
  stat = nf_inq_vardimid(ncid,varid,vardimids)
  CALL hdlerr(stat)
  stat = nf_inq_dimlen(ncid,vardimids(1),nnlon)
  CALL hdlerr(stat)
  IF (nnlon /= nlon) PRINT*, 'Dimensions of mask and data files do not match: ', nlon, nnlon 
  stat = nf_inq_dimlen(ncid,vardimids(2),nnlat)
  CALL hdlerr(stat)
  IF (nnlon /= nlon) PRINT*, 'Dimensions of mask and data files do not match: ', nlat, nnlat 
  IF (varndims > 2) THEN
     stat = nf_inq_dimlen(ncid,vardimids(3),ntiles)
     CALL hdlerr(stat)
     stat = nf_inq_dimname(ncid,vardimids(3),tile_name)
     CALL hdlerr(stat)
  END IF
  IF (varndims > 3) THEN
     stat = nf_inq_dimlen(ncid,vardimids(4),nsoil)
     CALL hdlerr(stat)
     stat = nf_inq_dimname(ncid,vardimids(4),soil_name)
     CALL hdlerr(stat)
  END IF
  DEALLOCATE(vardimids)

  nland=SUM(rmask(:,:))
  IF (varndims == 2) THEN
     ALLOCATE(unpacked2d(nlon,nlat))
     stat = nf_get_var_double(ncid,varid,unpacked2d)
     CALL hdlerr(stat)
  ELSE IF (varndims == 3) THEN
     ALLOCATE(unpacked3d(nlon,nlat,ntiles))
     stat = nf_get_var_double(ncid,varid,unpacked3d)
     CALL hdlerr(stat)
  ELSE IF (varndims == 4) THEN
     ALLOCATE(unpacked4d(nlon,nlat,ntiles,nsoil))
     stat = nf_get_var_double(ncid,varid,unpacked4d)
     CALL hdlerr(stat)
  ELSE
     STOP ('maximum number of dimensions is 4!')  
  END IF
  stat = nf_close(ncid)
  CALL hdlerr(stat)

  !-- pack the array

  ALLOCATE(lmask(nlon,nlat))
  WHERE (rmask == 1._dp)
     lmask = .TRUE.
  ELSEWHERE
     lmask = .FALSE.
  ENDWHERE

  IF (varndims == 2) THEN
     ALLOCATE(packed1d(nland))
     packed1d(:) = PACK(unpacked2d(:,:), MASK=lmask(:,:))
  ELSE IF (varndims == 3) THEN
     ALLOCATE(packed2d(nland,ntiles))
     DO i = 1, ntiles
        packed2d(:,i) = PACK(unpacked3d(:,:,i), MASK=lmask(:,:))
     END DO
  ELSE IF (varndims == 4) THEN
     ALLOCATE(packed3d(nland,ntiles,nsoil))
     DO j = 1, nsoil
        DO i = 1, ntiles
           packed3d(:,i,j) = PACK(unpacked4d(:,:,i,j), MASK=lmask(:,:))
        END DO
     END DO
  END IF

  !-- write the output file

  !-- dimensions
  stat = nf_create('1d_'//TRIM(array_name)//'.nc',NF_CLOBBER,ncid)
  CALL hdlerr(stat)
  stat = nf_def_dim(ncid, 'landpoint', nland, idland)
  CALL hdlerr(stat)
  IF (varndims > 2) THEN
     stat = nf_def_dim(ncid, tile_name, ntiles, idtile)
     CALL hdlerr(stat)
  END IF
  IF (varndims > 3) THEN
     stat = nf_def_dim(ncid, soil_name, nsoil, idsoil)
     CALL hdlerr(stat)
  END IF

  stat = nf_def_var(ncid, 'landpoint', NF_DOUBLE, 1, (/idland/), landid)
  CALL hdlerr(stat)
  IF (varndims > 2) THEN
     stat = nf_def_var(ncid, tile_name, NF_DOUBLE, 1, (/idtile/), tileid)
     CALL hdlerr(stat)
  END IF
  IF (varndims > 3) THEN
     stat = nf_def_var(ncid, soil_name, NF_DOUBLE, 1, (/idsoil/), soilid)
     CALL hdlerr(stat)
  END IF

  IF (varndims == 2) THEN
     stat = nf_def_var(ncid, array_name, NF_DOUBLE, 1, (/idland/), varid)
     CALL hdlerr(stat)
  ELSE IF (varndims == 3) THEN
     stat = nf_def_var(ncid, array_name, NF_DOUBLE, 2, (/idland,idtile/), varid)
     CALL hdlerr(stat)
  ELSE IF (varndims == 4) THEN
     stat = nf_def_var(ncid, array_name, NF_DOUBLE, 3, (/idland,idtile,idsoil/), varid)
     CALL hdlerr(stat)
  END IF
  stat = nf_enddef(ncid)
  CALL hdlerr(stat)

  !-- dimension variables
  ALLOCATE(landpoints(nland))
  DO i = 1, nland
     landpoints(i)=i
  END DO
  stat = nf_put_var_double(ncid,landid,landpoints)
  DEALLOCATE(landpoints)
  CALL hdlerr(stat)
  IF (varndims > 2) THEN  
     ALLOCATE(tiles(ntiles))
     DO i = 1, ntiles
        tiles(i)=i
     END DO
     stat = nf_put_var_double(ncid,tileid,tiles)
     CALL hdlerr(stat)
     DEALLOCATE(tiles)
  END IF

  !-- packed data array
  IF (varndims == 2) THEN
     stat = nf_put_var_double(ncid,varid,packed1d)
     CALL hdlerr(stat)
  ELSE IF (varndims == 3) THEN
     stat = nf_put_var_double(ncid,varid,packed2d)
     CALL hdlerr(stat)
  ELSE IF (varndims == 4) THEN
     stat = nf_put_var_double(ncid,varid,packed3d)
     CALL hdlerr(stat)
  END IF
  stat = nf_close(ncid)
  CALL hdlerr(stat)
  stat = nf_close(ncin)
  CALL hdlerr(stat)

  DEALLOCATE(rmask, lmask)
  IF (varndims == 2) THEN
     DEALLOCATE(packed1d, unpacked2d)
  ELSE IF (varndims == 3) THEN
     DEALLOCATE(packed2d, unpacked3d)
  ELSE IF (varndims == 4) THEN
     DEALLOCATE(packed3d, unpacked4d)
  END IF

END PROGRAM pack_array

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
