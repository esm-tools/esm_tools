MODULE MOD_DATA
! ************************************************************************
! * Reads the input atmospheric fields for dEBM                          *
! *    | get_init                                                        *
! ************************************************************************
! * MOD_DATA                                                             *
! *    | get_init                                                        *
! *         | get_dimension                                              *
! *         | get_shortwave_radiation_downward                           *
! *         | get_shortwave_radiation_TOA                                *
! *         | get_surface_temperature                                    *
! *         | get_precipitation                                          *
! *         | get_cloud_cover                                            *
! *         | get_emissivity                                             *
! *         | get_transmissivity                                         *
! *         | get_mask                                                   *
! *    | handle_err                                                      *
! *         detect error in reading initial fields                       *
! ************************************************************************

  USE MOD_PRE
  implicit none

  real(kind=WP), dimension(:,:,:,:), allocatable ::  shortwave_radiation_downward1, shortwave_radiation_TOA1,&
                                            &precipitation1,&
                                            &surface_temperature1,&
                                            &cloud_cover1, emissivity1, transmissivity1
  real(kind=WP), dimension(:,:,:,:), allocatable :: lat
  real(kind=WP), dimension(:), allocatable	  :: x1, y1, t1
  real(kind=WP), dimension(:,:), allocatable :: lon0, lat0

  logical, dimension(:,:,:), allocatable :: mask1

  integer 		:: status, ncid, varid
  integer			:: tlen, ylen, xlen
  integer			:: mlen, nlen
  integer			:: istart(3), icount(3)
  integer 		:: i, j, k

contains


  SUBROUTINE get_init
    ! ************************************************************************
    ! * get_init reads in atmosphere fields and ice-mask                     *
    ! * note that this subroutine is specifically generated for AWICM-PISM   *
    ! * for other input, units needs to be carefully checked                 *
    ! *                                                                      *
    ! * atmosphere fields include :                                          *
    ! *   variable name                       units                          *
    ! *   shortwave_radiation_downward        W/m2                           *
    ! *   shortwave_radiation_TOA (optional)  W/m2                           *
    ! *   surface_temperature                 K or degC                      *
    ! *   precipitation                       kg m-2 second-1 or mm day-1    *
    ! *   cloud_cover                         %                              *
    ! *   emissivity                          %                              *
    ! *   transmissivity                      %                              *
    ! *   mask (optional)                                                    *
    ! ************************************************************************
    implicit none
    include 'netcdf.inc'

    ! open file
    status = nf_open(trim(filename_in), nf_nowrite, ncid)
    if (status .ne. nf_noerr) then
      print*,'ERROR: CANNOT READ init_data FILE CORRECTLY !!!!!'
      print*,'Error in opening netcdf file'//trim(filename_in)
      call handle_err(status)
    end if

    ! get dimension x,y,t & lon,lat
    CALL get_dimension
    mlen=12        ! length of month
    nlen=tlen/mlen ! length of year

    write(*,*) " "
    write(*,*) "WARNING:"
    write(*,*) "This scheme is specifically generated for AWI-ESM-2.2-LR,"
    write(*,*) "for other input, units needs to be carefully checked!!!"
    write(*,*) " "

    ! inflating latitude into four dimensions
    allocate (lat(xlen,ylen,mlen,nlen))
    do i = 1, mlen
        do j = 1, nlen
           lat(:,:,i,j) = lat0
        end do
    end do

    ! determine if debug option is on
    if (debug_switch) then
      if ((debug_lon>xlen) .OR. (debug_lat>ylen)) then
        write(*,*) "debug lon or lat is out of range.."
        write(*,*) "plz turn of debug option by set debug_switch=.false."
        debug_switch=.false.
      end if
    end if

   ! get get_shortwave_radiation_downward
    CALL get_shortwave_radiation_downward

    ! get shortwave_radiation_TOA
    if (use_shortwave_radiation_TOA) then
      CALL get_shortwave_radiation_TOA
    else
      write(*,*) "not incoprate shortwave_radiation_TOA..."
      allocate (shortwave_radiation_TOA1(xlen,ylen,mlen,nlen))
      shortwave_radiation_TOA1=0.
    end if

    ! get get_surface_temperature
    CALL get_surface_temperature

    ! get get_precipitation
    CALL get_precipitation

    ! get get_cloud_cover
    CALL get_cloud_cover

    ! get get_emissivity
    CALL get_emissivity

    ! get get_transmissivity
    CALL get_transmissivity

    ! mask
    if (use_mask) then
      CALL get_mask
    else
      write(*,*) "do not use mask..."
      allocate (mask1(xlen, ylen, tlen))
      mask1 = .True.
    end if

    ! close
    status=nf_close(ncid)

    END SUBROUTINE get_init


    SUBROUTINE get_dimension
      ! ************************************************************************
      ! get_dimension x & y & t and lon & lat
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'
      integer			:: timeid, lonid, latid

      ! y
      status=nf_inq_dimid(ncid, 'y', latid)
      status=nf_inq_dimlen(ncid, latid, ylen)
      allocate(y1(ylen))
      status=nf_inq_varid(ncid, 'y', varid)
      status=nf_get_vara_double(ncid,varid,1,ylen,y1)

      ! x
      status=nf_inq_dimid(ncid, 'x', lonid)
      status=nf_inq_dimlen(ncid, lonid, xlen)
      allocate(x1(xlen))
      status=nf_inq_varid(ncid, 'x', varid)
      status=nf_get_vara_double(ncid,varid,1,xlen,x1)

      ! time
      status=nf_inq_dimid(ncid, 'time', timeid)  !inquire var
      status=nf_inq_dimlen(ncid, timeid, tlen)
      allocate(t1(tlen))
      status=nf_inq_varid(ncid, 'time', varid)
      status=nf_get_vara_double(ncid,timeid,1,tlen,t1)

      ! lat
      allocate (lat0(xlen,ylen))
      status=nf_inq_varid(ncid, latitude_varname, varid)  !inquire var
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(latitude_varname)
         call handle_err(status)
      end if
      status=nf_get_vara_double(ncid, varid, (/1,1/), (/xlen,ylen/), lat0)

      ! lon
      allocate (lon0(xlen,ylen))
      status=nf_inq_varid(ncid, longitude_varname, varid)  !inquire var
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(longitude_varname)
         call handle_err(status)
      end if
      status=nf_get_vara_double(ncid, varid, (/1,1/), (/xlen,ylen/), lon0)

    END SUBROUTINE get_dimension


    SUBROUTINE get_shortwave_radiation_downward
      ! ************************************************************************
      ! *   shortwave_radiation_downward        W/m2                           *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      character(len = 20) :: shortwave_radiation_downward_units
      real(kind=WP), dimension(:,:,:), allocatable :: shortwave_radiation_downward

      ! read
      status = nf_inq_varid(ncid, shortwave_radiation_downward_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(shortwave_radiation_downward_varname)
         call handle_err(status)
      end if
      allocate (shortwave_radiation_downward(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,shortwave_radiation_downward(:,:,k))
      end do

      ! reshape
      allocate (shortwave_radiation_downward1(xlen,ylen,mlen,nlen))
      shortwave_radiation_downward1 = reshape( shortwave_radiation_downward, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(shortwave_radiation_downward)

    END SUBROUTINE get_shortwave_radiation_downward


    SUBROUTINE get_shortwave_radiation_TOA
      ! ************************************************************************
      ! *   shortwave_radiation_TOA (optional)  W/m2                           *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      character(len = 20) :: precipitation_units
      real(kind=WP), dimension(:,:,:), allocatable :: shortwave_radiation_TOA

      ! read
      status = nf_inq_varid(ncid, shortwave_radiation_TOA_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(shortwave_radiation_TOA_varname)
         call handle_err(status)
      end if
      allocate (shortwave_radiation_TOA(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,shortwave_radiation_TOA(:,:,k))
      end do

      ! reshape
      allocate (shortwave_radiation_TOA1(xlen,ylen,mlen,nlen))
      shortwave_radiation_TOA1 = reshape( shortwave_radiation_TOA, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(shortwave_radiation_TOA)

    END SUBROUTINE get_shortwave_radiation_TOA


    SUBROUTINE get_surface_temperature
      ! ************************************************************************
      ! *   surface_temperature                 K or degC                      *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      character(len = 20) :: surface_temperature_units
      real(kind=WP), dimension(:,:,:), allocatable :: surface_temperature

      ! read
      status=nf_inq_varid(ncid, temperature_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(temperature_varname)
         call handle_err(status)
      end if
      allocate (surface_temperature(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,surface_temperature(:,:,k))
      end do

      ! units check
      status=nf_get_att_text(ncid, varid, "units", surface_temperature_units)
      if (status .ne. nf_noerr) then
          write(*,*) 'WARNING: units of surface_temperature not found'
          write(*,*) 'Take K as default units'
          write(*,*) 'Possibly wrong result! Plz double check your result!'
          write(*,*) 'expected units for surface temperature: K or degC'
          surface_temperature = surface_temperature - 273.15
      else
          if (surface_temperature_units=='K') then
                  write(*,*) 'input surface_temperature units is K'
                  surface_temperature = surface_temperature - 273.15
          elseif (surface_temperature_units=='degC') then
                  write(*,*) 'input surface_temperature units is degC'
          else
                  write(*,*) 'WARNING: Invalid units of surface temperature'
                  write(*,*) 'Invalid units',surface_temperature_units
                  write(*,*) 'Expected units: K or degC'
                  write(*,*) 'Here we will take K as default units'
                  write(*,*) 'Possibly wrong result! Plz double check your result!'
                  surface_temperature = surface_temperature - 273.15
          end if
      end if

      ! reshape
      allocate (surface_temperature1(xlen,ylen,mlen,nlen))
      surface_temperature1 = reshape( surface_temperature, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(surface_temperature)
    END SUBROUTINE get_surface_temperature


    SUBROUTINE get_precipitation
      ! ************************************************************************
      ! *   precipitation                       kg m-2 second-1 or mm day-1    *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      character(len = 20) :: precipitation_units
      real(kind=WP), dimension(:,:,:), allocatable :: precipitation

      ! read
      status=nf_inq_varid(ncid, precipitation_varname, varid)
      icount= (/xlen,ylen,1/)
      allocate (precipitation(xlen, ylen, tlen))
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,precipitation(:,:,k))
      end do

      ! units check
      status=nf_get_att_text(ncid, varid, "units", precipitation_units)
      if (status.ne.nf_noerr) then
          write(*,*) 'WARNING: units of precipitation not found'
          write(*,*) 'Take kg m-2 second-1 as default units'
          write(*,*) 'Possibly wrong result! Plz double check your result!'
          write(*,*) 'expected units for precipitation: kg m-2 second-1,&
                  &or mm day-1'
          precipitation = precipitation*24.*60.*60.
      else
          if (precipitation_units=='kg m-2 second-1') then
                  write(*,*) 'input precipitation units is kg m-2 second-1'
                  precipitation = precipitation*24.*60.*60.
          elseif (precipitation_units=='mm day-1') then
                  write(*,*) 'input precipitation units is mm day-1'
          else
                  write(*,*) 'WARNING: Invalid units of precipitation'
                  write(*,*) 'Invalid units',precipitation_units
                  write(*,*) 'Expected units: "kg m-2 second-1" or "mm day-1"'
                  write(*,*) 'Here we will take "kg m-2 second-1" as default units'
                  write(*,*) 'Possibly wrong result! Plz double check your result!'
                  precipitation = precipitation*24.*60.*60.
          end if
      end if

      ! reshape
      allocate (precipitation1(xlen,ylen,mlen,nlen))
      precipitation1 = reshape( precipitation, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(precipitation)

    END SUBROUTINE get_precipitation


    SUBROUTINE get_cloud_cover
      ! ************************************************************************
      ! *   cloud_cover                         %                              *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      real(kind=WP), dimension(:,:,:), allocatable :: cloud_cover

      ! read
      status = nf_inq_varid(ncid, cloud_cover_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(cloud_cover_varname)
         call handle_err(status)
      end if
      allocate (cloud_cover(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,cloud_cover(:,:,k))
      end do

      ! reshape
      allocate (cloud_cover1(xlen,ylen,mlen,nlen))
      cloud_cover1 = reshape(cloud_cover, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(cloud_cover)

    END SUBROUTINE get_cloud_cover


    SUBROUTINE get_emissivity
      ! ************************************************************************
      ! *   emissivity                                                         *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      real(kind=WP), dimension(:,:,:), allocatable :: emissivity

      ! read
      status = nf_inq_varid(ncid, emissivity_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(emissivity_varname)
         call handle_err(status)
      end if
      allocate (emissivity(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,emissivity(:,:,k))
      end do

      ! reshape
      allocate (emissivity1(xlen,ylen,mlen,nlen))
      emissivity1 = reshape( emissivity, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(emissivity)

    END SUBROUTINE get_emissivity


    SUBROUTINE get_transmissivity
      ! ************************************************************************
      ! *   transmissivity                                                     *
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      real(kind=WP), dimension(:,:,:), allocatable :: transmissivity

      ! read
      status = nf_inq_varid(ncid, transmissivity_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(transmissivity_varname)
         call handle_err(status)
      end if
      allocate (transmissivity(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,transmissivity(:,:,k))
      end do

      ! reshape
      allocate (transmissivity1(xlen,ylen,mlen,nlen))
      transmissivity1 = reshape( transmissivity, (/xlen,ylen,mlen,nlen/) )

      ! deallocate
      deallocate(transmissivity)

    END SUBROUTINE get_transmissivity


    SUBROUTINE get_mask
      ! ************************************************************************
      ! mask
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      real(kind=WP), dimension(:,:,:), allocatable :: mask

      ! read
      status = nf_inq_varid(ncid, mapping_varname, varid)
      if (status .ne. nf_noerr) then
         write(*,*) 'error by getting varid for ',trim(mapping_varname)
         call handle_err(status)
      end if
      allocate (mask(xlen, ylen, tlen))
      icount= (/xlen,ylen,1/)
      do k = 1, tlen
        istart = (/1,1,k/)
        status=nf_get_vara_double(ncid,varid,istart,icount,mask(:,:,k))
      end do

      ! reshape
      allocate (mask1(xlen, ylen, tlen))
      mask1 =  (mask(:,:,:) > 5)

      ! deallocate
      deallocate(mask)

    END SUBROUTINE get_mask


    SUBROUTINE handle_err(errcode)
      ! ************************************************************************
      ! handle error in read or write netcdf files
      ! ************************************************************************

      implicit none
      include 'netcdf.inc'
      integer :: errcode
      !
      print *, 'Error: ', nf_strerror(errcode)
      stop "stopped"

    END SUBROUTINE handle_err


END MODULE MOD_DATA
