MODULE MOD_OUTPUT
! ************************************************************************
! * Writes output files for the dEBM                                     *
! ************************************************************************
! * MOD_OUTPUT                                                           *
! *         | write_output                                               *
! ************************************************************************

 USE MOD_PRE
 USE MOD_DATA
 USE MOD_MAIN
 implicit none

contains

SUBROUTINE write_output(lon_output, lat_output, snh_output, smb_output, melt_output, refr_output, albedo_output, snow_output, rain_output)
  ! ************************************************************************
  ! * write_output produces write output files                             *
  ! * Variables includes: SNH, SMB, MELT, ACC, REFR, A                     *
  ! *                                                                      *
  ! *   varnmae      longname               units                          *
  ! *   SNH          snow height            m                              *
  ! *   SMB          surface mass balance   kg m-2 second-1                *
  ! *   ME           surface melt           kg m-2 second-1                *
  ! *   RZ           refreeze               kg m-2 second-1                *
  ! *   A            albedo                                                *
  ! *   SF           snow fall              kg m-2 second-1                *
  ! *   RF           rain fall              kg m-2 second-1                *
  ! ************************************************************************

    implicit none
    include 'netcdf.inc'

    real(kind=WP), intent(in), dimension(:,:) :: lon_output, lat_output
    real(kind=WP), intent(in), dimension(:,:,:,:) :: snh_output, smb_output, melt_output, refr_output, albedo_output, snow_output, rain_output

    character(len = *), parameter :: filename_out = "surface_mass_balance.nc"

    integer, parameter :: NDIMS = 3
    integer :: NLONS, NLATS, NTIMS
    integer :: m, n, start
    integer :: lon_dimid, lat_dimid,  tim_dimid
    integer :: t_dimid, y_dimid, x_dimid
    integer :: t_varid, y_varid, x_varid
    integer :: tim_varid, lat_varid, lon_varid
    integer :: melt_varid, snh_varid, smb_varid,&
                &refr_varid, albedo_varid,&
                &snow_varid, rain_varid

    ! Define variables name
    character(len = *), parameter :: MELT_NAME = "ME"
    character(len = *), parameter :: SNH_NAME = "SNH"
    character(len = *), parameter :: SMB_NAME = "SMB"
    character(len = *), parameter :: REFR_NAME = "RZ"
    character(len = *), parameter :: Albedo_NAME = "A"
    character(len = *), parameter :: RAIN_NAME = "RF"
    character(len = *), parameter :: SNOW_NAME = "SF"
    ! Define variables units
    character(len = *), parameter :: UNITS = "units"
    character(len = *), parameter :: LON_UNITS = "degrees_east"
    character(len = *), parameter :: LAT_UNITS = "degrees_north"
    ! character(len = *), parameter :: TIM_UNITS = "days since 0000-01-01"
    character(len = *), parameter :: MELT_UNITS = "kg m-2 second-1"
    character(len = *), parameter :: SNH_UNITS = "m"
    character(len = *), parameter :: SMB_UNITS = "kg m-2 second-1"
    character(len = *), parameter :: REFR_UNITS = "kg m-2 second-1"
    ! character(len = *), parameter :: Albedo_UNITS = ""
    character(len = *), parameter :: RAIN_UNITS = "kg m-2 second-1"
    character(len = *), parameter :: SNOW_UNITS = "kg m-2 second-1"
    ! Define variables longname
    character(len = *), parameter :: LONGNAME = "long_name"
    character(len = *), parameter :: LON_LONGNAME = "longitude"
    character(len = *), parameter :: LAT_LONGNAME = "latitude"
    character(len = *), parameter :: TIM_LONGNAME = "time"
    character(len = *), parameter :: MELT_LONGNAME = "surface melt rate"
    character(len = *), parameter :: SNH_LONGNAME = "snow height"
    character(len = *), parameter :: SMB_LONGNAME = "total surface mass balance"
    character(len = *), parameter :: REFR_LONGNAME = "refreezing rate"
    character(len = *), parameter :: Albedo_LONGNAME = "albedo"
    character(len = *), parameter :: RAIN_LONGNAME = "rain fall"
    character(len = *), parameter :: SNOW_LONGNAME = "snow fall"

    NLONS = xlen
    NLATS = ylen
    NTIMS = tlen

    ! Create the file.
    status = nf_create(filename_out, nf_clobber, ncid)
    if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive creating ",trim(filename_out)," ..."
    end if

    ! Define the dimensions.
    ! t
    status = nf_def_dim(ncid, 't', NF_UNLIMITED, t_dimid)
    if (status .ne. nf_noerr) call handle_err(status)
    ! x
    status = nf_def_dim(ncid, 'x', NLONS, x_dimid)
    if (status .ne. nf_noerr) call handle_err(status)
    ! y
    status = nf_def_dim(ncid, 'y', NLATS, y_dimid)
    if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive define dimension"
    end if

    ! Define the dimension variables
    ! t
    status = nf_def_var(ncid, 'time', NF_FLOAT, 1, t_dimid, t_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    ! x
    status = nf_def_var(ncid, 'x', NF_FLOAT, 1, x_dimid, x_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    ! y
    status = nf_def_var(ncid, 'y', NF_FLOAT, 1, y_dimid, y_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive define the dimension variables"
    end if

    ! Define the coordinate variables
    status = nf_def_var(ncid, 'lon', NF_FLOAT, 2, (/x_dimid, y_dimid/), lon_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_def_var(ncid, 'lat', NF_FLOAT, 2, (/x_dimid, y_dimid/), lat_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    ! status=nf_def_var(ncid, 'time', NF_FLOAT, 1, tim_dimid, tim_varid)
    ! if (status .ne. nf_noerr) call handle_err(status)

    ! Assign units attributes to coordinate variables.
    status = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS), LON_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS), LAT_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    ! status=nf_put_att_text(ncid, tim_varid, UNITS, len(TIM_UNITS), TIM_UNITS)
    ! if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive define lon & lat"
    end if

    ! snh
    status = nf_def_var(ncid, SNH_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), snh_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, snh_varid, UNITS, len(SNH_UNITS), SNH_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, SNH_varid, LONGNAME, len_trim(SNH_LONGNAME), trim(SNH_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! melt
    status = nf_def_var(ncid, MELT_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), melt_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, melt_varid, UNITS, len(MELT_UNITS), MELT_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, MELT_varid, LONGNAME, len_trim(MELT_LONGNAME), trim(MELT_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! smb
    status = nf_def_var(ncid, SMB_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), smb_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, smb_varid, UNITS, len(SMB_UNITS), SMB_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, SMB_varid, LONGNAME, len_trim(SMB_LONGNAME), trim(SMB_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! refr
    status = nf_def_var(ncid, REFR_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), refr_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, refr_varid, UNITS, len(REFR_UNITS), REFR_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, REFR_varid, LONGNAME, len_trim(REFR_LONGNAME), trim(REFR_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! albedo
    status = nf_def_var(ncid, Albedo_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), Albedo_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    ! status = nf_put_att_text(ncid, Albedo_varid, UNITS, len(Albedo_UNITS), Albedo_UNITS)
    ! if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, Albedo_varid, LONGNAME, len_trim(Albedo_LONGNAME), trim(Albedo_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! RAIN
    status = nf_def_var(ncid, RAIN_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), RAIN_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, RAIN_varid, UNITS, len(RAIN_UNITS), RAIN_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, RAIN_varid, LONGNAME, len_trim(RAIN_LONGNAME), trim(RAIN_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! SNOW
    status = nf_def_var(ncid, SNOW_NAME, NF_FLOAT, NDIMS, (/x_dimid, y_dimid, t_dimid/), SNOW_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, SNOW_varid, UNITS, len(SNOW_UNITS), SNOW_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, SNOW_varid, LONGNAME, len_trim(SNOW_LONGNAME), trim(SNOW_LONGNAME))
    if (status .ne. nf_noerr) call handle_err(status)

    ! Close define mode
    status = nf_enddef(ncid)
    if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive define output variables"
    end if

    ! write dimension information into the output file
    status=nf_put_vara_double(ncid, t_varid, 1, tlen, t1)
    if (status .ne. nf_noerr) call handle_err(status)
    status=nf_put_vara_double(ncid, y_varid, 1, ylen, y1)
    if (status .ne. nf_noerr) call handle_err(status)
    status=nf_put_vara_double(ncid, x_varid, 1, xlen, x1)
    if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive write dimension information"
    end if

    ! Write the coordinate variable data ( latitudes and longitudes) into the netCDF file.
    status = nf_put_vara_double(ncid, lat_varid, (/1, 1/), (/NLONS, NLATS/), lat_output)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_vara_double(ncid, lon_varid, (/1, 1/), (/NLONS, NLATS/), lon_output)
    if (status .ne. nf_noerr) call handle_err(status)
    if (debug_switch) then
      write(*,*) "survive write lon & lat"
    end if

    ! write 3D variables
    start = 1
    do n = 1, nlen
      do m = 1, mlen
         ! snh
         status = nf_put_vara_double(ncid, snh_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), snh_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         ! melt
         status = nf_put_vara_double(ncid, melt_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), melt_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         ! smb
         status = nf_put_vara_double(ncid, smb_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), smb_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         ! refr
         status = nf_put_vara_double(ncid, refr_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), refr_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         ! albedo
         status = nf_put_vara_double(ncid, albedo_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), albedo_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         ! RAIN
         status = nf_put_vara_double(ncid, RAIN_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), rain_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         ! SNOW
         status = nf_put_vara_double(ncid, SNOW_varid, (/1, 1, start/), (/NLONS, NLATS, 1/), snow_output(:, :, m, n))
         if (status .ne. nf_noerr) call handle_err(status)
         start = start + 1
       end do
    end do
    if (debug_switch) then
      write(*,*) "survive write all variables"
    end if

    ! close file
    status=nf_close(ncid)
    if (status .ne. nf_noerr) call handle_err(status)

  END SUBROUTINE write_output

END MODULE MOD_OUTPUT
