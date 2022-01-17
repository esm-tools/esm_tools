MODULE MOD_PRE
! ************************************************************************
! * Reads info from namelists                                            *
! ************************************************************************
! * MOD_PRE                                                              *
! *         | read_namelist                                              *
! ************************************************************************

  integer, parameter :: WP=8 ! Working precision

  character(len=100), parameter :: nmlfile="namelist.debm"
  real(kind=8) :: stddev, obliquity, cloud_bias,&
                     &Ans, Ads, Aws, tau_cs, residual

  character*300 :: filename_in
  character*20  :: precipitation_varname,&
                     &temperature_varname,&
                     &shortwave_radiation_downward_varname, &
                     &shortwave_radiation_TOA_varname,&
                     &cloud_cover_varname, &
                     &emissivity_varname, transmissivity_varname,&
                     &mapping_varname,&
                     &longitude_varname, latitude_varname, time_varname
  logical :: lresume, use_shortwave_radiation_TOA, debug_switch, use_mask
  integer :: debug_lon, debug_lat, debug_mon, debug_year
  integer :: hydmth_str

contains

  SUBROUTINE read_namelist
    ! ************************************************************************
    ! * read_namelist reads in namelists                                     *
    ! *                                                                      *
    ! * debm                                                                 *
    ! *       prescribed filename, variables names and tuned parameters
    ! * runctl                                                               *
    ! *       lresume: if or not restarted                                   *
    ! *       use_shortwave_radiation_TOA:                                   *
    ! *       debug_switch: debug option                                     *
    ! *       debug_lon..year: output the result of one grid point           *
    ! ************************************************************************

   namelist /debm/  filename_in,&
                    &precipitation_varname,&
                    &temperature_varname,&
                    &shortwave_radiation_downward_varname, &
                    &shortwave_radiation_TOA_varname,&
                    &cloud_cover_varname, &
                    &emissivity_varname, transmissivity_varname,&
                    &mapping_varname,&
                    &longitude_varname, latitude_varname, time_varname,&
                    &hydmth_str, stddev, obliquity, cloud_bias, &
                    &Ans, Ads, Aws, tau_cs, residual

  namelist /runctl/ lresume, use_shortwave_radiation_TOA, use_mask, &
                    &debug_switch, &
                    &debug_lon, debug_lat, debug_mon, debug_year

  ! read information from namelist
  open(10, file=nmlfile, status='old' )
  read(10, nml=runctl)
  read(10, nml=debm)
  close(10)

  if (debug_switch) then
    ! runctl
    write(*,*) "lresume",lresume
    write(*,*) "use_shortwave_radiation_TOA",use_shortwave_radiation_TOA
    write(*,*) "debug_switch",debug_switch
    write(*,*) "use_mask",use_mask

    ! debm
    write(*,*) "stddev",stddev
    write(*,*) "hydmth_str",hydmth_str
    write(*,*) "obliquity",obliquity
    write(*,*) "cloud_bias",cloud_bias
    write(*,*) "filename_in:",trim(filename_in)
    write(*,*) "precipitation_varname:",precipitation_varname
    write(*,*) "temperature_varname:",temperature_varname
    write(*,*) "shortwave_radiation_downward_varname:",shortwave_radiation_downward_varname
    write(*,*) "shortwave_radiation_TOA_varname:",shortwave_radiation_TOA_varname
    write(*,*) "emissivity_varname:",emissivity_varname
    write(*,*) "transmissivity_varname:",transmissivity_varname
    write(*,*) "mapping_varname:",mapping_varname
    write(*,*) "longitude_varname:",longitude_varname
    write(*,*) "latitude_varname:",transmissivity_varname
  end if

 END SUBROUTINE read_namelist

END MODULE MOD_PRE
