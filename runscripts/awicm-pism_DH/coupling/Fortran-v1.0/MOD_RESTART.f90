MODULE MOD_RESTART
  ! ************************************************************************
  ! * Writes and reads restart files                            *
  ! ************************************************************************
  ! * MOD_RESTART                                                          *
  ! *         | write_restart                                              *
  ! *         | read_restart                                               *
  ! ************************************************************************

 USE MOD_PRE
 USE MOD_DATA
 USE MOD_MAIN
 implicit none

 character(len = *), parameter :: restart_filename = "restart_debm.nc"
 character(len = *), parameter :: SNH_Sep_NAME = "SNH_Sep"
 character(len = *), parameter :: SNH_Dec_NAME = "SNH_Dec"

contains


SUBROUTINE write_restart(SNH_Sep, SNH_Dec)
  ! TODO：currently we only keep the minmal variables
  ! ************************************************************************
  ! * write_restart produces restart files                                 *
  ! *                                                                      *
  ! * Variables includes: SNH_Sep, SNH_Dec                                 *
  ! *   SNH_Sep : snow height of September                                  *
  ! *   SNH_Dec : ssnow height of December                                *
  ! ************************************************************************

    implicit none
    include 'netcdf.inc'

    real(kind=WP), intent(in), dimension(:,:)   :: SNH_Sep, SNH_Dec ! snow height of December and September will be saved

    integer :: NLONS, NLATS, NTIMS
    integer :: m, n, start
    integer :: y_dimid, x_dimid
    integer :: y_varid, x_varid
    integer :: SNH_Sep_varid, SNH_Dec_varid

    character(len = *), parameter :: UNITS = "units"
    character(len = *), parameter :: SNH_Sep_UNITS = "mm"
    character(len = *), parameter :: SNH_Dec_UNITS = "mm"

    write(*,*) " "
    write(*,*) "Start writing restart files"

    NLONS = xlen
    NLATS = ylen

    ! Create the file.
    status = nf_create(restart_filename, nf_clobber, ncid)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Define the dimensions.
    status = nf_def_dim(ncid, 'x', NLONS, x_dimid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_def_dim(ncid, 'y', NLATS, y_dimid)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Define the dimension variables
    status = nf_def_var(ncid, 'x', NF_FLOAT, 1, x_dimid, x_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_def_var(ncid, 'y', NF_FLOAT, 1, y_dimid, y_varid)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Define the coordinate variables
    status = nf_def_var(ncid, SNH_Sep_NAME, NF_FLOAT, 2, (/x_dimid, y_dimid/), SNH_Sep_varid)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_def_var(ncid, SNH_Dec_NAME, NF_FLOAT, 2, (/x_dimid, y_dimid/), SNH_Dec_varid)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Assign units attributes to coordinate variables.
    status = nf_put_att_text(ncid, SNH_Sep_varid, UNITS, len(SNH_Sep_UNITS), SNH_Sep_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_att_text(ncid, SNH_Dec_varid, UNITS, len(SNH_Dec_UNITS), SNH_Dec_UNITS)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Close define mode
    status = nf_enddef(ncid)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Write the data.
    status=nf_put_vara_double(ncid, y_varid, 1, ylen, y1)
    if (status .ne. nf_noerr) call handle_err(status)
    status=nf_put_vara_double(ncid, x_varid, 1, xlen, x1)
    if (status .ne. nf_noerr) call handle_err(status)

    ! Write the coordinate variable data SNH_Sep and SNH_Dec into the netCDF file.
    status = nf_put_vara_double(ncid, SNH_Sep_varid, (/1, 1/), (/NLONS, NLATS/), SNH_Sep)
    if (status .ne. nf_noerr) call handle_err(status)
    status = nf_put_vara_double(ncid, SNH_Dec_varid, (/1, 1/), (/NLONS, NLATS/), SNH_Dec)
    if (status .ne. nf_noerr) call handle_err(status)

    ! close file
    status=nf_close(ncid)
    if (status .ne. nf_noerr) call handle_err(status)

    write(*,*) " "
    write(*,*) "Finish writing restart files"

  END SUBROUTINE write_restart


  SUBROUTINE read_restart(SNH_Sepr, SNH_Decr)
    ! ************************************************************************
    ! * read_restart produces restart files                                  *
    ! *                                                                      *
    ! * Variables includes: SNH_Sep, SNH_Dec                                 *
    ! *   SNH_Sep : snow height of September                                  *
    ! *   SNH_Dec : ssnow height of December                                *
    ! ************************************************************************

      implicit none
      include 'netcdf.inc'

      integer :: i,j
      integer :: y_dimid, x_dimid
      integer :: y_varid, x_varid
      integer :: xlen0, ylen0
      real(kind=WP), dimension(:), allocatable :: x0, y0
      real(kind=WP), intent(out), dimension(:,:), allocatable :: SNH_Sepr, SNH_Decr ! snow height of December and September will be saved

      write(*,*) "Start reading restart files"

      ! open file.
      status = nf_open(trim(restart_filename), nf_nowrite, ncid)
      if (status .ne. nf_noerr) then
        write(*,*) "cannot oepn ",trim(restart_filename)," ..."
        call handle_err(status)
      end if

      ! get the dimensions to check if restart files matches input
      ! x
      status = nf_inq_dimid(ncid, 'x', x_dimid)
      status = nf_inq_dimlen(ncid, x_dimid, xlen0)
      status = nf_inq_varid(ncid, 'x', x_varid)
      allocate(x0(xlen0))
      status = nf_get_vara_double(ncid, x_varid, 1, xlen0, x0)
      if (status .ne. nf_noerr) call handle_err(status)
      ! y
      status = nf_inq_dimid(ncid, 'y', y_dimid)
      status = nf_inq_dimlen(ncid, y_dimid, ylen0)
      status = nf_inq_varid(ncid, 'y', y_varid)
      allocate(y0(ylen0))
      status = nf_get_vara_double(ncid, y_varid, 1, ylen0, y0)
      if (status .ne. nf_noerr) call handle_err(status)
      ! check if restart files matches input
      if ((xlen0 /= xlen) .OR. (ylen0 /= ylen)) then
        write(*,*) "restart files didn't match input files"
        write(*,*) "mesh grid mismatch"
        write(*,*) " input mesh is ", xlen, "x", ylen
        write(*,*) " restart mesh  is ", xlen, "x", ylen
        stop "error detected!"
      else
        do i = 1, xlen0
          do j = 1, ylen0
            if ((x0(i) /= x1(i)) .OR. (y0(j) /= y1(j))) then
              write(*,*) "restart files didn't match input files"
              write(*,*) "mesh grid mismatch", i, j
              write(*,*) " input lon is ", x0(i), "lat is", y0(j)
              write(*,*) " input lon is ", x1(i), "lat is", y1(j)
              stop "error detected!"
            end if
          end do
        end do
      end if

      ! get SNH_Sep
      allocate (SNH_Sepr(xlen0,ylen0))
      status = nf_inq_varid(ncid, SNH_Sep_NAME, varid)
      if (status .ne. nf_noerr) call handle_err(status)
      status = nf_get_vara_double(ncid, varid, (/1, 1/), (/xlen0, ylen0/), SNH_Sepr)
      if (status .ne. nf_noerr) call handle_err(status)

      ! get SNH_Dec
      allocate (SNH_Decr(xlen0,ylen0))
      status = nf_inq_varid(ncid, SNH_Dec_NAME, varid)
      status = nf_get_vara_double(ncid, varid, (/1, 1/), (/xlen0, ylen0/), SNH_Decr)
      if (status .ne. nf_noerr) call handle_err(status)

      ! close file
      status=nf_close(ncid)
      if (status .ne. nf_noerr) call handle_err(status)

      write(*,*) "Finish reading restart files"

    END SUBROUTINE read_restart

END MODULE MOD_RESTART
