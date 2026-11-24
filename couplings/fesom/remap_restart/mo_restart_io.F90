module mo_restart_io
    !!=========================================================================
    !! Remapping of FESOM2 restart fields from old to new mesh
    !! when cavity geometry changes between restarts.
    !!=========================================================================
    use netcdf
    implicit none
    public :: read_restart_var_2d, read_restart_var_3d

contains
    subroutine read_restart_var_2d(dirpath, varname, data_out)
    use netcdf
    implicit none
    character(len=*), intent(in)  :: dirpath
    character(len=*), intent(in)  :: varname
    real(kind=8),     allocatable, intent(out) :: data_out(:)  ! (nnodes)

    integer            :: ncid, varid, dimid, nrecords, nnodes
    integer            :: ndims, dimids(2)
    character(len=512) :: filepath

    filepath = trim(dirpath)//'/'//trim(varname)//'.nc'
    call check( nf90_open(filepath, NF90_NOWRITE, ncid) )
    call check( nf90_inq_dimid(ncid, 'time', dimid) )
    call check( nf90_inquire_dimension(ncid, dimid, len=nrecords) )
    call check( nf90_inq_varid(ncid, varname, varid) )
    ! Resolve the node-dimension length from the variable's first dim
    ! (was previously left uninitialized, leading to garbage allocations).
    call check( nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids) )
    call check( nf90_inquire_dimension(ncid, dimids(1), len=nnodes) )
    allocate(data_out(nnodes))
    call check( nf90_get_var(ncid, varid, data_out, &
                             start = [1,              nrecords], &
                             count = [size(data_out), 1       ]) )
    call check( nf90_close(ncid) )

contains
    subroutine check(status)
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
            print *, 'NetCDF error reading '//trim(filepath)//': ', nf90_strerror(status)
            stop 1
        end if
    end subroutine check
    end subroutine read_restart_var_2d

    subroutine read_restart_var_3d(dirpath, varname, data_out)
        !! Reads last time record from <dirpath>/<varname>.nc
        !! File layout on disk: (node, nz_1, time)
        !! data_out is returned as (nz_1, node) to match mo_remap_fields convention
        use netcdf
        implicit none
        character(len=*), intent(in)               :: dirpath
        character(len=*), intent(in)               :: varname
        real(kind=8),     allocatable, intent(out) :: data_out(:,:)  ! (nz, nnodes)

        integer            :: ncid, varid, dimid, nrecords, ndims
        integer            :: dimids(4), dimlens(4)
        integer            :: nnodes, nz
        character(len=512) :: filepath
        real(kind=8), allocatable :: buf(:,:)  ! (nnodes, nz) -- disk order

        filepath = trim(dirpath)//'/'//trim(varname)//'.nc'
        call check( nf90_open(filepath, NF90_NOWRITE, ncid) )
        call check( nf90_inq_varid(ncid, varname, varid) )
        call check( nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids) )
        call check( nf90_inquire_dimension(ncid, dimids(1), len=dimlens(1)) )  ! node
        call check( nf90_inquire_dimension(ncid, dimids(2), len=dimlens(2)) )  ! nz_1
        call check( nf90_inq_dimid(ncid, 'time', dimid) )
        call check( nf90_inquire_dimension(ncid, dimid, len=nrecords) )

        nnodes = dimlens(1)
        nz     = dimlens(2)

        allocate(buf(nnodes, nz))
        call check( nf90_get_var(ncid, varid, buf, &
                                 start = [1,      1,  nrecords], &
                                 count = [nnodes, nz, 1       ]) )
        call check( nf90_close(ncid) )

        ! transpose to (nz, nnodes) as expected by mo_remap_fields
        allocate(data_out(nz, nnodes))
        data_out = transpose(buf)
        deallocate(buf)

  !  end subroutine read_restart_var_3d
contains
    subroutine check(status)
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
            print *, 'NetCDF error reading '//trim(filepath)//': ', nf90_strerror(status)
            stop 1
        end if
    end subroutine check
    end subroutine read_restart_var_3d

end module mo_restart_io
