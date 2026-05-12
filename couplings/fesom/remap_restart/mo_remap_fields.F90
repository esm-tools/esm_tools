module mo_remap_fields
    !!=========================================================================
    !! Remapping of FESOM2 restart fields from old to new mesh
    !! when cavity geometry changes between restarts.
    !!=========================================================================
    use mo_remap_mesh
    use mo_restart_io
    use netcdf
    implicit none
    private

    !___________________________________________________________________________
    ! node classification flags
    integer, parameter :: FLAG_UNCHANGED     =  0
    integer, parameter :: FLAG_VERT_EXTENDED =  1
    integer, parameter :: FLAG_NEW_NODE      =  2
    integer, parameter :: FLAG_DROPPED       = -1

    public :: classify_nodes, remap_all_restarts

contains

    !===========================================================================
    subroutine classify_nodes(mesh_old, mesh_new, node_flag)
        type(t_mesh_remap), intent(in)  :: mesh_old, mesh_new
        integer, allocatable,intent(out):: node_flag(:)

        integer :: i_new, i_old, n_base
        integer :: cnt_unchanged, cnt_vert, cnt_new, cnt_drop

        allocate(node_flag(mesh_new%nod2D))
        node_flag    = FLAG_UNCHANGED
        cnt_unchanged = 0
        cnt_vert      = 0
        cnt_new       = 0
        cnt_drop      = 0

        !_______________________________________________________________________
        do i_new = 1, mesh_new%nod2D
            n_base = mesh_new%nod_map(i_new)
            i_old  = mesh_old%map_base_to_mesh(n_base)

            if (i_old == 0) then
                ! node not present in old mesh at all
                node_flag(i_new) = FLAG_NEW_NODE
                cnt_new = cnt_new + 1

            else if (mesh_new%ulevels_nod2D(i_new) < mesh_old%ulevels_nod2D(i_old) .or. &
                     mesh_new%nlevels_nod2D(i_new) > mesh_old%nlevels_nod2D(i_old)) then
                ! node exists in old mesh but has more active levels in new mesh
                node_flag(i_new) = FLAG_VERT_EXTENDED
                cnt_vert = cnt_vert + 1

            else
                node_flag(i_new) = FLAG_UNCHANGED
                cnt_unchanged = cnt_unchanged + 1
            end if
        end do

        !_______________________________________________________________________
        ! count dropped nodes
        do i_old = 1, mesh_old%nod2D
            n_base = mesh_old%nod_map(i_old)
            if (mesh_new%map_base_to_mesh(n_base) == 0) cnt_drop = cnt_drop + 1
        end do

        write(*,*) ' --> node classification:'
        write(*,*) '     unchanged         : ', cnt_unchanged
        write(*,*) '     vertically extended: ', cnt_vert
        write(*,*) '     entirely new       : ', cnt_new
        write(*,*) '     dropped            : ', cnt_drop

    end subroutine classify_nodes

    !===========================================================================
    ! For a given node i_new and level k in the new mesh, find the nearest
    ! node in the old mesh that has valid data at level k.
    ! Uses great-circle distance.
    subroutine find_nearest_old_node_at_level(i_new, k, mesh_old, mesh_new, i_ref)
        integer,            intent(in)  :: i_new, k
        type(t_mesh_remap), intent(in)  :: mesh_old, mesh_new
        integer,            intent(out) :: i_ref

        integer  :: j
        real(WP) :: dist, dist_min
        real(WP) :: lon_new, lat_new, lon_j, lat_j, alpha

        real(WP), parameter :: r_earth = 6371000.0_WP

        lon_new  = mesh_new%coord(1, i_new)
        lat_new  = mesh_new%coord(2, i_new)
        dist_min = huge(1.0_WP)
        i_ref    = -1

        do j = 1, mesh_old%nod2D
            ! check if level k is valid at node j in old mesh
            if (mesh_old%ulevels_nod2D(j) <= k .and. &
                mesh_old%nlevels_nod2D(j)-1 >= k) then

                lon_j = mesh_old%coord(1, j)
                lat_j = mesh_old%coord(2, j)
                alpha = acos(max(-1.0_WP, min(1.0_WP, &
                        cos(lat_new)*cos(lat_j)*cos(lon_new-lon_j) + &
                        sin(lat_new)*sin(lat_j))))
                dist = r_earth * abs(alpha)

                if (dist < dist_min) then
                    dist_min = dist
                    i_ref    = j
                end if
            end if
        end do

        if (i_ref < 0) then
            write(*,*) 'WARNING: no reference node found for i_new=', i_new, ' k=', k
            write(*,*) '         using nearest node regardless of level'
            ! fallback: nearest node ignoring level
            do j = 1, mesh_old%nod2D
                lon_j = mesh_old%coord(1, j)
                lat_j = mesh_old%coord(2, j)
                alpha = acos(max(-1.0_WP, min(1.0_WP, &
                        cos(lat_new)*cos(lat_j)*cos(lon_new-lon_j) + &
                        sin(lat_new)*sin(lat_j))))
                dist = r_earth * abs(alpha)
                if (dist < dist_min) then
                    dist_min = dist
                    i_ref    = j
                end if
            end do
        end if

    end subroutine find_nearest_old_node_at_level

    !===========================================================================
    ! Remap a single 3D node-based field.
    ! field_old: (nl-1, nod2D_old)
    ! field_new: (nl-1, nod2D_new)  -- allocated on output
    subroutine remap_node_field_3d(field_old, field_new, &
                                    mesh_old, mesh_new, node_flag, &
                                    set_new_to_zero)
        real(WP),           intent(in)  :: field_old(:,:)  ! (nl-1, nod2D_old)
        real(WP),           allocatable, intent(out) :: field_new(:,:)
        type(t_mesh_remap), intent(in)  :: mesh_old, mesh_new
        integer,            intent(in)  :: node_flag(:)
        logical,            intent(in)  :: set_new_to_zero ! if .true.: new nodes=0

        integer  :: i_new, i_old, i_ref, n_base, k
        integer  :: ul_new, nl_new, ul_old, nl_old
        integer  :: nz_old
        real(WP) :: dz, cf_a, cf_b, z_k

        ! Honour the level dim of the input so node-based fields with full nl
        ! levels (w, w_expl, w_impl) get a matching nl-sized output array
        ! instead of being silently truncated to nl-1.
        nz_old = size(field_old, 1)
        allocate(field_new(nz_old, mesh_new%nod2D))
        field_new = 0.0_WP

        do i_new = 1, mesh_new%nod2D
            n_base = mesh_new%nod_map(i_new)
            i_old  = mesh_old%map_base_to_mesh(n_base)
            ul_new = mesh_new%ulevels_nod2D(i_new)
            nl_new = mesh_new%nlevels_nod2D(i_new)
            ul_old = mesh_old%ulevels_nod2D(i_old)
            nl_old = mesh_old%nlevels_nod2D(i_old)

            select case (node_flag(i_new))

            !___________________________________________________________________
            case (FLAG_UNCHANGED)
                ! direct copy -- level indices identical
                field_new(ul_new:nl_new-1, i_new) = &
                    field_old(ul_new:nl_new-1, i_old)

            !___________________________________________________________________
            case (FLAG_VERT_EXTENDED)
                ul_old = mesh_old%ulevels_nod2D(i_old)
                nl_old = mesh_old%nlevels_nod2D(i_old)

                ! copy levels that exist in old mesh
                do k = ul_new, nl_new-1
                    if (k >= ul_old .and. k <= nl_old-1) then
                        field_new(k, i_new) = field_old(k, i_old)

                    !___________________________________________________________
                    ! new levels above old shelf base (cavity deepened into ice):
                    ! find nearest old node valid at this level
                    else if (k < ul_old) then
                        if (set_new_to_zero) then
                            field_new(k, i_new) = 0.0_WP
                        else
                            call find_nearest_old_node_at_level( &
                                i_new, k, mesh_old, mesh_new, i_ref)
                            field_new(k, i_new) = field_old(k, i_ref)
                        end if

                    !___________________________________________________________
                    ! new levels below old bottom (bathymetry deepened):
                    ! linear extrapolation from last two available levels
                    else
                        if (set_new_to_zero) then
                            field_new(k, i_new) = 0.0_WP    ! UKK is it a good idea to assume zero salinity at the lowest layers?
                        else
                            if (nl_old-1 >= ul_old+1) then
                                dz   =  mesh_old%zbar(nl_old) - &
                                        mesh_old%zbar(nl_old-1)
                                cf_a = (field_old(nl_old-1, i_old) - &
                                        field_old(nl_old-2, i_old)) / dz
                                cf_b =  field_old(nl_old-1, i_old) - &
                                        cf_a * mesh_old%zbar(nl_old)
                                z_k  =  mesh_new%zbar(k)
                                field_new(k, i_new) = cf_a * z_k + cf_b
                            else
                                ! fallback: constant extrapolation
                                field_new(k, i_new) = field_old(nl_old-1, i_old)
                            end if
                        end if
                    end if
                end do
            !___________________________________________________________________
            case (FLAG_NEW_NODE)
                if (set_new_to_zero) then
                    field_new(ul_new:nl_new-1, i_new) = 0.0_WP
                else
                    ! find nearest old node for each level
                    do k = ul_new, nl_new-1
                        call find_nearest_old_node_at_level( &
                            i_new, k, mesh_old, mesh_new, i_ref)
                        field_new(k, i_new) = field_old(k, i_ref)
                    end do
                end if

            end select
        end do

    end subroutine remap_node_field_3d

    !===========================================================================
    ! Remap a 2D node-based field (e.g. ssh, hbar).
    ! New nodes get value from nearest old node.
    subroutine remap_node_field_2d(field_old, field_new, &
                                    mesh_old, mesh_new, node_flag, &
                                    set_new_to_zero)
        real(WP),           intent(in)  :: field_old(:)    ! (nod2D_old)
        real(WP),           allocatable,intent(out) :: field_new(:)
        type(t_mesh_remap), intent(in)  :: mesh_old, mesh_new
        integer,            intent(in)  :: node_flag(:)
        logical,            intent(in)  :: set_new_to_zero

        integer  :: i_new, i_old, i_ref, n_base
        real(WP) :: dist, dist_min, alpha
        real(WP) :: lon_new, lat_new
        real(WP), parameter :: r_earth = 6371000.0_WP

        allocate(field_new(mesh_new%nod2D))
        field_new = 0.0_WP

        do i_new = 1, mesh_new%nod2D
            n_base = mesh_new%nod_map(i_new)
            i_old  = mesh_old%map_base_to_mesh(n_base)

            select case (node_flag(i_new))

            case (FLAG_UNCHANGED, FLAG_VERT_EXTENDED)
                field_new(i_new) = field_old(i_old)

            case (FLAG_NEW_NODE)
                if (set_new_to_zero) then
                    field_new(i_new) = 0.0_WP
                else
                    ! nearest neighbour in old mesh
                    lon_new  = mesh_new%coord(1, i_new)
                    lat_new  = mesh_new%coord(2, i_new)
                    dist_min = huge(1.0_WP)
                    i_ref    = 1
                    do i_old = 1, mesh_old%nod2D
                        alpha = acos(max(-1.0_WP, min(1.0_WP, &
                                cos(lat_new)*cos(mesh_old%coord(2,i_old))* &
                                cos(lon_new-mesh_old%coord(1,i_old)) + &
                                sin(lat_new)*sin(mesh_old%coord(2,i_old)))))
                        dist = r_earth * abs(alpha)
                        if (dist < dist_min) then
                            dist_min = dist
                            i_ref    = i_old
                        end if
                    end do
                    field_new(i_new) = field_old(i_ref)
                end if

            end select
        end do

    end subroutine remap_node_field_2d

    !===========================================================================
    ! Remap a 3D element-based field (u, v, w, urhs_AB, vrhs_AB).
    ! New cavity elements are set to zero.
    ! Element e_new corresponds to old element via the three nodes.
    subroutine remap_elem_field_3d(field_old, field_new, &
                                    mesh_old, mesh_new, node_flag)
        real(WP),           intent(in)  :: field_old(:,:)  ! (nl-1, elem2D_old)
        real(WP),           allocatable,intent(out) :: field_new(:,:)
        type(t_mesh_remap), intent(in)  :: mesh_old, mesh_new
        integer,            intent(in)  :: node_flag(:)    ! on nodes of new mesh

        integer :: e_new, e_old, k
        integer :: nodes_new(3), nodes_old(3)
        integer :: ul_new, nl_new, ul_old, nl_old
        integer :: n_base, i_old, j
        logical :: all_unchanged

        allocate(field_new(mesh_new%nl-1, mesh_new%elem2D))
        field_new = 0.0_WP

        do e_new = 1, mesh_new%elem2D
            nodes_new = mesh_new%elem2D_nodes(1:3, e_new)

            !___________________________________________________________________
            ! find corresponding old element via node mapping
            ! an element is "unchanged" if all three nodes are unchanged
            ! we use the first node to find the old element index
            all_unchanged = all(node_flag(nodes_new) == FLAG_UNCHANGED)

            if (all_unchanged) then
                ! all nodes unchanged -> find old element via node mapping
                do j = 1, 3
                    n_base     = mesh_new%nod_map(nodes_new(j))
                    nodes_old(j) = mesh_old%map_base_to_mesh(n_base)
                end do

                ! find the element in old mesh that contains these three nodes
                call find_old_elem(nodes_old, mesh_old, e_old)

                if (e_old > 0) then
                    ul_new = minval(mesh_new%ulevels_nod2D(nodes_new))
                    nl_new = maxval(mesh_new%nlevels_nod2D(nodes_new))
                    ul_old = minval(mesh_old%ulevels_nod2D(nodes_old))
                    nl_old = maxval(mesh_old%nlevels_nod2D(nodes_old))
                    do k = ul_old, nl_old-1
                        field_new(k, e_new) = field_old(k, e_old)
                    end do
                end if

            else
                ! element has new or extended nodes -> set to zero
                field_new(:, e_new) = 0.0_WP
            end if

        end do

    end subroutine remap_elem_field_3d

    !===========================================================================
    ! Find old element index given three old node indices.
    ! Searches nod_in_elem for the first node and checks the others.
    ! Returns -1 if not found.
    subroutine find_old_elem(nodes_old, mesh_old, e_old)
        integer,            intent(in)  :: nodes_old(3)
        type(t_mesh_remap), intent(in)  :: mesh_old
        integer,            intent(out) :: e_old

        integer :: e, j
        logical :: found

        e_old = -1
        ! brute force search -- could be optimized with nod_in_elem2D
        ! but is only called for unchanged elements so acceptable
        do e = 1, mesh_old%elem2D
            found = .true.
            do j = 1, 3
                if (.not. any(mesh_old%elem2D_nodes(1:3, e) == nodes_old(j))) then
                    found = .false.
                    exit
                end if
            end do
            if (found) then
                e_old = e
                return
            end if
        end do

    end subroutine find_old_elem

    !===========================================================================
    ! NetCDF helpers
    !===========================================================================
    subroutine nc_check(status, msg)
        integer,          intent(in) :: status
        character(len=*), intent(in) :: msg
        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF error: ', trim(msg), ': ', trim(nf90_strerror(status))
            stop
        end if
    end subroutine nc_check

    !===========================================================================
    subroutine read_nc_3d(filename, varname, field, nl1, nnod)
        ! reads field(nl1, nnod) from file
        character(len=*), intent(in)  :: filename, varname
        real(WP),         allocatable, intent(out) :: field(:,:)
        integer,          intent(in)  :: nl1, nnod

        integer :: ncid, varid
        real(WP), allocatable :: buf(:,:,:)  ! (time, nl1, nnod)

        call nc_check(nf90_open(filename, nf90_nowrite, ncid), &
                      'open '//trim(filename))
        call nc_check(nf90_inq_varid(ncid, varname, varid), &
                      'inq_varid '//trim(varname))
        allocate(buf(nnod, nl1, 1))
        !call nc_check(nf90_get_var(ncid, varid, buf, start=[1,1,1], count=[1,9,15]), &
        !                     'get_var '//trim(varname))
        call nc_check(nf90_get_var(ncid, varid, buf), &
                             'get_var '//trim(varname))
        call nc_check(nf90_close(ncid), 'close '//trim(filename))

        allocate(field(nl1, nnod))
        field = buf(1,:,:)
        deallocate(buf)

    end subroutine read_nc_3d

    !===========================================================================
    subroutine read_nc_2d(filename, varname, field, nnod)
        character(len=*), intent(in)  :: filename, varname
        real(WP),         allocatable, intent(out) :: field(:)
        integer,          intent(in)  :: nnod

        integer :: ncid, varid
        real(WP), allocatable :: buf(:,:)  ! (time, nnod)

        call nc_check(nf90_open(filename, nf90_nowrite, ncid), &
                      'open '//trim(filename))
        call nc_check(nf90_inq_varid(ncid, varname, varid), &
                      'inq_varid '//trim(varname))

        allocate(buf(1, nnod))
        call nc_check(nf90_get_var(ncid, varid, buf), &
                      'get_var '//trim(varname))
        call nc_check(nf90_close(ncid), 'close '//trim(filename))

        allocate(field(nnod))
        field = buf(1,:)
        deallocate(buf)

    end subroutine read_nc_2d

    !===========================================================================
    subroutine write_nc_3d(filename, varname, long_name, units, &
                        field, nl1, nnod, dim2_name, time_val, iter_val)
        character(len=*), intent(in) :: filename, varname, long_name, units
        real(WP),         intent(in) :: field(:,:)   ! (nz, nnodes) in memory
        integer,          intent(in) :: nnod, nl1
        character(len=*), intent(in) :: dim2_name    ! 'node' or 'elem'
        real(WP),         intent(in) :: time_val
        integer,          intent(in) :: iter_val

        integer :: ncid, varid, varid_time, varid_iter
        integer :: dim_nz1, dim_nod, dim_time

        call nc_check(nf90_create(filename, nf90_clobber, ncid), &
                  'create '//trim(filename))
        call nc_check(nf90_def_dim(ncid, 'time', nf90_unlimited, dim_time), 'def_dim time')
        call nc_check(nf90_def_dim(ncid, trim(dim2_name), nnod,  dim_nod),  'def_dim node')
        call nc_check(nf90_def_dim(ncid, 'nz_1',          nl1,   dim_nz1),  'def_dim nz_1')  
        call nc_check(nf90_def_var(ncid, 'time', nf90_double, [dim_time], varid_time), 'def_var time')
        call nc_check(nf90_def_var(ncid, 'iter', nf90_int,    [dim_time], varid_iter), 'def_var iter')
        call nc_check(nf90_def_var(ncid, trim(varname), nf90_double, &
                  [dim_nod, dim_nz1, dim_time], varid), &
                  'def_var '//trim(varname))
        call nc_check(nf90_put_att(ncid, varid, 'units',     trim(units)),     'att units')
        call nc_check(nf90_put_att(ncid, varid, 'long_name', trim(long_name)), 'att long_name')
        call nc_check(nf90_enddef(ncid), 'enddef')

        call nc_check(nf90_put_var(ncid, varid_time, [time_val]), 'put_var time')
        call nc_check(nf90_put_var(ncid, varid_iter, [iter_val]), 'put_var iter')
        call nc_check(nf90_put_var(ncid, varid, transpose(field), &
                  start=[1, 1, 1], count=[nnod, nl1, 1]), &
                  'put_var '//trim(varname))
        call nc_check(nf90_close(ncid), 'close '//trim(filename))
 
     end subroutine write_nc_3d
    !===========================================================================
    subroutine write_nc_2d(filename, varname, long_name, units, &
                            field, nnod, time_val, iter_val)
        character(len=*), intent(in) :: filename, varname, long_name, units
        real(WP),         intent(in) :: field(:)
        integer,          intent(in) :: nnod
        real(WP),         intent(in) :: time_val
        integer,          intent(in) :: iter_val

        integer :: ncid, varid_data, varid_time, varid_iter
        integer :: dim_time, dim_nod

        call nc_check(nf90_create(filename, nf90_clobber, ncid), &
                      'create '//trim(filename))
        call nc_check(nf90_def_dim(ncid, 'time', nf90_unlimited, dim_time), &
                      'def_dim time')
        call nc_check(nf90_def_dim(ncid, 'node', nnod, dim_nod), &
                      'def_dim node')
        call nc_check(nf90_def_var(ncid, 'time', nf90_double, [dim_time], varid_time), &
                      'def_var time')
        call nc_check(nf90_def_var(ncid, 'iter', nf90_int,    [dim_time], varid_iter), &
                      'def_var iter')
        call nc_check(nf90_def_var(ncid, trim(varname), nf90_double, &
                      [dim_nod, dim_time], varid_data), &
                      'def_var '//trim(varname))
        call nc_check(nf90_put_att(ncid, varid_data, 'units',     trim(units)),     'att units')
        call nc_check(nf90_put_att(ncid, varid_data, 'long_name', trim(long_name)), 'att long_name')
        call nc_check(nf90_enddef(ncid), 'enddef')

        call nc_check(nf90_put_var(ncid, varid_time, [time_val]), 'put_var time')
        call nc_check(nf90_put_var(ncid, varid_iter, [iter_val]), 'put_var iter')
        call nc_check(nf90_put_var(ncid, varid_data, &
                      reshape(field, [nnod, 1])), 'put_var '//trim(varname))
        call nc_check(nf90_close(ncid), 'close '//trim(filename))

    end subroutine write_nc_2d

    !===========================================================================
    ! read time and iter from any restart file
    subroutine read_time_iter(filename, time_val, iter_val)
        character(len=*), intent(in)  :: filename
        real(WP),         intent(out) :: time_val
        integer,          intent(out) :: iter_val

        integer :: ncid, varid
        real(WP) :: tbuf(1)
        integer  :: ibuf(1)

        call nc_check(nf90_open(filename, nf90_nowrite, ncid), &
                      'open '//trim(filename))
        call nc_check(nf90_inq_varid(ncid, 'time', varid), 'inq time')
        call nc_check(nf90_get_var(ncid, varid, tbuf), 'get time')
        call nc_check(nf90_inq_varid(ncid, 'iter', varid), 'inq iter')
        call nc_check(nf90_get_var(ncid, varid, ibuf), 'get iter')
        call nc_check(nf90_close(ncid), 'close')
        time_val = tbuf(1)
        iter_val = ibuf(1)

    end subroutine read_time_iter

    !===========================================================================
    ! Master routine: remap all restart files
    !===========================================================================
    subroutine remap_all_restarts(mesh_old, mesh_new, node_flag, &
                                   path_old, path_new, restart_year)
        type(t_mesh_remap), intent(in) :: mesh_old, mesh_new
        integer,            intent(in) :: node_flag(:)
        character(len=*),   intent(in) :: path_old, path_new
        integer,            intent(in) :: restart_year

        integer  :: nl1, nod_old, nod_new, elem_old, elem_new
        real(WP) :: time_val
        integer  :: iter_val
        character(len=256) :: fin, fout
        character(len=10)  :: year_str

        real(WP), allocatable :: field_old_3d(:,:), field_new_3d(:,:)
        real(WP), allocatable :: field_old_2d(:),   field_new_2d(:)

        write(year_str,'(I4)') restart_year
        nl1      = mesh_new%nl - 1
        nod_old  = mesh_old%nod2D
        nod_new  = mesh_new%nod2D
        elem_old = mesh_old%elem2D
        elem_new = mesh_new%elem2D

        !allocate(field_old_3d(nod_old,nl1))
        ! read time/iter from temp.nc
        fin = trim(path_old)//'temp.nc'
        call read_time_iter(fin, time_val, iter_val)
        write(*,*) ' time=', time_val, ' iter=', iter_val

        !_______________________________________________________________________
        ! temp.nc  -- full extrapolation for new/extended nodes
        write(*,*) ' --> temp.nc'
        fin  = trim(path_old)//'temp.nc'
        fout = trim(path_new)//'temp.nc'
        write(*,*) 'about to read', trim(fin)
        call read_restart_var_3d(path_old, 'temp', field_old_3d)
        !call read_nc_3d(fin, 'temp', field_old_3d, nl1, nod_old)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.false.)
        call write_nc_3d(fout, 'temp', 'potential temperature', 'degC', &
                          field_new_3d, nl1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        !_______________________________________________________________________
        ! salt.nc
        write(*,*) ' --> salt.nc'
        fin  = trim(path_old)//'salt.nc'
        fout = trim(path_new)//'salt.nc'
        call read_restart_var_3d(path_old, 'salt', field_old_3d)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.false.)
        call write_nc_3d(fout, 'salt', 'salinity', 'psu', &
                          field_new_3d, nl1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        !_______________________________________________________________________
        ! temp_AB.nc, salt_AB.nc  -- set new nodes to zero (simpler, one timestep effect)
        write(*,*) ' --> temp_AB.nc'
        fin  = trim(path_old)//'temp_AB.nc'
        fout = trim(path_new)//'temp_AB.nc'
        call read_restart_var_3d(path_old, 'temp', field_old_3d)
        !call read_nc_3d(fin, 'temp', field_old_3d, nl1, nod_old)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_3d(fout, 'temp', 'potential temperature AB', 'degC', &
                          field_new_3d, nl1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        write(*,*) ' --> salt_AB.nc'
        fin  = trim(path_old)//'salt_AB.nc'
        fout = trim(path_new)//'salt_AB.nc'
        call read_restart_var_3d(path_old, 'salt', field_old_3d)
        !call read_nc_3d(fin, 'salt', field_old_3d, nl1, nod_old)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_3d(fout, 'salt', 'salinity AB', 'psu', &
                          field_new_3d, nl1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        !_______________________________________________________________________
        ! u.nc, v.nc  -- element-based, new cavity elements = 0
        write(*,*) ' --> u.nc'
        fin  = trim(path_old)//'u.nc'
        fout = trim(path_new)//'u.nc'
        call read_restart_var_3d(path_old, 'u', field_old_3d)
        !call read_nc_3d(fin, 'u', field_old_3d, nl1, elem_old)
        call remap_elem_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag)
        call write_nc_3d(fout, 'u', 'zonal velocity', 'm/s', &
                          field_new_3d, nl1, elem_new, 'elem', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        write(*,*) ' --> v.nc'
        fin  = trim(path_old)//'v.nc'
        fout = trim(path_new)//'v.nc'
        call read_restart_var_3d(path_old, 'v', field_old_3d)
        !call read_nc_3d(fin, 'v', field_old_3d, nl1, elem_old)
        call remap_elem_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag)
        call write_nc_3d(fout, 'v', 'meridional velocity', 'm/s', &
                          field_new_3d, nl1, elem_new, 'elem', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        !_______________________________________________________________________
        ! w.nc, w_expl.nc, w_impl.nc  -- node-based, nl levels (not nl-1)
        ! these have dimension (nl, node) not (nl-1, node)
        ! treat as nl-1 for simplicity since w(nl,:)=0 always
        write(*,*) ' --> w.nc'
        fin  = trim(path_old)//'w.nc'
        fout = trim(path_new)//'w.nc'
        call read_restart_var_3d(path_old, 'w', field_old_3d)
        !call read_nc_3d(fin, 'w', field_old_3d, nl1+1, nod_old)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_3d(fout, 'w', 'vertical velocity', 'm/s', &
                          field_new_3d, nl1+1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        write(*,*) ' --> w_expl.nc'
        fin  = trim(path_old)//'w_expl.nc'
        fout = trim(path_new)//'w_expl.nc'
        call read_restart_var_3d(path_old, 'w', field_old_3d)
        !call read_nc_3d(fin, 'w', field_old_3d, nl1+1, nod_old)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_3d(fout, 'w', 'explicit vertical velocity', 'm/s', &
                          field_new_3d, nl1+1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        write(*,*) ' --> w_impl.nc'
        fin  = trim(path_old)//'w_impl.nc'
        fout = trim(path_new)//'w_impl.nc'
        call read_restart_var_3d(path_old, 'w', field_old_3d)
        !call read_nc_3d(fin, 'w', field_old_3d, nl1+1, nod_old)
        call remap_node_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_3d(fout, 'w', 'implicit vertical velocity', 'm/s', &
                          field_new_3d, nl1+1, nod_new, 'node', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        !_______________________________________________________________________
        ! urhs_AB.nc, vrhs_AB.nc  -- element-based RHS history, set new to zero
        write(*,*) ' --> urhs_AB.nc'
        fin  = trim(path_old)//'urhs_AB.nc'
        fout = trim(path_new)//'urhs_AB.nc'
        call read_restart_var_3d(path_old, 'u', field_old_3d)
        !call read_nc_3d(fin, 'u', field_old_3d, nl1, elem_old)
        call remap_elem_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag)
        call write_nc_3d(fout, 'u', 'zonal velocity RHS AB', 'm/s2', &
                          field_new_3d, nl1, elem_new, 'elem', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        write(*,*) ' --> vrhs_AB.nc'
        fin  = trim(path_old)//'vrhs_AB.nc'
        fout = trim(path_new)//'vrhs_AB.nc'
        call read_restart_var_3d(path_old, 'v', field_old_3d)
        !call read_nc_3d(fin, 'v', field_old_3d, nl1, elem_old)
        call remap_elem_field_3d(field_old_3d, field_new_3d, &
                                  mesh_old, mesh_new, node_flag)
        call write_nc_3d(fout, 'v', 'meridional velocity RHS AB', 'm/s2', &
                          field_new_3d, nl1, elem_new, 'elem', time_val, iter_val)
        deallocate(field_old_3d, field_new_3d)

        !_______________________________________________________________________
        ! ssh.nc, ssh_rhs_old.nc, hbar.nc  -- 2D node-based
        write(*,*) ' --> ssh.nc'
        fin  = trim(path_old)//'ssh.nc'
        fout = trim(path_new)//'ssh.nc'
        call read_restart_var_2d(path_old, 'ssh', field_old_2d)
        !call read_nc_2d(fin, 'ssh', field_old_2d, nod_old)
        call remap_node_field_2d(field_old_2d, field_new_2d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_2d(fout, 'ssh', 'sea surface height', 'm', &
                          field_new_2d, nod_new, time_val, iter_val)
        deallocate(field_old_2d, field_new_2d)

        write(*,*) ' --> ssh_rhs_old.nc'
        fin  = trim(path_old)//'ssh_rhs_old.nc'
        fout = trim(path_new)//'ssh_rhs_old.nc'
        call read_restart_var_2d(path_old, 'ssh', field_old_2d)
        !call read_nc_2d(fin, 'ssh', field_old_2d, nod_old)
        call remap_node_field_2d(field_old_2d, field_new_2d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_2d(fout, 'ssh', 'ssh rhs old', 'm', &
                          field_new_2d, nod_new, time_val, iter_val)
        deallocate(field_old_2d, field_new_2d)

        write(*,*) ' --> hbar.nc'
        fin  = trim(path_old)//'hbar.nc'
        fout = trim(path_new)//'hbar.nc'
        call read_restart_var_2d(path_old, 'hbar', field_old_2d)
        !call read_nc_2d(fin, 'hbar', field_old_2d, nod_old)
        call remap_node_field_2d(field_old_2d, field_new_2d, &
                                  mesh_old, mesh_new, node_flag, &
                                  set_new_to_zero=.true.)
        call write_nc_2d(fout, 'hbar', 'hbar', 'm', &
                          field_new_2d, nod_new, time_val, iter_val)
        deallocate(field_old_2d, field_new_2d)

        !_______________________________________________________________________
        ! hnode.nc  -- 3D node-based layer thickness
        ! new nodes get nominal layer thickness from zbar
        write(*,*) ' --> hnode.nc'
        call remap_hnode(mesh_old, mesh_new, node_flag, &
                          path_old, path_new, time_val, iter_val)

        write(*,*) ' --> all restart files remapped.'

    end subroutine remap_all_restarts

    !===========================================================================
    ! hnode needs special treatment: new levels get nominal thickness
    subroutine remap_hnode(mesh_old, mesh_new, node_flag, &
                            path_old, path_new, time_val, iter_val)
        type(t_mesh_remap), intent(in) :: mesh_old, mesh_new
        integer,            intent(in) :: node_flag(:)
        character(len=*),   intent(in) :: path_old, path_new
        real(WP),           intent(in) :: time_val
        integer,            intent(in) :: iter_val

        real(WP), allocatable :: hnode_old(:,:), hnode_new(:,:)
        integer  :: nl1, nod_old, nod_new
        integer  :: i_new, i_old, n_base, k, ul_new, nl_new, ul_old, nl_old

        nl1     = mesh_new%nl - 1
        nod_old = mesh_old%nod2D
        nod_new = mesh_new%nod2D
        call read_restart_var_3d(trim(path_old), 'hnode', hnode_old)

        !call read_nc_3d(trim(path_old)//'hnode.nc', 'hnode', &
         !                hnode_old, nl1, nod_old)

        allocate(hnode_new(nl1, nod_new))
        hnode_new = 0.0_WP

        do i_new = 1, nod_new
            n_base = mesh_new%nod_map(i_new)
            i_old  = mesh_old%map_base_to_mesh(n_base)
            ul_new = mesh_new%ulevels_nod2D(i_new)
            nl_new = mesh_new%nlevels_nod2D(i_new)

            select case (node_flag(i_new))

            case (FLAG_UNCHANGED)
                hnode_new(ul_new:nl_new-1, i_new) = &
                    hnode_old(ul_new:nl_new-1, i_old)

            case (FLAG_VERT_EXTENDED)
                ul_old = mesh_old%ulevels_nod2D(i_old)
                nl_old = mesh_old%nlevels_nod2D(i_old)
                ! copy existing levels
                do k = ul_new, nl_new-1
                    if (k >= ul_old .and. k <= nl_old-1) then
                        hnode_new(k, i_new) = hnode_old(k, i_old)
                    else
                        ! new levels: nominal thickness from zbar
                        hnode_new(k, i_new) = abs(mesh_new%zbar(k+1) - &
                                                   mesh_new%zbar(k))
                    end if
                end do

            case (FLAG_NEW_NODE)
                ! all levels: nominal thickness
                do k = ul_new, nl_new-1
                    hnode_new(k, i_new) = abs(mesh_new%zbar(k+1) - &
                                               mesh_new%zbar(k))
                end do

            end select
        end do

        call write_nc_3d(trim(path_new)//'hnode.nc', 'hnode', &
                          'layer thickness at node', 'm', &
                          hnode_new, nl1, nod_new, 'node', time_val, iter_val)
        deallocate(hnode_old, hnode_new)

    end subroutine remap_hnode

end module mo_remap_fields
