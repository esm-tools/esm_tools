program remap_restart
    !!=========================================================================
    !! Standalone tool to remap FESOM2 restart files from old to new mesh
    !! when cavity geometry changes between restarts.
    !!
    !! Usage: 
    !!   remap_restart --config namelist.remap
    !!
    !! namelist.remap:
    !!   &remap_config
    !!     path_base        = '/path/to/base/mesh/'
    !!     path_old         = '/path/to/old/mesh/'
    !!     path_new         = '/path/to/new/mesh/'
    !!     path_restart_old = '/path/to/old/restart/'
    !!     path_restart_new = '/path/to/new/restart/'
    !!     restart_year     = 2025
    !!     nod2D_base       = 123456
    !!   /
    !!=========================================================================
    use mo_remap_mesh
    use mo_remap_fields
    use mo_restart_io
    implicit none

    character(len=256) :: path_base, path_old, path_new
    character(len=256) :: path_restart_old, path_restart_new
    integer            :: restart_year, nod2D_base

    namelist /remap_config/ path_base, path_old, path_new, &
                             path_restart_old, path_restart_new, &
                             restart_year, nod2D_base

    type(t_mesh_remap)   :: mesh_old, mesh_new
    integer, allocatable :: node_flag(:)
    character(len=256)   :: path_ice_old
    integer              :: ipos
    integer(8)           :: clk_rate, clk0, clk1
    !     type(t_cavity_line)  :: cav_line

    !___________________________________________________________________________
    ! read namelist
    open(20, file='namelist.remap', status='old')
    read(20, nml=remap_config)
    close(20)

    write(*,*) '============================================'
    write(*,*) ' FESOM2 restart remapping tool'
    write(*,*) '============================================'
    call system_clock(count_rate=clk_rate)
    call system_clock(clk0)

    !___________________________________________________________________________
    ! read old and new mesh
    write(*,*) ' --> reading old mesh'
    call read_mesh_remap(path_old, nod2D_base, mesh_old)
    call system_clock(clk1); write(*,'(A,F8.2,A)') '     [TIMER] read old mesh        : ', real(clk1-clk0)/clk_rate, ' s'; clk0=clk1

    write(*,*) ' --> reading new mesh'
    call read_mesh_remap(path_new, nod2D_base, mesh_new)
    call system_clock(clk1); write(*,'(A,F8.2,A)') '     [TIMER] read new mesh        : ', real(clk1-clk0)/clk_rate, ' s'; clk0=clk1

    !___________________________________________________________________________
    ! classify nodes and compute cavity line
    write(*,*) ' --> classifying nodes'
    call classify_nodes(mesh_old, mesh_new, node_flag)
    call system_clock(clk1); write(*,'(A,F8.2,A)') '     [TIMER] classify_nodes       : ', real(clk1-clk0)/clk_rate, ' s'; clk0=clk1

    write(*,*) ' --> computing new cavity line'
    !call compute_new_cavity_line(mesh_new, node_flag, cav_line)

    !___________________________________________________________________________
    ! remap restart fields
    write(*,*) ' --> remapping ocean restart'
    call remap_all_restarts(mesh_old, mesh_new, node_flag, &
                   path_restart_old, path_restart_new, restart_year)
    call system_clock(clk1); write(*,'(A,F8.2,A)') '     [TIMER] remap ocean restart  : ', real(clk1-clk0)/clk_rate, ' s'; clk0=clk1

    write(*,*) ' --> remapping ice restart'
    ! The ice restart lives next to the ocean one, in fesom.<year-1>.ice.restart/.
    ! Derive its path from path_restart_old by swapping 'oce.restart'->'ice.restart'.
    path_ice_old = path_restart_old
    ipos = index(path_ice_old, 'oce.restart')
    if (ipos > 0) then
        path_ice_old(ipos:ipos+2) = 'ice'
        call remap_ice(mesh_old, mesh_new, node_flag, &
                       path_ice_old, path_restart_new, restart_year)
    else
        write(*,*) '     (skipped: could not locate oce.restart in path_restart_old)'
    end if
    call system_clock(clk1); write(*,'(A,F8.2,A)') '     [TIMER] remap ice restart    : ', real(clk1-clk0)/clk_rate, ' s'; clk0=clk1

    write(*,*) '============================================'
    write(*,*) ' remapping done.'
    write(*,*) '============================================'

end program remap_restart
