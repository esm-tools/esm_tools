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
    !     type(t_cavity_line)  :: cav_line

    !___________________________________________________________________________
    ! read namelist
    open(20, file='namelist.remap', status='old')
    read(20, nml=remap_config)
    close(20)

    write(*,*) '============================================'
    write(*,*) ' FESOM2 restart remapping tool'
    write(*,*) '============================================'

    !___________________________________________________________________________
    ! read old and new mesh
    write(*,*) ' --> reading old mesh'
    call read_mesh_remap(path_old, nod2D_base, mesh_old)

    write(*,*) ' --> reading new mesh'
    call read_mesh_remap(path_new, nod2D_base, mesh_new)

    !___________________________________________________________________________
    ! classify nodes and compute cavity line
    write(*,*) ' --> classifying nodes'
    call classify_nodes(mesh_old, mesh_new, node_flag)

    write(*,*) ' --> computing new cavity line'
    !call compute_new_cavity_line(mesh_new, node_flag, cav_line)

    !___________________________________________________________________________
    ! remap restart fields
    write(*,*) ' --> remapping ocean restart'
    call remap_all_restarts(mesh_old, mesh_new, node_flag, &
                   path_restart_old, path_restart_new, restart_year)

    write(*,*) ' --> remapping ice restart'
    !call remap_ice(mesh_old, mesh_new, node_flag, &
    !               path_restart_old, path_restart_new, restart_year)

    write(*,*) '============================================'
    write(*,*) ' remapping done.'
    write(*,*) '============================================'

end program remap_restart
