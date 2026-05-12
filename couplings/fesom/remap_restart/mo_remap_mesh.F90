module mo_remap_mesh
    !!=========================================================================
    !! Lightweight mesh structure and reader for the remap tool.
    !! Uses only the fields needed for remapping, reads directly
    !! from FESOM2 mesh ASCII files.
    !!=========================================================================
    implicit none
    integer, parameter :: WP = 8

    type t_mesh_remap
        !_______________________________________________________________________
        ! basic dimensions
        integer :: nod2D        ! nodes in this mesh
        integer :: elem2D       ! elements in this mesh
        integer :: nl           ! number of vertical levels
        integer :: nod2D_base   ! nodes in base mesh

        !_______________________________________________________________________
        ! 2D geometry (lon/lat in radians)
        real(WP), allocatable :: coord(:,:)   ! (2, nod2D)

        !_______________________________________________________________________
        ! element connectivity
        integer, allocatable  :: elem2D_nodes(:,:)  ! (3, elem2D)

        !_______________________________________________________________________
        ! vertical structure per node
        integer, allocatable  :: nlevels_nod2D(:)   ! (nod2D) bottom level
        integer, allocatable  :: ulevels_nod2D(:)   ! (nod2D) top active level

        !_______________________________________________________________________
        ! vertical level depths (same for all nodes)
        real(WP), allocatable :: zbar(:)    ! (nl)   full level depths [m], negative
        real(WP), allocatable :: Z(:)       ! (nl-1) mid-level depths  [m], negative

        !_______________________________________________________________________
        ! base mesh mapping
        integer, allocatable  :: nod_map(        :)  ! (nod2D)      -> base mesh index
        integer, allocatable  :: map_base_to_mesh(:)  ! (nod2D_base) -> this mesh index
                                                       ! 0 = not present
    end type t_mesh_remap

    public :: t_mesh_remap, read_mesh_remap, WP

contains

    subroutine read_mesh_remap(path, nod2D_base, mesh)
        character(len=*),   intent(in)  :: path
        integer,            intent(in)  :: nod2D_base
        type(t_mesh_remap), intent(out) :: mesh

        integer  :: n, nl, iost, fid
        integer  :: dummy_idx, dummy_bnd
        real(WP) :: x1, x2
        real(WP), parameter :: rad = 3.14159265358979323846_WP / 180.0_WP
        logical  :: file_exist
        integer, allocatable :: elem_data(:)
        integer  :: i_error

        mesh%nod2D_base = nod2D_base

        !_______________________________________________________________________
        ! nod2d.out
        write(*,*) '   read: ', trim(path)//'nod2d.out'
        open(newunit=fid, file=trim(path)//'nod2d.out', &
             status='old', action='read', iostat=iost)
        if (iost/=0) call remap_abort('cannot open '//trim(path)//'nod2d.out')
        read(fid,*) mesh%nod2D
        allocate(mesh%coord(2, mesh%nod2D))
        do n=1, mesh%nod2D
            read(fid,*) dummy_idx, x1, x2, dummy_bnd
            mesh%coord(1,n) = x1 * rad
            mesh%coord(2,n) = x2 * rad
        end do
        close(fid)

        !_______________________________________________________________________
        ! elem2d.out
        write(*,*) '   read: ', trim(path)//'elem2d.out'
        open(newunit=fid, file=trim(path)//'elem2d.out', &
             status='old', action='read', iostat=iost)
        if (iost/=0) call remap_abort('cannot open '//trim(path)//'elem2d.out')
        read(fid,*) mesh%elem2D
        allocate(mesh%elem2D_nodes(3, mesh%elem2D))
        allocate(elem_data(4*mesh%elem2D))
        elem_data = -1
        read(fid,*,iostat=i_error) elem_data(1:4*mesh%elem2D)
        if (i_error == 0) then
            ! quad or mixed mesh: take first 3 nodes only
            mesh%elem2D_nodes = reshape(elem_data(1:3*mesh%elem2D), &
                                        shape(mesh%elem2D_nodes))
        else
            ! triangles only
            mesh%elem2D_nodes = reshape(elem_data(1:3*mesh%elem2D), &
                                        shape(mesh%elem2D_nodes))
        end if
        deallocate(elem_data)
        close(fid)

        !_______________________________________________________________________
        ! aux3d.out  --> zbar and Z
        write(*,*) '   read: ', trim(path)//'aux3d.out'
        open(newunit=fid, file=trim(path)//'aux3d.out', &
             status='old', action='read', iostat=iost)
        if (iost/=0) call remap_abort('cannot open '//trim(path)//'aux3d.out')
        read(fid,*) nl
        mesh%nl = nl
        allocate(mesh%zbar(nl))
        read(fid,*) mesh%zbar
        if (mesh%zbar(2) > 0.0_WP) mesh%zbar = -mesh%zbar  ! ensure negative
        allocate(mesh%Z(nl-1))
        mesh%Z = 0.5_WP * (mesh%zbar(1:nl-1) + mesh%zbar(2:nl))
        close(fid)

        !_______________________________________________________________________
        ! nlvls.out  --> nlevels_nod2D
        write(*,*) '   read: ', trim(path)//'nlvls.out'
        allocate(mesh%nlevels_nod2D(mesh%nod2D))
        open(newunit=fid, file=trim(path)//'nlvls.out', &
             status='old', action='read', iostat=iost)
        if (iost/=0) call remap_abort('cannot open '//trim(path)//'nlvls.out')
        do n=1, mesh%nod2D
            read(fid,*) mesh%nlevels_nod2D(n)
        end do
        close(fid)

        !_______________________________________________________________________
        ! cavity_nlvls.out  --> ulevels_nod2D
        ! if not present: set all to 1 (no cavity)
        allocate(mesh%ulevels_nod2D(mesh%nod2D))
        mesh%ulevels_nod2D = 1
        inquire(file=trim(path)//'cavity_nlvls.out', exist=file_exist)
        if (file_exist) then
            write(*,*) '   read: ', trim(path)//'cavity_nlvls.out'
            open(newunit=fid, file=trim(path)//'cavity_nlvls.out', &
                 status='old', action='read', iostat=iost)
            do n=1, mesh%nod2D
                read(fid,*) mesh%ulevels_nod2D(n)
            end do
            close(fid)
        else
            write(*,*) '   cavity_nlvls.out not found, assuming no cavity'
        end if

        !_______________________________________________________________________
        ! map_nod.out  --> nod_map + inverse map_base_to_mesh
        inquire(file=trim(path)//'map_nod.out', exist=file_exist)
        if (file_exist) then
            write(*,*) '   read: ', trim(path)//'map_nod.out'
            allocate(mesh%nod_map(mesh%nod2D))
            open(newunit=fid, file=trim(path)//'map_nod.out', &
                 status='old', action='read', iostat=iost)
            do n=1, mesh%nod2D
                read(fid,*) mesh%nod_map(n)
            end do
            close(fid)

            ! build inverse mapping
            allocate(mesh%map_base_to_mesh(nod2D_base))
            mesh%map_base_to_mesh = 0
            do n=1, mesh%nod2D
                mesh%map_base_to_mesh(mesh%nod_map(n)) = n
            end do
        else
            call remap_abort('map_nod.out not found in '//trim(path)// &
                             ' -- required for remapping')
        end if

        write(*,*) '   mesh read: nod2D=', mesh%nod2D, &
                   ' elem2D=', mesh%elem2D, ' nl=', mesh%nl

    end subroutine read_mesh_remap

    !===========================================================================
    subroutine remap_abort(msg)
        character(len=*), intent(in) :: msg
        write(*,*) 'REMAP ERROR: ', trim(msg)
        error stop 1
    end subroutine remap_abort

end module mo_remap_mesh
