module m_mesh
  
  type t_mesh
     integer              :: nod2D, elem2D, num_layers
     integer, allocatable :: elem(:,:)
     real, allocatable    :: coord(:,:)
     real, allocatable    :: depth(:)
     real, allocatable    :: layer(:)
     integer, allocatable :: boundary(:) ! 1 = closed boundary
                                         ! 0 = inner node
     integer, allocatable :: nod_map(:)    ! index mapping to max mesh
     integer, allocatable :: elem_map(:)   ! index mapping to max mesh
     character(len=100)   :: MeshPath
  end type t_mesh
  
end module m_mesh

module ListUtils
    implicit none
contains
subroutine remove_element(arr, pos)
        integer, allocatable, intent(inout) :: arr(:)
        integer, allocatable :: arr2(:)
        integer, intent(in) :: pos
        integer :: i, n
        n = size(arr)

        if (pos < 1 .or. pos > n) return

        ! Shift elements left
        do i = pos, n - 1
            arr(i) = arr(i + 1)
        end do
        ! Resize array
        call move_alloc(arr, arr2)  ! Needed to reallocate safely
        allocate(arr(n - 1))
        arr(1:n-1) = arr2(1:n - 1) 
end subroutine remove_element
end module ListUtils
program fesom_submesh

  use m_mesh
  implicit none
  type(t_mesh)       :: maxmesh
  type(t_mesh)       :: sub_mesh
  real,allocatable   :: topo_raw(:), cavity_raw(:)
  integer,allocatable:: mesh_flag_raw(:), mask_raw(:)
  INTEGER :: i, arg, num_args

  ! Get the number of arguments
  num_args = command_argument_count()

  ! Check if we have at least 2 arguments
  if (num_args < 2) then
     print *, "Usage: submesh_fesom maxmesh submesh"
     stop
  end if

  ! Get the arguments
  call get_command_argument(1, maxmesh%MeshPath )
  call get_command_argument(2, sub_mesh%MeshPath)

  print *, 'UKK Reducing maxmesh and'//adjustl(maxmesh%MeshPath) 
  print *, 'UKK writing submesh to '//adjustl(sub_mesh%MeshPath)

  call mesh_read(maxmesh)

  allocate(topo_raw(maxmesh%nod2D))
  allocate(cavity_raw(maxmesh%nod2D))
  allocate(mesh_flag_raw(maxmesh%nod2D))
  allocate(mask_raw(maxmesh%nod2D))

  call read_topo_cavity(maxmesh, topo_raw, cavity_raw, mesh_flag_raw, mask_raw)

  !call mesh_reduce_byflag(maxmesh, mesh_flag_raw, sub_mesh, mask_raw)   
  call mesh_reduce_byflag(maxmesh, mesh_flag_raw, sub_mesh, mask_raw, cavity_raw, topo_raw) ! UKK we need the depth of the cavity roof here 

  call mesh_write(sub_mesh)

  call mesh_write_cavity(maxmesh, sub_mesh, cavity_raw)

  call mesh_clean(sub_mesh)


end program fesom_submesh

!======================================================================

subroutine mesh_read(M)
  use m_mesh
  implicit none

  type(t_mesh), intent(inout) :: M

  integer :: infile_nod2d, infile_elem2d, infile_aux3d
  integer :: n, idx, i_bound, num_layers, el
  real    :: lonlat(2)
  
  ! We need nod2d.out, elem2d.out, aux3d.out
  
  open(newunit=infile_nod2d, file=TRIM(M%MeshPath)//'nod2d.out', status= 'old', &
                              action = 'read')
  read(infile_nod2d, *) M%nod2D

  allocate(M%coord(2, M%nod2D))
  allocate(M%boundary(M%nod2D))
  allocate(M%depth(   M%nod2D))

  do n=1,M%nod2D
     read(infile_nod2d, *) idx, lonlat(1:2), i_bound
     M%coord(1:2,idx)     = lonlat(1:2)
     M%boundary(idx) = i_bound
  enddo
  close(infile_nod2D)
  
  open(newunit=infile_aux3d, file=TRIM(M%MeshPath)//'aux3d.out', status= 'old', &
                              action = 'read')
  read(infile_aux3d, *) M%num_layers
  allocate(M%layer(M%num_layers))

  do n=1, M%num_layers
     read(infile_aux3d, *) M%layer(n)
  end do
  do n=1,M%nod2D
     read(infile_aux3d, *) M%depth(n)
  enddo
  close(infile_aux3d)

  ! some files use positive values
  if (sum(M%depth) > 0.0) then
     M%depth = -M%depth
  endif

  !==================================
  
  open(newunit=infile_elem2d, file=TRIM(M%MeshPath)//'elem2d.out', status= 'old', &
                              action = 'read')

  read(infile_elem2d, *) M%elem2D

  allocate(M%elem(3,M%elem2D))
  do el=1, M%elem2D
     read(infile_elem2d, *) M%elem(1:3,el)
  end do
  
  close(infile_elem2d)
  
end subroutine mesh_read

!===================================================

subroutine mesh_write(M)
  use m_mesh
  implicit none

  type(t_mesh), intent(in) :: M

  integer :: outfile_nod2d, outfile_elem2d, outfile_aux3d
  integer :: outfile_nmap,  outfile_emap, outfile_nodhn
  integer :: n, idx, i_bound, num_layers, el
  real    :: lonlat(2)
  
  ! We need nod2d.out, elem2d.out, aux3d.out, nodhn.out

  print *,'Writing mesh to '//adjustl(M%MeshPath)
  print *,'nod2D  = ',M%nod2D
  print *,'elem2D = ',M%elem2D

  
  open(newunit=outfile_nod2d, file=TRIM(M%MeshPath)//'nod2d.out', status= 'new', &
                              action = 'write')
  write(outfile_nod2d, *) M%nod2D

  do n=1,M%nod2D
     write(outfile_nod2d, *) n, M%coord(1:2,n), M%boundary(n)
  enddo
  close(outfile_nod2D)

  
  open(newunit=outfile_aux3d, file=TRIM(M%MeshPath)//'aux3d.out', status= 'new', &
                              action = 'write')
  write(outfile_aux3d, *) M%num_layers

  do n=1, M%num_layers
     write(outfile_aux3d, *) M%layer(n)
  end do
  
  do n=1,M%nod2D
     write(outfile_aux3d, *) M%depth(n)
  enddo
  close(outfile_aux3d)
  
  open(newunit=outfile_nodhn, file=TRIM(M%MeshPath)//'nodhn.out', status= 'new', &
                              action = 'write')
  do n=1,M%nod2D
     write(outfile_nodhn, *) M%depth(n)
  enddo
  close(outfile_nodhn)
  
  open(newunit=outfile_elem2d, file=TRIM(M%MeshPath)//'elem2d.out', status= 'new', &
                              action = 'write')
  write(outfile_elem2d, *) M%elem2D

  do el=1, M%elem2D
     write(outfile_elem2d, *) M%elem(1:3,el)
  end do
  close(outfile_elem2d)

  if (allocated(M%nod_map)) then
      open(newunit=outfile_nmap, file=TRIM(M%MeshPath)//'map_nod.out', status= 'new', &
           action = 'write')
      
      do n=1,M%nod2D
         write(outfile_nmap, *) M%nod_map(n)
      enddo
      close(outfile_nmap)
  endif
  if (allocated(M%elem_map)) then
      open(newunit=outfile_emap, file=TRIM(M%MeshPath)//'map_elem.out', status= 'new', &
           action = 'write')
      
      do el=1,M%elem2D
         write(outfile_emap, *) M%elem_map(el)
      enddo
      close(outfile_emap)
   endif

   print *,'==================================='
  
end subroutine mesh_write

!===============================================================

subroutine mesh_reduce_byflag(M, nflag, Mred, mask_raw, cavity_raw, topo_raw)   
!UKK this subroutine needs another list of  nodeflags mask_raw which is 
!UKK 2 outside of the PISM domain (will never be removed) and
!UKK 1 within the PISM domain if ocean (only relevant if we update with nflag
!UKK 0 otherwise
!UKK might be combined with nflag and might be also used to optimize 
!UKK  the other element sweeps, if wanted

  use m_mesh
  use ListUtils
  implicit none
 
  type(t_mesh), intent(in)    :: M
  integer, intent(inout)      :: nflag(M%nod2D)
  type(t_mesh), intent(inout) :: Mred
  integer, intent(inout)      :: mask_raw(M%nod2D) !UKK flags indicating open ocean/potential lakes
  real, intent(in)            :: cavity_raw(M%nod2D) !UKK flags indicating open ocean/potential lakes
  real, intent(in)            :: topo_raw(M%nod2D) !UKK flags indicating open ocean/potential lakes

! UKK definitions for floodfill
  integer, allocatable :: elListLakeCheck(:)
  integer :: NLakeCheck
  logical :: elFound_Open
  ! UKK end
  integer                   :: bnd(M%nod2D), map(M%nod2D)
  integer                   :: elflag(M%elem2D)
  integer                   :: elflag2(M%elem2D)
  integer                   :: el, n
  integer                   :: count_flag0, count_flag1, count_nod, count_el, count_bd
  integer                   :: count_elem
  character(len=6)          :: ID_sl
  integer                   :: outfile_log
  character(len=150)        :: logfile
  logical                   :: l_element_removed
  integer                   :: nod_nod(16*3)   ! 16*3 should be more than sufficient for the #nodes/patch 
  integer                   :: nod_el(16,M%nod2D)   ! 16 should be more than sufficient for the
                                                    ! number of elements in a patch.
  integer                   :: iA, iB, iC, el1, j, k 
  logical                   :: l_AB, l_BC, l_AC
  integer                   :: count_situations
    
  ! This is the main routine.
  ! It is written with "please, work correctly" in mind.
  ! As paleo meshes are usually small, optimizations does not seem
  ! an issue to care about.
  
  
  ! 1. Determine new MeshPath as subdirectory from M%MeshPath

  call system('mkdir -p '//trim(adjustl(Mred%MeshPath)))

  print *,''
  print *,'Setting up submesh: '//adjustl(Mred%MeshPath)

  ! 2. Copy the boundary index (0=inner node, 1=closed boundary)
  bnd(:) = M%boundary(:)
  
  ! 3. Flag all elements: keep (1) or erase (0)
  !    Count the kept elements
  count_elem = 0
  count_situations=0
  do el=1,M%elem2D
     if (all(nflag(M%elem(1:3,el)) >= 0)) then

        if (maxval(topo_raw(M%elem(1:3,el))) >= minval(cavity_raw(M%elem(1:3,el)))) then
          ! if some nodes have no vertical overlap
          elflag(el) = 0
          ! In addition, the remaining nodes turn from inner nodes
          ! to boundary nodes. We do not distinguish here if the node
          ! is kept - for nodes to be removed, it is just information
          ! we do not regard later.
          count_situations=count_situations+1
         ! print*, count_situations, "HUHU"
          bnd(M%elem(1:3,el)) = 1        
        else
          ! all nodes are kept, keep the element
          count_elem = count_elem+1
          elflag(el) = 1
          ! For each node, we also count to how many elements it
          ! will belong to in the reduced mesh. A logfile is written in 5.
          nflag(M%elem(1:3,el)) = nflag(M%elem(1:3,el)) +1
        end if
     elseif (all(nflag(M%elem(1:3,el)) == -1)) then
        ! all nodes are removed, remove the element
        elflag(el) = 0
     else
        ! if some nodes are removed, remove the element, too.
        elflag(el) = 0
        ! In addition, the remaining nodes turn from inner nodes
        ! to boundary nodes. We do not distinguish here if the node
        ! is kept - for nodes to be removed, it is just information
        ! we do not regard later.
        bnd(M%elem(1:3,el)) = 1        
     end if
  end do
  
  ! We need addtional sweeps to remove the elements at corners,
  ! for which one node has nflag()=1. This has to be repeated until 
  ! all elements with just one neighbour are removed.
  
  l_element_removed = .true.
  do while (l_element_removed)
     l_element_removed = .false.
     
     do el=1,M%elem2D
        if (elflag(el) == 1 .and. any(nflag(M%elem(1:3,el)) == 1)) then
           elflag(el) = 0
           bnd(M%elem(1:3,el)) = 1  
           count_elem = count_elem-1                                                           
           nflag(M%elem(1:3,el)) = nflag(M%elem(1:3,el)) -1
           l_element_removed = .true.
        endif
     end do
  enddo
  
  ! Oh, but other special situations may arise, for which it does not help
  ! to count the elements for each node: it might be two elements, but opposite
  ! to each other in the patch.
  !
  ! And, again, we have to iterate the procedure, as a removed element might
  ! orphan a neighbour... and these loops are rather expensive


  l_element_removed = .true.
  do while (l_element_removed)
     l_element_removed = .false.

     ! We have no helpfuls structures here like edges. Thus, first, set up an 
     ! array nodes -> adjacent elements.
     ! It is a brute force to re-calculate this array over and over,
     ! but removing single nodes and elements is tedious to code.
     
     nod_el(:,:) = -1
     
     do el = 1, M%elem2D
        if (elflag(el) == 1) then
           iA = M%elem(1,el)
           iB = M%elem(2,el)
           iC = M%elem(3,el)
           do j=1,16
              if (nod_el(j,iA) == -1) then
                 nod_el(j,iA) = el
                 exit
              endif
           enddo
           do j=1,16
              if (nod_el(j,iB) == -1) then
                 nod_el(j,iB) = el
                 exit
              endif
           enddo
           do j=1,16
              if (nod_el(j,iC) == -1) then
                 nod_el(j,iC) = el
                 exit
              endif
           enddo
        endif
     enddo
  
  ! Now, use this neighbourhood array to check for bad nodes.
     do n=1,M%nod2d
       if ((bnd(n) == 1) .and. (mask_raw(n) < 2)) then                                ! find coasts in the ice domain
         count_el=0                                                                 ! to count the elements sharing this node
         count_bd=0                                                                 ! count nodes which are at the same coast    
         nod_nod=0
         do j=1,16
           if (nod_el(j,n) == -1) then
             exit
           else
             count_el=count_el+1 !count_el+1
             nod_nod((j-1)*3+1:3)=M%elem(1:3,nod_el(j,n))
           endif
         enddo
         if (count_el > 3) then                                                    ! if all the elements which are sharing the node n         
           do j = 1, count_el*3                                                    ! are not connected under each other (sharing at least one edge), 
             if (all([nod_nod(1:j-1), nod_nod(j+1:)] /= nod_nod(j))) then          ! we can detect this because then we have more than
                count_bd=count_bd+1                                                ! two nodes which appear only once in our neighborhood   
             endif                                                                   
           enddo
           if (count_bd >= 3) then
              print *, 'UKK', nod_nod    ! we still need to exclude some elements in this situation 
           endif        
         endif
       endif  
     enddo
  ! Now, use this neighbourhood array to check for neighbouring elements.
     do el = 1, M%elem2D
        if (elflag(el) == 1) then
           iA = M%elem(1,el)
           iB = M%elem(2,el)
           iC = M%elem(3,el)
           ! We check for neighbours with edge iA-iB, iA-iC, iB-iC
           l_AB = .false.
           l_AC = .false.
           l_BC = .false.
           do j=1,16
              el1 = nod_el(j,iA)
              if (el1 == -1) exit
              if (el1 /= el) then
                 if (any(M%elem(1:3,el1) == iB)) l_AB = .true.
                 if (any(M%elem(1:3,el1) == iC)) l_AC = .true.
              endif
           enddo
           if (l_AB .and. l_AC) cycle   ! This element has two neighbours, check.
           
           do j=1,16
              el1 = nod_el(j,iB)
              if (el1 == -1) exit
              if (el1 /= el .and. any(M%elem(1:3,el1) == iC)) then
                 l_BC = .true.
                 exit
              endif
           enddo
           
           if (l_AB .and. l_BC) cycle   ! This element has two neighbours, check.
           if (l_AC .and. l_BC) cycle   ! This element has two neighbours, check.
           
           ! Last case: less than two neighbours, remove this element
           elflag(el) = 0
           bnd(M%elem(1:3,el)) = 1  
           count_elem = count_elem-1                                                           
           nflag(M%elem(1:3,el)) = nflag(M%elem(1:3,el)) -1
           l_element_removed = .true.
        endif
     enddo
  enddo
  !3.2 UKK check for elements which are lakes/not connected to the open ocean (floodfill)
  !    Build a list of elements which need to be checked
  ! If the first two sweeps are slow this might be integrated above
   
  NLakeCheck = 0
  if (NLakeCheck==0) then
  elflag2 = elflag*0
  do el = 1, M%elem2D
    if ((elflag(el) >= 1) .and. (all(mask_raw(M%elem(1:3,el)) < 2))) then
       NLakeCheck = NLakeCheck +1
       elflag2(el) = 1     
    elseif (elflag(el) >= 1) then      
       mask_raw(M%elem(1:3,el)) = 2    
    endif
  enddo
  allocate(elListLakeCheck(NLakeCheck))
  print *,NLakeCheck ,' elements to be checked'
  j=1
  do el = 1, M%elem2D
    if (elflag2(el) == 1) then
       elListLakeCheck(j) = el       
       elflag2(el) = 0
       j=j+1
    endif
  enddo
  
  print *,j ,' elements to be checked'
  elFound_Open = .true.

  ! Repeat until a full pass is made without removing any elements
  do while (elFound_Open)
    elFound_Open = .false.
    j = 1
    print *,NLakeCheck ,' elements to be checked' 
    do while (j <= NLakeCheck)
    ! remove if element has openocean nodes 
            el = elListLakeCheck(j)
            if (any(mask_raw(M%elem(1:3,el)) == 2)) then
                call remove_element(elListLakeCheck, j)
                mask_raw(M%elem(1:3,el)) = 2
                elFound_Open = .true.
                NLakeCheck   = NLakeCheck-1
            else
                j = j + 1
            end if
        end do
    end do

    print *, 'LakeElements', NLakeCheck

  
    do j = 1, NLakeCheck
      el = elListLakeCheck(j) 
      elflag(el)=0
      nflag((M%elem(1:3,el)))=nflag((M%elem(1:3,el)))-1
    end do
  endif
  ! UKK end

  ! 4. Determine number of elements, nodes in reduced mesh 
  !    and build the nod_map from maximum to reduced mesh
  Mred%elem2D = count_elem

  count_flag0 = 0
  count_flag1 = 0
  count_nod   = 0

  do n=1, M%nod2D
     if (nflag(n) > 1) then
        count_nod = count_nod +1
        map(n)    = count_nod
        
     elseif (nflag(n) == 1) then
        count_flag1 = count_flag1+1
        map(n) = 0
        
     elseif (nflag(n) == 0) then
        count_flag0 = count_flag0+1
        map(n) = 0        
     else
        map(n) =0
     endif        
  enddo
  Mred%nod2D = count_nod

  ! 5. Issue a warning and write a log file if nodes are erased that were flagged for
  ! the reduced mesh, but would belong to no or or only one element.
  if ( count_flag0 + count_flag1 > 0) then
     logfile = trim(adjustl(Mred%MeshPath))//'log_solitary_nodes-removed.out'
     open(newunit=outfile_log, file=logfile, status= 'new', action = 'write')

     print *,''
     print *,'Warning:',count_flag0 + count_flag1,'nodes should belong to the reduced mesh,'
     print *,'         however, they would end up with only none or only one element,'
     print *,'         and will be removed.'
     print *,'See '//trim(logfile)
     print *,'         for a list of these solitary nodes.'
     print *,''
     
     write(outfile_log,*) '% index in full mesh,, lat, lon, No of elements in submesh (also removed)'
     do n=1,M%nod2D
        if (nflag(n)==1 .or. nflag(n)==0) then
           write(outfile_log,*) n,M%coord(1,n),M%coord(2,n),nflag(n)
        endif
     end do
     close(outfile_log)
  end if

  ! 6. allocate and fill the new fields
  allocate(Mred%coord(2, Mred%nod2D))
  allocate(Mred%depth(   Mred%nod2D))
  allocate(Mred%boundary(Mred%nod2D))
  allocate(Mred%nod_map( Mred%nod2D))

  allocate(Mred%elem( 3, Mred%elem2D))
  allocate(Mred%elem_map(Mred%elem2D))

  
  count_nod = 0
  do n=1,M%nod2D
     if (map(n) > 0) then
        count_nod = count_nod+1
        Mred%coord(1:2,count_nod) = M%coord(1:2,n)
        Mred%depth(    count_nod) = M%depth(n)
        Mred%boundary( count_nod) = bnd(n)
        Mred%nod_map(  count_nod) = n
     endif
  enddo

  count_elem = 0
  do el=1,M%elem2D
     if (elflag(el)==1) then
        count_elem = count_elem + 1
        Mred%elem(1:3,count_elem) = map(M%elem(1:3, el))
        Mred%elem_map(count_elem) = el
     endif
  enddo
  
  ! No changes for vertical layering so far...
  Mred%num_layers = M%num_layers
  allocate(Mred%layer(Mred%num_layers))
  Mred%layer(:) = M%layer(:)

  

  
end subroutine mesh_reduce_byflag


subroutine mesh_clean(M)
  use m_mesh
  implicit none
  
  type(t_mesh), intent(out) :: M

  M%nod2D  = 0
  M%elem2D = 0
  M%num_layers = 0

  if (allocated(M%elem    )) deallocate(M%elem)
  if (allocated(M%coord   )) deallocate(M%coord)
  if (allocated(M%depth   )) deallocate(M%depth)
  if (allocated(M%layer   )) deallocate(M%layer)
  if (allocated(M%boundary)) deallocate(M%boundary)
  if (allocated(M%nod_map )) deallocate(M%nod_map) 
  if (allocated(M%elem_map)) deallocate(M%elem_map)  
  
  M%MeshPath = ''
  
end subroutine mesh_clean


subroutine read_topo_cavity(M, topo_raw, cavity_raw, mesh_flag_raw, mask_raw)
  ! read raw topo and cavity data on meshmax mesh
  ! and define flags for valid nodes and open ocean

  use m_mesh
  implicit none

  ! variable setting
  type(t_mesh),intent(in) :: M
  real, intent(inout)     :: topo_raw(M%nod2D)
  real, intent(inout)     :: cavity_raw(M%nod2D)
  integer, intent(inout)  :: mask_raw(M%nod2D)
  integer, intent(inout)  :: mesh_flag_raw(M%nod2D)

  integer :: n, infile_topo, infile_cavity, infile_mask

  ! read in files
  open(newunit=infile_topo, file= './topo_raw.txt', status= 'old', &
                            action = 'read')
  do n=1,M%nod2D
     read(infile_topo, *) topo_raw(n)
  enddo
  close(infile_topo)

  open(newunit=infile_cavity, file= './cavity_raw.txt', status= 'old', &
                              action = 'read')
  do n=1,M%nod2D
     read(infile_cavity, *) cavity_raw(n)
     if (cavity_raw(n)<-30) then                   !UKK this is a test
       topo_raw(n) = -2000+mod(n,2)*1000
       cavity_raw(n) = -2500+mod(n,2)*1000
     end if        
  enddo
  close(infile_cavity)

  open(newunit=infile_mask, file= './mask.txt', status= 'old', &
                              action = 'read')
  do n=1,M%nod2D
     read(infile_mask, *) mask_raw(n)
  enddo
  close(infile_mask)

  ! initialise
  mesh_flag_raw = 0

  ! mesh_flag_raw: valid depth;
  do n = 1,M%nod2D
     if (mask_raw(n)<1) then
        mesh_flag_raw(n) = -1
     endif
  enddo

end subroutine read_topo_cavity


subroutine mesh_write_cavity(M, Mred, cavity_raw)
  use m_mesh
  implicit none

  type(t_mesh), intent(in) :: M, Mred
  real, intent(in) :: cavity_raw(M%nod2D)
  integer :: n, outfile_cavdep

  open(newunit=outfile_cavdep, file=TRIM(Mred%MeshPath)//'cavity_depth.out', &
                               status= 'new', action = 'write')
  do n=1,Mred%nod2D
     write(outfile_cavdep, *) cavity_raw(Mred%nod_map(n))
  enddo
  close(outfile_cavdep)

end subroutine mesh_write_cavity


