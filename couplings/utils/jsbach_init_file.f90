!------------------------------------------------------------------------------
! Program to generate initial files for JSBACH
!------------------------------------------------------------------------------
!   based on Thomas Raddatz' programs 
!        jsbach_init_file_13tiles.f90
!        jsbach_init_file_8tiles.f90    and 
!        jsbach_init_file_3or4tiles.f90
!   and scripts and programs by Stefan Hagemann in
!        soil_para.com
!
! This script does not exactly reproduces initial files generated with the 
! programs listed above. Here the differences: 
!   - The variables are defined in a new order
!   - Added global attributes
!   - Added attributes to several variables; particularly cover_type now has 
!     attributes defining the different land cover types
!   - There is no longer a variable lct - only the dimension
!   - Consistent use of selected_real_kind _dp
!        => slight differences in veg_ratio_factor, albedo_soil_nir, 
!           albedo_soil_vis, albedo_veg_nir and albedo_veg_vis
!   - Consider albedo_compensate with all numbers of tiles
!   - The definition of a minimum cover fraction and the scaling happens only 
!     once, at the end of the calculations 
!        => small differences in cover_fract, might also lead to different
!           cover_type
!
!
! Arrays in the jsbach initial file
!
! -----------------------------------------------------------------------------
! *)  variable           description              read from file     modified?
! -----------------------------------------------------------------------------
!  i  init_moist         soil wetness             jan_surf.nc WS     corrected
!  i  snow               snow depth               jan_surf.nc SN     unchanged
!  c  slf                fract. sea land mask     jan_surf.nc SLM    unchanged
!  c  slm                sea land mask                               calculated
!  -  roughness_length   roughness length         jan_surf.nc AZ0    unchanged
!  -  albedo             background albedo        jan_surf.nc ALB    unchanged
!  c  elevation          mean orography           jan_surf.nc OROMEA unchanged
!  c  orography_std_dev  orogr. standard dev.     jan_surf.nc OROSTD unchanged
!  c  glac               glacier mask             jan_surf.nc GLAC   unchanged
!  c  fao                FAO soil data flags      jan_surf.nc FAO    filled
!  c  maxmoist           soil field capacity      jan_surf.nc WSMX   filled,modifs
!  c                                            or soil_parameters.nc
!  -  forest_fract       forest fraction          jan_surf.nc FOREST unchanged
!  i  lai_clim           leaf area index          VLTCLIM.nc         unchanged
!  -  veg_fract          vegetation fraction      VGRATCLIM.nc       unchanged
!  i  surf_temp          surface temperature      TSLCLIM2.nc        unchanged
! c/i cover_fract        vegetation ratio         vegtype_pa14.nc    calculated
!  c  cover_type         vegetation cover type    vegtype_pa14.nc    calculated
! c/i veg_ratio_max      max. vegetation ratio    vegmax_6.lola      reorder
! c/- albedo_veg_vis     vegetation albedo (vis)  albedo.lola        modifs
! c/- albedo_veg_nir     vegetation albedo (NIR)  albedo.lola        modifs
!  c  albedo_soil_vis    soil albedo (visible)    albedo.lola        modifs
!  c  albedo_soil_nir    soil albedo (NIR)        albedo.lola        modifs
!  c  roughness_length_oro roughness without veg  topo_75.lola       modifs
! c/- natural_veg        potential vegetation     natural_veg.nc     calculated 
!  c  root_depth         effective root depth                        calculated
!  c  soil_depth         soil_depth               soil_parameters.nc filled,modifs
!  c  matrix_potential   Saturated matrix pot.    soil_parameters.nc filled
!  c  heat_capacity      Volume heat cap. of soil soil_parameters.nc filled
!  c  heat_conductivity  Heat conductivity of s.  soil_parameters.nc filled
!  c  bclapp             Clapp&Horneberger exp.   soil_paramsters.nc filled
!  c  hyd_cond_sat       Saturated hydraulic      soil_parameters.nc filled
!                        conductivity
!  c  pore_size_index    Soil pore s. dist. index soil_parameters.nc filled
!  c  soil_porosity      Volumetric soil porosity soil_parameters.nc filled
!  c  wilting_point      Volumetric soil          soil_parameters.nc filled
!                        permanent wilting point
!
!*) usage within jsbach
! ---------------------
! i: used for initialization
! c: used throughout the whole simulation
! -: not used in jsbach standard setup
!
!------------------------------------------------------------------------------
! To compile and run use
!     --------------------
!     jsbach_init_file.ksh
!     --------------------
!
! Veronika Gayler   , MPI, March   2008: Original code
! Stiig Wilkenskjeld, MPI, January 2012: Added 5-layer soil variables
!------------------------------------------------------------------------------

MODULE mo_kinds
  ! kind parameters
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307)
  INTEGER, PARAMETER :: i4 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(14)
  REAL(dp),PARAMETER :: pi = 3.14159265358979323846_dp
  REAL(dp),PARAMETER :: radius_of_earth = 6371000._dp
END MODULE mo_kinds

MODULE mo_vegparams
  USE mo_kinds
  ! vegetation parameters
  REAL(dp), PARAMETER :: small_fract = 1.E-10_dp
  REAL(dp), PARAMETER :: substantial_woody = 0.05_dp
  REAL(dp), PARAMETER :: substantial_tundra = 0.05_dp

  !
  ! land cover types as defined in lctlib file 
  !

  ! Parameters for nlct=21
  INTEGER, PARAMETER :: lct_glacier = 1
  INTEGER, PARAMETER :: lct_tropical_evergreen = 2
  INTEGER, PARAMETER :: lct_tropical_deciduous = 3
  INTEGER, PARAMETER :: lct_extratrop_evergreen = 4
  INTEGER, PARAMETER :: lct_extratrop_deciduous = 5
  INTEGER, PARAMETER :: lct_temperate_broadl_evergreen = 6
  INTEGER, PARAMETER :: lct_temperate_broadl_deciduous = 7
  INTEGER, PARAMETER :: lct_evergreen_conifer = 8
  INTEGER, PARAMETER :: lct_deciduous_conifer = 9
  INTEGER, PARAMETER :: lct_raingreen_shrub = 10
  INTEGER, PARAMETER :: lct_deciduous_shrub = 11
  INTEGER, PARAMETER :: lct_c3grass = 12
  INTEGER, PARAMETER :: lct_c4grass = 13
  INTEGER, PARAMETER :: lct_pasture = 14
  INTEGER, PARAMETER :: lct_c3pasture = 15
  INTEGER, PARAMETER :: lct_c4pasture = 16
  INTEGER, PARAMETER :: lct_tundra = 17
  INTEGER, PARAMETER :: lct_swamp  = 18
  INTEGER, PARAMETER :: lct_crop = 19
  INTEGER, PARAMETER :: lct_c3crop = 20
  INTEGER, PARAMETER :: lct_c4crop = 21
  INTEGER, PARAMETER :: lctmax=21  ! highest lct number

  !
  ! land cover types of the input vegetation maps (1-14)
  !
  INTEGER, PARAMETER :: nvegtyp = 14          ! number of vegetation types
  INTEGER, PARAMETER :: nwoody = 9            !   number of woody types
  INTEGER, PARAMETER :: woody_types(nwoody) = (/1,2,3,4,5,6,7,8,11/)
  INTEGER, PARAMETER :: veg_tropical_evergreen = 1
  INTEGER, PARAMETER :: veg_tropical_deciduous = 2
  INTEGER, PARAMETER :: veg_temperate_broadl_evergreen = 3
  INTEGER, PARAMETER :: veg_temperate_broadl_deciduous = 4
  INTEGER, PARAMETER :: veg_evergreen_conifer = 5
  INTEGER, PARAMETER :: veg_deciduous_conifer = 6
  INTEGER, PARAMETER :: veg_raingreen_shrub = 7
  INTEGER, PARAMETER :: veg_deciduous_shrub = 8
  INTEGER, PARAMETER :: veg_c3grass = 9
  INTEGER, PARAMETER :: veg_c4grass = 10
  INTEGER, PARAMETER :: veg_tundra = 11
  INTEGER, PARAMETER :: veg_crop = 12
  INTEGER, PARAMETER :: veg_c3pasture = 13
  INTEGER, PARAMETER :: veg_c4pasture = 14

  INTEGER, PARAMETER :: lct(nvegtyp) = &
       (/lct_tropical_evergreen, lct_tropical_deciduous, &
       lct_temperate_broadl_evergreen, lct_temperate_broadl_deciduous, &
       lct_evergreen_conifer, lct_deciduous_conifer, &
       lct_raingreen_shrub, lct_deciduous_shrub, &
       lct_c3grass, lct_c4grass, &
       lct_tundra, lct_crop ,lct_c3pasture, lct_c4pasture /)

  INTEGER :: glacier_tile = -1  ! tile that is reserved for glaciers; set in
                                ! calc_cover_types.

END MODULE mo_vegparams

!------------------------------------------------------------------------------
PROGRAM jsbach_init_file

  USE mo_kinds
  USE mo_vegparams
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

  INTERFACE 
     SUBROUTINE open_file(info,infile,ncin,ret_stat)
        CHARACTER*100, INTENT(IN)            :: infile
        LOGICAL,       INTENT(IN)            :: info
        INTEGER, INTENT(OUT) :: ncin
        INTEGER, INTENT(OUT), OPTIONAL :: ret_stat
     END SUBROUTINE

     SUBROUTINE define_var(ncin, ncout, varname_in, varname_out, outdimids, missval)
        USE mo_kinds
        INTEGER,       INTENT(IN)            :: ncin, ncout
        CHARACTER*30,  INTENT(IN)            :: varname_in, varname_out
        INTEGER,       INTENT(IN)            :: outdimids(6)
        REAL(dp),      INTENT(OUT), OPTIONAL :: missval
     END SUBROUTINE
  END INTERFACE

  CHARACTER*20  :: svn_rev = "$Revision: 9230 $"
  CHARACTER*128 :: svn_url =    &
  "$HeadURL: https://svn.zmaw.de/svn/cosmos/branches/mpiesm-landveg/contrib/initial_tarfiles/jsbach_init_file.f90 $"

  ! Dimensions
  INTEGER :: nlon, nlat, ntiles, nlct, nsoil, ntime

  ! Variables
  REAL(dp), ALLOCATABLE, DIMENSION(:)      ::  lat
  REAL(dp), ALLOCATABLE, DIMENSION(:)      ::  lon
  INTEGER,  ALLOCATABLE, DIMENSION(:)      ::  tiles
  INTEGER,  ALLOCATABLE, DIMENSION(:)      ::  time_days
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  veg_fract
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  cover_fract
  INTEGER,  ALLOCATABLE, DIMENSION(:,:,:)  ::  cover_type
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  cover_type_dp
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  lai
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  elevation
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  elevationrms
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  az0
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  roughness_oro
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  albedo
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  snow
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  slf
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  slm
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  non_glacier_land
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  lake
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  glac_dp
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  fao
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  forest_fract
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  surf_temp
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  vegfract
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  vegfract_cf
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  veg_ratio_max
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  albedo_veg_vis, albedo_veg_vis_in
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  albedo_veg_nir, albedo_veg_nir_in
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  albedo_soil_vis, albedo_soil_vis_in
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  albedo_soil_nir, albedo_soil_nir_in
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  C3C4_flag
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  C3_crop
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  C4_crop
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  crop
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    ::  pasture
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  potveg
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  ::  natural_veg
  REAL(dp), ALLOCATABLE, DIMENSION(:)      ::  vegfract_for_types
  LOGICAL,  ALLOCATABLE, DIMENSION(:)      ::  is_naturalveg

! soil parameters and soil moisture
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: max_moisture
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: max_soilwater
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: moisture
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: soilwater
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  :: soilwater_layer
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: soil_depth
  REAL(dp), ALLOCATABLE, DIMENSION(:,:,:)  :: soil_depth_layer
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: matrix_potential
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: soil_field_capacity
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: heat_capacity
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: heat_conductivity
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: bclapp
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: hyd_cond_sat
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: pore_size_index
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: soil_porosity
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: wilting_point
  REAL(dp), ALLOCATABLE, DIMENSION(:,:)    :: root_depth
  REAL(dp), ALLOCATABLE, DIMENSION(:)      :: soillev  
  REAL(dp), ALLOCATABLE, DIMENSION(:)      :: layer_depth  


  ! Indices
  INTEGER  ::  i,j,k

  ! dummies and temporary arrays
  REAL(dp), ALLOCATABLE, DIMENSION(:)   ::  dummy
  REAL(dp), ALLOCATABLE, DIMENSION(:,:) ::  grid_area
  REAL(dp), ALLOCATABLE, DIMENSION(:)   ::  gauss_weights
  REAL(dp)  ::  area_tropical_forest, area_temperate_forest
  REAL(dp)  ::  area_woody, area_shrubs, area_crops, area_pasture
  REAL(dp)  ::  area_grass, area_all, area_land, area_lake
  REAL(dp)  ::  area_land_tropics, area_land_extratropics, area_glac

  ! names of the in- and output files
  CHARACTER(100):: fnam, fnam0, fnam1, fnam2, fnam3, fnam4, fnam5, fnam6, fnam7
  CHARACTER(100):: fnam8, fnam9, fnam10, fnam11, fnam12, fnam13, fnam14, fnam15
  CHARACTER(100):: fnamout
  CHARACTER(8)  :: cyear_ct, cyear_cf, ctiles, clayer, cvegtyp
  CHARACTER(10) :: dynveg_tag

  ! netcdf
  INTEGER       :: stat,  ncin, ncout, ncid, ncin15, varid, dimid
  INTEGER       :: outdimids(6)     ! netcdf dimension ids in the output file
  INTEGER       :: dimids(6)        ! netcdf dimension ids
  CHARACTER(30) :: varname, varname_out

  ! parameters
  REAL(dp)           :: albedo_compensate

  INTEGER            :: second_grass
  INTEGER            :: second_woody
  INTEGER            :: dominant_crop
  CHARACTER(30)      :: resol
  LOGICAL            :: slm_in_input
  REAL(dp)           :: missval, missmpo, missbcl, misshca, misshco, misspor
  REAL(dp)           :: missdep, missfca, misspsi, misshyc, misswpo, missmxm

  ! Namelist Parameters
  INTEGER, PARAMETER :: nnml = 30 ! Fortran unit to read namelist
  LOGICAL        :: lcouple, ldynveg, lc3c4crop, lpasture, lread_pasture, lpasture_rule
  LOGICAL        :: grid_from_rerun, info, cover_fract_only, echam_fractional
  LOGICAL        :: desert_only, woods_only, grass_only
  LOGICAL        :: ignore_measurements
  CHARACTER(10)  :: res_atm, res_oce, maxmoist_version, pasture_tag 
  CHARACTER(100) :: masks_file
  INTEGER        :: year_cf, year_ct
  REAL(dp)       :: slm_fraction  ! only used with T31 in coupled setup, if SLM
                                  !  not available in the echam initial file

  NAMELIST /initctl/ &
  res_atm,         & ! model resolution (T31, T42, ...)
  ntiles,          & ! number of tiles
  nsoil,           & ! number of soil layers
  nlct,            & ! number of land cover types
  year_cf,         & ! reference year for cover fractions
  year_ct,         & ! reference year for cover types
  lcouple,         & ! initial file for coupled run 
  res_oce,         & ! if coupled: ocean model resolution
  ldynveg,         & ! initial file for a run with dynamic vegetation
  lc3c4crop,       & ! initial file for a run with C3 and C4 crops       
  lpasture,        & ! distinguish pastures from grasses       
  lread_pasture,   & ! read pasture and crop fractions from a separate file
  pasture_tag,     & ! tag to define crops and pasture input file (with lread_pasture)
  lpasture_rule,   & ! apply the pasture rule: pasture is allocated primarily on grass land
  slm_fraction,    & ! minimum land fraction of a land grid cell
  echam_fractional, & ! echam uses fractional land see mask
  masks_file,       & ! read land sea, glacier and lake masks from this file
  maxmoist_version, & ! maximum soil moisture array as in echam (LSP2) or new (LSP3) 
  grid_from_rerun, & ! read grid from rerun file instead of jan_surf file
  fnam0,           & ! filename of restart file (if grid_from_rerun = .TRUE.)
  desert_only,     & ! Initialize all land as desert
  grass_only,      & ! Initialize all land with grass
  woods_only,      & ! Initialize all land with woods
  cover_fract_only,& ! Generate file containing just cover fractions and types
  ignore_measurements, & ! Ignore measured data, only use background values
  info               ! TRUE: print some extra information
   
!------------------------------------------------------------------------------
! read namelist
!-------------------------------------
  !-- defaults:
  res_atm          = 'T31'
  ntiles           = 4
  nsoil            = 5
  nlct             = 14
  year_ct          = 800
  year_cf          = 800
  lcouple          = .FALSE.
  res_oce          = 'GR30'
  ldynveg          = .FALSE.
  lc3c4crop        = .FALSE.
  lpasture         = .FALSE.
  lread_pasture    = .FALSE.
  pasture_tag      = 'LUH'
  lpasture_rule    = .FALSE.
  slm_fraction     = 0.5_dp
  echam_fractional = .FALSE.
  maxmoist_version = 'LSP3'
  grid_from_rerun  = .FALSE.
  masks_file       = 'default'
  fnam0            = 'restart_file_name'
  desert_only      = .FALSE.
  grass_only       = .FALSE.
  woods_only       = .FALSE.
  cover_fract_only = .FALSE.
  ignore_measurements = .FALSE.
  info             = .TRUE.

  OPEN (nnml, file='namelist')
  READ (nnml, initctl)
  WRITE(ctiles,'(I0)') ntiles
  WRITE(clayer,'(I0)') nsoil
  WRITE(cyear_ct,'(I0.4)') year_ct
  WRITE(cyear_cf,'(I0.4)') year_cf
  WRITE(cvegtyp,'(I0)') nvegtyp

  IF (info) THEN
     WRITE(*,*) 'Namelist Parameters '
     WRITE(*,*) '  Atmosphere resolution:                 ', res_atm
     WRITE(*,*) '  Configuration with ocean:              ', lcouple
     IF (lcouple) &
        WRITE(*,*) '  Ocean resolution:                      ', res_oce
     WRITE(*,*) '  Number of tiles:                       ', ntiles
     WRITE(*,*) '  Number of land cover types:            ', nlct
     WRITE(*,*) '  Reference year for cover types:        ', year_ct
     WRITE(*,*) '  Reference year for cover fractions:    ', year_cf
     WRITE(*,*) '  Configuration with dynamic vegetation: ', ldynveg
     WRITE(*,*) '  Configuration with C3 and C4 crops:    ',lc3c4crop
     WRITE(*,*) '  Threshold for land in slm:            ', slm_fraction
     WRITE(*,*) '  Pasture:                               ',lpasture
     WRITE(*,*) '    read pasture:                        ',lread_pasture
     WRITE(*,*) '    pasture rule:                        ',lpasture_rule
     WRITE(*,*) '  Desert only:                           ',desert_only
     WRITE(*,*) '  Grass  only:                           ',grass_only
     WRITE(*,*) '  Woods  only:                           ',woods_only
     WRITE(*,*) '  Cover fract only:                      ',cover_fract_only
     WRITE(*,*) '  Ignore measurements:                   ', ignore_measurements
     WRITE(*,*) '  Print extra information:               ', info ! Pretty dummy output :-)
     WRITE(*,*) '  Read grid from another file:           ', grid_from_rerun
     IF (grid_from_rerun) &
        WRITE (*,*) '  Grid file name:                        ',fnam0
     WRITE(*,*) ''
  ENDIF

!-------------------------------------
! define filenames
!-------------------------------------
  !-- input files:
  IF (lcouple) THEN
    resol=TRIM(res_atm)//TRIM(res_oce)
  ELSE
    resol=TRIM(res_atm)
  END IF
  fnam1=TRIM(resol)//'_jan_surf.nc'
  fnam2=TRIM(resol)//'_VLTCLIM.nc'
  fnam3=TRIM(resol)//'_VGRATCLIM.nc'
  fnam4=TRIM(res_atm)//'_TSLCLIM2.nc'
  fnam5='vegtype_'//TRIM(cyear_ct)//'_'//TRIM(res_atm)//'gauss_pa'//TRIM(cvegtyp)//'.nc'
  fnam6='vegmax_6_'//TRIM(res_atm)//'.nc'
  fnam7='albedo_'//TRIM(res_atm)//'.nc'
  fnam8=TRIM(res_atm)//'_topo_75.nc'
  fnam9='vegtype_'//TRIM(cyear_cf)//'_'//TRIM(res_atm)//'gauss_pa'//TRIM(cvegtyp)//'.nc'
  fnam10='C3C4_mask_'//TRIM(res_atm)//'gauss.nc'
  fnam11='C3C4_crop_'//TRIM(res_atm)//'.nc'
  fnam12='LUH_states_'//TRIM(cyear_cf)//'_'//TRIM(res_atm)//'.nc'
  fnam13='potveg_'//TRIM(res_atm)//'.nc'
  fnam14='soil_parameters_'//TRIM(res_atm)//'.nc'
  fnam15=TRIM(masks_file)

  !-- output file:
  IF (echam_fractional) resol=TRIM(resol)//'_fractional'
  IF (cover_fract_only) THEN
     fnamout='cover_fract_'//TRIM(res_atm)//'_'//TRIM(ctiles)//'tiles_'//TRIM(cyear_cf)//'.nc'
  ELSE
     IF (ldynveg) THEN
        dynveg_tag='_dynveg'
     ELSE
        dynveg_tag='_no-dynveg'
     END IF
     IF (nsoil > 1) THEN
        fnamout='jsbach_'//TRIM(resol)//'_'//TRIM(ctiles)//'tiles_'//TRIM(clayer)//'layers_'// &
             TRIM(cyear_cf)//TRIM(dynveg_tag)//'.nc'
     ELSE
        fnamout='jsbach_'//TRIM(resol)//'_'//TRIM(ctiles)//'tiles_'//TRIM(cyear_cf)// &
             TRIM(dynveg_tag)//'.nc'
     END IF
     IF (TRIM(cyear_cf) == '0000') THEN
        fnamout='jsbach_'//TRIM(resol)//'_'//TRIM(ctiles)//'tiles_'//TRIM(clayer)//'layers_natural-veg.nc'
     END IF

  END IF

  !-- print info
  IF (info) THEN
     IF (grid_from_rerun) WRITE (*,*) 'input0: ',fnam0
     WRITE (*,*) 'input 1: ',fnam1
     WRITE (*,*) 'input 2: ',fnam2
     WRITE (*,*) 'input 3: ',fnam3
     WRITE (*,*) 'input 4: ',fnam4
     WRITE (*,*) 'input 5: ',fnam5
     WRITE (*,*) 'input 6: ',fnam6
     WRITE (*,*) 'input 7: ',fnam7
     WRITE (*,*) 'input 8: ',fnam8
     WRITE (*,*) 'input 9: ',fnam9
     WRITE (*,*) 'input10: ',fnam10
     WRITE (*,*) 'input11: ',fnam11
     WRITE (*,*) 'input12: ',fnam12
     WRITE (*,*) 'input13: ',fnam13
     WRITE (*,*) 'input14: ',fnam14
     IF (TRIM(fnam15) /= 'default')  WRITE (*,*) 'input15: ',fnam15
    
     WRITE (*,*) 'output:  ',fnamout
     WRITE (*,*) ' '
  ENDIF

!------------------------------------------------------------------------------
! Check consitency of namelist parameters
!------------------------------------------------------------------------------
  IF (lc3c4crop .AND. (ntiles == 1 .OR. ntiles == 8 .OR. ntiles == 9)) THEN
     CALL hdlerr &
          (1,'C3 and C4 crops indistinguishable in current setups for 1 or 8 tiles')
  END IF
  IF (.NOT. lc3c4crop .AND. (ntiles == 5 .OR. ntiles == 10)) THEN
     CALL hdlerr &
          (1,'lc3c4crop needs to be true in current setups for 5 or 10 tiles')
  END IF
  IF (ldynveg .AND. (ntiles < 8 .OR. ntiles == 13)) THEN 
     CALL hdlerr &
          (1,'running dynveg not possible with less then 8 tiles or 13 tiles')
  END IF
  IF (ldynveg .AND. cover_fract_only) THEN
     CALL hdlerr &
          (1,'generation of land cover maps does not make sense for runs with dynveg')
  END IF
  IF ((grass_only .OR. woods_only) .AND. (ntiles < 8 .OR. ntiles == 13)) THEN 
     CALL hdlerr &
          (1,'grass/woods-only not implemented for less then 8 or 13 tiles')
  END IF
  IF ((lpasture .OR. lread_pasture .OR. lpasture_rule) &
       .AND. (ntiles /= 11 .AND. ntiles /= 12)) THEN
     CALL hdlerr &
          (1,'lpasture, lread_pasture and lpasture_rule need a setup with pasture tile')
  END IF
  IF (lread_pasture .AND. .NOT. lpasture) THEN 
     CALL hdlerr (1,'lread_pasture can only be used with lpasture')
  END IF
  IF (lpasture_rule .AND. .NOT. lread_pasture) THEN 
     CALL hdlerr (1,'pasture rule only possible in a setup with lread_pasture')
  END IF
  IF (lpasture .AND. ntiles /= 11 .AND. .NOT. ldynveg) THEN 
     CALL hdlerr (1,'for lpasture you need a pasture tile. Use 11 tiles.')
  END IF

!------------------------------------------------------------------------------
! Read from input files and define output file
!------------------------------------------------------------------------------
! input-file0: rerun (or jan_surf)
!-------------------------------------
  IF (grid_from_rerun) THEN
     CALL open_file(info,fnam0,ncin)
  ELSE
     CALL open_file(info,fnam1,ncin)
  END IF

  stat = nf_inq_dimid(ncin,'lon',dimid)
  CALL hdlerr(stat,'cannot find dimension lon')
  stat = nf_inq_dimlen(ncin,dimid,nlon)
  CALL hdlerr(stat,'problems with nlon')
  stat = nf_inq_dimid(ncin,'lat',dimid)
  CALL hdlerr(stat,'cannot find dimension lat')
  stat = nf_inq_dimlen(ncin,dimid,nlat)
  CALL hdlerr(stat,'problems with nlat')
  ntime=12

!-------------------------------------
! define the output-file dimensions 
!-------------------------------------
  stat = nf_create(fnamout,NF_CLOBBER,ncout)
  CALL hdlerr(stat,'creating '//fnamout)
  stat = nf_def_dim(ncout,'lon',nlon,outdimids(1))
  CALL hdlerr(stat,'define lon dimension')
  stat = nf_def_dim(ncout,'lat',nlat,outdimids(2))
  CALL hdlerr(stat,'define lat dimension')
  stat = nf_def_dim(ncout,'ntiles',ntiles,outdimids(3))
  CALL hdlerr(stat,'define tile dimension')
  stat = nf_def_dim(ncout,'lct',nlct,outdimids(4))
  CALL hdlerr(stat,'define lct dimension')
  stat = nf_def_dim(ncout,'soillev',nsoil,outdimids(5))
  CALL hdlerr(stat,'define soillev dimension')
  stat = nf_def_dim(ncout,'time',ntime,outdimids(6))
  CALL hdlerr(stat,'define time dimension')

  !-- get longitudes
  varname='lon'
  varname_out='lon'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (lon(nlon))
  stat = nf_get_var_double(ncin,varid,lon)
  CALL hdlerr(stat,'get latitudes from '//fnam0)
  CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  !-- get latitudes
  varname='lat'
  varname_out='lat'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (lat(nlat))
  stat = nf_get_var_double(ncin,varid,lat)
  CALL hdlerr(stat,'get latitudes from '//fnam0)
  CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam0)

  !-- define tiles
  stat = nf_def_var(ncout, 'ntiles', NF_INT, 1, outdimids(3), varid)
  CALL hdlerr(stat,'define var ntiles')
  stat = nf_put_att_text(ncout, varid,'long_name', 5, 'tiles')
  CALL hdlerr(stat,'define var ntiles')

  !-- define soil layers
  stat = nf_def_var(ncout, 'soillev', NF_DOUBLE, 1, outdimids(5), varid)
  CALL hdlerr(stat,'define var soillev')
  stat = nf_put_att_text(ncout, varid, 'long_name', 27, 'soil layer (lower boundary)')
  CALL hdlerr(stat,'define long_name soillev')
  stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
  CALL hdlerr(stat,'define unit soillev')
  stat = nf_put_att_text(ncout, varid, 'axis', 1, 'Z')
  CALL hdlerr(stat,'define axis type soillev')

  !-- define soil layer thickness
  stat = nf_def_var(ncout, 'soil_layer_depth', NF_DOUBLE, 1, outdimids(5), varid)
  CALL hdlerr(stat,'define var soil_layer_depth')
  stat = nf_put_att_text(ncout, varid, 'long_name', 20, 'soil layer thickness')
  CALL hdlerr(stat,'define var soil_layer_depth')
  stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
  CALL hdlerr(stat,'define unit soil_layer_depth')

  !-- define time
  stat = nf_def_var(ncout, 'time', NF_INT, 1, outdimids(6), varid)
  CALL hdlerr(stat,'define var time')
  stat = nf_put_att_text(ncout,varid,'units',16,'days since 1-1-1')
  CALL hdlerr(stat,'define time units')

!-------------------------------------
! input-file1: jan_surf
!-------------------------------------
  CALL open_file(info,fnam1,ncin)
  IF (TRIM(masks_file) /= 'default') CALL open_file(info, fnam15, ncin15)

  !-- get soil wetness
  varname='WS'
  varname_out='init_moist'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (moisture(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,moisture)
  CALL hdlerr(stat,'get soil wetness from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  !-- get snow depth
  varname='SN'
  varname_out='snow'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (snow(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,snow)
  CALL hdlerr(stat,'get snow depth from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  !-- get fractional and integer land sea mask (if available)

  ! check whether integer mask is available
  IF (TRIM(masks_file) /= 'default') THEN
     ncid=ncin15
  ELSE
     ncid=ncin
  END IF
  varname='SLF'
  stat=nf_inq_varid(ncid,varname,varid)
  IF (stat == NF_NOERR) THEN
     slm_in_input=.TRUE.
  ELSE
     slm_in_input=.FALSE.
     varname='SLM'  ! contains fractional land sea mask
  END IF

  !-- get fractional land sea mask
  varname_out='slf'
  CALL check_dimensions(varname, ncid, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (slf(nlon,nlat))   ! fractional land sea mask
  stat = nf_get_var_double(ncid,varid,slf)
  CALL hdlerr(stat,'get fractional land sea mask from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)

  !-- get fractional lake mask
  varname='ALAKE'
  varname_out='lake'
  stat=nf_inq_varid(ncid,varname,varid)
  CALL check_dimensions(varname, ncid, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (lake(nlon,nlat))
  stat = nf_get_var_double(ncid,varid,lake)
  CALL hdlerr(stat,'get lake mask from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)

  ! -- get integer land sea mask and glacier mask
  IF (slm_in_input) THEN
     varname='SLM'
     varname_out='slm'
     CALL check_dimensions(varname, ncid, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     ALLOCATE (slm(nlon,nlat))
     stat = nf_get_var_double(ncid,varid,slm)
     CALL hdlerr(stat,'get integer land sea mask from '//fnam1)
     IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)
  ELSE
     ! calculate SLM from SLF and ALAKE 
     ALLOCATE (slm(nlon,nlat))   ! integer land sea mask
     slm=0._dp
     IF (TRIM(res_atm) == 'T31' .AND. TRIM(res_oce) == 'GR30') THEN
        IF (slm_fraction /= 0.435_dp) THEN
           CALL hdlerr(stat,'slm_fraction for T31GR30 should be 0.435')
        END IF
        DO i=1,nlon
           DO j=1,nlat
              IF (j <= nlat/3 .OR. j > 2*nlat/3) THEN    ! extratropics
                 IF (slf(i,j) > 0.5_dp) slm(i,j) = 1._dp
              ELSE                                       ! tropics
                 IF (slf(i,j) > slm_fraction) slm(i,j) = 1._dp
                 IF (slf(i,j) > slm_fraction .AND. slf(i,j) <= 0.5_dp) &
                      WRITE (*,*) 'Land point while slf is < 0.5: ',i,j,slf(i,j)
              END IF
           END DO
        END DO
     ELSE
        WHERE (slf(:,:) > slm_fraction .AND. .NOT. lake > 0.5_dp)
!             .OR. lake >= 1._dp - slm_fraction .AND. .NOT. lake > 0.5_dp)
           slm(:,:) = 1._dp
        END WHERE
     END IF
     varname_out='slm'
     IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)
  END IF

  ! LU-maps should be independant from the land-sea-mask (i.e. work with different ocean resolutions)
  IF (cover_fract_only) THEN
     slm(:,:) = 1._dp
  END IF

  varname='GLAC'
  varname_out='glac'
  CALL check_dimensions(varname, ncid, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (glac_dp(nlon,nlat))
  stat = nf_get_var_double(ncid,varid,glac_dp)
  CALL hdlerr(stat,'get glacier mask from'//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)

  IF (echam_fractional) THEN
     missval=-9999._dp
     WHERE (slm(:,:) == 0._dp .AND. SPREAD(ABS(lat(:)), DIM=1, NCOPIES=nlon) > 50)
        glac_dp(:,:) = missval
     END WHERE
     WHERE (slf(:,:) > 0._dp .AND. lake(:,:) < 1._dp)
        slm(:,:) = 1._dp
     END WHERE
     CALL extrap(nlon, nlat, lon, lat, slm, missval, 'modal', glac_dp)
  END IF

  ! define non-glacier land mask (internally used)
  ALLOCATE (non_glacier_land(nlon,nlat))
  WHERE (slm(:,:) > 0._dp .AND. glac_dp(:,:) < 1._dp)
     non_glacier_land = 1._dp
  ELSEWHERE
     non_glacier_land = 0._dp
  END WHERE

  !-- get surface roughness length
  varname='AZ0'
  varname_out='roughness_length'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (az0(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,az0)
  CALL hdlerr(stat,'get surface roughness length from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  !-- get surface background albedo
  varname='ALB'
  varname_out='albedo'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (albedo(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,albedo)
  CALL hdlerr(stat,'get surface background albedo from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  !-- get mean orography
  varname='OROMEA'
  varname_out='elevation'
  CALL check_dimensions(varname, ncid, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (elevation(nlon,nlat))
  stat = nf_get_var_double(ncid,varid,elevation)
  CALL hdlerr(stat,'get mean orography from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)

  !-- get orographic standard deviation
  varname='OROSTD'
  varname_out='orography_std_dev'
  CALL check_dimensions(varname, ncid, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (elevationrms(nlon,nlat))
  stat = nf_get_var_double(ncid,varid,elevationrms)
  CALL hdlerr(stat,'get orographic standard deviation from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncid, ncout, varname, varname_out, outdimids)

  !-- get FAO data set (soil data flags)
  varname='FAO'
  varname_out='fao'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (fao(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,fao)
  CALL hdlerr(stat,'get FAO from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  !-- get forest fraction
  varname='FOREST'
  varname_out='forest_fract'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (forest_fract(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,forest_fract)
  CALL hdlerr(stat,'get forest fraction from '//fnam1)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam1)

!------------------------------------------
! input-file1 or 14: field capacity of soil
!------------------------------------------

  IF (maxmoist_version == 'LSP2') THEN
     varname='WSMX'
     fnam=fnam1
  ELSE IF (maxmoist_version == 'LSP3') THEN

     ! With dynveg, the soilwater in the root zone needs to be above a certain minimum value,
     ! not to suppress plant growth with climate change.
     IF (ldynveg) THEN 
        varname='wcap_dynveg'
     ELSE
        varname='wcap'
     END IF
     fnam=fnam14
  ELSE
     CALL hdlerr(stat,'value for namelist parameter maxmoist_version not supported')
  END IF
  CALL open_file(info,fnam,ncin)

  varname_out='maxmoist'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (max_moisture(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,max_moisture)
  CALL hdlerr(stat,'get field capacity of soil from '//fnam)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, missmxm)
  IF (maxmoist_version == 'LSP2') missmxm = 0._dp

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam)

!-------------------------------------
! input-file2: VLTCLIM
!-------------------------------------
  CALL open_file(info,fnam2,ncin)

  !-- get climatolgy of the leaf area index
  varname='VLTCLIM'
  varname_out='lai_clim'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (lai(nlon,nlat,ntime))
  stat = nf_get_var_double(ncin,varid,lai)
  CALL hdlerr(stat,'get leaf area index from'//fnam2)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam2)

!-------------------------------------
! input-file3: VGRATCLIM
!-------------------------------------
  CALL open_file(info,fnam3,ncin)

  !-- get climatology of vegetation ratio
  varname='VGRATCLIM'
  varname_out='veg_fract'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (veg_fract(nlon,nlat,ntime))
  stat = nf_get_var_double(ncin,varid,veg_fract)
  CALL hdlerr(stat,'get vegetation ratio from'//fnam3)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam3)

!-------------------------------------
! input-file4: TSLCLIM2
!-------------------------------------
  CALL open_file(info,fnam4,ncin)

  !-- get climatology of land surface temperature
  varname='TSLCLIM'
  varname_out='surf_temp'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (surf_temp(nlon,nlat,ntime))
  stat = nf_get_var_double(ncin,varid,surf_temp)
  CALL hdlerr(stat,'get surface temperature from'//fnam4)
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam4)

!-------------------------------------
! input-file5: cover fractions from Julia Pongratz (30.8.2006)
!              based on natural vegetation distribution of Ramankutty
!-------------------------------------
  ! With lread_pasture we use natural vegetation and separate maps for crops and pastures.
  IF ( .NOT. lread_pasture) THEN
    CALL open_file(info,fnam5,ncin)

    !-- get fractional cover
    varname='vegfract'
    CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
    ALLOCATE (vegfract(nlon,nlat,nvegtyp))
    stat = nf_get_var_double(ncin,varid,vegfract)
    CALL hdlerr(stat,'get fractional cover from'//fnam5)

    stat = nf_close(ncin)
    CALL hdlerr(stat,'closing '//fnam5)
  ELSE
    ALLOCATE (vegfract(nlon,nlat,nvegtyp))
  END IF

  varname_out='cover_fract'
  CALL define_var_new(ncout, varname_out, 3, outdimids(1:3))
  varname_out='cover_type'
  CALL define_var_new(ncout, varname_out, 3, outdimids(1:3))

!-------------------------------------
! input-file6: vegmax_6 (max. vegetation cover)
!-------------------------------------
  CALL open_file(info,fnam6,ncin)
  
  !-- get veg_ratio_max
  varname='veg_ratio_max'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (veg_ratio_max(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,veg_ratio_max)
  CALL hdlerr(stat,'get veg_ratio_max from'//fnam6)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam6)

  IF (desert_only) veg_ratio_max(:,:) = 0._dp
  IF (grass_only .OR. woods_only) THEN
     WHERE (slm(:,:) > 0.5_dp .AND. glac_dp(:,:) < 0.5_dp)
        veg_ratio_max(:,:) = 1._dp
     END WHERE
  END IF
  IF (ignore_measurements) THEN
     veg_ratio_max(:,:) = 1._dp
  END IF

  !-- veg_ratio_max should be non-zero
  missval=0._dp
  CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missval, 'mean', veg_ratio_max)
  WHERE (non_glacier_land(:,:) > 0.5_dp)
     veg_ratio_max(:,:) = MAX(small_fract,veg_ratio_max(:,:))
  END WHERE
  varname_out='veg_ratio_max'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))

!-------------------------------------
! input-file7: albedo (nir/vis vegetation/soil albedo)
!-------------------------------------
  CALL open_file(info,fnam7,ncin)

  !-- get albedo
  varname='albedo_veg_vis'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (albedo_veg_vis_in(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,albedo_veg_vis_in)
  CALL hdlerr(stat,'get vegetation albedo in vis. range from'//fnam7)

  varname='albedo_veg_nir'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (albedo_veg_nir_in(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,albedo_veg_nir_in)
  CALL hdlerr(stat,'get vegetation albedo in NIR from'//fnam7)

  varname='albedo_soil_vis'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (albedo_soil_vis_in(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,albedo_soil_vis_in)
  CALL hdlerr(stat,'get soil albedo in vis. range from'//fnam7)

  varname='albedo_soil_nir'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (albedo_soil_nir_in(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,albedo_soil_nir_in)
  CALL hdlerr(stat,'get soil albedo in NIR from'//fnam7)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam7)

  ALLOCATE (albedo_veg_vis(nlon,nlat))
  ALLOCATE (albedo_veg_nir(nlon,nlat))
  ALLOCATE (albedo_soil_vis(nlon,nlat))
  ALLOCATE (albedo_soil_nir(nlon,nlat))
  DO j = 1,nlat
     albedo_compensate = 0.0_dp
     IF (ABS(lat(j)) > 40._dp) THEN
        albedo_compensate = 0.01_dp
     END IF
     IF (ABS(lat(j)) > 50._dp) THEN
        albedo_compensate = 0.02_dp
     END IF
     IF (ABS(lat(j)) > 60._dp) THEN
        albedo_compensate = 0.03_dp
     END IF
     IF (ABS(lat(j)) > 70._dp) THEN
        albedo_compensate = 0.04_dp
     END IF
     IF (.NOT. ignore_measurements) THEN
        DO i = 1,nlon/2
           albedo_veg_vis(i,j) =  albedo_veg_vis_in(i + nlon/2,j)  + albedo_compensate
           albedo_veg_nir(i,j) =  albedo_veg_nir_in(i + nlon/2,j)  + albedo_compensate
           albedo_soil_vis(i,j) = albedo_soil_vis_in(i + nlon/2,j) + albedo_compensate
           albedo_soil_nir(i,j) = albedo_soil_nir_in(i + nlon/2,j) + albedo_compensate
        END DO
        DO i = nlon/2 + 1,nlon
           albedo_veg_vis(i,j) =  albedo_veg_vis_in(i - nlon/2,j)  + albedo_compensate
           albedo_veg_nir(i,j) =  albedo_veg_nir_in(i - nlon/2,j)  + albedo_compensate
           albedo_soil_vis(i,j) = albedo_soil_vis_in(i - nlon/2,j) + albedo_compensate
           albedo_soil_nir(i,j) = albedo_soil_nir_in(i - nlon/2,j) + albedo_compensate
        END DO
     ELSE   ! ignore measured data from the continents, only use background values
        albedo_veg_vis(:,j) =  albedo_veg_vis_in(1,1)  + albedo_compensate
        albedo_veg_nir(:,j) =  albedo_veg_nir_in(1,1)  + albedo_compensate
        albedo_soil_vis(:,j) = albedo_soil_vis_in(1,1) + albedo_compensate
        albedo_soil_nir(:,j) = albedo_soil_nir_in(1,1) + albedo_compensate
     END IF
  END DO
  DEALLOCATE (albedo_veg_vis_in, albedo_veg_nir_in, albedo_soil_vis_in, albedo_soil_nir_in)
  
  varname_out='albedo_veg_vis'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))
  varname_out='albedo_veg_nir'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))
  varname_out='albedo_soil_vis'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))
  varname_out='albedo_soil_nir'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))

!-------------------------------------
! input-file8: topo_75 (roughness of the orography)
!-------------------------------------
  ALLOCATE (roughness_oro(nlon,nlat))
  IF (.NOT. ignore_measurements) THEN
     CALL open_file(info,fnam8,ncin)

     !-- get roughness length
     varname='roughness_length_oro'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     stat = nf_get_var_double(ncin,varid,roughness_oro)
     CALL hdlerr(stat,'get roughness length of the orography from'//fnam8)

     stat = nf_close(ncin)
     CALL hdlerr(stat,'closing '//fnam8)

     roughness_oro(:,:) = MIN(20._dp,roughness_oro(:,:))
  ELSE
     ! simply taking the same arrays for roughness_length and roughness_length_oro 
     roughness_oro(:,:) =  MIN(20._dp,az0(:,:))
  END IF
  varname_out='roughness_length_oro'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))

!-------------------------------------
! input-file9: cover fractions from Julia Pongratz (30.8.2006)
!              based on natural vegetation distribution of Ramankutty
!-------------------------------------

  IF (year_ct /= year_cf .AND. .NOT. lread_pasture) THEN
     CALL open_file(info,fnam9,ncin)

     !-- get cover fractions of year year_cf
     varname='vegfract'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     ALLOCATE (vegfract_cf(nlon,nlat,nvegtyp))
     stat = nf_get_var_double(ncin,varid,vegfract_cf)
     CALL hdlerr(stat,'get fractional cover from'//fnam9)

     stat = nf_close(ncin)
     CALL hdlerr(stat,'closing '//fnam9)
  END IF
 
!-------------------------------------
! input-file10:   C3/C4 mask
!-------------------------------------
  CALL open_file(info,fnam10,ncin)

  !-- get C3/C4 fmask
  varname='1C3_0C4'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (C3C4_flag(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,C3C4_flag)
  CALL hdlerr(stat,'get C3/C4 flag from'//fnam10)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam10)

  C3C4_flag(:,:) = MIN(1.1_dp,MAX(-0.1_dp,C3C4_flag(:,:)))

!-------------------------------------
! input-file11:   C3 and C4 crops
!-------------------------------------

  ALLOCATE (C3_crop(nlon,nlat))
  C3_crop(:,:) = 0._dp
  ALLOCATE (C4_crop(nlon,nlat))
  C4_crop(:,:) = 0._dp
  
  IF (lc3c4crop) THEN 
     CALL open_file(info,fnam11,ncin)

     !-- get C3 and C4 maps
     varname='C3_crop'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     stat = nf_get_var_double(ncin,varid,C3_crop)
     CALL hdlerr(stat,'get map of C3 crop from'//fnam11)
     varname='C4_crop'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     stat = nf_get_var_double(ncin,varid,C4_crop)
     CALL hdlerr(stat,'get map of C4 crop from'//fnam11)

     stat = nf_close(ncin)
     CALL hdlerr(stat,'closing '//fnam11)
  END IF

!-------------------------------------
! input-file12: pasture and crop fractions
!               based on harmonized landuse scenarios needed for CMIP
!-------------------------------------

  ALLOCATE (pasture(nlon,nlat))
  pasture(:,:) = 0._dp
  ALLOCATE (crop(nlon,nlat))
  crop(:,:) = 0._dp

  IF (lread_pasture) THEN 
     CALL open_file(info,fnam12,ncin)

     !-- get pasture fractions
     varname='gpast'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     stat = nf_get_var_double(ncin,varid,pasture)
     CALL hdlerr(stat,'get map of pasture fractions from'//fnam12)
     CALL get_missval(ncin, varid, missval)
     WHERE (pasture(:,:) == missval) pasture(:,:) = 0._dp

     !-- get crop fractions
     varname='gcrop'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     stat = nf_get_var_double(ncin,varid,crop)
     CALL hdlerr(stat,'get map of crop fractions from'//fnam12)
     CALL get_missval(ncin, varid, missval)
     WHERE (crop(:,:) == missval) crop(:,:) = 0._dp

     stat = nf_close(ncin)
     CALL hdlerr(stat,'closing '//fnam12)
  END IF

!-------------------------------------
! input-file13: potential vegetation (Julia Pongratz)
!               based on natural vegetation distribution of Ramankutty
!-------------------------------------
  IF (ldynveg .OR. lread_pasture) THEN
     CALL open_file(info,fnam13,ncin)

     !-- get fractional cover
     varname='cover_fract'
     CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
     ALLOCATE (potveg(nlon,nlat,nvegtyp))
     stat = nf_get_var_double(ncin,varid,potveg)
     CALL hdlerr(stat,'get potential vegetation from'//fnam13)

     stat = nf_close(ncin)
     CALL hdlerr(stat,'closing '//fnam13)

     varname_out='natural_veg'
     IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 3, outdimids(1:3))
  END IF

!-------------------------------------
! input-file14: 5 soil layer parameters (Stefan Hagemann)
!-------------------------------------
  CALL open_file(info,fnam14,ncin)

  !-- get soil depth
  varname='soildepth'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (soil_depth(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,soil_depth)
  CALL hdlerr(stat,'get soil depth from'//fnam14)
  varname_out='soil_depth'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, missdep)

  !-- get soil porosity 
  varname='volporosity'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (soil_porosity(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,soil_porosity)
  CALL hdlerr(stat,'get soil porosity from'//fnam14)
  varname_out='soil_porosity'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, misspor)

  !-- get soil pore size index 
  varname='psi_lambda'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (pore_size_index(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,pore_size_index)
  CALL hdlerr(stat,'get soil pore size index from'//fnam14)
  varname_out='pore_size_index'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, misspsi)

  !-- get soil field capacity 
  varname='volfc'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (soil_field_capacity(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,soil_field_capacity)
  CALL hdlerr(stat,'get soil field cap from'//fnam14)
  varname_out='soil_field_cap'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, missfca)

  !-- get soil heat capacity 
  varname='heatcapacity'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (heat_capacity(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,heat_capacity)
  CALL hdlerr(stat,'get soil heat capacity from'//fnam14)
  varname_out='heat_capacity'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, misshca)

  !-- get soil heat conductivity 
  varname='heatcond'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (heat_conductivity(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,heat_conductivity)
  CALL hdlerr(stat,'get soil heat conductivity from'//fnam14)
  varname_out='heat_conductivity'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, misshco)

  !-- get soil moisture potential 
  varname='matrixpot'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (matrix_potential(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,matrix_potential)
  CALL hdlerr(stat,'get soil moisture potential from'//fnam14)
  varname_out='moisture_pot'
  IF (.NOT. cover_fract_only)  CALL define_var(ncin, ncout, varname, varname_out, outdimids, missmpo)

  !-- get saturated hydraulic conductivity
  varname='hydcond'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (hyd_cond_sat(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,hyd_cond_sat)
  CALL hdlerr(stat,'get saturated hydraulic conductivity from'//fnam14)
  varname_out='hyd_cond_sat'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, misshyc)

  !-- get wilting point 
  varname='volpwp'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (wilting_point(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,wilting_point)
  CALL hdlerr(stat,'get wilting point from'//fnam14)
  varname_out='wilting_point'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, misswpo)

  !-- get Clapp & Hornberger exponent b
  varname='bclapp'
  CALL check_dimensions(varname, ncin, varid, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime)
  ALLOCATE (bclapp(nlon,nlat))
  stat = nf_get_var_double(ncin,varid,bclapp)
  CALL hdlerr(stat,'get Clapp & Hornberger exponent b from'//fnam14)
  varname_out='bclapp'
  IF (.NOT. cover_fract_only) CALL define_var(ncin, ncout, varname, varname_out, outdimids, missbcl)

  stat = nf_close(ncin)
  CALL hdlerr(stat,'closing '//fnam14)

  !-- Define output variables to be calculated
  varname_out='root_depth'
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 2, outdimids(1:2))

  varname_out='layer_moist'
  dimids(1:3)=(/outdimids(1),outdimids(2),outdimids(5)/)
  IF (.NOT. cover_fract_only) CALL define_var_new(ncout, varname_out, 3, dimids(1:3))

!------------------------------------------------------------------------------
! CREATE OUTPUT FIELDS
!------------------------------------------------------------------------------
! TILES
!--------------------
  ALLOCATE (tiles(ntiles))
  DO i=1,ntiles
     tiles(i) = i
  END DO

! SOIL LAYER
!--------------------
  ALLOCATE (layer_depth(nsoil))
  IF (nsoil == 5) THEN
    layer_depth=(/0.065_dp,0.254_dp,0.913_dp,2.902_dp,5.700_dp/)
  ELSE IF (nsoil /= 1) THEN
    CALL hdlerr(1,"currently only 5 soil layers supported")
  END IF

  ALLOCATE (soillev(nsoil))
  soillev(1) = layer_depth(1)
  DO i=2,nsoil
     soillev(i) = soillev(i-1) + layer_depth(i)
  END DO

!--------------------
! TIME
!--------------------
  ALLOCATE (time_days(12))
  time_days=(/30,58,89,119,150,180,211,242,272,303,333,364/)


!---------------------------------
! cover types and cover fractions
!---------------------------------

  ALLOCATE (cover_type(nlon,nlat,ntiles))
  ALLOCATE (cover_fract(nlon,nlat,ntiles))
  ALLOCATE (vegfract_for_types(nvegtyp))
  ALLOCATE (is_naturalveg(ntiles))
  IF (ldynveg .OR. lread_pasture) ALLOCATE (natural_veg(nlon,nlat,ntiles))

  DO j=1,nlat
     DO i=1,nlon
        !
        !-- calculate cover fractions depending on the number of tiles
        !

        IF (slm(i,j) < 0.5_dp) THEN

           !-- ocean
           cover_type(i,j,:) = 0
           cover_fract(i,j,:) = 0._dp
           IF (ldynveg .OR. lread_pasture) natural_veg(i,j,:) = 0._dp
        ELSE

           !-- land

           ! if crops and pastures are read from a separate file, use the
           ! potential vegetation to get the fractions of the natural types
           IF (lread_pasture) vegfract(i,j,:) = potveg(i,j,:)


           !-- adapt vegetation fractions to derive reasonable types

           vegfract_for_types(:) = vegfract(i,j,:)
           CALL adapt_vegfract (lat(j), NINT(C3C4_flag(i,j)), info, .TRUE., &
                lpasture, ignore_measurements, vegfract_for_types(:))


           !-- calculate the cover types

           CALL calc_cover_types (ntiles, lat(j), NINT(glac_dp(i,j)), &
                vegfract_for_types(:), lc3c4crop, C3_crop(i,j), C4_crop(i,j), &
                cover_type(i,j,:), second_woody, second_grass, dominant_crop, &
                is_naturalveg(:))


           !-- make sure all land points have reasonable vegetation fractions
           !   (actual vegetation: vegfract; potential vegetation: potveg)

           IF (year_ct == year_cf .OR. lread_pasture) THEN
              CALL adapt_vegfract (lat(j), NINT(C3C4_flag(i,j)), info, .FALSE., &
                   lpasture, ignore_measurements, vegfract(i,j,:))
           ELSE
              CALL adapt_vegfract (lat(j), NINT(C3C4_flag(i,j)), info, .FALSE., &
                   lpasture, ignore_measurements, vegfract_cf(i,j,:))
              vegfract(i,j,:) = vegfract_cf(i,j,:)
           END IF
           IF (ldynveg .OR. lread_pasture) THEN
              CALL adapt_vegfract (lat(j), NINT(C3C4_flag(i,j)), info, .FALSE., &
                   .FALSE., ignore_measurements, potveg(i,j,:))
           END IF


           !-- calculate the actual cover fractions

           CALL calc_cover_fractions (ntiles, lat(j), cover_type(i,j,:), &
                second_woody, second_grass, NINT(glac_dp(i,j)), vegfract(i,j,:), &
                grass_only, woods_only, lc3c4crop, C3_crop(i,j), C4_crop(i,j), &
                crop(i,j), lread_pasture, lpasture_rule, pasture(i,j), &
                NINT(C3C4_flag(i,j)), cover_fract(i,j,:))

           !-- make sure, that all vegetation types have a minimum fraction of 
           !   small_fract and the sum of all vegetation is 1. in all grid cells.

           CALL scale_cover_fract (ntiles, is_naturalveg(:), NINT(glac_dp(i,j)), &
                cover_fract(i,j,:))

           IF (ldynveg .OR. lread_pasture) THEN

              !-- calculate the potential vegetation

              CALL calc_cover_fractions (ntiles, lat(j), cover_type(i,j,:), &
                   second_woody, second_grass, NINT(glac_dp(i,j)), potveg(i,j,:), &
                   grass_only, woods_only, lc3c4crop, 0._dp, 0._dp, &
                   0._dp, .FALSE., lpasture_rule, 0._dp, &
                   NINT(C3C4_flag(i,j)), natural_veg(i,j,:))

              CALL scale_cover_fract (ntiles, is_naturalveg(:), NINT(glac_dp(i,j)), &
                   natural_veg(i,j,:))

              !-- make sure, that potential cover fractions of natural vegetation are
              !   greater than the actual cover fractions.

              CALL harmonize_fractions (ntiles, is_naturalveg(:), NINT(glac_dp(i,j)), &
                   natural_veg(i,j,:), cover_fract(i,j,:))
           END IF

        END IF
     END DO
  END DO

  DEALLOCATE (crop, pasture, vegfract_for_types, C3C4_flag, is_naturalveg)
  IF (year_ct /= year_cf .AND. .NOT. lread_pasture) DEALLOCATE(vegfract_cf)
  IF (ldynveg .OR. lread_pasture) DEALLOCATE (potveg)


  IF (.NOT. cover_fract_only) THEN
     !--------------------------------------------------
     ! get fao map consistent with land-sea mask
     !--------------------------------------------------
     missval=0._dp
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missval, 'modal', fao)
     WHERE (glac_dp(:,:) == 1._dp)
        fao(:,:) = 3._dp
     END WHERE

     !---------------------------------------------------------------------
     ! get soil parameter arrays consistent with land sea and glacier mask
     !---------------------------------------------------------------------
     IF (ignore_measurements) THEN
        moisture(:,:)            = 0.3_dp       ! [m]
        max_moisture(:,:)        = 0.5_dp       ! [m]
        fao(:,:)                 = 3._dp        ! []
        matrix_potential(:,:)    = 0.3_dp       ! [m]
        bclapp(:,:)              = 6._dp        ! []
        heat_capacity(:,:)       = 2.26E06_dp   ! [J m-3 K-1]
        heat_conductivity(:,:)   = 6._dp        ! [J m-1 s-1 K-1]
        soil_porosity(:,:)       = 0.45_dp      ! [m/m]
        soil_depth(:,:)          = 5._dp        ! [m]
        soil_field_capacity(:,:) = 0.3_dp       ! [m/m]
        pore_size_index(:,:)     = 0.3_dp       ! []
        hyd_cond_sat(:,:)        = 6.E-06_dp    ! [m/s]
        wilting_point(:,:)       = 0.15_dp      ! [m/m]
     END IF
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missmpo, 'mean', matrix_potential)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missbcl, 'mean', bclapp)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, misshca, 'mean', heat_capacity)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, misshco, 'mean', heat_conductivity)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, misspor, 'mean', soil_porosity)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missdep, 'mean', soil_depth)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missfca, 'mean', soil_field_capacity)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, misspsi, 'mean', pore_size_index)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, misshyc, 'mean', hyd_cond_sat)
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, misswpo, 'mean', wilting_point)


     !----------------------------------------------------------------------
     ! get consistant values for water holding capacity and soil parameters
     !----------------------------------------------------------------------
     ALLOCATE (max_soilwater(nlon,nlat))
     ALLOCATE (root_depth(nlon,nlat))

     !
     !    max_soilwater: maximum amount of water possibly held in soil
     !        soilwater: initial amount of soil water
     !     max_moisture: maximum amount of water reachable for plants
     !         moisture: initial amount of water reachable for plants
     !       root_depth: length of the roots
     !       soil_depth: depth of the soil above bedrock

     ! soil_depth
     !------------
     !  Since Lithosols are defined by FAO/Unesco as "soils which are limited in
     !  depth by continuous coherent hard rock within 10 cm of the surface," I
     !  don't see the need to differentiate between them to such a degree (for
     !  my purposes). I consider all Lithosols to be 10 cm thick, and use only
     !  the surface texture to distinguish them. (K. Dunne, pers. comm to St. Hagemann, 1995)
     ! => soil_depth needs to be at least 10 cm if area is not impermeable.
     WHERE (non_glacier_land == 1)
        soil_depth(:,:) = MAX(soil_depth(:,:), 0.1_dp)
     END WHERE

     ! maximum amount of water fitting into the soil
     max_soilwater(:,:) = soil_field_capacity(:,:) * soil_depth(:,:)

     ! max_moisture
     !--------------
     ! get max_moisture values for all land grid cells
     CALL extrap(nlon, nlat, lon, lat, non_glacier_land, missmxm, 'mean', max_moisture)

     ! maximum amount of water in the root zone cannot be more than what is fitting into the soil
     max_moisture(:,:) = MIN(max_moisture(:,:), max_soilwater(:,:))

     ! define small value on glaciers for technical reasons
     WHERE (glac_dp == 1)
       max_moisture(:,:) = small_fract
     END WHERE

     ! root_depth
     !------------
     ! root depth is calculated backwards from the maximum amout of water in the root zone
     WHERE (soil_field_capacity(:,:) > 0._dp)
        root_depth(:,:) = MIN(soil_depth(:,:), max_moisture(:,:)/soil_field_capacity(:,:))
     ELSEWHERE
        root_depth(:,:) = 0._dp
     ENDWHERE

     DEALLOCATE (max_soilwater)


     !-----------------------------------------------------------
     ! adapt initial soil moisture
     !-----------------------------------------------------------

     ALLOCATE (soilwater(nlon,nlat))
     ALLOCATE (soil_depth_layer(nlon,nlat,nsoil))
     ALLOCATE (soilwater_layer(nlon,nlat,nsoil))

     ! initial soil moisture must be below maximum soil moisture
     moisture(:,:)   = MIN(moisture(:,:),     max_moisture(:,:))
     !   ... and should not be too small to prevent vegetation die back due to initialization
     moisture(:,:)   = MAX(moisture(:,:), 0.5_dp*max_moisture(:,:))

     ! initialize soil moisture below the wilting point in desert areas 
     WHERE (veg_ratio_max(:,:) < 0.2_dp)
        moisture(:,:) = MIN(moisture(:,:), 0.3_dp*max_moisture(:,:))
     END WHERE

     ! initial moisture value for each soil level
     ! ------------------------------------------

     ! initial value of total amount of soil water (not in initial file)
     WHERE (root_depth(:,:) > 0._dp)
        soilwater(:,:) = moisture(:,:) * soil_depth(:,:) / root_depth(:,:)
     ELSEWHERE
        soilwater(:,:) = 0._dp
     ENDWHERE

     ! calculate soil layer depth for each soil level
     soil_depth_layer(:,:,1) = MIN(soil_depth(:,:), layer_depth(1))
     DO i=2,nsoil
        WHERE (soil_depth(:,:) > SUM(layer_depth(1:i-1)))
           soil_depth_layer(:,:,i) = MIN(soil_depth(:,:)-SUM(layer_depth(1:i-1)), layer_depth(i))
        ELSEWHERE
           soil_depth_layer(:,:,i) = 0._dp
        ENDWHERE
     END DO

     ! initial moisture value for each soil level
     ! Conservation of soil water (all soil water, even below root zone)
     DO i=1,nsoil
        WHERE (soilwater(:,:) >= 0._dp .AND. soil_depth(:,:) > 0._dp )
           soilwater_layer(:,:,i) = soilwater(:,:)/soil_depth(:,:) * soil_depth_layer(:,:,i)
        ELSEWHERE
           soilwater_layer(:,:,i) = 0._dp
        ENDWHERE
     END DO

     DEALLOCATE(soilwater)
     DEALLOCATE(soil_depth_layer)

  END IF  ! .NOT. cover_fract_only
   
!-----------------------------------------------------------
! calculate areas covered by grass, woods, deserts
!-----------------------------------------------------------
  ALLOCATE (dummy(nlat))
  ALLOCATE (gauss_weights(nlat))
  CALL gauaw(dummy,gauss_weights,nlat,pi)
  ALLOCATE (grid_area(nlon,nlat))
  IF (echam_fractional) THEN
     grid_area(:,:) = slf(:,:) * SPREAD(gauss_weights(:), DIM=1, NCOPIES=nlon) &
          * 2._dp * pi * radius_of_earth**2 / REAL(nlon)
  ELSE
     grid_area(:,:) = SPREAD(gauss_weights(:), DIM=1, NCOPIES=nlon) &
          * 2._dp * pi * radius_of_earth**2 / REAL(nlon)
  END IF
  DEALLOCATE (dummy)
  area_tropical_forest   = 0._dp
  area_temperate_forest  = 0._dp
  area_woody             = 0._dp
  area_shrubs            = 0._dp
  area_crops             = 0._dp
  area_pasture           = 0._dp
  area_grass             = 0._dp
  area_all               = 0._dp
  area_lake              = 0._dp
  area_land              = 0._dp
  area_land_tropics      = 0._dp
  area_land_extratropics = 0._dp
  area_glac              = 0._dp
  DO j = 1,nlat
     DO i = 1,nlon
        IF (echam_fractional) THEN
           area_all = area_all + gauss_weights(j) * 2._dp * pi * radius_of_earth**2 / REAL(nlon)
        ELSE
           area_all = area_all + grid_area(i,j)
        END IF
        IF (lake(i,j) > 0.5_dp) area_lake = area_lake + grid_area(i,j)
        IF (slm(i,j) > 0.5_dp) THEN                   ! land
           area_land = area_land + grid_area(i,j)
           DO k = 1,ntiles

              ! tropical forest
              IF (cover_type(i,j,k) == lct_tropical_evergreen &
                   .OR. cover_type(i,j,k) == lct_tropical_deciduous) THEN
                 area_tropical_forest = area_tropical_forest + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
                 area_woody = area_woody + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)

              ! temperate forest
              ELSE IF (cover_type(i,j,k) == lct_extratrop_evergreen &
                   .OR. cover_type(i,j,k) == lct_extratrop_deciduous &
                   .OR. cover_type(i,j,k) == lct_temperate_broadl_evergreen &
                   .OR. cover_type(i,j,k) == lct_temperate_broadl_deciduous &
                   .OR. cover_type(i,j,k) == lct_evergreen_conifer &
                   .OR. cover_type(i,j,k) == lct_deciduous_conifer) THEN
                 area_temperate_forest = area_temperate_forest + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
                 area_woody = area_woody + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)

              ! shrubs
              ELSE IF (cover_type(i,j,k) == lct_raingreen_shrub &
                   .OR. cover_type(i,j,k) == lct_deciduous_shrub) THEN
                 area_shrubs = area_shrubs + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
                 area_woody = area_woody + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)

              ! grass (and pasture)
              ELSE IF (cover_type(i,j,k) == lct_c3grass &
                   .OR. cover_type(i,j,k) == lct_c4grass) THEN
                 area_grass = area_grass + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)

              ! crops
              ELSE IF (cover_type(i,j,k) == lct_crop &
                   .OR. cover_type(i,j,k) == lct_c3crop &
                   .OR. cover_type(i,j,k) == lct_c4crop) THEN
                 area_crops = area_crops + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)

              ! pasture
              ELSE IF (cover_type(i,j,k) == lct_pasture &
                   .OR. cover_type(i,j,k) == lct_c3pasture &
                   .OR. cover_type(i,j,k) == lct_c4pasture) THEN
                 area_pasture = area_pasture + &
                      grid_area(i,j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
              END IF

              ! glaciers
              IF (glac_dp(i,j) >= 0.5_dp) THEN
                 area_glac = area_glac + grid_area(i,j) * cover_fract(i,j,k)

              ! tropical land
              ELSE IF (j <= nlat/3 .OR. j > 2*nlat/3) THEN
                 area_land_extratropics = area_land_extratropics &
                      + grid_area(i,j) * cover_fract(i,j,k)

              ! extratropical land
              ELSE
                 area_land_tropics = area_land_tropics &
                      + grid_area(i,j) * cover_fract(i,j,k)
              END IF
           END DO
        END IF
     END DO 
  END DO
  DEALLOCATE (gauss_weights, grid_area)
  DEALLOCATE (non_glacier_land)

  IF (info) THEN
     WRITE (*,*) ''
     WRITE (*,*) 'The surface of the globe [km2]: ',area_all * 1.e-06
     WRITE (*,*) 'Global land surface [km2]: ',area_land * 1.e-06
     WRITE (*,*) 'Global land surface including lakes [km2]: ',(area_land+area_lake) * 1.e-06
     WRITE (*,*) 'Global glacier [km2]: ',area_glac * 1.e-06_dp
     WRITE (*,*) 'Land tropics [km2]: ',area_land_tropics * 1.e-06
     WRITE (*,*) 'Land extratropics [km2]: ',area_land_extratropics * 1.e-06
     WRITE (*,*) 'nlat,nlat/3,2*nlat/3: ',nlat,nlat/3,2*nlat/3
     WRITE (*,*) 'Area of tropical forest [km2]: ',area_tropical_forest * 1.e-06
     WRITE (*,*) 'Area of temperate + boreal forest [km2]: ',area_temperate_forest * 1.e-06
     WRITE (*,*) 'Area of crops[km2]: ',area_crops * 1.e-06
     WRITE (*,*) 'Area of pasture [km2]: ',area_pasture * 1.e-06
     WRITE (*,*) 'Area of all woody types [km2]: ',area_woody * 1.e-06
     WRITE (*,*) 'Area of grass lands (incl. pastures)[km2]: ',area_grass * 1.e-06
  ENDIF

!------------------------------------------------------------------------------
! Write the output file
!------------------------------------------------------------------------------

  CALL set_global_attributes(ncout, year_ct, year_cf, res_oce, res_atm, &
       nlct, ntiles, nsoil, lcouple, ldynveg, lc3c4crop, lpasture, lread_pasture, &
       lpasture_rule, desert_only, grass_only, woods_only, ignore_measurements, &
       echam_fractional, maxmoist_version, pasture_tag, masks_file, svn_url, svn_rev)
  stat = nf_enddef(ncout)
  CALL hdlerr(stat,'problem with enddef')

  !-- dimension variables

  stat = nf_inq_varid(ncout,'lat',varid)
  CALL hdlerr(stat,'cannot find lat')
  stat = nf_put_var_double(ncout,varid,lat(:))
  CALL hdlerr(stat,'put variable lat')
  DEALLOCATE(lat)

  stat = nf_inq_varid(ncout,'lon',varid)
  CALL hdlerr(stat,'cannot find lon')
  stat = nf_put_var_double(ncout,varid,lon(:))
  CALL hdlerr(stat,'put variable lon')
  DEALLOCATE(lon)

  stat = nf_inq_varid(ncout,'ntiles',varid)
  CALL hdlerr(stat,'cannot find ntiles')
  stat = nf_put_var_int(ncout,varid,tiles(:))
  CALL hdlerr(stat,'put variable tiles')
  DEALLOCATE(tiles)

  stat = nf_inq_varid(ncout,'soillev',varid)
  CALL hdlerr(stat,'cannot find soillev')
  stat = nf_put_var_double(ncout,varid,soillev(:))
  CALL hdlerr(stat,'put variable soillev')
  DEALLOCATE(soillev)

  stat = nf_inq_varid(ncout,'soil_layer_depth',varid)
  CALL hdlerr(stat,'cannot find soil_layer_depth')
  stat = nf_put_var_double(ncout,varid,layer_depth(:))
  CALL hdlerr(stat,'put variable soil_layer_depth')
  DEALLOCATE(layer_depth)

  stat = nf_inq_varid(ncout,'time',varid)
  CALL hdlerr(stat,'cannot find time')
  stat = nf_put_var_int(ncout,varid,time_days(:))
  CALL hdlerr(stat,'put variable time')
  DEALLOCATE(time_days)

  !-- other variables

  stat = nf_inq_varid(ncout,'cover_fract',varid)
  CALL hdlerr(stat,'cannot find cover_fract')
  stat = nf_put_var_double(ncout,varid,cover_fract(:,:,:))
  CALL hdlerr(stat,'put variable cover_fract')
  DEALLOCATE(cover_fract)

  ALLOCATE(cover_type_dp(nlon,nlat,ntiles))
  cover_type_dp(:,:,:)=REAL(cover_type(:,:,:))
  DEALLOCATE(cover_type)
  stat = nf_inq_varid(ncout,'cover_type',varid)
  CALL hdlerr(stat,'cannot find cover_type')
  stat = nf_put_var_double(ncout,varid,cover_type_dp(:,:,:))
  CALL hdlerr(stat,'put variable cover_type')
  DEALLOCATE(cover_type_dp)

  IF (.NOT. cover_fract_only) THEN
     stat = nf_inq_varid(ncout,'veg_fract',varid)
     CALL hdlerr(stat,'cannot find veg_fract')
     stat = nf_put_var_double(ncout,varid,veg_fract(:,:,:))
     CALL hdlerr(stat,'put variable veg_fract')
     DEALLOCATE(veg_fract)

     stat = nf_inq_varid(ncout,'lai_clim',varid)
     CALL hdlerr(stat,'cannot find lai_clim')
     stat = nf_put_var_double(ncout,varid,lai(:,:,:))
     CALL hdlerr(stat,'put variable lai_clim')
     DEALLOCATE(lai)

     stat = nf_inq_varid(ncout,'elevation',varid)
     CALL hdlerr(stat,'cannot find elevation')
     stat = nf_put_var_double(ncout,varid,elevation(:,:))
     CALL hdlerr(stat,'put variable elevation')
     DEALLOCATE(elevation)

     stat = nf_inq_varid(ncout,'orography_std_dev',varid)
     CALL hdlerr(stat,'cannot find orography_std_dev')
     stat = nf_put_var_double(ncout,varid,elevationrms(:,:))
     CALL hdlerr(stat,'put variable orography_std_dev')
     DEALLOCATE(elevationrms)

     stat = nf_inq_varid(ncout,'roughness_length',varid)
     CALL hdlerr(stat,'cannot find roughness_length')
     stat = nf_put_var_double(ncout,varid,az0(:,:))
     CALL hdlerr(stat,'put variable roughness_length')
     DEALLOCATE(az0)

     stat = nf_inq_varid(ncout,'roughness_length_oro',varid)
     CALL hdlerr(stat,'cannot find roughness_length_oro')
     stat = nf_put_var_double(ncout,varid,roughness_oro(:,:))
     CALL hdlerr(stat,'put variable roughness_length_oro')
     DEALLOCATE(roughness_oro)

     stat = nf_inq_varid(ncout,'albedo',varid)
     CALL hdlerr(stat,'cannot find albedo')
     stat = nf_put_var_double(ncout,varid,albedo(:,:))
     CALL hdlerr(stat,'put variable albedo')
     DEALLOCATE(albedo)

     stat = nf_inq_varid(ncout,'fao',varid)
     CALL hdlerr(stat,'cannot find fao')
     stat = nf_put_var_double(ncout,varid,fao(:,:))
     CALL hdlerr(stat,'put variable fao')
     DEALLOCATE(fao)

     stat = nf_inq_varid(ncout,'maxmoist',varid)
     CALL hdlerr(stat,'cannot find maxmoist')
     stat = nf_put_var_double(ncout,varid,max_moisture(:,:))
     CALL hdlerr(stat,'put variable maxmoist')
     DEALLOCATE(max_moisture)

     stat = nf_inq_varid(ncout,'forest_fract',varid)
     CALL hdlerr(stat,'cannot find forest_fract')
     stat = nf_put_var_double(ncout,varid,forest_fract(:,:))
     CALL hdlerr(stat,'put variable forest_fract')
     DEALLOCATE(forest_fract)

     stat = nf_inq_varid(ncout,'init_moist',varid)
     CALL hdlerr(stat,'cannot find init_moist')
     stat = nf_put_var_double(ncout,varid,moisture(:,:))
     CALL hdlerr(stat,'put variable init_moist')
     DEALLOCATE(moisture)

     stat = nf_inq_varid(ncout,'veg_ratio_max',varid)
     CALL hdlerr(stat,'cannot find veg_ratio_max')
     stat = nf_put_var_double(ncout,varid,veg_ratio_max(:,:))
     CALL hdlerr(stat,'put variable veg_ratio_max')
     DEALLOCATE(veg_ratio_max)

     stat = nf_inq_varid(ncout,'albedo_veg_vis',varid)
     CALL hdlerr(stat,'cannot find albedo_veg_vis')
     stat = nf_put_var_double(ncout,varid,albedo_veg_vis(:,:))
     CALL hdlerr(stat,'put variable albedo_veg_vis')
     DEALLOCATE(albedo_veg_vis)

     stat = nf_inq_varid(ncout,'albedo_veg_nir',varid)
     CALL hdlerr(stat,'cannot find albedo_veg_nir')
     stat = nf_put_var_double(ncout,varid,albedo_veg_nir(:,:))
     CALL hdlerr(stat,'put variable albedo_veg_nir')
     DEALLOCATE(albedo_veg_nir)

     stat = nf_inq_varid(ncout,'albedo_soil_vis',varid)
     CALL hdlerr(stat,'cannot find albedo_soil_vis')
     stat = nf_put_var_double(ncout,varid,albedo_soil_vis(:,:))
     CALL hdlerr(stat,'put variable albedo_soil_vis')
     DEALLOCATE(albedo_soil_vis)

     stat = nf_inq_varid(ncout,'albedo_soil_nir',varid)
     CALL hdlerr(stat,'cannot find albedo_soil_nir')
     stat = nf_put_var_double(ncout,varid,albedo_soil_nir(:,:))
     CALL hdlerr(stat,'put variable albedo_soil_nir')
     DEALLOCATE(albedo_soil_nir)

     stat = nf_inq_varid(ncout,'snow',varid)
     CALL hdlerr(stat,'cannot find snow')
     stat = nf_put_var_double(ncout,varid,snow(:,:))
     CALL hdlerr(stat,'put variable snow')
     DEALLOCATE(snow)

     stat = nf_inq_varid(ncout,'slf',varid)
     CALL hdlerr(stat,'cannot find slf')
     stat = nf_put_var_double(ncout,varid,slf(:,:))
     CALL hdlerr(stat,'put variable slf')
     DEALLOCATE(slf)

     stat = nf_inq_varid(ncout,'lake',varid)
     CALL hdlerr(stat,'cannot find lake')
     stat = nf_put_var_double(ncout,varid,lake(:,:))
     CALL hdlerr(stat,'put variable lake')
     DEALLOCATE(lake)

     stat = nf_inq_varid(ncout,'slm',varid)
     CALL hdlerr(stat,'cannot find slm')
     stat = nf_put_var_double(ncout,varid,slm(:,:))
     CALL hdlerr(stat,'put variable slm')
     DEALLOCATE(slm)

     stat = nf_inq_varid(ncout,'glac',varid)
     CALL hdlerr(stat,'cannot find glac')
     stat = nf_put_var_double(ncout,varid,glac_dp(:,:))
     CALL hdlerr(stat,'put variable glac')
     DEALLOCATE(glac_dp)

     stat = nf_inq_varid(ncout,'surf_temp',varid)
     CALL hdlerr(stat,'cannot find surf_temp')
     stat = nf_put_var_double(ncout,varid,surf_temp(:,:,:))
     CALL hdlerr(stat,'put variable surf_temp')
     DEALLOCATE(surf_temp)

     IF (ldynveg .OR. lread_pasture) THEN
        stat = nf_inq_varid(ncout,'natural_veg',varid)
        CALL hdlerr(stat,'cannot find natural_veg')
        stat = nf_put_var_double(ncout,varid,natural_veg(:,:,:))
        CALL hdlerr(stat,'put variable natural_veg')
        DEALLOCATE(natural_veg)
     END IF

     stat = nf_inq_varid(ncout,'soil_depth',varid)
     CALL hdlerr(stat,'cannot find soil_depth')
     stat = nf_put_var_double(ncout,varid,soil_depth(:,:))
     CALL hdlerr(stat,'put variable soil_depth')
     DEALLOCATE(soil_depth)

     stat = nf_inq_varid(ncout,'heat_conductivity',varid)
     CALL hdlerr(stat,'cannot find heat_conductivity')
     stat = nf_put_var_double(ncout,varid,heat_conductivity(:,:))
     CALL hdlerr(stat,'put variable heat_conductivity')
     DEALLOCATE(heat_conductivity)

     stat = nf_inq_varid(ncout,'heat_capacity',varid)
     CALL hdlerr(stat,'cannot find heat_capacity')
     stat = nf_put_var_double(ncout,varid,heat_capacity(:,:))
     CALL hdlerr(stat,'put variable heat_capacity')
     DEALLOCATE(heat_capacity)

     stat = nf_inq_varid(ncout,'bclapp',varid)
     CALL hdlerr(stat,'cannot find bclapp')
     stat = nf_put_var_double(ncout,varid,bclapp(:,:))
     CALL hdlerr(stat,'put variable bclapp')
     DEALLOCATE(bclapp)

     stat = nf_inq_varid(ncout,'soil_porosity',varid)
     CALL hdlerr(stat,'cannot find soil_porosity')
     stat = nf_put_var_double(ncout,varid,soil_porosity(:,:))
     CALL hdlerr(stat,'put variable soil_porosity')
     DEALLOCATE(soil_porosity)

     stat = nf_inq_varid(ncout,'moisture_pot',varid)
     CALL hdlerr(stat,'cannot find moisture_pot')
     stat = nf_put_var_double(ncout,varid,matrix_potential(:,:))
     CALL hdlerr(stat,'put variable moisture_pot')
     DEALLOCATE(matrix_potential)

     stat = nf_inq_varid(ncout,'soil_field_cap',varid)
     CALL hdlerr(stat,'cannot find soil_field_cap')
     stat = nf_put_var_double(ncout,varid,soil_field_capacity(:,:))
     CALL hdlerr(stat,'put variable soil_field_cap')
     DEALLOCATE(soil_field_capacity)

     stat = nf_inq_varid(ncout,'hyd_cond_sat',varid)
     CALL hdlerr(stat,'cannot find hyd_cond_sat')
     stat = nf_put_var_double(ncout,varid,hyd_cond_sat(:,:))
     CALL hdlerr(stat,'put variable hyd_cond_sat')
     DEALLOCATE(hyd_cond_sat)

     stat = nf_inq_varid(ncout,'pore_size_index',varid)
     CALL hdlerr(stat,'cannot find pore_size_index')
     stat = nf_put_var_double(ncout,varid,pore_size_index(:,:))
     CALL hdlerr(stat,'put variable pore_size_index')
     DEALLOCATE(pore_size_index)

     stat = nf_inq_varid(ncout,'wilting_point',varid)
     CALL hdlerr(stat,'cannot find wilting_point')
     stat = nf_put_var_double(ncout,varid,wilting_point(:,:))
     CALL hdlerr(stat,'put variable wilting_point')
     DEALLOCATE(wilting_point)

     stat = nf_inq_varid(ncout,'layer_moist',varid)
     CALL hdlerr(stat,'cannot find layer_moist')
     stat = nf_put_var_double(ncout,varid,soilwater_layer(:,:,:))
     CALL hdlerr(stat,'put variable layer_moist')
     DEALLOCATE(soilwater_layer)

     stat = nf_inq_varid(ncout,'root_depth',varid)
     CALL hdlerr(stat,'cannot find root_depth')
     stat = nf_put_var_double(ncout,varid,root_depth(:,:))
     CALL hdlerr(stat,'put variable root_depth')
     DEALLOCATE(root_depth)

  END IF

  stat = nf_close(ncout)
  CALL hdlerr(stat,'closing file')
   
 END PROGRAM jsbach_init_file

!------------------------------------------------------------------------------
SUBROUTINE adapt_vegfract (lat, C3C4_flag, info, for_types, lpasture, &
     ignore_measurements, vegfract)
!------------------------------------------------------------------------------
!
! Routine to modify vegetation fractions
!
!------------------------------------------------------------------------------
  USE mo_kinds
  USE mo_vegparams

  IMPLICIT NONE

  !-- INTENT(in)
  INTEGER,      INTENT(in) :: C3C4_flag   ! 1 for C3, 0 for C4 grasses
  REAL(dp),     INTENT(in) :: lat         ! latitude
  LOGICAL,      INTENT(in) :: info        ! print some additional information
  LOGICAL,      INTENT(in) :: for_types   ! calculation of cover types will be 
                                          ! based on vegfract calculated here
  LOGICAL,      INTENT(in) :: lpasture    ! distinguish pasture from grass lands
  LOGICAL,      INTENT(in) :: ignore_measurements ! covertypes only depend on latitude

  !-- INTENT(inout)
  REAL(dp),     INTENT(inout) :: vegfract(nvegtyp)

  !-- LOCAL
  INTEGER, SAVE      :: icount=0
  INTEGER      :: k
  INTEGER      :: idominant, isecond

!------------------------------------------------------------------------------

  IF (nvegtyp /= 14) THEN
     CALL hdlerr(1,'adapt_vegfract: Routine for 14 vegetation types')
  END IF

  IF (ignore_measurements) THEN
     vegfract(:) = 0._dp
  END IF

  !-- There should not be negative vegetation coverage (missing value)

  vegfract(:) = MAX(vegfract(:),0._dp)

  !-- Define reasonable cover fractions for land points without any vegetation

  If (SUM(vegfract(:)) <= small_fract+EPSILON(1._dp)) THEN
     IF (ABS(lat) < 30._dp) THEN
        vegfract(veg_tropical_evergreen) = 2._dp * small_fract
     ELSE IF  (ABS(lat) < 40._dp) THEN
        vegfract(veg_temperate_broadl_evergreen) = 2._dp * small_fract
     ELSE 
        vegfract(veg_evergreen_conifer) = 2._dp * small_fract
     END IF
     IF (ABS(lat) < 40._dp) THEN
        vegfract(veg_raingreen_shrub) = 2._dp * small_fract
        vegfract(veg_c4grass) = 2._dp * small_fract
     ELSE
        vegfract(veg_deciduous_shrub) = 2._dp * small_fract
        vegfract(veg_c3grass) = 2._dp * small_fract
     END IF
  END IF

  !-- change tropical forest to temperate/boreal forest outside 40S-40N

  IF (ABS(lat) > 40._dp) THEN
     vegfract(veg_temperate_broadl_evergreen) = vegfract(veg_temperate_broadl_evergreen) &
          + vegfract(veg_tropical_evergreen)
     vegfract(veg_temperate_broadl_deciduous) = vegfract(veg_temperate_broadl_deciduous) &
          + vegfract(veg_tropical_deciduous)
     vegfract(veg_tropical_evergreen) = 0._dp
     vegfract(veg_tropical_deciduous) = 0._dp
  END IF

  IF (.NOT. lpasture) THEN
     !-- add C3 pasture to C3 grass and C4 pasture to C4 grass

     vegfract(veg_c3grass) = vegfract(veg_c3grass) + vegfract(veg_c3pasture)
     vegfract(veg_c4grass) = vegfract(veg_c4grass) + vegfract(veg_c4pasture)
     vegfract(veg_c3pasture) = 0._dp
     vegfract(veg_c4pasture) = 0._dp
  END IF


  !-- In case C3 and C4 grasses have the same cover fraction, the C3/C4 flag 
  !   determines which grass dominates

  IF (ABS(vegfract(veg_c3grass) - vegfract(veg_c4grass)) < small_fract) THEN
     !! IF (info) WRITE (*,*) 'C3 and C4 grasses have the same cover fractions: ', &
     !!     vegfract(veg_c3grass), vegfract(veg_c4grass)
     IF (C3C4_flag == 0) THEN
        vegfract(veg_c4grass) = MIN(1._dp,vegfract(veg_c4grass) + small_fract*10._dp)
        vegfract(veg_c3grass) = MAX(0._dp,vegfract(veg_c3grass) - small_fract*10._dp)
     ELSE
        vegfract(veg_c4grass) = MAX(0._dp,vegfract(veg_c4grass) - small_fract*10._dp)
        vegfract(veg_c3grass) = MIN(1._dp,vegfract(veg_c3grass) + small_fract*10._dp)
     END IF
  END IF

  IF (for_types) THEN
     !-- In case the cover fractions of the two dominant PFTs are the same, they
     !   are slightly modified.

     idominant = 1
     DO k = 2,nvegtyp
        IF (vegfract(k) > vegfract(idominant)) idominant = k
     END DO
     IF (idominant /= 1) isecond = 1
     IF (idominant == 1) isecond = 2
     DO k = 2,nvegtyp
        IF (vegfract(k) > vegfract(isecond) .AND. k /= idominant) isecond = k
     END DO
     IF (ABS(vegfract(idominant) - vegfract(isecond)) < small_fract) THEN
        IF (info) WRITE (*,*) 'The two dominant PFTs have the same cover fractions: ', &
             idominant, isecond, vegfract(idominant), vegfract(isecond)
        IF (MOD(icount,2) == 1) THEN
           vegfract(idominant) = vegfract(idominant) + small_fract * 10._dp
           vegfract(isecond)   = vegfract(isecond)   - small_fract * 10._dp
        ELSE
           vegfract(idominant) = vegfract(idominant) - small_fract * 10._dp
           vegfract(isecond)   = vegfract(isecond)   + small_fract * 10._dp
        END IF
        icount = icount + 1
     END IF
  END IF

END SUBROUTINE adapt_vegfract

!------------------------------------------------------------------------------
SUBROUTINE calc_cover_fractions (ntiles, lat, cover_type, second_woody, &
     second_grass, glac, vegfract, grass_only, woods_only, lc3c4crop, &
     C3_crop, C4_crop, crop, lread_pasture, lpasture_rule, pasture, &
     C3C4_flag, cover_fract)
!------------------------------------------------------------------------------
!
! Routine to calculate land cover types and fractions depending on the number
! of tiles
!
!------------------------------------------------------------------------------
  USE mo_kinds
  USE mo_vegparams

  IMPLICIT NONE

  !-- INTENT(in)
  INTEGER,  INTENT(in) :: ntiles      ! number of tiles
  REAL(dp), INTENT(in) :: lat         ! latitude
  INTEGER,  INTENT(in) :: glac        ! glacier flag (1: glacier)
  INTEGER,  INTENT(in) :: cover_type(ntiles)
  INTEGER,  INTENT(in) :: second_woody, second_grass
  REAL(dp), INTENT(in) :: vegfract(nvegtyp)
  LOGICAL,  INTENT(in) :: woods_only, grass_only
  LOGICAL,  INTENT(in) :: lc3c4crop        ! distinguish C3 crops from C4 crops
  LOGICAL,  INTENT(in) :: lread_pasture   ! use pastures of array 'pasture' and
                                           !    crops from array 'crop'
  LOGICAL,  INTENT(in) :: lpasture_rule    ! allocate pasture primarily on grass lands
  REAL(dp), INTENT(in) :: C3_crop     ! fraction of C3 crops
  REAL(dp), INTENT(in) :: C4_crop     ! fraction of C4 crops
  REAL(dp), INTENT(in) :: crop        ! crop fraction (if lread_pasture)
  REAL(dp), INTENT(in) :: pasture     ! pasture fraction (if lread_pasture)
  INTEGER,  INTENT(in) :: C3C4_flag   ! flag used to decide whether C3 or C4
                                      ! grass/pasture is dominant

  !-- INTENT(out)
  REAL(dp),    INTENT(out) :: cover_fract(ntiles)

  !-- LOCAL
  INTEGER      :: k
  REAL(dp)     :: vegfract_woody, vegfract_grass
  REAL(dp)     :: vegfract_c3grass, vegfract_c4grass
  REAL(dp)     :: vegfract_crop, vegfract_c3crop, vegfract_c4crop
  REAL(dp)     :: vegfract_c3pasture, vegfract_c4pasture
  REAL(dp)     :: c3ratio

!------------------------------------------------------------------------------

  IF (nvegtyp /= 14) THEN
     CALL hdlerr(1,'calc_cover_fractions: Routine for 14 vegetation types')
  END IF

  !-- calculate woody fraction and grass fraction
  vegfract_woody = 0._dp
  DO k=1,nwoody
     vegfract_woody =  vegfract_woody + vegfract(woody_types(k))
  END DO
  vegfract_grass = vegfract(veg_c3grass) + vegfract(veg_c4grass)

  !-- find out C3 ratio of grass land
  IF (vegfract_grass > 0._dp) THEN
     c3ratio = vegfract(veg_c3grass) / vegfract_grass
  ELSE
     c3ratio = REAL(C3C4_flag, dp)    ! 1 for C3, 0 for C4
  END IF
  
  !-- apply the pasture rule: pasture is primarily allocated on grass lands
  IF (lpasture_rule .AND. lread_pasture) THEN
     vegfract_grass = MAX(0._dp, vegfract_grass - pasture)
     vegfract_c3grass = vegfract_grass * c3ratio
     vegfract_c4grass = vegfract_grass * (1._dp - c3ratio)
  ELSE
     vegfract_c3grass = vegfract(veg_c3grass)
     vegfract_c4grass = vegfract(veg_c4grass)
  END IF

  !-- Calculate the fractions of C3 and C4 pasture from the total pasture
  IF (lread_pasture) THEN
     vegfract_c3pasture = pasture * c3ratio
     vegfract_c4pasture = pasture * (1._dp - c3ratio)
  ELSE
     vegfract_c3pasture = vegfract(veg_c3pasture)
     vegfract_c4pasture = vegfract(veg_c4pasture)
  END IF
   
  !-- Calculate crop fractions
  IF (lread_pasture) THEN
     vegfract_crop = crop
  ELSE
     vegfract_crop = vegfract(veg_crop)
  END IF
  IF (lc3c4crop) THEN
     !-- Calculate C3 and C4 crop fractions by scaling with C3/C4_crop maps
     !     needed only if there is more than one tile reserved for crops
     IF ((C3_crop + C4_crop) > EPSILON(1._dp)) THEN
        vegfract_c3crop = vegfract(veg_crop) * (C3_crop / (C3_crop + C4_crop))
        vegfract_c4crop = vegfract(veg_crop) * (C4_crop / (C3_crop + C4_crop))
     ELSE
        vegfract_c3crop = 0._dp
        vegfract_c4crop = 0._dp
     END IF
  ELSE
     vegfract_c3crop = 0._dp
     vegfract_c4crop = 0._dp
  END IF

  !-- glaciers
  IF (glac == 1) THEN
     cover_fract(1) = 1._dp
     cover_fract(2:ntiles) = 0._dp
  ELSE
     SELECT CASE (ntiles)
        CASE (1)
           cover_fract(1) = 1._dp
        CASE (3)
           cover_fract(1) = vegfract_woody
           cover_fract(2) = vegfract_grass
           cover_fract(3) = vegfract_crop
        CASE (4)
           IF (cover_type(3) == lct(second_woody)) THEN
              cover_fract(1) = vegfract_woody - vegfract(second_woody)
              cover_fract(2) = vegfract_grass
              cover_fract(3) = vegfract(second_woody)
           ELSE
              cover_fract(1) = vegfract_woody
              cover_fract(2) = vegfract_grass - vegfract(second_grass)
              cover_fract(3) = vegfract(second_grass)
           END IF
           cover_fract(4) = vegfract_crop
        CASE (5)
           IF (cover_type(3) == lct(second_woody)) THEN
              cover_fract(1) = vegfract_woody - vegfract(second_woody)
              cover_fract(2) = vegfract_grass
              cover_fract(3) = vegfract(second_woody)
           ELSE
              cover_fract(1) = vegfract_woody
              cover_fract(2) = vegfract_grass - vegfract(second_grass)
              cover_fract(3) = vegfract(second_grass)
           END IF
           cover_fract(4) = vegfract_c3crop
           cover_fract(5) = vegfract_c4crop
        CASE (6)
           cover_fract(1) = vegfract(veg_tropical_deciduous) &
                          + vegfract(veg_temperate_broadl_deciduous) &
                          + vegfract(veg_deciduous_conifer)
           cover_fract(2) = vegfract(veg_tropical_evergreen) &
                          + vegfract(veg_temperate_broadl_evergreen) &
                          + vegfract(veg_evergreen_conifer)
           cover_fract(3) = vegfract(veg_raingreen_shrub) &
                          + vegfract(veg_deciduous_shrub) &
                          + vegfract(veg_tundra)
           cover_fract(4) = vegfract_c3grass
           cover_fract(5) = vegfract_c4grass
           cover_fract(6) = vegfract_crop
        CASE (8)
           cover_fract(1) = vegfract(veg_tropical_evergreen)
           cover_fract(2) = vegfract(veg_tropical_deciduous)
           cover_fract(3) = vegfract(veg_temperate_broadl_evergreen) &
                          + vegfract(veg_evergreen_conifer)
           cover_fract(4) = vegfract(veg_temperate_broadl_deciduous) &
                          + vegfract(veg_deciduous_conifer)
           cover_fract(5) = vegfract(veg_raingreen_shrub)
           cover_fract(6) = vegfract(veg_deciduous_shrub)
           cover_fract(7) = vegfract_c3grass + vegfract(veg_tundra)
           cover_fract(8) = vegfract_c4grass
        CASE (9)
           cover_fract(1) = 0._dp
           cover_fract(2) = vegfract(veg_tropical_evergreen)
           cover_fract(3) = vegfract(veg_tropical_deciduous)
           cover_fract(4) = vegfract(veg_temperate_broadl_evergreen) &
                          + vegfract(veg_evergreen_conifer)
           cover_fract(5) = vegfract(veg_temperate_broadl_deciduous) &
                          + vegfract(veg_deciduous_conifer)
           cover_fract(6) = vegfract(veg_raingreen_shrub)
           cover_fract(7) = vegfract(veg_deciduous_shrub)
           cover_fract(8) = vegfract_c3grass + vegfract(veg_tundra)
           cover_fract(9) = vegfract_c4grass
        CASE (10)
           cover_fract(1) = vegfract(veg_tropical_evergreen)
           cover_fract(2) = vegfract(veg_tropical_deciduous)
           cover_fract(3) = vegfract(veg_temperate_broadl_evergreen) + vegfract(veg_evergreen_conifer)
           cover_fract(4) = vegfract(veg_temperate_broadl_deciduous) + vegfract(veg_deciduous_conifer)
           cover_fract(5) = vegfract(veg_raingreen_shrub)
           cover_fract(6) = vegfract(veg_deciduous_shrub)
           cover_fract(7) = vegfract_c3grass + vegfract(veg_tundra)
           cover_fract(8) = vegfract_c4grass
           cover_fract(9) = vegfract_c3crop
           cover_fract(10)= vegfract_c4crop
        CASE (11)
           cover_fract(1) = vegfract(veg_tropical_evergreen)
           cover_fract(2) = vegfract(veg_tropical_deciduous)
           cover_fract(3) = vegfract(veg_temperate_broadl_evergreen) + vegfract(veg_evergreen_conifer)
           cover_fract(4) = vegfract(veg_temperate_broadl_deciduous) + vegfract(veg_deciduous_conifer)
           cover_fract(5) = vegfract(veg_raingreen_shrub)
           cover_fract(6) = vegfract(veg_deciduous_shrub)
           cover_fract(7) = vegfract_c3grass + vegfract(veg_tundra)
           cover_fract(8) = vegfract_c4grass
           cover_fract(9) = vegfract_c3pasture
           cover_fract(10)= vegfract_c4pasture
           cover_fract(11) = vegfract_crop
        CASE (12)
           cover_fract(1) = 0._dp
           cover_fract(2) = vegfract(veg_tropical_evergreen)
           cover_fract(3) = vegfract(veg_tropical_deciduous)
           cover_fract(4) = vegfract(veg_temperate_broadl_evergreen) + vegfract(veg_evergreen_conifer)
           cover_fract(5) = vegfract(veg_temperate_broadl_deciduous) + vegfract(veg_deciduous_conifer)
           cover_fract(6) = vegfract(veg_raingreen_shrub)
           cover_fract(7) = vegfract(veg_deciduous_shrub)
           cover_fract(8) = vegfract_c3grass + vegfract(veg_tundra)
           cover_fract(9) = vegfract_c4grass
           cover_fract(10)= vegfract_c3pasture
           cover_fract(11)= vegfract_c4pasture
           cover_fract(12) = vegfract_crop
        CASE (13)
           cover_fract(1) = vegfract(veg_tropical_evergreen)
           cover_fract(2) = vegfract(veg_tropical_deciduous)
           cover_fract(3) = vegfract(veg_temperate_broadl_evergreen)
           cover_fract(4) = vegfract(veg_temperate_broadl_deciduous)
           cover_fract(5) = vegfract(veg_evergreen_conifer)
           cover_fract(6) = vegfract(veg_deciduous_conifer)
           cover_fract(7) = vegfract(veg_raingreen_shrub)
           cover_fract(8) = 0._dp
           cover_fract(9) = vegfract_c3grass
           cover_fract(10) = vegfract_c4grass
           cover_fract(11) = vegfract(veg_tundra)
           cover_fract(12) = 0._dp
           cover_fract(13) = vegfract_crop
        CASE (14)
           cover_fract(1) = 0._dp
           cover_fract(2) = vegfract(veg_tropical_evergreen)
           cover_fract(3) = vegfract(veg_tropical_deciduous)
           cover_fract(4) = vegfract(veg_temperate_broadl_evergreen)
           cover_fract(5) = vegfract(veg_temperate_broadl_deciduous)
           cover_fract(6) = vegfract(veg_evergreen_conifer)
           cover_fract(7) = vegfract(veg_deciduous_conifer)
           cover_fract(8) = vegfract(veg_raingreen_shrub)
           cover_fract(9) = 0._dp
           cover_fract(10)= vegfract_c3grass
           cover_fract(11) = vegfract_c4grass
           cover_fract(12) = vegfract(veg_tundra)
           cover_fract(13) = 0._dp
           cover_fract(14) = vegfract_crop
        CASE DEFAULT
           CALL hdlerr(1,'land cover: number of tiles not supported')
     END SELECT

     ! cover fractions for grass-only or woods-only simulations
     IF (grass_only) THEN
        cover_fract(1:6) = 0._dp
        IF (C3C4_flag == 1) THEN
           cover_fract(7) = 1._dp
        ELSE
           cover_fract(8) = 1._dp
        END IF
     ELSE IF (woods_only) THEN
        cover_fract(5:8) = 0._dp
        IF (SUM(cover_fract(1:4)) <= small_fract) THEN
           IF (ABS(lat) < 30._dp) THEN
              cover_fract(1) = 1._dp
           ELSE
              cover_fract(3) = 1._dp
           END IF
        END IF
     END IF

  END IF

END SUBROUTINE calc_cover_fractions

!------------------------------------------------------------------------------
SUBROUTINE calc_cover_types (ntiles, lat, glac, vegfract, lc3c4crop, C3_crop, &
     C4_crop, cover_type, second_woody, second_grass, dominant_crop, &
     is_naturalveg)
!------------------------------------------------------------------------------
!
! Routine to calculate land cover types depending on the number of tiles
!
!------------------------------------------------------------------------------
  USE mo_kinds
  USE mo_vegparams
  IMPLICIT NONE

  !-- INTENT(IN)
  INTEGER,  INTENT(in)  :: ntiles   ! number of tiles
  REAL(dp), INTENT(in)  :: lat      ! latitude
  INTEGER,  INTENT(in)  :: glac     ! glacier flag (1: glacier)
  REAL(dp), INTENT(in)  :: vegfract(nvegtyp) ! vegetated area fraction
  LOGICAL,  INTENT(in)  :: lc3c4crop         ! distinguish C3 and C4 crops
  REAL(dp), INTENT(in)  :: C3_crop
  REAL(dp), INTENT(in)  :: C4_crop

  !-- INTENT(OUT)
  INTEGER,  INTENT(out) :: cover_type(ntiles)
  INTEGER,  INTENT(out) :: second_woody, second_grass, dominant_crop
  LOGICAL,  INTENT(out) :: is_naturalveg(ntiles)

  !-- LOCAL
  INTEGER               :: k
  INTEGER               :: dominant_woody, dominant_grass
  INTEGER               :: dominant_pft(1)
  REAL(dp)              :: vegfract_woody

!------------------------------------------------------------------------------

  IF (nvegtyp /= 14) THEN
     CALL hdlerr(1,'calc_cover_types: Routine for 14 vegetation types')
  END IF
  
  !-- calculate woody fraction and grass fraction

  vegfract_woody = 0._dp
  DO k=1,nwoody
     vegfract_woody =  vegfract_woody + vegfract(woody_types(k))
  END DO

  !-- find out most spread woody type

  dominant_woody = woody_types(1)
  DO k = 2,nwoody
     IF (vegfract(woody_types(k)) > vegfract(dominant_woody)) &
          dominant_woody = woody_types(k)
  END DO

  !-- find out second most spread woody type

  IF (woody_types(1) /= dominant_woody) THEN
     second_woody = woody_types(1)
  ELSE
     second_woody = woody_types(2)
  END IF
  DO k = 1,nwoody
     IF (vegfract(woody_types(k)) > vegfract(second_woody) .AND. &
          woody_types(k) /= dominant_woody) second_woody = woody_types(k)
  END DO
  !substantial tundra is always represented
  IF (dominant_woody /= veg_tundra .AND. vegfract(veg_tundra) > substantial_tundra) &
       second_woody = veg_tundra

  !-- find out most and second most spread grass type

  IF (vegfract(veg_c3grass) > vegfract(veg_c4grass)) THEN
     dominant_grass = veg_c3grass
     second_grass = veg_c4grass
  ELSE
     dominant_grass = veg_c4grass
     second_grass = veg_c3grass
  END IF

  !-- find out most spread crop type

  IF (C3_crop > C4_crop .AND. C3_crop > small_fract) THEN
     dominant_crop = lct_c3crop
  ELSE IF (C4_crop > C3_crop .AND. C4_crop > small_fract) THEN
     dominant_crop = lct_c4crop
  ELSE IF (dominant_grass == veg_c3grass) THEN
     dominant_crop = lct_c3crop
  ELSE
     dominant_crop = lct_c4crop
  END IF

  !-- find out dominant type

  dominant_pft(:) = MAXLOC(vegfract(:))

  !-- initialize natural vegetation flag

  IF (glac == 1) THEN
     is_naturalveg(:) = .FALSE.
  ELSE
     is_naturalveg(:) = .TRUE.
  END IF

  IF (ntiles == 1) THEN
!------------------------------------------------------------------------------
! 1 tile: dominant PFT
!------------------------------------------------------------------------------
     IF (glac == 1) THEN           ! glacier
        cover_type(1) = lct_glacier

     ELSE IF (MAXVAL(vegfract) <= small_fract * 10._dp) THEN

        !-- no substantial vegetation coverage

        IF (ABS(lat) < 30._dp) THEN
           cover_type(1) = lct_tropical_deciduous
        ELSE
           cover_type(1) = lct_evergreen_conifer
        END IF
     ELSE
        !-- substantial vegetation

        cover_type(1) = lct(dominant_pft(1))

     ENDIF

  ELSE IF (ntiles == 3) THEN
!------------------------------------------------------------------------------
! 3 tiles: 1: woody
!          2: grasses
!          3: crops
!------------------------------------------------------------------------------
     IF (glac == 1) THEN           ! glacier
        cover_type(1) = lct_glacier
        cover_type(2) = lct_c3grass

     ELSE IF (MAXVAL(vegfract) <= small_fract * 10._dp) THEN

        !-- no substantial vegetation coverage

        IF (ABS(lat) < 30._dp) THEN
           cover_type(1) = lct_tropical_deciduous
           cover_type(2) = lct_c4grass
        ELSE
           cover_type(1) = lct_evergreen_conifer
           cover_type(2) = lct_c3grass
        END IF
     ELSE

        !-- substantial vegetation

        cover_type(1) = lct(dominant_woody)
        cover_type(2) = lct(dominant_grass)
     ENDIF
     cover_type(3) = lct_crop
     is_naturalveg(3) = .FALSE.

  ELSE IF (ntiles == 4) THEN
!------------------------------------------------------------------------------
! 4 tiles: 1: woody
!          2: grasses
!          3: second tree or grass (depending on fraction)
!          4: crops
!------------------------------------------------------------------------------ 
     IF (glac == 1) THEN           ! glacier
        cover_type(1) = lct_glacier
        cover_type(2) = lct_c3grass
        cover_type(3) = lct_evergreen_conifer

     ELSE IF (MAXVAL(vegfract) <= small_fract * 10._dp) THEN

        !-- no substantial vegetation coverage

        IF (ABS(lat) < 30._dp) THEN
           cover_type(1) = lct_tropical_deciduous
           cover_type(2) = lct_c4grass
           cover_type(3) = lct_tropical_evergreen
        ELSE
           cover_type(1) = lct_evergreen_conifer
           cover_type(2) = lct_c3grass
           cover_type(3) = lct_temperate_broadl_deciduous
        END IF
     ELSE

        !-- substantial vegetation

        cover_type(1) = lct(dominant_woody)
        cover_type(2) = lct(dominant_grass)
        IF (vegfract(second_woody) > vegfract(second_grass) &
             .OR. vegfract(second_woody) > substantial_woody) THEN
           cover_type(3) = lct(second_woody)
        ELSE
           cover_type(3) = lct(second_grass)
        END IF
     END IF
     IF (lc3c4crop) THEN
        cover_type(4) = dominant_crop
     ELSE
        cover_type(4) = lct_crop
     END IF
     is_naturalveg(4) = .FALSE.

  ELSE IF (ntiles == 5) THEN
!------------------------------------------------------------------------------
! 5 tiles: 1: woody
!          2: grasses
!          3: second tree or grass (depending on fraction)
!          4: C3 crops
!          5: C4 crops
!------------------------------------------------------------------------------ 
     IF (glac == 1) THEN           ! glacier
        cover_type(1) = lct_glacier
        cover_type(2) = lct_c3grass
        cover_type(3) = lct_evergreen_conifer

     ELSE IF (MAXVAL(vegfract) <= small_fract * 10._dp) THEN

        !-- no substantial vegetation coverage

        IF (ABS(lat) < 30._dp) THEN
           cover_type(1) = lct_tropical_deciduous
           cover_type(2) = lct_c4grass
           cover_type(3) = lct_tropical_evergreen
        ELSE
           cover_type(1) = lct_evergreen_conifer
           cover_type(2) = lct_c3grass
           cover_type(3) = lct_temperate_broadl_deciduous
        END IF
     ELSE

        !-- substantial vegetation

        cover_type(1) = lct(dominant_woody)
        cover_type(2) = lct(dominant_grass)
        IF (vegfract(second_woody) > vegfract(second_grass) &
             .OR. vegfract(second_woody) > substantial_woody) THEN
           cover_type(3) = lct(second_woody)
        ELSE
           cover_type(3) = lct(second_grass)
        END IF
     END IF
     cover_type(4) = lct_c3crop
     cover_type(5) = lct_c4crop

     is_naturalveg(4) = .FALSE.
     is_naturalveg(5) = .FALSE.

  ELSE IF (ntiles == 6) THEN
!------------------------------------------------------------------------------
! 6 tiles: 1: deciduous trees
!          2: eveergreen trees
!          3: shrub and tundra
!          4: C3 grasses and pasture
!          5: C4 grasses
!          6: crops
!------------------------------------------------------------------------------ 
     IF (glac == 1) THEN           ! glacier
        cover_type(1) = lct_glacier
        cover_type(2) = lct_evergreen_conifer
        cover_type(3) = lct_deciduous_shrub

     ELSE IF (MAXVAL(vegfract) <= small_fract * 10._dp) THEN

        !-- no substantial vegetation coverage

        IF (ABS(lat) < 30._dp) THEN
           cover_type(1) = lct_tropical_deciduous
           cover_type(2) = lct_tropical_evergreen
           cover_type(3) = lct_raingreen_shrub
        ELSE IF (ABS(lat) < 60._dp) THEN
           cover_type(1) = lct_temperate_broadl_deciduous
           cover_type(2) = lct_temperate_broadl_evergreen
           cover_type(3) = lct_deciduous_shrub
        ELSE
           cover_type(1) = lct_deciduous_conifer
           cover_type(2) = lct_evergreen_conifer
           cover_type(3) = lct_tundra
        END IF
     ELSE

        !-- substantial vegetation

        IF (vegfract(veg_tropical_deciduous) > vegfract(veg_temperate_broadl_deciduous) .AND. &
             vegfract(veg_tropical_deciduous) > vegfract(veg_deciduous_conifer)) THEN
           cover_type(1) = lct_tropical_deciduous
        ELSE IF(vegfract(veg_temperate_broadl_deciduous) > vegfract(veg_tropical_deciduous) .AND. &
             vegfract(veg_temperate_broadl_deciduous) >= vegfract(veg_deciduous_conifer)) THEN
           cover_type(1) = lct_temperate_broadl_deciduous
        ELSE IF(vegfract(veg_deciduous_conifer) > vegfract(veg_tropical_deciduous) .AND. &
             vegfract(veg_deciduous_conifer) > vegfract(veg_temperate_broadl_deciduous)) THEN 
           cover_type(1) = lct_deciduous_conifer
        ELSE
           IF (ABS(lat) < 30._dp) THEN
              cover_type(1) = lct_tropical_deciduous
           ELSE
              cover_type(1) = lct_temperate_broadl_deciduous
           END IF
        END IF

        IF (vegfract(veg_tropical_evergreen) > vegfract(veg_temperate_broadl_evergreen) .AND. &
             vegfract(veg_tropical_evergreen) > vegfract(veg_evergreen_conifer)) THEN
           cover_type(2) = lct_tropical_evergreen
        ELSE IF(vegfract(veg_temperate_broadl_evergreen) >= vegfract(veg_tropical_evergreen) .AND. &
             vegfract(veg_temperate_broadl_evergreen) > vegfract(veg_evergreen_conifer)) THEN
           cover_type(2) = lct_temperate_broadl_evergreen
        ELSE IF(vegfract(veg_evergreen_conifer) >= vegfract(veg_tropical_evergreen) .AND. &
             vegfract(veg_evergreen_conifer) >= vegfract(veg_temperate_broadl_evergreen)) THEN 
           cover_type(2) = lct_evergreen_conifer
        END IF

        IF (vegfract(veg_raingreen_shrub) > vegfract(veg_deciduous_shrub) .AND. &
             vegfract(veg_raingreen_shrub) > vegfract(veg_tundra)) THEN
           cover_type(3) = lct_raingreen_shrub
        ELSE IF(vegfract(veg_deciduous_shrub) >= vegfract(veg_raingreen_shrub) .AND. &
             vegfract(veg_deciduous_shrub) >= vegfract(veg_tundra)) THEN
           cover_type(3) = lct_deciduous_shrub
        ELSE IF(vegfract(veg_tundra) >= vegfract(veg_raingreen_shrub) .AND. &
             vegfract(veg_tundra) > vegfract(veg_deciduous_shrub)) THEN
           cover_type(3) = lct_tundra
        END IF
     END IF
     cover_type(4) = lct_c3grass
     cover_type(5) = lct_c4grass
     IF (lc3c4crop) THEN
        cover_type(6) = dominant_crop
     ELSE
        cover_type(6) = lct_crop
     ENDIF
     is_naturalveg(6) = .FALSE.

  ELSE IF (ntiles == 8) THEN
!------------------------------------------------------------------------------
! 8 tiles: 1: tropical broadleaf evergreen        7: C3 grass
!          2: tropical broadleaf deciduous        8: C4 grass
!          3: extra-tropical evergreen
!          4: extra-tropical deciduous
!          5: raingreen shrubs
!          6: deciduous shrubs
!------------------------------------------------------------------------------
     IF (glac == 1) THEN
        cover_type(1) = lct_glacier
     ELSE
        cover_type(1) = lct_tropical_evergreen
     ENDIF
     cover_type(2) = lct_tropical_deciduous
     cover_type(3) = lct_extratrop_evergreen
     cover_type(4) = lct_extratrop_deciduous
     cover_type(5) = lct_raingreen_shrub
     cover_type(6) = lct_deciduous_shrub
     cover_type(7) = lct_c3grass
     cover_type(8) = lct_c4grass

  ELSE IF (ntiles == 9) THEN
!------------------------------------------------------------------------------
! 9 tiles: 1: glacier                             8: C3 grass
!          2: tropical broadleaf evergreen        9: C4 grass
!          3: tropical broadleaf deciduous
!          4: extra-tropical evergreen
!          5: extra-tropical deciduous
!          6: raingreen shrubs
!          7: deciduous shrubs
!------------------------------------------------------------------------------
     cover_type(1) = lct_glacier
     cover_type(2) = lct_tropical_evergreen
     cover_type(3) = lct_tropical_deciduous
     cover_type(4) = lct_extratrop_evergreen
     cover_type(5) = lct_extratrop_deciduous
     cover_type(6) = lct_raingreen_shrub
     cover_type(7) = lct_deciduous_shrub
     cover_type(8) = lct_c3grass
     cover_type(9) = lct_c4grass

     glacier_tile = 1
     is_naturalveg(1) = .FALSE.

  ELSE IF (ntiles == 10) THEN
!------------------------------------------------------------------------------
! 10 tiles: 1: tropical broadleaf evergreen        7: C3 grass
!           2: tropical broadleaf deciduous        8: C4 grass
!           3: extra-tropical evergreen            9: C3 crops
!           4: extra-tropical deciduous           10: C4 crops
!           5: raingreen shrubs
!           6: deciduous shrubs
!------------------------------------------------------------------------------
     IF (glac == 1) THEN
        cover_type(1) = lct_glacier
     ELSE
        cover_type(1) = lct_tropical_evergreen
     ENDIF
     cover_type(2) = lct_tropical_deciduous
     cover_type(3) = lct_extratrop_evergreen
     cover_type(4) = lct_extratrop_deciduous
     cover_type(5) = lct_raingreen_shrub
     cover_type(6) = lct_deciduous_shrub
     cover_type(7) = lct_c3grass
     cover_type(8) = lct_c4grass
     cover_type(9) = lct_c3crop
     cover_type(10)= lct_c4crop

     is_naturalveg(9)  = .FALSE.
     is_naturalveg(10) = .FALSE.

  ELSE IF (ntiles == 11) THEN
!------------------------------------------------------------------------------
! 11 tiles: 1: tropical broadleaf evergreen        7: C3 grass
!           2: tropical broadleaf deciduous        8: C4 grass
!           3: extra-tropical evergreen            9: C3 pasture
!           4: extra-tropical deciduous           10: C4 pasture
!           5: raingreen shrubs                   11: C3/C4 crop
!           6: deciduous shrubs
!------------------------------------------------------------------------------
     IF (glac == 1) THEN
        cover_type(1) = lct_glacier
     ELSE
        cover_type(1) = lct_tropical_evergreen
     END IF
     cover_type(2) = lct_tropical_deciduous
     cover_type(3) = lct_extratrop_evergreen
     cover_type(4) = lct_extratrop_deciduous
     cover_type(5) = lct_raingreen_shrub
     cover_type(6) = lct_deciduous_shrub
     cover_type(7) = lct_c3grass
     cover_type(8) = lct_c4grass
     cover_type(9) = lct_c3pasture
     cover_type(10)= lct_c4pasture
     cover_type(11)= dominant_crop

     is_naturalveg(9)  = .FALSE.
     is_naturalveg(10) = .FALSE.
     is_naturalveg(11) = .FALSE.

  ELSE IF (ntiles == 12) THEN
!------------------------------------------------------------------------------
! 12 tiles: 1: glacier                             7: deciduous shrubs
!           2: tropical broadleaf evergreen        8: C3 grass
!           3: tropical broadleaf deciduous        9: C4 grass
!           4: extra-tropical evergreen           10: C3 pasture
!           5: extra-tropical deciduous           11: C4 pasture
!           6: raingreen shrubs                   12: C3/C4 crop
!           
!------------------------------------------------------------------------------
     cover_type(1) = lct_glacier
     cover_type(2) = lct_tropical_evergreen
     cover_type(3) = lct_tropical_deciduous
     cover_type(4) = lct_extratrop_evergreen
     cover_type(5) = lct_extratrop_deciduous
     cover_type(6) = lct_raingreen_shrub
     cover_type(7) = lct_deciduous_shrub
     cover_type(8) = lct_c3grass
     cover_type(9) = lct_c4grass
     cover_type(10)= lct_c3pasture
     cover_type(11)= lct_c4pasture
     cover_type(12)= dominant_crop

     glacier_tile = 1
     is_naturalveg(1)  = .FALSE.
     is_naturalveg(10) = .FALSE.
     is_naturalveg(11) = .FALSE.
     is_naturalveg(12) = .FALSE.

  ELSE IF (ntiles == 13) THEN
!------------------------------------------------------------------------------
! 13 tiles: 1: tropical broadleaf evergreen        8: deciduous shrubs
!           2: tropical broadleaf deciduous        9: C3 grass
!           3: temperate broadleaf evergreen      10: C4 grass
!           4: temperate broadleaf deciduous      11: tundra
!           5: evergreen coniferous               12: swamps
!           6: deciduous coniferous               13: crops
!           7: raingreen shrubs                
!------------------------------------------------------------------------------ 
     IF (glac == 1) THEN
        cover_type(1) = lct_glacier
     ELSE
        cover_type(1) = lct_tropical_evergreen
     END IF
     cover_type(2) = lct_tropical_deciduous
     cover_type(3) = lct_temperate_broadl_evergreen
     cover_type(4) = lct_temperate_broadl_deciduous
     cover_type(5) = lct_evergreen_conifer
     cover_type(6) = lct_deciduous_conifer
     cover_type(7) = lct_raingreen_shrub
     cover_type(8) = lct_deciduous_shrub
     cover_type(9) = lct_c3grass
     cover_type(10)= lct_c4grass
     cover_type(11)= lct_tundra
     cover_type(12)= lct_swamp
     cover_type(13)= lct_crop

     is_naturalveg(13) = .FALSE.

  ELSE IF (ntiles == 14) THEN
!------------------------------------------------------------------------------
! 14 tiles: 1: glacier                             8: raingreen shrubs
!           2: tropical broadleaf evergreen        9: deciduous shrubs
!           3: tropical broadleaf deciduous       10: C3 grass
!           4: temperate broadleaf evergreen      11: C4 grass
!           5: temperate broadleaf deciduous      12: tundra
!           6: evergreen coniferous               13: swamps
!           7: deciduous coniferous               14: crops
!           
!------------------------------------------------------------------------------ 
     cover_type(1) = lct_glacier
     cover_type(2) = lct_tropical_evergreen
     cover_type(3) = lct_tropical_deciduous
     cover_type(4) = lct_temperate_broadl_evergreen
     cover_type(5) = lct_temperate_broadl_deciduous
     cover_type(6) = lct_evergreen_conifer
     cover_type(7) = lct_deciduous_conifer
     cover_type(8) = lct_raingreen_shrub
     cover_type(9) = lct_deciduous_shrub
     cover_type(10)= lct_c3grass
     cover_type(11)= lct_c4grass
     cover_type(12)= lct_tundra
     cover_type(13)= lct_swamp
     cover_type(14)= lct_crop

     glacier_tile = 1
     is_naturalveg(1)  = .FALSE.
     is_naturalveg(14) = .FALSE.

  ELSE
     CALL hdlerr(1,'calc_cover_types: number of tiles not supported')
  END IF

  IF (ANY(cover_type(:) == 0)) THEN
     CALL hdlerr(1,'a cover type is 0. Possible reason: ' &
           //'lct definitions do not match this routine')
  END IF

END SUBROUTINE calc_cover_types

!------------------------------------------------------------------------------
SUBROUTINE harmonize_fractions(ntiles, is_naturalveg, glac, natural_veg, cover_fract)
!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! Rescaling of cover fractions to assure that potential natural vegetation
! is greater or equal than the actual natural vegetation on all tiles.
! 
!------------------------------------------------------------------------------
  USE mo_kinds
  USE mo_vegparams
  IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  INTEGER,  INTENT(in)    :: ntiles                ! number of tiles
  INTEGER,  INTENT(in)    :: glac                  ! glacier flag (1:glacier)
  LOGICAL,  INTENT(in)    :: is_naturalveg(ntiles) ! flag for natural vegetation
  REAL(dp), INTENT(inout) :: natural_veg(ntiles)   ! potentially vegetated fraction 
                                                   ! of natural vegetation types
!
! !IN- and OUTPUT PARAMETERS:
! 
  REAL(dp), INTENT(inout) :: cover_fract(ntiles)  ! vegetated fraction

! !LOCAL PARAMETERS
  INTEGER  :: i
  REAL(dp) :: delta(ntiles)    ! missmach of actual and potential natural types
!------------------------------------------------------------------------------

  ! glacier points need not be regarded

  IF (glac == 1) RETURN

  delta(:) = 0._dp
  WHERE (is_naturalveg)
     delta(:) = cover_fract(:) - natural_veg(:)
     cover_fract(:) = MIN(cover_fract(:),natural_veg(:))
  END WHERE
    
  ! Due to the pasture rule, in cells with pastures actual grass fractions are
  ! disproportionately smaller than potential grass fractions. The relative
  ! scaling in scale_cover_fract can lead to slightly greater fractions of the
  ! remaining natural vegetation types if called for cover_fract (in comparison
  ! to scale_cover_fract calls for natural_veg).
  ! Thus calls to scale_cover_fract can distroy the harmonization just achieved.
  ! We thus have to assure here, that no further calls of scale_cover_fract are
  ! needed.
  ! We add the missmatch (actual natural vegetation fraction is slightly
  ! greater than the potential fraction) to the dominant natural grass types.

  DO i = 1, ntiles
     IF (delta(i) > 0._dp) THEN
        IF (cover_fract(veg_c3grass) > cover_fract(veg_c4grass)) THEN
           cover_fract(veg_c3grass) = cover_fract(veg_c3grass) + delta(i)
        ELSE
           cover_fract(veg_c4grass) = cover_fract(veg_c4grass) + delta(i)
        END IF
     END IF
  END DO

END SUBROUTINE harmonize_fractions

!------------------------------------------------------------------------------
SUBROUTINE scale_cover_fract (ntiles, is_naturalveg, glac, cover_fract)
!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! Rescaling of cover fractions to assure that
!  - the sum of cover fractions is one
!  - all non-glacier grid cells have at least a minimum vegetated fraction
!
! This is not a copy of the current jsbach routine in mo_land_surface.
!------------------------------------------------------------------------------
  USE mo_kinds
  USE mo_vegparams
  IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  INTEGER,  INTENT(in)  :: ntiles                 ! number of tiles
  INTEGER,  INTENT(in)  :: glac                   ! glacier flag (1:glacier)
  LOGICAL,  INTENT(in)  :: is_naturalveg(ntiles)  ! flag for natural vegetation
!
! !IN- and OUTPUT PARAMETERS:
! 
  REAL(dp), INTENT(inout) :: cover_fract(ntiles)  ! vegetated fraction

! !LOCAL VARIABLES:
!
  INTEGER   :: i, iter
  INTEGER   :: niter             ! number of iterations needed
  INTEGER   :: nsparce           ! number of PFTs with a vegetated fraction of less then small_fract
  INTEGER   :: nsparce_anthro    ! number of non natural PFTs with a vegetated fraction < small_fract
  INTEGER   :: nnatural          ! number natural PFTs
  REAL(dp)  :: sum_fract         ! sum of all cover fractions
  REAL(dp)  :: excluded_fract    ! sum of all cover fractions

!------------------------------------------------------------------------------

  ! cover fractions of glacier points have already been set in calc_cover_fract

  IF (glac == 1) RETURN

  ! Make sure, crop and pasture have a cover fraction of at least small_fract

  WHERE (.NOT. is_naturalveg)
     cover_fract(:) = MAX(small_fract,cover_fract(:))
  END WHERE

  ! If there is a tile reserved for glaciers, it's fractions need to be 0 on non glacier points

  IF (glacier_tile /= -1) THEN
     cover_fract(glacier_tile) = 0._dp
  END IF

  ! Crops and pastures only need to be scaled if their total fraction is greater than 
  ! 1-nnatural*small_fract.   
 
  excluded_fract = SUM(cover_fract(:), MASK = .NOT. is_naturalveg)
  nnatural = 0
  nsparce_anthro = 0
  DO i = 1, ntiles
     IF (is_naturalveg(i) .AND. glacier_tile /= i) THEN
        nnatural = nnatural + 1
     END IF
     IF (.NOT. is_naturalveg(i) .AND. cover_fract(i) <= small_fract .AND. glacier_tile /= i) THEN
        nsparce_anthro = nsparce_anthro + 1
     END IF
  END DO
  IF (excluded_fract > 1._dp - REAL(nnatural+nsparce_anthro,dp)*small_fract) THEN
     WHERE (.NOT. is_naturalveg .AND. cover_fract(:) > small_fract)
        cover_fract(:) = cover_fract(:) * (1._dp - REAL(nnatural+nsparce_anthro,dp)*small_fract) &
                                          / (excluded_fract-REAL(nsparce_anthro,dp)*small_fract)

     END WHERE
  END IF

  ! Scale the natural vegetation that there is a minimum cover fraction of small_fract on all
  ! tiles and the total cover fraction is 1.

  niter = ntiles                             ! to assure binary identical fractions whether or not
  IF (glacier_tile /= -1) niter = niter - 1  ! a glacier tile is used, the niter has to be fix.
  DO iter = 1, niter

     sum_fract = 0._dp
     excluded_fract = 0._dp
     nsparce = 0

     DO i = 1,ntiles
        IF (cover_fract(i) > small_fract .AND. is_naturalveg(i)) THEN
           sum_fract = sum_fract + cover_fract(i)
        ELSE IF (is_naturalveg(i)) THEN
           nsparce = nsparce + 1
        ELSE
           excluded_fract = excluded_fract + cover_fract(i)
        END IF
     END DO
     DO i = 1,ntiles
        IF (cover_fract(i) > small_fract .AND. is_naturalveg(i)) THEN
           cover_fract(i) = cover_fract(i) * (1._dp - excluded_fract - REAL(nsparce,dp)*small_fract) / sum_fract
        ELSE IF (glacier_tile == i) THEN
           cover_fract(i) = 0._dp
        ELSE IF (is_naturalveg(i)) THEN
           cover_fract(i) = small_fract
        ELSE
           cover_fract(i) = MAX(small_fract,cover_fract(i))
        END IF
     END DO

  END DO

  IF (ANY(cover_fract(:) < small_fract) .AND. glacier_tile == -1) THEN
     WRITE(*,*) 'cover_fract still smaller ', small_fract, ' after ', niter, ' iterations:', &
          MINVAL(cover_fract(:)), ' (tile: ', MINLOC(cover_fract(:)), ')'
     STOP 1
  END IF

  IF (SUM(cover_fract(:)) > 1._dp + REAL(ntiles,dp)*EPSILON(1._dp) .OR. &
      SUM(cover_fract(:)) < 1._dp - REAL(ntiles,dp)*EPSILON(1._dp)) THEN
     WRITE(*,*) 'SUM(cover_fract) differs from 1: ', SUM(cover_fract(:))
     STOP 1
  END IF

END SUBROUTINE scale_cover_fract

!------------------------------------------------------------------------------
SUBROUTINE put_lct_attributes(ncout, varid)
!------------------------------------------------------------------------------
!
! Write attributes for land cover types
!
!------------------------------------------------------------------------------
  USE mo_vegparams
  IMPLICIT NONE
  include 'netcdf.inc'

  !-- INTENT(in)
  INTEGER, INTENT(IN) :: ncout, varid

  !-- INTENT(out)

  !-- LOCAL
  INTEGER      :: stat
  CHARACTER(5) :: attnam
!------------------------------------------------------------------------------

  stat = nf_put_att_text(ncout,varid,'long_name',15,'land cover type')
  CALL hdlerr(stat,'put_lct_attributes: long_name')
  stat = nf_put_att_text(ncout,varid,'units',1,'1')
  CALL hdlerr(stat,'put_lct_attributes: units')

  IF (lct_glacier /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_glacier
     stat = nf_put_att_text(ncout,varid,attnam, 7,'glacier')
     CALL hdlerr(stat,'put_lct_attributes: lct_glacier')
  END IF
  IF (lct_tropical_evergreen /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_tropical_evergreen
     stat = nf_put_att_text(ncout,varid,attnam,28,'tropical broadleaf evergreen')
     CALL hdlerr(stat,'put_lct_attributes: lct_tropical_evergreen')
  END IF
  IF (lct_tropical_deciduous /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_tropical_deciduous
     stat = nf_put_att_text(ncout,varid,attnam,28,'tropical broadleaf deciduous')
     CALL hdlerr(stat,'put_lct_attributes: lct_tropical_deciduous')
  END IF
  IF (lct_extratrop_evergreen /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_extratrop_evergreen
     stat = nf_put_att_text(ncout,varid,attnam,24,'extra-tropical evergreen')
     CALL hdlerr(stat,'put_lct_attributes: lct_extratrop_evergreen')
  END IF
  IF (lct_extratrop_deciduous /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_extratrop_deciduous
     stat = nf_put_att_text(ncout,varid,attnam,24,'extra-tropical deciduous')
     CALL hdlerr(stat,'put_lct_attributes: lct_extratrop_deciduous')
  END IF
  IF (lct_temperate_broadl_evergreen /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_temperate_broadl_evergreen
     stat = nf_put_att_text(ncout,varid,attnam,29,'temperate broadleaf evergreen')
     CALL hdlerr(stat,'put_lct_attributes: lct_temperate_broadl_evergreen')
  END IF
  IF (lct_temperate_broadl_deciduous /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_temperate_broadl_deciduous
     stat = nf_put_att_text(ncout,varid,attnam,29,'temperate broadleaf deciduous')
     CALL hdlerr(stat,'put_lct_attributes: lct_temperate_broadl_deciduous')
  END IF
  IF (lct_evergreen_conifer /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_evergreen_conifer
     stat = nf_put_att_text(ncout,varid,attnam,20,'evergreen coniferous')
     CALL hdlerr(stat,'put_lct_attributes: lct_evergreen_conifer')
  END IF
  IF (lct_deciduous_conifer /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_deciduous_conifer
     stat = nf_put_att_text(ncout,varid,attnam,20,'deciduous coniferous')
     CALL hdlerr(stat,'put_lct_attributes: lct_deciduous_conifer')
  END IF
  IF (lct_raingreen_shrub /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_raingreen_shrub
     stat = nf_put_att_text(ncout,varid,attnam,16,'raingreen shrubs')
     CALL hdlerr(stat,'put_lct_attributes: lct_raingreen_shrub')
  END IF
  IF (lct_deciduous_shrub /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_deciduous_shrub
     stat = nf_put_att_text(ncout,varid,attnam,16,'deciduous shrubs')
     CALL hdlerr(stat,'put_lct_attributes: lct_deciduous_shrub')
  END IF
  IF (lct_c3grass /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_c3grass
     stat = nf_put_att_text(ncout,varid,attnam, 8,'C3 grass')
     CALL hdlerr(stat,'put_lct_attributes: lct_c3grass')
  END IF
  IF (lct_c4grass /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_c4grass
     stat = nf_put_att_text(ncout,varid,attnam, 8,'C4 grass')
     CALL hdlerr(stat,'put_lct_attributes: lct_c4gras')
  END IF
  IF (lct_c3pasture /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_c3pasture
     stat = nf_put_att_text(ncout,varid,attnam,10,'C3 pasture')
     CALL hdlerr(stat,'put_lct_attributes: lct_c3pasture')
  END IF
  IF (lct_c4pasture /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_c4pasture
     stat = nf_put_att_text(ncout,varid,attnam,10,'C4 pasture')
     CALL hdlerr(stat,'put_lct_attributes: lct_c4pasture')
  END IF
  IF (lct_tundra /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_tundra
     stat = nf_put_att_text(ncout,varid,attnam, 6,'tundra')
     CALL hdlerr(stat,'put_lct_attributes: lct_tundra')
  END IF
  IF (lct_swamp /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_swamp
     stat = nf_put_att_text(ncout,varid,attnam, 5,'swamp')
     CALL hdlerr(stat,'put_lct_attributes: lct_swamp')
  END IF
  IF (lct_crop /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_crop
     stat = nf_put_att_text(ncout,varid,attnam, 5,'crops')
     CALL hdlerr(stat,'put_lct_attributes: lct_crop')
  END IF
  IF (lct_c3crop /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_c3crop
     stat = nf_put_att_text(ncout,varid,attnam, 8,'C3 crops')
     CALL hdlerr(stat,'put_lct_attributes: lct_c3crop')
  END IF
  IF (lct_c4crop /= 0) THEN
     WRITE(attnam,'(a3,i2.2)') 'lct', lct_c4crop
     stat = nf_put_att_text(ncout,varid,attnam, 8,'C4 crops')
     CALL hdlerr(stat,'put_lct_attributes: lct_c4crop')
  END IF

END SUBROUTINE put_lct_attributes

!------------------------------------------------------------------------------
SUBROUTINE gauaw (pa, pw, nlat, api)
!------------------------------------------------------------------------------
  !-- copied from echam

    ! Description:
    !
    ! Compute abscissas and weights for Gaussian integration.
    !
    USE mo_kinds
    IMPLICIT NONE

    INTEGER :: nlat
    REAL(dp) :: pa(nlat), pw(nlat)
    REAL(dp) :: api

    REAL(dp), PARAMETER :: epsil = EPSILON(0.0_dp)
    INTEGER, PARAMETER :: itemax = 20

    INTEGER :: iter, ins2, isym, jn, jgl
    REAL(dp) :: za, zw, z, zan
    REAL(dp) :: zk, zkm1, zkm2, zx, zxn, zldn, zmod

    !  Intrinsic functions 
    INTRINSIC ABS, COS, MOD, TAN

    ins2 = nlat/2+MOD(nlat,2)

    DO jgl = 1, ins2
       z = REAL(4 * jgl - 1) * api / REAL(4 * nlat + 2)
       pa(jgl) = COS(z + 1._dp / (TAN(z) * REAL(8 * nlat**2)))
    END DO

    DO jgl = 1, ins2

       za = pa(jgl)
    
       DO iter = 1, itemax+1
          zk = 0._dp

          zkm2 = 1._dp
          zkm1 = za
          zx = za
          DO jn = 2, nlat
             zk = (REAL(2 * jn - 1) * zx * zkm1 - REAL(jn - 1) * zkm2) / REAL(jn)
             zkm2 = zkm1
             zkm1 = zk
          END DO
          zkm1 = zkm2
          zldn = (REAL(nlat) * (zkm1 - zx * zk)) / (1._dp - zx * zx)
          zmod = -zk / zldn
          zxn = zx + zmod
          zan = zxn
    
          zkm2 = 1._dp
          zkm1 = zxn
          zx = zxn
          DO jn = 2,nlat
             zk = (REAL(2 * jn - 1) * zx * zkm1 - REAL(jn - 1) * zkm2) / REAL(jn)
             zkm2 = zkm1
             zkm1 = zk
          END DO
          zkm1 = zkm2
          zw = (1._dp - zx * zx) / (REAL(nlat * nlat) * zkm1 * zkm1)
          za = zan
          IF (ABS(zmod) <= epsil) EXIT
       END DO

       pa(jgl) = zan
       pw(jgl) = 2._dp * zw
    
    ENDDO

    DO jgl = 1, nlat/2
       isym = nlat - jgl + 1
       pa(isym) = -pa(jgl)
       pw(isym) = pw(jgl)
    ENDDO

END SUBROUTINE gauaw

!------------------------------------------------------------------------------
SUBROUTINE extrap(nlon, nlat, lon, lat, mask, missval, method, val)
!------------------------------------------------------------------------------
! Extrapolation to grid cells with missing values
!
!   extrapolation methods (argument method):
!
!      nneigh: use value of the nearest neighbor only
!        mean: distance weighted mean of neighboring cells
!       modal: modal value of neighboring cells, ignoring the distance. This 
!              only works for integers (technically defined as real).
!
!  search for neighbors takes place in a square around the grid cell, with the
!  radius being increased until valid neighbors are found.
!  This is not optimal for large radii and/or high latitudes, as diagonal 
!  neighbors in a greater distance might be taken into account while closer 
!  cells to the east and west might not.
!
!------------------------------------------------------------------------------
  USE mo_kinds
  IMPLICIT NONE

  INTEGER,      INTENT(in) :: nlon, nlat           ! dimensions
  REAL(dp),     INTENT(in) :: lon(nlon), lat(nlat) ! longitudes, latitudes 
  REAL(dp),     INTENT(in) :: mask(nlon,nlat)      ! land sea /glacier mask
  REAL(dp),     INTENT(in) :: missval              ! value for missing data
  CHARACTER(*), INTENT(in) :: method               ! extrapolation method: nneigh, mean, modal
  REAL(dp),  INTENT(inout) :: val(nlon,nlat)       ! array to be extrapolated
 
  REAL(dp), PARAMETER :: deg2rad = 1.74532925199432957692e-2  ! Degree to rad: 2pi/360

  INTEGER  :: i, j                    ! looping index of global grid
  INTEGER  :: ip, im, jp, jm, ii, jj
  INTEGER  :: ni, nj                  ! looping index on neighbors array
  INTEGER  :: m                       ! iteration counter
  INTEGER  :: mmax                    ! maximum number of iterations
  INTEGER  :: mini, maxi              ! minimum, maximum value
  INTEGER  :: nval                    ! number of different values
  INTEGER  :: loc(2)                  ! location of nearest neighbor
  LOGICAL  :: filled                  ! flag to indicate if gap is filled
  REAL(dp) :: x1, y1, z1, x2, y2, z2  ! x,y,z-coordinates
  REAL(dp) :: dx                      ! distance
  REAL(dp) :: lon_ctl, lat_ctl        ! coordinates of missing cell
  REAL(dp) :: lon_nbr, lat_nbr        ! coordinates of neighboring cell
  REAL(dp) :: sum_dist                ! sum of distances of the neighbors
  REAL(dp) :: val_new(nlon,nlat)      ! filled array (no missing values on land)
  REAL(dp), ALLOCATABLE :: neighbours_val(:,:)  ! value on neighbors array
  REAL(dp), ALLOCATABLE :: neighbours_lon(:,:)  ! corresponding longitude
  REAL(dp), ALLOCATABLE :: neighbours_lat(:,:)  ! corresponding latitude
  REAL(dp), ALLOCATABLE :: neighbours_dist(:,:) ! distance of the neighbor
  REAL(dp), ALLOCATABLE :: count_values(:)      ! array to count different values

  DO i = 1, nlon
     DO j = 1, nlat

        ! gap on non-glacier land grid cell
        IF (mask(i,j) > 0._dp .AND. val(i,j) == missval) THEN
           
           ! check neighboring grid cells

           ! initializations
           ip = i
           im = i
           jp = j
           jm = j

           m=1
           mmax = nlat  ! maximum number of iterations
           filled = .FALSE.

           sum_dist = 0._dp

           DO WHILE (.NOT. filled)

              ip = ip+1
              im = im-1
              jp = MIN(jp+1, nlat)
              jm = MAX(jm-1, 1)

              ALLOCATE(neighbours_val(ip-im+1,jp-jm+1))
              ALLOCATE(neighbours_lon(ip-im+1,jp-jm+1))
              ALLOCATE(neighbours_lat(ip-im+1,jp-jm+1))
              ALLOCATE(neighbours_dist(ip-im+1,jp-jm+1))

              DO ni = 1, ip-im+1   ! index on neighbors array
              DO nj = 1, jp-jm+1
                 ii = im + ni - 1   ! index on global grid
                 jj = jm + nj - 1
              
                 IF (ii <= 0)   ii = ii + nlon
                 IF (ii > nlon) ii = ii - nlon
                 
                 neighbours_lon(ni,nj) = lon(ii)
                 neighbours_lat(ni,nj) = lat(jj)
                 neighbours_val(ni,nj) = val(ii,jj)
              END DO
              END DO

              IF ( ANY(neighbours_val(:,:) /= missval) ) THEN
                 SELECT CASE (method)
                 CASE ('modal')
                    mini = NINT(MINVAL(neighbours_val(:,:), MASK=neighbours_val(:,:)/=missval))
                    maxi = NINT(MAXVAL(neighbours_val(:,:), MASK=neighbours_val(:,:)/=missval))
                    ALLOCATE (count_values(maxi-mini+1))
                    count_values(:) = 0
                    DO nval = mini, maxi
                       DO ni = 1, ip-im+1 
                       DO nj = 1, jp-jm+1   ! index on neighbors array
                          IF (NINT(neighbours_val(ni,nj)) == nval) &
                               count_values(nval-mini+1) = count_values(nval-mini+1) + 1
                       END DO
                       END DO
                    END DO
                    val_new(i,j) =  MAXLOC(count_values(:), DIM=1) + mini - 1
                    DEALLOCATE (count_values)

                 CASE ('nneigh', 'mean')
                    lon_ctl = lon(i) * deg2rad     ! central longitude [rad]
                    lat_ctl = lat(j) * deg2rad     ! central latitude [rad]

                    DO nj = 1, jp-jm+1   ! index on neighbors array
                    DO ni = 1, ip-im+1

                       IF (neighbours_val(ni,nj) /= missval) THEN

                          lon_nbr = neighbours_lon(ni,nj) * deg2rad  ! lon of neighbor [rad]
                          lat_nbr = neighbours_lat(ni,nj) * deg2rad  ! lat of neighbor [rad]

                          ! Transformation to x,y,z-coordinates

                          x1 = cos(lat_ctl)*cos(lon_ctl)
                          y1 = cos(lat_ctl)*sin(lon_ctl)
                          z1 = sin(lat_ctl)

                          x2 = cos(lat_nbr)*cos(lon_nbr)
                          y2 = cos(lat_nbr)*sin(lon_nbr)
                          z2 = sin(lat_nbr)

                          ! direct distance
                          dx = SQRT((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

                          ! distance along the surface
                          dx = 2*ASIN(dx/2)

                          neighbours_dist(ni,nj) = dx
                          sum_dist = sum_dist + neighbours_dist(ni,nj)
                       ELSE
                          neighbours_dist(ni,nj) = 0._dp
                       END IF
                    END DO
                    END DO
                    IF (method ==  'nneigh') THEN
                       loc(:) = MINLOC(neighbours_dist(:,:), MASK=neighbours_dist(:,:) /= 0._dp)
                       val_new(i,j) = neighbours_val(loc(1),loc(2))
                    ELSE IF (method ==  'mean') THEN
                       val_new(i,j) = SUM(neighbours_val(:,:) * neighbours_dist(:,:)/sum_dist)
                    END IF

                 CASE default
                    CALL hdlerr(1, "extrapolation method not supported")
                 END SELECT
                 filled = .TRUE.

              ELSE
                 IF (m >= mmax) CALL hdlerr(1, "Not enough iterations for extrapolation")
                 m = m + 1 
              END IF

              DEALLOCATE(neighbours_val)
              DEALLOCATE(neighbours_lon)
              DEALLOCATE(neighbours_lat)
              DEALLOCATE(neighbours_dist)

           END DO
        ELSE
           IF (mask(i,j) == 0._dp) THEN
              val_new(i,j) = 0._dp 
           ELSE 
              val_new(i,j) = val(i,j)
           END IF 
        END IF
     END DO
  END DO
  val(:,:)=val_new(:,:)

END SUBROUTINE extrap

!------------------------------------------------------------------------------
SUBROUTINE open_file(info, infile, ncin, ret_stat)
!------------------------------------------------------------------------------
!
! Routine to open a netcdf file as read only input file
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  include 'netcdf.inc'

  !-- INTENT(in)
  CHARACTER*100, INTENT(IN)            :: infile
  LOGICAL,       INTENT(IN)            :: info

  !-- INTENT(out)
  INTEGER, INTENT(OUT) :: ncin
  INTEGER, INTENT(OUT), OPTIONAL :: ret_stat
  
  !-- LOCAL
  INTEGER :: stat
!------------------------------------------------------------------------------

  stat=nf_open(infile, NF_NOWRITE, ncin)
  IF (PRESENT(ret_stat) .AND. stat /= NF_NOERR) THEN
    ret_stat = stat
    RETURN
  ENDIF
  CALL hdlerr(stat,'opening file '//infile)
  IF (info) WRITE(*,*) ' File ', TRIM(infile),' opened'

END SUBROUTINE open_file

!------------------------------------------------------------------------------
SUBROUTINE check_dimensions (varname, ncin, varid, nlon, nlat, ntiles, nlct, &
                             nvegtyp, nsoil, ntime)
!------------------------------------------------------------------------------
!
! Routine to find dimensions of an input variable and to compare them with the
! dimensions defined in the output file
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  include 'netcdf.inc'

  !-- INTENT(in)
  CHARACTER*30, INTENT(in) :: varname
  INTEGER,      INTENT(in) :: ncin, nlon, nlat, ntiles, nlct, nvegtyp, nsoil, ntime


  !-- INTENT(out)
  INTEGER, INTENT(out) :: varid

!-- LOCAL
  INTEGER      :: stat, len, dim, ndims
  INTEGER, ALLOCATABLE :: dimids(:)
  CHARACTER*30 :: dimname
!------------------------------------------------------------------------------

  stat=nf_inq_varid(ncin, varname, varid)
  CALL hdlerr(stat,'check_dimensions: inq. varid of '//varname)

  stat=nf_inq_varndims(ncin, varid, ndims)
  CALL hdlerr(stat,'check_dimensions: inq. ndims of '//varname)

  ALLOCATE(dimids(ndims))
  stat=nf_inq_vardimid(ncin, varid, dimids(:))
  CALL hdlerr(stat,'check_dimensions: inq. dimids of '//varname)

  DO dim=1,ndims
     stat=nf_inq_dim(ncin, dimids(dim), dimname, len)
     CALL hdlerr(stat,'check_dimensions: inq. dim of '//varname)
     IF (dimname == 'lon') THEN
        IF (nlon /= len) WRITE (*,*) 'Dimension error: nlon=',nlon,' len=', len  
     ELSE IF (dimname == 'lat') THEN
        IF (nlat /= len) WRITE (*,*) 'Dimension error: nlat=',nlat,' len=', len  
     ELSE IF (dimname == 'level') THEN
        IF (1 /= len) WRITE (*,*) 'Dimension error: nlevel=', 1,' len=', len  
     ELSE IF (dimname == 'lct') THEN
        IF (nlct /= len) WRITE (*,*) 'Dimension error: nlct=',nlct,' len=', len  
     ELSE IF (dimname == 'ntiles') THEN
        IF (ntiles /= len) WRITE (*,*) 'Dimension error: ntiles=',ntiles,' len=', len  
     ELSE IF (dimname == 'soillev') THEN
        IF (nsoil /= len) WRITE (*,*) 'Dimension error: soillev=',nsoil,' len=', len  
     ELSE IF (dimname == 'time') THEN
        IF (ntime /= len .AND. len /= 1) &
             WRITE (*,*) 'Dimension error: ntime=',ntime,' len=', len
     ELSE IF (dimname == 'vegtype') THEN
        IF (len /= nvegtyp) WRITE (*,*) &
             'Dimension error: nvegtyp=', nvegtyp, ' len=', len
     ELSE
        WRITE (*,*) 'Unexpected dimension: ', dimname,' of ',varname
     END IF  
  END DO
  DEALLOCATE(dimids)

END SUBROUTINE check_dimensions

!------------------------------------------------------------------------------
SUBROUTINE get_missval (ncid, varid, missval)
!------------------------------------------------------------------------------
!
! Routine to find out the missing value of an input file variable
!
!------------------------------------------------------------------------------
  USE mo_kinds
  IMPLICIT NONE
  include 'netcdf.inc'

  !-- INTENT(in)
  INTEGER,      INTENT(in)  :: ncid, varid

  !-- INTENT(out)
  REAL(dp),     INTENT(out) :: missval

  !-- LOCAL
  INTEGER      :: stat
!------------------------------------------------------------------------------

  stat = nf_get_att_double(ncid, varid, "_FillValue", missval)
  CALL hdlerr(stat,'get_missval: no  missing value available')

END SUBROUTINE get_missval

!------------------------------------------------------------------------------
SUBROUTINE define_var(ncin, ncout, varname_in, varname_out, outdimids, missval)
!------------------------------------------------------------------------------
!
! Routine to define an output variable that had been read from another
! netcdf file. Attributes and dimensions are copied.
!
!------------------------------------------------------------------------------
  USE mo_kinds

  IMPLICIT NONE

  include 'netcdf.inc'

  !-- INTENT(in)
  CHARACTER*30, INTENT(in) :: varname_in, varname_out
  INTEGER,      INTENT(in) :: ncin, ncout
  INTEGER,      INTENT(in) :: outdimids(6)
  REAL(dp),    INTENT(out), OPTIONAL :: missval

  !-- LOCAL
  LOGICAL      :: dimlon, dimlat, dimtil, dimlct, dimnsoil, dimtim
  INTEGER      :: stat, dim, att 
  INTEGER      :: varid_in, varid_out, type, natts 
  INTEGER      :: ndims_in, ndims_out
  INTEGER, ALLOCATABLE :: dimids_in(:), dimids_out(:)
  CHARACTER*30 :: dimname, attname
  REAL(dp)     :: miss
!------------------------------------------------------------------------------
  stat=nf_inq_varid(ncin, varname_in, varid_in)
  CALL hdlerr(stat,'define_var: inq. varid of '//varname_in)

  stat=nf_inq_varndims(ncin, varid_in, ndims_in)
  CALL hdlerr(stat,'define_var: inq. ndims of '//varname_in)
  ALLOCATE(dimids_in(ndims_in))
  stat=nf_inq_vardimid(ncin, varid_in, dimids_in(:))
  CALL hdlerr(stat,'define_var: inq. dimids of '//varname_in)

  ndims_out=0
  dimlon   = .FALSE.
  dimlat   = .FALSE.
  dimtil   = .FALSE.
  dimlct   = .FALSE.
  dimtim   = .FALSE.
  dimnsoil = .FALSE.

  DO dim=1,ndims_in
     stat=nf_inq_dimname(ncin, dimids_in(dim), dimname)
     CALL hdlerr(stat,'define_var: dimname of '//varname_in)
     IF (dimname == 'lon') THEN
        dimlon = .TRUE.
        ndims_out = ndims_out + 1
     ELSE IF (dimname == 'lat') THEN
        dimlat = .TRUE.
        ndims_out = ndims_out + 1
     ELSE IF (dimname == 'ntiles') THEN
        dimtil = .TRUE.
        ndims_out = ndims_out + 1
     ELSE IF (dimname == 'time') THEN
        dimtim = .TRUE.
        ndims_out = ndims_out + 1
     ELSE IF (dimname == 'lct') THEN
        dimlct = .TRUE.
        ndims_out = ndims_out + 1
     ELSE IF (dimname == 'nsoil') THEN
        dimnsoil = .TRUE.
        ndims_out = ndims_out + 1
     ELSE
        WRITE(*,*) "New dimension: "//dimname
     END IF
  END DO

  ALLOCATE(dimids_out(ndims_out))
  IF (ndims_out == 1) THEN
     IF (dimlon)   dimids_out(1:1) = (/outdimids(1)/)
     IF (dimlat)   dimids_out(1:1) = (/outdimids(2)/)
     IF (dimtil)   dimids_out(1:1) = (/outdimids(3)/)
     IF (dimlct)   dimids_out(1:1) = (/outdimids(4)/)
     IF (dimtim)   dimids_out(1:1) = (/outdimids(5)/)
     IF (dimnsoil) dimids_out(1:1) = (/outdimids(6)/)
  ELSE IF (ndims_out == 2) THEN
     dimids_out(1:2) = (/outdimids(1),outdimids(2)/)
  ELSE IF (ndims_out == 3) THEN
     IF (dimtil)   dimids_out(1:3) = outdimids((/1,2,3/))
     IF (dimlct)   dimids_out(1:3) = outdimids((/1,2,4/))
     IF (dimnsoil) dimids_out(1:3) = outdimids((/1,2,5/))
     IF (dimtim)   dimids_out(1:3) = outdimids((/1,2,6/))
  ELSE IF (ndims_out == 4) THEN
     IF (dimtil) dimids_out(1:4) = (/outdimids(1),outdimids(2),outdimids(3),outdimids(5)/)
     IF (dimlct) dimids_out(1:4) = (/outdimids(1),outdimids(2),outdimids(4),outdimids(5)/)
  ELSE
     WRITE(*,*) "WARNING: No more than four dimensions expected"
  END IF

  stat=nf_inq_vartype(ncin, varid_in, type)
  CALL hdlerr(stat,'define_var: inq. type of '//varname_in)
  stat=nf_inq_varnatts(ncin, varid_in, natts)
  CALL hdlerr(stat,'define_var: inq. natts of '//varname_in)

  stat=nf_def_var(ncout, varname_out, type, ndims_out, dimids_out, varid_out)
  CALL hdlerr(stat,'define_var: defining var '//varname_out)
  DO att=1,natts
     stat = nf_inq_attname(ncin, varid_in, att, attname)
     CALL hdlerr(stat,'define_var: attname of '//varname_in)
     stat = nf_copy_att(ncin, varid_in, attname, ncout,varid_out)
     CALL hdlerr(stat,'define_var: copy attribute '//attname)
     IF (attname == "_FillValue") THEN
        stat = nf_get_att_double(ncin, varid_in, attname, miss)
        CALL hdlerr(stat,'get missing value of: '//varname_in)
     ENDIF 
  END DO
  IF (PRESENT(missval)) missval = miss 

  DEALLOCATE(dimids_in)
  DEALLOCATE(dimids_out)

END SUBROUTINE define_var

!------------------------------------------------------------------------------
SUBROUTINE define_var_new(ncout, varname, ndims, outdimids)
!------------------------------------------------------------------------------
!
! Routine to define an output variable that had not been read from an other
! netcdf file.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE
  include 'netcdf.inc'

  !-- INTENT(in)
  CHARACTER*30, INTENT(in) :: varname
  INTEGER,      INTENT(in) :: ncout, ndims
  INTEGER,      INTENT(in) :: outdimids(ndims)

  !-- LOCAL
  INTEGER      :: stat, varid
  INTEGER, ALLOCATABLE :: dimids(:)
!------------------------------------------------------------------------------

  ALLOCATE(dimids(ndims))
  IF (ndims == 2) THEN
     dimids(1:2) = (/outdimids(1),outdimids(2)/)
  ELSE IF (ndims == 3) THEN
     dimids(1:3) = (/outdimids(1),outdimids(2),outdimids(3)/)
  ELSE
     WRITE(*,*) 'Only 2 or 3 dimensions supported so far'
  END IF

  stat=nf_def_var(ncout, varname, NF_DOUBLE, ndims, dimids, varid)
  CALL hdlerr(stat,'define_var_new: defining var '//varname)
  DEALLOCATE(dimids)

  ! define attributes

  IF (varname == 'slm') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 19, 'land sea mask')
    CALL hdlerr(stat,'define_var_new: put slm long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put slm units')

  ELSE IF (varname == 'cover_fract') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 19, 'land cover fraction')
    CALL hdlerr(stat,'define_var_new: put cover_fract long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put cover_fract units')
  
  ELSE IF (varname == 'cover_type') THEN
    CALL put_lct_attributes(ncout, varid)

  ELSE IF (varname == 'veg_ratio_max') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 27, 'maximum vegetation fraction')
    CALL hdlerr(stat,'define_var_new: put veg_ratio_max long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put veg_ratio_max units')

  ELSE IF (varname == 'albedo_veg_vis') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 38, &
         'vegetation albedo in the visible range')
    CALL hdlerr(stat,'define_var_new: put albedo_veg_vis long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put albedo_veg_vis units')

  ELSE IF (varname == 'albedo_veg_nir') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 28, 'vegetation albedo in the NIR')
    CALL hdlerr(stat,'define_var_new: put albedo_veg_nir long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put albedo_veg_nir units')

  ELSE IF (varname == 'albedo_soil_vis') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 32, 'soil albedo in the visible range')
    CALL hdlerr(stat,'define_var_new: put albedo_soil_vis long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put albedo_soil_vis units')

  ELSE IF (varname == 'albedo_soil_nir') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 22, 'soil albedo in the NIR')
    CALL hdlerr(stat,'define_var_new: put albedo_soil_nir long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put albedo_soil_nir units')

  ELSE IF (varname == 'roughness_length_oro') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 33, 'roughness_length due to orography')
    CALL hdlerr(stat,'define_var_new: put roughness_length_oro long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
    CALL hdlerr(stat,'define_var_new: put roughness_length_oro units')

  ELSE IF (varname == 'natural_veg') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 30, 'natural (potential) vegetation')
    CALL hdlerr(stat,'define_var_new: put natural_veg long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put natural_veg units')

  ELSE IF (varname == 'soil_depth') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 10, 'soil depth')
    CALL hdlerr(stat,'define_var_new: put soil_depth long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
    CALL hdlerr(stat,'define_var_new: put soil_depth units')
 
  ELSE IF (varname == 'soil_porosity') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 14, 'soil porosity')
    CALL hdlerr(stat,'define_var_new: put soil_porosity long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
    CALL hdlerr(stat,'define_var_new: put soil_porosity units')

  ELSE IF (varname == 'pore_size_index') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 20, 'soil pore size index')
    CALL hdlerr(stat,'define_var_new: put pore_size_index long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put pore_size_index units')

  ELSE IF (varname == 'soil_field_cap') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 19, 'soil field capacity')
    CALL hdlerr(stat,'define_var_new: put soil_field_cap long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 4, 'mm-1')
    CALL hdlerr(stat,'define_var_new: put soil_field_cap units')

  ELSE IF (varname == 'heat_capacity') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 25, 'heat capacity of dry soil')
    CALL hdlerr(stat,'define_var_new: put heat_capacity long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 7, 'Jm-3K-1')
    CALL hdlerr(stat,'define_var_new: put heat_capacity units')

  ELSE IF (varname == 'heat_conductivity') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 29, 'heat conductivity of dry soil')
    CALL hdlerr(stat,'define_var_new: put heat_conductivity long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 10, 'Jm-1s-1K-1')
    CALL hdlerr(stat,'define_var_new: put heat_conductivity units')

  ELSE IF (varname == 'moisture_pot') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 34, 'saturated matrix potential of soil')
    CALL hdlerr(stat,'define_var_new: put moisture_pot long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
    CALL hdlerr(stat,'define_var_new: put moisture_pot units')

  ELSE IF (varname == 'hyd_cond_sat') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 19, 'saturated hydraulic conductivity')
    CALL hdlerr(stat,'define_var_new: put hyd_cond_sat long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 4, 'ms-1')
    CALL hdlerr(stat,'define_var_new: put hyd_cond_sat units')

  ELSE IF (varname == 'wilting_point') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 39, 'Volumetric soil permanent wilting point')
    CALL hdlerr(stat,'define_var_new: put wilting_point long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 4, 'mm-1')
    CALL hdlerr(stat,'define_var_new: put wilting_point units')

  ELSE IF (varname == 'bclapp') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 31, 'Clapp and Hornberger b exponent')
    CALL hdlerr(stat,'define_var_new: put bclapp long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, '1')
    CALL hdlerr(stat,'define_var_new: put bclapp units')

  ELSE IF (varname == 'root_depth') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 13, 'rooting depth')
    CALL hdlerr(stat,'define_var_new: put root_depth long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
    CALL hdlerr(stat,'define_var_new: put root_depth units')

  ELSE IF (varname == 'layer_moist') THEN
    stat = nf_put_att_text(ncout, varid, 'long_name', 37, 'initial moisture for multilayer soils')
    CALL hdlerr(stat,'define_var_new: put layer_moist long_name')
    stat = nf_put_att_text(ncout, varid, 'units', 1, 'm')
    CALL hdlerr(stat,'define_var_new: put layer_moist units')

  ENDIF

END SUBROUTINE define_var_new

!------------------------------------------------------------------------------
SUBROUTINE set_global_attributes(ncout, year_ct, year_cf, res_oce, res_atm, &
     nlct, ntiles, nsoil, lcouple, ldynveg, lc3c4crop, lpasture, lread_pasture, &
     lpasture_rule, desert_only, grass_only, woods_only, ignore_measurements, &
     echam_fractional, maxmoist_version, pasture_tag, masks_file, svn_url, svn_rev)
!------------------------------------------------------------------------------
!
! Routine to set global attributes
!
!------------------------------------------------------------------------------
  IMPLICIT NONE
  include 'netcdf.inc'

  !-- INTENT(in)
  INTEGER,      INTENT(in) :: ncout
  INTEGER,      INTENT(in) :: nlct, ntiles, nsoil
  INTEGER,      INTENT(in) :: year_ct, year_cf
  CHARACTER(8), INTENT(in) :: res_oce, res_atm, maxmoist_version, pasture_tag
  LOGICAL,      INTENT(in) :: lcouple, desert_only, grass_only, woods_only
  LOGICAL,      INTENT(in) :: ldynveg, lc3c4crop, lpasture, lread_pasture, lpasture_rule
  LOGICAL,      INTENT(in) :: echam_fractional, ignore_measurements
  CHARACTER(100), INTENT(in) :: masks_file
  CHARACTER(*), INTENT(in) :: svn_url, svn_rev

  !-- LOCAL
  INTEGER        :: stat
  CHARACTER(8)   :: date
  CHARACTER(10)  :: time
  CHARACTER(400) :: comment, history, references, configuration
  CHARACTER(120) :: url, rev
  
!------------------------------------------------------------------------------

    stat = nf_put_att_text(ncout, NF_GLOBAL, 'title', 31, &
         'initial surface data for JSBACH')
    CALL hdlerr(stat,'set_global_attributes: title')

    stat = nf_put_att_text(ncout, NF_GLOBAL, 'institution', 54, &
         'Max Planck Institute for Meteorology, Hamburg, Germany')
    CALL hdlerr(stat,'set_global_attributes: institution')

    stat = nf_put_att_text(ncout, NF_GLOBAL, 'Conventions', 6, 'CF-1.0')
    CALL hdlerr(stat,'set_global_attributes: Conventions')

    stat = nf_put_att_int(ncout, NF_GLOBAL, 'refyear_for_cover_types', NF_INT, 1, year_ct )
    CALL hdlerr(stat,'set_global_attributes: year_ct')

    stat = nf_put_att_int(ncout, NF_GLOBAL, 'refyear_for_cover_fractions', NF_INT, 1, year_cf )
    CALL hdlerr(stat,'set_global_attributes: year_cf')

    stat = nf_put_att_text(ncout, NF_GLOBAL, 'spherical_truncation', LEN_TRIM(res_atm), TRIM(res_atm))
    CALL hdlerr(stat,'set_global_attributes: spherical_truncation')

    stat = nf_put_att_int(ncout, NF_GLOBAL, 'nlct', NF_INT, 1, nlct)
    CALL hdlerr(stat,'set_global_attributes: nlct')

    WRITE (configuration,'(a,i2,a,i2.1,a)') TRIM(res_atm)//TRIM(res_oce)//' ', ntiles, 'tiles ', nsoil, 'layers'
    stat = nf_put_att_text(ncout, NF_GLOBAL, 'configuration', LEN_TRIM(configuration), TRIM(configuration))
    CALL hdlerr(stat,'set_global_attributes: configuration')

    IF (lcouple) THEN
       comment = 'setup for an experiment with ocean model: grid '//TRIM(res_oce)
    ELSE
       comment = 'setup for an experiment without ocean model'
    END IF
    IF (desert_only) THEN
       comment = TRIM(comment)//CHAR(10)//'desert_only: all land has desert conditions'
    ELSE IF (grass_only) THEN
       comment = TRIM(comment)//CHAR(10)//'grass_only: all land covered by grass'
    ELSE IF (woods_only) THEN
       comment = TRIM(comment)//CHAR(10)//'woods_only: all land covered by woods'
    END IF
    IF (ignore_measurements) THEN
       comment = TRIM(comment)//CHAR(10)//'ignore_measurements: cover fractions only depend on latitude'
    END IF
    IF (ldynveg) THEN
       comment = TRIM(comment)//CHAR(10)//'setup for runs with dynamic vegetation'
    ELSE
       comment = TRIM(comment)//CHAR(10)//'setup for runs without dynamic vegetation'
    END IF
    IF (lc3c4crop) THEN
       comment = TRIM(comment)//CHAR(10)//'C3 and C4 crops distinguished'
    ELSE
       comment = TRIM(comment)//CHAR(10)//'C3 and C4 crops not distinguished'
    END IF
    IF (lpasture) THEN
       comment = TRIM(comment)//CHAR(10)//'pastures distinguished from crops'
    ELSE
       comment = TRIM(comment)//CHAR(10)//'pastures not distinguished form crops'
    END IF
    IF (lread_pasture) THEN
       SELECT CASE (TRIM(pasture_tag))
          CASE ('LUH')
              comment = TRIM(comment)//CHAR(10)// &
                   'crop and pasture fractions calculated from LUH harmonized land use data '// &
                   'generated for CMIP5'
          CASE ('LUH2v2h')
              comment = TRIM(comment)//CHAR(10)// &
                   'crop and pasture fractions calculated from LUH2 v2h Release (10/14/16)' &
                   //CHAR(10)//'crop cover types calculated from 1961 to 2005 average of FAO data'
          CASE DEFAULT
              comment = TRIM(comment)//CHAR(10)// &
                   'crop and pasture fractions from '//(TRIM(pasture_tag))
       END SELECT
    END IF
    IF (lpasture_rule) THEN
       comment = TRIM(comment)//CHAR(10)//'pasture rule applied'
    END IF
    IF (echam_fractional) THEN
       comment = TRIM(comment)//CHAR(10)//'setup for ECHAM with fractional land sea mask'
    END IF
    IF (maxmoist_version == 'LSP3') THEN
       comment = TRIM(comment)//CHAR(10)//'maximum soil moisture from LSP3 (Stacke 2013) '
    ELSE IF (maxmoist_version == 'LSP2') THEN
       comment = TRIM(comment)//CHAR(10)//'maximum soil moisture from LSP2 (as in echam5) '
    END IF
    IF (TRIM(masks_file) /= 'default') THEN
      comment = TRIM(comment)//CHAR(10)//'land sea, glacier and lake masks read from file: ' &
           //TRIM(masks_file)
    END IF    
    stat = nf_put_att_text(ncout, NF_GLOBAL, 'comment', LEN_TRIM(comment), TRIM(comment))
    CALL hdlerr(stat,'set_global_attributes: comment')

    CALL DATE_AND_TIME(date,time)
    url=svn_url(11:LEN_TRIM(svn_url)-2)
    rev=svn_rev(2:LEN_TRIM(svn_rev)-2)
    history = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '       &
               //time(1:2)//':'//time(3:4)//' : created with '     &
               //CHAR(10)//TRIM(url)//CHAR(10)//TRIM(rev) 
    stat = nf_put_att_text(ncout, NF_GLOBAL, 'history', LEN_TRIM(history), TRIM(history))
    CALL hdlerr(stat,'set_global_attributes: history')


    references = 'Hagemann, S. (2002): An improved land surface parameter ' &
       //CHAR(10)//'  dataset for global and regional climate models, ' &
       //CHAR(10)//'  Max Planck Institute for Meteorology, Report 336' &
       //CHAR(10)//'Pongratz, J. et al. (2008), A reconstruction of global ' &
       //CHAR(10)//'  agricultural areas and land cover for the last millennium, '&
       //CHAR(10)//'  Global Biogeochem. Cycles, 22, GB3018, doi:10.1029/2007GB003153.'
    stat = nf_put_att_text(ncout, NF_GLOBAL, 'references', LEN_TRIM(references), TRIM(references))
    CALL hdlerr(stat,'set_global_attributes: references')
  

END SUBROUTINE set_global_attributes

!------------------------------------------------------------------------------
SUBROUTINE hdlerr(stat,string)
!------------------------------------------------------------------------------
!
!  Routine to handle netcdf errors
!
!------------------------------------------------------------------------------

  IMPLICIT NONE

  include 'netcdf.inc'

! INTENT(in)
  INTEGER,       INTENT(in) :: stat
  CHARACTER*(*), INTENT(in) :: string

!------------------------------------------------------------------------------

  IF (stat /= NF_NOERR) THEN
     WRITE (6,*) '--------'
     WRITE (6,*) ' ERROR:  ', string
     WRITE (6,*) '--------'
     STOP 1
  END IF

END SUBROUTINE hdlerr
!------------------------------------------------------------------------------
