!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!! NEMO/OPA  :  1 - run manager      (namrun)
!! namelists    2 - Domain           (namcfg, namzgr, namzgr_sco, namdom, namtsd)
!!              3 - Surface boundary (namsbc, namsbc_ana, namsbc_flx, namsbc_clio, namsbc_core, namsbc_sas
!!                                    namsbc_cpl, namtra_qsr, namsbc_rnf,
!!                                    namsbc_apr, namsbc_ssr, namsbc_alb)
!!              4 - lateral boundary (namlbc, namcla, namagrif, nambdy, nambdy_tide)
!!              5 - bottom  boundary (nambfr, nambbc, nambbl)
!!              6 - Tracer           (nameos, namtra_adv, namtra_ldf, namtra_dmp)
!!              7 - dynamics         (namdyn_adv, namdyn_vor, namdyn_hpg, namdyn_spg, namdyn_ldf)
!!              8 - Verical physics  (namzdf, namzdf_ric, namzdf_tke, namzdf_kpp, namzdf_ddm, namzdf_tmx, namzdf_tmx_new)
!!              9 - diagnostics      (namnc4, namtrd, namspr, namflo, namhsb, namsto)
!!             10 - miscellaneous    (namsol, nammpp, namctl)
!!             11 - Obs & Assim      (namobs, nam_asminc)
!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!!======================================================================
!!                   ***  Run management namelists  ***
!!======================================================================
!!   namrun       parameters of the run
!!======================================================================
!
!-----------------------------------------------------------------------
&namrun        !   parameters of the run
!-----------------------------------------------------------------------
   nn_no       =       0   !  job number (no more used...)
   cn_exp      =  "ORCA2"  !  experience name
   nn_it000    =       1   !  first time step
   nn_itend    =    5475   !  last  time step (std 5475)
   nn_date0    =  010101   !  date at nit_0000 (format yyyymmdd) used if ln_rstart=F or (ln_rstart=T and nn_rstctl=0 or 1)
   nn_leapy    =       0   !  Leap year calendar (1) or not (0)
   ln_rstart   = .false.   !  start from rest (F) or from a restart file (T)
   nn_euler    =       1   !  = 0 : start with forward time step if ln_rstart=T
   nn_rstctl   =       0   !  restart control ==> activated only if ln_rstart=T
                           !    = 0 nn_date0 read in namelist ; nn_it000 : read in namelist
                           !    = 1 nn_date0 read in namelist ; nn_it000 : check consistancy between namelist and restart
                           !    = 2 nn_date0 read in restart  ; nn_it000 : check consistancy between namelist and restart
   cn_ocerst_in  = "restart"   !  suffix of ocean restart name (input)
   cn_ocerst_indir = "."       !  directory from which to read input ocean restarts
   cn_ocerst_out = "restart"   !  suffix of ocean restart name (output)
   cn_ocerst_outdir = "."      !  directory in which to write output ocean restarts
   nn_istate   =       0   !  output the initial state (1) or not (0)
   ln_rst_list = .false.   !  output restarts at list of times using nn_stocklist (T) or at set frequency with nn_stock (F)
   nn_stock    =    5475   !  frequency of creation of a restart file (modulo referenced to 1)
   nn_stocklist = 0,0,0,0,0,0,0,0,0,0 ! List of timesteps when a restart file is to be written
   nn_write    =    5475   !  frequency of write in the output file   (modulo referenced to nn_it000)
   ln_dimgnnn  = .false.   !  DIMG file format: 1 file for all processors (F) or by processor (T)
   ln_mskland  = .false.   !  mask land points in NetCDF outputs (costly: + ~15%)
   ln_cfmeta   = .false.   !  output additional data to netCDF files required for compliance with the CF metadata standard
   ln_clobber  = .false.   !  clobber (overwrite) an existing file
   nn_chunksz  =       0   !  chunksize (bytes) for NetCDF file (works only with iom_nf90 routines)
/
!
!!======================================================================
!!                      ***  Domain namelists  ***
!!======================================================================
!!   namcfg       parameters of the configuration
!!   namzgr       vertical coordinate
!!   namzgr_sco   s-coordinate or hybrid z-s-coordinate
!!   namdom       space and time domain (bathymetry, mesh, timestep)
!!   namtsd       data: temperature & salinity
!!======================================================================
!
!-----------------------------------------------------------------------
&namcfg     !   parameters of the configuration
!-----------------------------------------------------------------------
   cp_cfg      =  "default"            !  name of the configuration
   cp_cfz      =  "no zoom"            !  name of the zoom of configuration
   jp_cfg      =       0               !  resolution of the configuration
   jpidta      =      10               !  1st lateral dimension ( >= jpi )
   jpjdta      =      12               !  2nd    "         "    ( >= jpj )
   jpkdta      =      31               !  number of levels      ( >= jpk )
   jpiglo      =      10               !  1st dimension of global domain --> i =jpidta
   jpjglo      =      12               !  2nd    -                  -    --> j =jpjdta
   jpizoom     =       1               !  left bottom (i,j) indices of the zoom
   jpjzoom     =       1               !  in data domain indices
   jperio      =       0               !  lateral cond. type (between 0 and 6)
                                       !  = 0 closed                 ;   = 1 cyclic East-West
                                       !  = 2 equatorial symmetric   ;   = 3 North fold T-point pivot
                                       !  = 4 cyclic East-West AND North fold T-point pivot
                                       !  = 5 North fold F-point pivot
                                       !  = 6 cyclic East-West AND North fold F-point pivot
   ln_use_jattr = .false.              !  use (T) the file attribute: open_ocean_jstart, if present
                                       !  in netcdf input files, as the start j-row for reading
/
!-----------------------------------------------------------------------
&namzgr        !   vertical coordinate
!-----------------------------------------------------------------------
   ln_zco      = .false.   !  z-coordinate - full    steps   (T/F)      ("key_zco" may also be defined)
   ln_zps      = .true.    !  z-coordinate - partial steps   (T/F)
   ln_sco      = .false.   !  s- or hybrid z-s-coordinate    (T/F)
   ln_isfcav   = .false.   !  ice shelf cavity               (T/F)
/
!-----------------------------------------------------------------------
&namzgr_sco    !   s-coordinate or hybrid z-s-coordinate
!-----------------------------------------------------------------------
   ln_s_sh94   = .true.    !  Song & Haidvogel 1994 hybrid S-sigma   (T)|
   ln_s_sf12   = .false.   !  Siddorn & Furner 2012 hybrid S-z-sigma (T)| if both are false the NEMO tanh stretching is applied
   ln_sigcrit  = .false.   !  use sigma coordinates below critical depth (T) or Z coordinates (F) for Siddorn & Furner stretch
                           !  stretching coefficients for all functions
   rn_sbot_min =   10.0    !  minimum depth of s-bottom surface (>0) (m)
   rn_sbot_max = 7000.0    !  maximum depth of s-bottom surface (= ocean depth) (>0) (m)
   rn_hc       =  150.0    !  critical depth for transition to stretched coordinates
                        !!!!!!!  Envelop bathymetry
   rn_rmax     =    0.3    !  maximum cut-off r-value allowed (0<r_max<1)
                        !!!!!!!  SH94 stretching coefficients  (ln_s_sh94 = .true.)
   rn_theta    =    6.0    !  surface control parameter (0<=theta<=20)
   rn_bb       =    0.8    !  stretching with SH94 s-sigma
                        !!!!!!!  SF12 stretching coefficient  (ln_s_sf12 = .true.)
   rn_alpha    =    4.4    !  stretching with SF12 s-sigma
   rn_efold    =    0.0    !  efold length scale for transition to stretched coord
   rn_zs       =    1.0    !  depth of surface grid box
                           !  bottom cell depth (Zb) is a linear function of water depth Zb = H*a + b
   rn_zb_a     =    0.024  !  bathymetry scaling factor for calculating Zb
   rn_zb_b     =   -0.2    !  offset for calculating Zb
                        !!!!!!!! Other stretching (not SH94 or SF12) [also uses rn_theta above]
   rn_thetb    =    1.0    !  bottom control parameter  (0<=thetb<= 1)
/
!-----------------------------------------------------------------------
&namdom        !   space and time domain (bathymetry, mesh, timestep)
!-----------------------------------------------------------------------
   nn_bathy    =    1      !  compute (=0) or read (=1) the bathymetry file
   rn_bathy    =    0.     !  value of the bathymetry. if (=0) bottom flat at jpkm1
   nn_closea   =    0      !  remove (=0) or keep (=1) closed seas and lakes (ORCA)
   nn_msh      =    1      !  create (=1) a mesh file or not (=0)
   rn_hmin     =   -3.     !  min depth of the ocean (>0) or min number of ocean level (<0)
   rn_e3zps_min=   20.     !  partial step thickness is set larger than the minimum of
   rn_e3zps_rat=    0.1    !  rn_e3zps_min and rn_e3zps_rat*e3t, with 0<rn_e3zps_rat<1
                           !
   rn_rdt      = 5760.     !  time step for the dynamics (and tracer if nn_acc=0)
   rn_atfp     =    0.1    !  asselin time filter parameter
   nn_acc      =    0      !  acceleration of convergence : =1      used, rdt < rdttra(k)
                                 !                          =0, not used, rdt = rdttra
   rn_rdtmin   = 28800.          !  minimum time step on tracers (used if nn_acc=1)
   rn_rdtmax   = 28800.          !  maximum time step on tracers (used if nn_acc=1)
   rn_rdth     =  800.           !  depth variation of tracer time step  (used if nn_acc=1)
   ln_crs      = .false.      !  Logical switch for coarsening module
   jphgr_msh   =       0               !  type of horizontal mesh
                                       !  = 0 curvilinear coordinate on the sphere read in coordinate.nc
                                       !  = 1 geographical mesh on the sphere with regular grid-spacing
                                       !  = 2 f-plane with regular grid-spacing
                                       !  = 3 beta-plane with regular grid-spacing
                                       !  = 4 Mercator grid with T/U point at the equator
   ppglam0     =       0.0             !  longitude of first raw and column T-point (jphgr_msh = 1)
   ppgphi0     =     -35.0             ! latitude  of first raw and column T-point (jphgr_msh = 1)
   ppe1_deg    =       1.0             !  zonal      grid-spacing (degrees)
   ppe2_deg    =       0.5             !  meridional grid-spacing (degrees)
   ppe1_m      =    5000.0             !  zonal      grid-spacing (degrees)
   ppe2_m      =    5000.0             !  meridional grid-spacing (degrees)
   ppsur       =    -4762.96143546300  !  ORCA r4, r2 and r05 coefficients
   ppa0        =      255.58049070440  ! (default coefficients)
   ppa1        =      245.58132232490  !
   ppkth       =       21.43336197938  !
   ppacr       =        3.0            !
   ppdzmin     =       10.             !  Minimum vertical spacing
   pphmax      =     5000.             !  Maximum depth
   ldbletanh   =    .TRUE.             !  Use/do not use double tanf function for vertical coordinates
   ppa2        =      100.760928500000 !  Double tanh function parameters
   ppkth2      =       48.029893720000 !
   ppacr2      =       13.000000000000 !
/
!-----------------------------------------------------------------------
&namsplit      !   time splitting parameters                            ("key_dynspg_ts")
!-----------------------------------------------------------------------
   ln_bt_fw      =    .TRUE.           !  Forward integration of barotropic equations
   ln_bt_av      =    .TRUE.           !  Time filtering of barotropic variables
   ln_bt_nn_auto =    .TRUE.           !  Set nn_baro automatically to be just below
                                       !  a user defined maximum courant number (rn_bt_cmax)
   nn_baro       =    30               !  Number of iterations of barotropic mode
                                       !  during rn_rdt seconds. Only used if ln_bt_nn_auto=F
   rn_bt_cmax    =    0.8              !  Maximum courant number allowed if ln_bt_nn_auto=T
   nn_bt_flt     =    1                !  Time filter choice
                                       !  = 0 None
                                       !  = 1 Boxcar over   nn_baro barotropic steps
                                       !  = 2 Boxcar over 2*nn_baro     "        "
/
!-----------------------------------------------------------------------
&namcrs        !   Grid coarsening for dynamics output and/or
               !   passive tracer coarsened online simulations
!-----------------------------------------------------------------------
   nn_factx    = 3         !  Reduction factor of x-direction
   nn_facty    = 3         !  Reduction factor of y-direction
   nn_binref   = 0         !  Bin centering preference: NORTH or EQUAT
                           !  0, coarse grid is binned with preferential treatment of the north fold
                           !  1, coarse grid is binned with centering at the equator
                           !    Symmetry with nn_facty being odd-numbered. Asymmetry with even-numbered nn_facty.
   nn_msh_crs  = 1         !  create (=1) a mesh file or not (=0)
   nn_crs_kz   = 0         ! 0, MEAN of volume boxes
                           ! 1, MAX of boxes
                           ! 2, MIN of boxes
   ln_crs_wn   = .true.    ! wn coarsened (T) or computed using horizontal divergence ( F )
/
!-----------------------------------------------------------------------
&namc1d        !   1D configuration options                             ("key_c1d")
!-----------------------------------------------------------------------
   rn_lat1d    =      50   !  Column latitude (default at PAPA station)
   rn_lon1d    =    -145   !  Column longitude (default at PAPA station)
   ln_c1d_locpt=  .true.   ! Localization of 1D config in a grid (T) or independant point (F)
/
!-----------------------------------------------------------------------
&namtsd    !   data : Temperature  & Salinity
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          !  file name                            ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!          !                                       !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_tem  = 'data_1m_potential_temperature_nomask',         -1        ,'votemper' ,    .true.    , .true. , 'yearly'   , ''       ,   ''    ,    ''
   sn_sal  = 'data_1m_salinity_nomask'             ,         -1        ,'vosaline' ,    .true.    , .true. , 'yearly'   , ''       ,   ''    ,    ''
   !
   cn_dir        = './'     !  root directory for the location of the runoff files
   ln_tsd_init   = .true.   !  Initialisation of ocean T & S with T &S input data (T) or not (F)
   ln_tsd_tradmp = .true.   !  damping of ocean T & S toward T &S input data (T) or not (F)
/
!!======================================================================
!!            ***  Surface Boundary Condition namelists  ***
!!======================================================================
!!   namsbc          surface boundary condition
!!   namsbc_ana      analytical         formulation
!!   namsbc_flx      flux               formulation
!!   namsbc_clio     CLIO bulk formulae formulation
!!   namsbc_core     CORE bulk formulae formulation
!!   namsbc_mfs      MFS  bulk formulae formulation
!!   namsbc_cpl      CouPLed            formulation                     ("key_oasis3")
!!   namsbc_sas      StAndalone Surface module
!!   namtra_qsr      penetrative solar radiation
!!   namsbc_rnf      river runoffs
!!   namsbc_isf      ice shelf melting/freezing
!!   namsbc_apr      Atmospheric Pressure
!!   namsbc_ssr      sea surface restoring term (for T and/or S)
!!   namsbc_alb      albedo parameters
!!======================================================================
!
!-----------------------------------------------------------------------
&namsbc        !   Surface Boundary Condition (surface module)
!-----------------------------------------------------------------------
   nn_fsbc     = 5         !  frequency of surface boundary condition computation
                           !     (also = the frequency of sea-ice model call)
   ln_ana      = .false.   !  analytical formulation                    (T => fill namsbc_ana )
   ln_flx      = .false.   !  flux formulation                          (T => fill namsbc_flx )
   ln_blk_clio = .false.   !  CLIO bulk formulation                     (T => fill namsbc_clio)
   ln_blk_core = .true.    !  CORE bulk formulation                     (T => fill namsbc_core)
   ln_blk_mfs  = .false.   !  MFS bulk formulation                      (T => fill namsbc_mfs )
   ln_cpl      = .false.   !  atmosphere coupled   formulation          ( requires key_oasis3 )
   ln_mixcpl   = .false.   !  forced-coupled mixed formulation          ( requires key_oasis3 )
   nn_components = 0       !  configuration of the opa-sas OASIS coupling
                           !  =0 no opa-sas OASIS coupling: default single executable configuration
                           !  =1 opa-sas OASIS coupling: multi executable configuration, OPA component
                           !  =2 opa-sas OASIS coupling: multi executable configuration, SAS component 
   ln_apr_dyn  = .false.   !  Patm gradient added in ocean & ice Eqs.   (T => fill namsbc_apr )
   nn_ice      = 2         !  =0 no ice boundary condition   ,
                           !  =1 use observed ice-cover      ,
                           !  =2 ice-model used                         ("key_lim3" or "key_lim2")
   nn_ice_embd = 1         !  =0 levitating ice (no mass exchange, concentration/dilution effect)
                           !  =1 levitating ice with mass and salt exchange but no presure effect
                           !  =2 embedded sea-ice (full salt and mass exchanges and pressure)
   ln_dm2dc    = .false.   !  daily mean to diurnal cycle on short wave
   ln_rnf      = .true.    !  runoffs                                   (T   => fill namsbc_rnf)
   nn_isf      = 0         !  ice shelf melting/freezing                (/=0 => fill namsbc_isf)
                           !  0 =no isf                  1 = presence of ISF
                           !  2 = bg03 parametrisation   3 = rnf file for isf
                           !  4 = ISF fwf specified
                           !  option 1 and 4 need ln_isfcav = .true. (domzgr)
   ln_ssr      = .true.    !  Sea Surface Restoring on T and/or S       (T => fill namsbc_ssr)
   nn_fwb      = 2         !  FreshWater Budget: =0 unchecked
                           !     =1 global mean of e-p-r set to zero at each time step
                           !     =2 annual global mean of e-p-r set to zero
   ln_wave = .false.       !  Activate coupling with wave (either Stokes Drift or Drag coefficient, or both)  (T => fill namsbc_wave)
   ln_cdgw = .false.       !  Neutral drag coefficient read from wave model (T => fill namsbc_wave)
   ln_sdw  = .false.       !  Computation of 3D stokes drift                (T => fill namsbc_wave)
   nn_lsm  = 0             !  =0 land/sea mask for input fields is not applied (keep empty land/sea mask filename field) ,
                           !  =1:n number of iterations of land/sea mask application for input fields (fill land/sea mask filename field)
   nn_limflx = -1          !  LIM3 Multi-category heat flux formulation (use -1 if LIM3 is not used)
                           !  =-1  Use per-category fluxes, bypass redistributor, forced mode only, not yet implemented coupled
                           !  = 0  Average per-category fluxes (forced and coupled mode)
                           !  = 1  Average and redistribute per-category fluxes, forced mode only, not yet implemented coupled
                           !  = 2  Redistribute a single flux over categories (coupled mode only)
/
!-----------------------------------------------------------------------
&namsbc_ana    !   analytical surface boundary condition
!-----------------------------------------------------------------------
   nn_tau000   =   0       !  gently increase the stress over the first ntau_rst time-steps
   rn_utau0    =   0.5     !  uniform value for the i-stress
   rn_vtau0    =   0.e0    !  uniform value for the j-stress
   rn_qns0     =   0.e0    !  uniform value for the total heat flux
   rn_qsr0     =   0.e0    !  uniform value for the solar radiation
   rn_emp0     =   0.e0    !  uniform value for the freswater budget (E-P)
/
!-----------------------------------------------------------------------
&namsbc_flx    !   surface boundary condition : flux formulation
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_utau     = 'utau'      ,        24         , 'utau'    , .false.      , .false., 'yearly'  , ''       , ''       , ''
   sn_vtau     = 'vtau'      ,        24         , 'vtau'    , .false.      , .false., 'yearly'  , ''       , ''       , ''
   sn_qtot     = 'qtot'      ,        24         , 'qtot'    , .false.      , .false., 'yearly'  , ''       , ''       , ''
   sn_qsr      = 'qsr'       ,        24         , 'qsr'     , .false.      , .false., 'yearly'  , ''       , ''       , ''
   sn_emp      = 'emp'       ,        24         , 'emp'     , .false.      , .false., 'yearly'  , ''       , ''       , ''

   cn_dir      = './'      !  root directory for the location of the flux files
/
!-----------------------------------------------------------------------
&namsbc_clio   !   namsbc_clio  CLIO bulk formulae
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_utau     = 'taux_1m'   ,       -1          , 'sozotaux',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_vtau     = 'tauy_1m'   ,       -1          , 'sometauy',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_wndm     = 'flx'       ,       -1          , 'socliowi',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_tair     = 'flx'       ,       -1          , 'socliot2',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_humi     = 'flx'       ,       -1          , 'socliohu',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_ccov     = 'flx'       ,       -1          , 'socliocl',   .false.    , .true. , 'yearly'  , ''       , ''       , ''
   sn_prec     = 'flx'       ,       -1          , 'socliopl',   .false.    , .true. , 'yearly'  , ''       , ''       , ''

   cn_dir      = './'      !  root directory for the location of the bulk files are
/
!-----------------------------------------------------------------------
&namsbc_core   !   namsbc_core  CORE bulk formulae
!-----------------------------------------------------------------------
!              !  file name                    ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights                               ! rotation ! land/sea mask !
!              !                               !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename                              ! pairing  ! filename      !
   sn_wndi     = 'u_10.15JUNE2009_fill'        ,         6         , 'U_10_MOD',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bicubic_noc.nc'   , 'Uwnd'   , ''
   sn_wndj     = 'v_10.15JUNE2009_fill'        ,         6         , 'V_10_MOD',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bicubic_noc.nc'   , 'Vwnd'   , ''
   sn_qsr      = 'ncar_rad.15JUNE2009_fill'    ,        24         , 'SWDN_MOD',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''
   sn_qlw      = 'ncar_rad.15JUNE2009_fill'    ,        24         , 'LWDN_MOD',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''
   sn_tair     = 't_10.15JUNE2009_fill'        ,         6         , 'T_10_MOD',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''
   sn_humi     = 'q_10.15JUNE2009_fill'        ,         6         , 'Q_10_MOD',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''
   sn_prec     = 'ncar_precip.15JUNE2009_fill' ,        -1         , 'PRC_MOD1',   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''
   sn_snow     = 'ncar_precip.15JUNE2009_fill' ,        -1         , 'SNOW'    ,   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''
   sn_tdif     = 'taudif_core'                 ,        24         , 'taudif'  ,   .false.    , .true. , 'yearly'  , 'weights_core_orca2_bilinear_noc.nc'  , ''       , ''

   cn_dir      = './'      !  root directory for the location of the bulk files
   ln_taudif   = .false.   !  HF tau contribution: use "mean of stress module - module of the mean stress" data
   rn_zqt      = 10.        !  Air temperature and humidity reference height (m)
   rn_zu       = 10.        !  Wind vector reference height (m)
   rn_pfac     = 1.        !  multiplicative factor for precipitation (total & snow)
   rn_efac     = 1.        !  multiplicative factor for evaporation (0. or 1.)
   rn_vfac     = 0.        !  multiplicative factor for ocean/ice velocity
                           !  in the calculation of the wind stress (0.=absolute winds or 1.=relative winds)
/
!-----------------------------------------------------------------------
&namsbc_mfs   !   namsbc_mfs  MFS bulk formulae
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights     ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename    ! pairing  ! filename      !
   sn_wndi     =   'ecmwf'   ,        6          , 'u10'     ,    .true.    , .false. , 'daily'  ,'bicubic.nc' , ''       , ''
   sn_wndj     =   'ecmwf'   ,        6          , 'v10'     ,    .true.    , .false. , 'daily'  ,'bicubic.nc' , ''       , ''
   sn_clc      =   'ecmwf'   ,        6          , 'clc'     ,    .true.    , .false. , 'daily'  ,'bilinear.nc', ''       , ''
   sn_msl      =   'ecmwf'   ,        6          , 'msl'     ,    .true.    , .false. , 'daily'  ,'bicubic.nc' , ''       , ''
   sn_tair     =   'ecmwf'   ,        6          , 't2'      ,    .true.    , .false. , 'daily'  ,'bicubic.nc' , ''       , ''
   sn_rhm      =   'ecmwf'   ,        6          , 'rh'      ,    .true.    , .false. , 'daily'  ,'bilinear.nc', ''       , ''
   sn_prec     =   'ecmwf'   ,        6          , 'precip'  ,    .true.    , .true.  , 'daily'  ,'bicubic.nc' , ''       , ''

   cn_dir      = './ECMWF/'      !  root directory for the location of the bulk files
/
!-----------------------------------------------------------------------
&namsbc_cpl    !   coupled ocean/atmosphere model                       ("key_oasis3")
!-----------------------------------------------------------------------
!                    !     description       !  multiple  !    vector   !      vector          ! vector !
!                    !                       ! categories !  reference  !    orientation       ! grids  !
! send
   sn_snd_temp   =       'weighted oce and ice' ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_alb    =       'weighted ice'         ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_thick  =       'none'                 ,    'no'   ,     ''      ,         ''           ,   ''
   sn_snd_crt    =       'none'                 ,    'no'    , 'spherical' , 'eastward-northward' ,  'T'
   sn_snd_co2    =       'coupled'              ,    'no'    ,     ''      ,         ''           ,   ''
! receive
   sn_rcv_w10m   =       'none'                 ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_taumod =       'coupled'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_tau    =       'oce only'             ,    'no'    , 'cartesian' , 'eastward-northward',  'U,V'
   sn_rcv_dqnsdt =       'coupled'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_qsr    =       'oce and ice'          ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_qns    =       'oce and ice'          ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_emp    =       'conservative'         ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_rnf    =       'coupled'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_cal    =       'coupled'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_co2    =       'coupled'              ,    'no'    ,     ''      ,         ''          ,   ''
!
   nn_cplmodel   =     1     !  Maximum number of models to/from which NEMO is potentialy sending/receiving data
   ln_usecplmask = .false.   !  use a coupling mask file to merge data received from several models
                             !   -> file cplmask.nc with the float variable called cplmask (jpi,jpj,nn_cplmodel)
/
!-----------------------------------------------------------------------
&namsbc_sas    !   analytical surface boundary condition
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_usp      = 'sas_grid_U' ,    120           , 'vozocrtx' ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''
   sn_vsp      = 'sas_grid_V' ,    120           , 'vomecrty' ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''
   sn_tem      = 'sas_grid_T' ,    120           , 'sosstsst' ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''
   sn_sal      = 'sas_grid_T' ,    120           , 'sosaline' ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''
   sn_ssh      = 'sas_grid_T' ,    120           , 'sossheig' ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''
   sn_e3t      = 'sas_grid_T' ,    120           , 'e3t_m'    ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''
   sn_frq      = 'sas_grid_T' ,    120           , 'frq_m'    ,  .true.    , .true. ,   'yearly'  , ''       , ''             , ''

   ln_3d_uve   = .true.    !  specify whether we are supplying a 3D u,v and e3 field
   ln_read_frq = .false.    !  specify whether we must read frq or not
   cn_dir      = './'      !  root directory for the location of the bulk files are
/
!-----------------------------------------------------------------------
&namtra_qsr    !   penetrative solar radiation
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_chl      ='chlorophyll',        -1         , 'CHLA'    ,   .true.     , .true. , 'yearly'  , ''       , ''       , ''

   cn_dir      = './'      !  root directory for the location of the runoff files
   ln_traqsr   = .true.    !  Light penetration (T) or not (F)
   ln_qsr_rgb  = .true.    !  RGB (Red-Green-Blue) light penetration
   ln_qsr_2bd  = .false.   !  2 bands              light penetration
   ln_qsr_bio  = .false.   !  bio-model light penetration
   nn_chldta   =      1    !  RGB : 2D Chl data (=1), 3D Chl data (=2) or cst value (=0)
   rn_abs      =   0.58    !  RGB & 2 bands: fraction of light (rn_si1)
   rn_si0      =   0.35    !  RGB & 2 bands: shortess depth of extinction
   rn_si1      =   23.0    !  2 bands: longest depth of extinction
   ln_qsr_ice  = .true.    !  light penetration for ice-model LIM3
/
!-----------------------------------------------------------------------
&namsbc_rnf    !   runoffs namelist surface boundary condition
!-----------------------------------------------------------------------
!              !  file name           ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !                      !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_rnf      = 'runoff_core_monthly',        -1         , 'sorunoff',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_cnf      = 'runoff_core_monthly',         0         , 'socoefr0',   .false.    , .true. , 'yearly'  , ''       , ''       , ''
   sn_s_rnf    = 'runoffs'            ,        24         , 'rosaline',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_t_rnf    = 'runoffs'            ,        24         , 'rotemper',   .true.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_dep_rnf  = 'runoffs'            ,         0         , 'rodepth' ,   .false.    , .true. , 'yearly'  , ''       , ''       , ''

   cn_dir       = './'      !  root directory for the location of the runoff files
   ln_rnf_mouth = .true.    !  specific treatment at rivers mouths
   rn_hrnf      =  15.e0    !  depth over which enhanced vertical mixing is used
   rn_avt_rnf   =   1.e-3   !  value of the additional vertical mixing coef. [m2/s]
   rn_rfact     =   1.e0    !  multiplicative factor for runoff
   ln_rnf_depth = .false.   !  read in depth information for runoff
   ln_rnf_tem   = .false.   !  read in temperature information for runoff
   ln_rnf_sal   = .false.   !  read in salinity information for runoff
   ln_rnf_depth_ini = .false.  ! compute depth at initialisation from runoff file
   rn_rnf_max   = 5.735e-4  !  max value of the runoff climatologie over global domain ( ln_rnf_depth_ini = .true )
   rn_dep_max   = 150.      !  depth over which runoffs is spread ( ln_rnf_depth_ini = .true )
   nn_rnf_depth_file = 0    !  create (=1) a runoff depth file or not (=0)
/
!-----------------------------------------------------------------------
&namsbc_isf    !  Top boundary layer (ISF)
!-----------------------------------------------------------------------
!              ! file name ! frequency (hours) ! variable ! time interpol. !  clim   ! 'yearly'/ ! weights  ! rotation !
!              !           !  (if <0  months)  !   name   !    (logical)   !  (T/F)  ! 'monthly' ! filename ! pairing  !
! nn_isf == 4
   sn_qisf      = 'rnfisf' ,         -12      ,'sohflisf',    .false.      , .true.  , 'yearly'  ,  ''      ,   ''
   sn_fwfisf    = 'rnfisf' ,         -12      ,'sowflisf',    .false.      , .true.  , 'yearly'  ,  ''      ,   ''
! nn_isf == 3
   sn_rnfisf    = 'runoffs' ,         -12      ,'sofwfisf',    .false.      , .true.  , 'yearly'  ,  ''      ,   ''
! nn_isf == 2 and 3
   sn_depmax_isf = 'runoffs' ,       -12        ,'sozisfmax' ,   .false.  , .true.  , 'yearly'  ,  ''      ,   ''
   sn_depmin_isf = 'runoffs' ,       -12        ,'sozisfmin' ,   .false.  , .true.  , 'yearly'  ,  ''      ,   ''
! nn_isf == 2
   sn_Leff_isf = 'rnfisf' ,       0          ,'Leff'         ,   .false.  , .true.  , 'yearly'  ,  ''      ,   ''
! for all case
   ln_divisf   = .true.  ! apply isf melting as a mass flux or in the salinity trend. (maybe I should remove this option as for runoff?)
! only for nn_isf = 1 or 2
   rn_gammat0  = 1.0e-4   ! gammat coefficient used in blk formula
   rn_gammas0  = 1.0e-4   ! gammas coefficient used in blk formula
! only for nn_isf = 1
   nn_isfblk   =  1       ! 1 ISOMIP ; 2 conservative (3 equation formulation, Jenkins et al. 1991 ??)
   rn_hisf_tbl =  30.      ! thickness of the top boundary layer           (Losh et al. 2008)
                          ! 0 => thickness of the tbl = thickness of the first wet cell
   ln_conserve = .true.   ! conservative case (take into account meltwater advection)
   nn_gammablk = 1        ! 0 = cst Gammat (= gammat/s)
                          ! 1 = velocity dependend Gamma (u* * gammat/s)  (Jenkins et al. 2010)
                          !     if you want to keep the cd as in global config, adjust rn_gammat0 to compensate
                          ! 2 = velocity and stability dependent Gamma    Holland et al. 1999
/
!-----------------------------------------------------------------------
&namsbc_apr    !   Atmospheric pressure used as ocean forcing or in bulk
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_apr      = 'patm'      ,         -1        ,'somslpre',    .true.     , .true. , 'yearly'  ,  ''      ,   ''     , ''

   cn_dir      = './'       !  root directory for the location of the bulk files
   rn_pref     = 101000.    !  reference atmospheric pressure   [N/m2]/
   ln_ref_apr  = .false.    !  ref. pressure: global mean Patm (T) or a constant (F)
   ln_apr_obc  = .false.    !  inverse barometer added to OBC ssh data
/
!-----------------------------------------------------------------------
&namsbc_ssr    !   surface boundary condition : sea surface restoring
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_sst      = 'sst_data'  ,        24         ,  'sst'    ,    .false.   , .false., 'yearly'  , ''       , ''       , ''
   sn_sss      = 'sss_data'  ,        -1         ,  'sss'    ,    .true.    , .true. , 'yearly'  , ''       , ''       , ''

   cn_dir      = './'      !  root directory for the location of the runoff files
   nn_sstr     =     0     !  add a retroaction term in the surface heat       flux (=1) or not (=0)
   nn_sssr     =     2     !  add a damping     term in the surface freshwater flux (=2)
                           !  or to SSS only (=1) or no damping term (=0)
   rn_dqdt     =   -40.    !  magnitude of the retroaction on temperature   [W/m2/K]
   rn_deds     =  -166.67  !  magnitude of the damping on salinity   [mm/day]
   ln_sssr_bnd =   .true.  !  flag to bound erp term (associated with nn_sssr=2)
   rn_sssr_bnd =   4.e0    !  ABS(Max/Min) value of the damping erp term [mm/day]
/
!-----------------------------------------------------------------------
&namsbc_alb    !   albedo parameters
!-----------------------------------------------------------------------
   nn_ice_alb   =    1   !  parameterization of ice/snow albedo
                         !     0: Shine & Henderson-Sellers (JGR 1985), giving clear-sky albedo
                         !     1: "home made" based on Brandt et al. (JClim 2005) and Grenfell & Perovich (JGR 2004),
                         !        giving cloud-sky albedo
   rn_alb_sdry  =  0.85  !  dry snow albedo         : 0.80 (nn_ice_alb = 0); 0.85 (nn_ice_alb = 1); obs 0.85-0.87 (cloud-sky)
   rn_alb_smlt  =  0.75  !  melting snow albedo     : 0.65 ( '' )          ; 0.75 ( '' )          ; obs 0.72-0.82 ( '' )
   rn_alb_idry  =  0.60  !  dry ice albedo          : 0.72 ( '' )          ; 0.60 ( '' )          ; obs 0.54-0.65 ( '' )
   rn_alb_imlt  =  0.50  !  bare puddled ice albedo : 0.53 ( '' )          ; 0.50 ( '' )          ; obs 0.49-0.58 ( '' )
/
!-----------------------------------------------------------------------
&namberg       !   iceberg parameters
!-----------------------------------------------------------------------
      ln_icebergs              = .false.
      ln_bergdia               = .true.               ! Calculate budgets
      nn_verbose_level         = 1                    ! Turn on more verbose output if level > 0
      nn_verbose_write         = 15                   ! Timesteps between verbose messages
      nn_sample_rate           = 1                    ! Timesteps between sampling for trajectory storage
                                                      ! Initial mass required for an iceberg of each class
      rn_initial_mass          = 8.8e7, 4.1e8, 3.3e9, 1.8e10, 3.8e10, 7.5e10, 1.2e11, 2.2e11, 3.9e11, 7.4e11
                                                      ! Proportion of calving mass to apportion to each class
      rn_distribution          = 0.24, 0.12, 0.15, 0.18, 0.12, 0.07, 0.03, 0.03, 0.03, 0.02
                                                      ! Ratio between effective and real iceberg mass (non-dim)
                                                      ! i.e. number of icebergs represented at a point
      rn_mass_scaling          = 2000, 200, 50, 20, 10, 5, 2, 1, 1, 1
                                                      ! thickness of newly calved bergs (m)
      rn_initial_thickness     = 40., 67., 133., 175., 250., 250., 250., 250., 250., 250.
      rn_rho_bergs             = 850.                 ! Density of icebergs
      rn_LoW_ratio             = 1.5                  ! Initial ratio L/W for newly calved icebergs
      ln_operator_splitting    = .true.               ! Use first order operator splitting for thermodynamics
      rn_bits_erosion_fraction = 0.                   ! Fraction of erosion melt flux to divert to bergy bits
      rn_sicn_shift            = 0.                   ! Shift of sea-ice concn in erosion flux (0<sicn_shift<1)
      ln_passive_mode          = .false.              ! iceberg - ocean decoupling
      nn_test_icebergs         =  10                  ! Create test icebergs of this class (-1 = no)
                                                      ! Put a test iceberg at each gridpoint in box (lon1,lon2,lat1,lat2)
      rn_test_box              = 108.0,  116.0, -66.0, -58.0
      rn_speed_limit           = 0.                   ! CFL speed limit for a berg

!              ! file name ! frequency (hours) !   variable   ! time interp.   !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !           !  (if <0  months)  !     name     !   (logical)    !  (T/F ) ! 'monthly' ! filename ! pairing  ! filename      !
      sn_icb =  'calving' ,       -1           , 'calvingmask',  .true.        , .true.  , 'yearly'  , ''       , ''       , ''

      cn_dir = './'
/

!!======================================================================
!!               ***  Lateral boundary condition  ***
!!======================================================================
!!   namlbc        lateral momentum boundary condition
!!   namcla        cross land advection
!!   namagrif      agrif nested grid ( read by child model only )       ("key_agrif")
!!   nambdy        Unstructured open boundaries                         ("key_bdy")
!!   namtide       Tidal forcing at open boundaries                     ("key_bdy_tides")
!!======================================================================
!
!-----------------------------------------------------------------------
&namlbc        !   lateral momentum boundary condition
!-----------------------------------------------------------------------
   rn_shlat    =    2.     !  shlat = 0  !  0 < shlat < 2  !  shlat = 2  !  2 < shlat
                           !  free slip  !   partial slip  !   no slip   ! strong slip
   ln_vorlat   = .false.   !  consistency of vorticity boundary condition with analytical eqs.
/
!-----------------------------------------------------------------------
&namcla        !   cross land advection
!-----------------------------------------------------------------------
   nn_cla      =    0      !  advection between 2 ocean pts separates by land
/
!-----------------------------------------------------------------------
&namagrif      !  AGRIF zoom                                            ("key_agrif")
!-----------------------------------------------------------------------
   nn_cln_update =    3    !  baroclinic update frequency
   ln_spc_dyn    = .true.  !  use 0 as special value for dynamics
   rn_sponge_tra = 2880.   !  coefficient for tracer   sponge layer [m2/s]
   rn_sponge_dyn = 2880.   !  coefficient for dynamics sponge layer [m2/s]
/
!-----------------------------------------------------------------------
&nam_tide      !   tide parameters (#ifdef key_tide)
!-----------------------------------------------------------------------
   ln_tide_pot   = .true.   !  use tidal potential forcing
   ln_tide_ramp  = .false.  !
   rdttideramp   =    0.    !
   clname(1)     = 'DUMMY'  !  name of constituent - all tidal components must be set in namelist_cfg
/
!-----------------------------------------------------------------------
&nambdy        !  unstructured open boundaries                          ("key_bdy")
!-----------------------------------------------------------------------
    nb_bdy         = 0                    !  number of open boundary sets
    ln_coords_file = .true.               !  =T : read bdy coordinates from file
    cn_coords_file = 'coordinates.bdy.nc' !  bdy coordinates files
    ln_mask_file   = .false.              !  =T : read mask from file
    cn_mask_file   = ''                   !  name of mask file (if ln_mask_file=.TRUE.)
    cn_dyn2d       = 'none'               !
    nn_dyn2d_dta   =  0                   !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
                                          !  = 2, use tidal harmonic forcing data from files
                                          !  = 3, use external data AND tidal harmonic forcing
    cn_dyn3d      =  'none'               !
    nn_dyn3d_dta  =  0                    !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
    cn_tra        =  'none'               !
    nn_tra_dta    =  0                    !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
    cn_ice_lim      =  'none'             !
    nn_ice_lim_dta  =  0                  !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
    rn_ice_tem      = 270.                !  lim3 only: arbitrary temperature of incoming sea ice
    rn_ice_sal      = 10.                 !  lim3 only:      --   salinity           --
    rn_ice_age      = 30.                 !  lim3 only:      --   age                --

    ln_tra_dmp    =.false.                !  open boudaries conditions for tracers
    ln_dyn3d_dmp  =.false.                !  open boundary condition for baroclinic velocities
    rn_time_dmp   =  1.                   ! Damping time scale in days
    rn_time_dmp_out =  1.                 ! Outflow damping time scale
    nn_rimwidth   = 10                    !  width of the relaxation zone
    ln_vol        = .false.               !  total volume correction (see nn_volctl parameter)
    nn_volctl     = 1                     !  = 0, the total water flux across open boundaries is zero
/
!-----------------------------------------------------------------------
&nambdy_dta      !  open boundaries - external data           ("key_bdy")
!-----------------------------------------------------------------------
!              !  file name      ! frequency (hours) ! variable   ! time interp.   !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !                 !  (if <0  months)  !   name     !   (logical)    !  (T/F ) ! 'monthly' ! filename ! pairing  ! filename      !
   bn_ssh =     'amm12_bdyT_u2d' ,         24        , 'sossheig' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   bn_u2d =     'amm12_bdyU_u2d' ,         24        , 'vobtcrtx' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   bn_v2d =     'amm12_bdyV_u2d' ,         24        , 'vobtcrty' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   bn_u3d  =    'amm12_bdyU_u3d' ,         24        , 'vozocrtx' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   bn_v3d  =    'amm12_bdyV_u3d' ,         24        , 'vomecrty' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   bn_tem  =    'amm12_bdyT_tra' ,         24        , 'votemper' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   bn_sal  =    'amm12_bdyT_tra' ,         24        , 'vosaline' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
! for lim2
!   bn_frld  =    'amm12_bdyT_ice' ,         24        , 'ileadfra' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
!   bn_hicif =    'amm12_bdyT_ice' ,         24        , 'iicethic' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
!   bn_hsnif =    'amm12_bdyT_ice' ,         24        , 'isnowthi' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
! for lim3
!   bn_a_i  =    'amm12_bdyT_ice' ,         24        , 'ileadfra' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
!   bn_ht_i =    'amm12_bdyT_ice' ,         24        , 'iicethic' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
!   bn_ht_s =    'amm12_bdyT_ice' ,         24        , 'isnowthi' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''     , ''
   cn_dir  =    'bdydta/'
   ln_full_vel = .false.
/
!-----------------------------------------------------------------------
&nambdy_tide     ! tidal forcing at open boundaries
!-----------------------------------------------------------------------
   filtide          = 'bdydta/amm12_bdytide_'         !  file name root of tidal forcing files
   ln_bdytide_2ddta = .false.
   ln_bdytide_conj  = .false.
/
!!======================================================================
!!                 ***  Bottom boundary condition  ***
!!======================================================================
!!   nambfr        bottom friction
!!   nambbc        bottom temperature boundary condition
!!   nambbl        bottom boundary layer scheme                         ("key_trabbl")
!!======================================================================
!
!-----------------------------------------------------------------------
&nambfr        !   bottom friction
!-----------------------------------------------------------------------
   nn_bfr      =    1      !  type of bottom friction :   = 0 : free slip,  = 1 : linear friction
                           !                              = 2 : nonlinear friction
   rn_bfri1    =    4.e-4  !  bottom drag coefficient (linear case)
   rn_bfri2    =    1.e-3  !  bottom drag coefficient (non linear case). Minimum coeft if ln_loglayer=T
   rn_bfri2_max =   1.e-1  !  max. bottom drag coefficient (non linear case and ln_loglayer=T)
   rn_bfeb2    =    2.5e-3 !  bottom turbulent kinetic energy background  (m2/s2)
   rn_bfrz0    =    3.e-3  !  bottom roughness [m] if ln_loglayer=T
   ln_bfr2d    = .false.   !  horizontal variation of the bottom friction coef (read a 2D mask file )
   rn_bfrien   =    50.    !  local multiplying factor of bfr (ln_bfr2d=T)
   rn_tfri1    =    4.e-4  !  top drag coefficient (linear case)
   rn_tfri2    =    2.5e-3 !  top drag coefficient (non linear case). Minimum coeft if ln_loglayer=T
   rn_tfri2_max =   1.e-1  !  max. top drag coefficient (non linear case and ln_loglayer=T)
   rn_tfeb2    =    0.0    !  top turbulent kinetic energy background  (m2/s2)
   rn_tfrz0    =    3.e-3  !  top roughness [m] if ln_loglayer=T
   ln_tfr2d    = .false.   !  horizontal variation of the top friction coef (read a 2D mask file )
   rn_tfrien   =    50.    !  local multiplying factor of tfr (ln_tfr2d=T)

   ln_bfrimp   = .true.    !  implicit bottom friction (requires ln_zdfexp = .false. if true)
   ln_loglayer = .false.   !  logarithmic formulation (non linear case)
/
!-----------------------------------------------------------------------
&nambbc        !   bottom temperature boundary condition
!-----------------------------------------------------------------------
!              !                              !  (if <0  months)  !  
!              !  file name      ! frequency (hours) ! variable   ! time interp.   !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !                 !  (if <0  months)  !   name     !   (logical)    !  (T/F ) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_qgh      ='geothermal_heating.nc',  -12.  , 'heatflow'      ,   .false.      , .true.  , 'yearly'  , ''       , ''       , ''
   !
   cn_dir      = './'      !  root directory for the location of the runoff files
   ln_trabbc   = .true.    !  Apply a geothermal heating at the ocean bottom
   nn_geoflx   =    2      !  geothermal heat flux: = 0 no flux
                           !     = 1 constant flux
                           !     = 2 variable flux (read in geothermal_heating.nc in mW/m2)
   rn_geoflx_cst = 86.4e-3 !  Constant value of geothermal heat flux [W/m2]

/
!-----------------------------------------------------------------------
&nambbl        !   bottom boundary layer scheme
!-----------------------------------------------------------------------
   nn_bbl_ldf  =  1      !  diffusive bbl (=1)   or not (=0)
   nn_bbl_adv  =  0      !  advective bbl (=1/2) or not (=0)
   rn_ahtbbl   =  1000.  !  lateral mixing coefficient in the bbl  [m2/s]
   rn_gambbl   =  10.    !  advective bbl coefficient                 [s]
/

!!======================================================================
!!                        Tracer (T & S ) namelists
!!======================================================================
!!   nameos        equation of state
!!   namtra_adv    advection scheme
!!   namtra_adv_mle   mixed layer eddy param. (Fox-Kemper param.)
!!   namtra_ldf    lateral diffusion scheme
!!   namtra_dmp    T & S newtonian damping
!!======================================================================
!
!-----------------------------------------------------------------------
&nameos        !   ocean physical parameters
!-----------------------------------------------------------------------
   nn_eos      =  -1     !  type of equation of state and Brunt-Vaisala frequency
                                 !  =-1, TEOS-10
                                 !  = 0, EOS-80
                                 !  = 1, S-EOS   (simplified eos)
   ln_useCT    = .true.  ! use of Conservative Temp. ==> surface CT converted in Pot. Temp. in sbcssm
   !                             !
   !                     ! S-EOS coefficients :
   !                             !  rd(T,S,Z)*rau0 = -a0*(1+.5*lambda*dT+mu*Z+nu*dS)*dT+b0*dS
   rn_a0       =  1.6550e-1      !  thermal expension coefficient (nn_eos= 1)
   rn_b0       =  7.6554e-1      !  saline  expension coefficient (nn_eos= 1)
   rn_lambda1  =  5.9520e-2      !  cabbeling coeff in T^2  (=0 for linear eos)
   rn_lambda2  =  7.4914e-4      !  cabbeling coeff in S^2  (=0 for linear eos)
   rn_mu1      =  1.4970e-4      !  thermobaric coeff. in T (=0 for linear eos)
   rn_mu2      =  1.1090e-5      !  thermobaric coeff. in S (=0 for linear eos)
   rn_nu       =  2.4341e-3      !  cabbeling coeff in T*S  (=0 for linear eos)
/
!-----------------------------------------------------------------------
&namtra_adv    !   advection scheme for tracer
!-----------------------------------------------------------------------
   ln_traadv_cen2   =  .false.   !  2nd order centered scheme
   ln_traadv_tvd    =  .true.    !  TVD scheme
   ln_traadv_muscl  =  .false.   !  MUSCL scheme
   ln_traadv_muscl2 =  .false.   !  MUSCL2 scheme + cen2 at boundaries
   ln_traadv_ubs    =  .false.   !  UBS scheme
   ln_traadv_qck    =  .false.   !  QUICKEST scheme
   ln_traadv_msc_ups=  .false.   !  use upstream scheme within muscl
   ln_traadv_tvd_zts=  .false.  !  TVD scheme with sub-timestepping of vertical tracer advection
/
!-----------------------------------------------------------------------
&namtra_adv_mle !   mixed layer eddy parametrisation (Fox-Kemper param)
!-----------------------------------------------------------------------
   ln_mle    = .true.      ! (T) use the Mixed Layer Eddy (MLE) parameterisation
   rn_ce     = 0.06        ! magnitude of the MLE (typical value: 0.06 to 0.08)
   nn_mle    = 1           ! MLE type: =0 standard Fox-Kemper ; =1 new formulation
   rn_lf     = 5.e+3       ! typical scale of mixed layer front (meters)                      (case rn_mle=0)
   rn_time   = 172800.     ! time scale for mixing momentum across the mixed layer (seconds)  (case rn_mle=0)
   rn_lat    = 20.         ! reference latitude (degrees) of MLE coef.                        (case rn_mle=1)
   nn_mld_uv = 0           ! space interpolation of MLD at u- & v-pts (0=min,1=averaged,2=max)
   nn_conv   = 0           ! =1 no MLE in case of convection ; =0 always MLE
   rn_rho_c_mle  = 0.01    ! delta rho criterion used to calculate MLD for FK
/
!----------------------------------------------------------------------------------
&namtra_ldf    !   lateral diffusion scheme for tracers
!----------------------------------------------------------------------------------
   !                       !  Operator type:
   ln_traldf_lap    =  .true.   !  laplacian operator
   ln_traldf_bilap  =  .false.  !  bilaplacian operator
   !                       !  Direction of action:
   ln_traldf_level  =  .false.  !  iso-level
   ln_traldf_hor    =  .false.  !  horizontal (geopotential)   (needs "key_ldfslp" when ln_sco=T)
   ln_traldf_iso    =  .true.   !  iso-neutral                 (needs "key_ldfslp")
   !		       	   !  Griffies parameters              (all need "key_ldfslp")
   ln_traldf_grif   =  .false.  !  use griffies triads
   ln_traldf_gdia   =  .false.  !  output griffies eddy velocities
   ln_triad_iso     =  .false.  !  pure lateral mixing in ML
   ln_botmix_grif   =  .false.  !  lateral mixing on bottom
   !                       !  Coefficients
   ! Eddy-induced (GM) advection always used with Griffies; otherwise needs "key_traldf_eiv"
   ! Value rn_aeiv_0 is ignored unless = 0 with Held-Larichev spatially varying aeiv
   !                                  (key_traldf_c2d & key_traldf_eiv & key_orca_r2, _r1 or _r05)
   rn_aeiv_0        =  2000.    !  eddy induced velocity coefficient [m2/s]
   rn_aht_0         =  2000.    !  horizontal eddy diffusivity for tracers [m2/s]
   rn_ahtb_0        =     0.    !  background eddy diffusivity for ldf_iso [m2/s]
   !                                           (normally=0; not used with Griffies)
   rn_slpmax        =     0.01  !  slope limit
   rn_chsmag        =     1.    !  multiplicative factor in Smagorinsky diffusivity
   rn_smsh          =     1.    !  Smagorinsky diffusivity: = 0 - use only sheer
   rn_aht_m         =  2000.    !  upper limit or stability criteria for lateral eddy diffusivity (m2/s)
/
!-----------------------------------------------------------------------
&namtra_dmp    !   tracer: T & S newtonian damping
!-----------------------------------------------------------------------
   ln_tradmp   =  .true.   !  add a damping termn (T) or not (F)
   nn_zdmp     =    0      !  vertical   shape =0    damping throughout the water column
                           !                   =1 no damping in the mixing layer (kz  criteria)
                           !                   =2 no damping in the mixed  layer (rho crieria)
   cn_resto    = 'resto.nc' ! Name of file containing restoration coefficient field (use dmp_tools to create this)
/

!!======================================================================
!!                      ***  Dynamics namelists  ***
!!======================================================================
!!   namdyn_adv    formulation of the momentum advection
!!   namdyn_vor    advection scheme
!!   namdyn_hpg    hydrostatic pressure gradient
!!   namdyn_spg    surface pressure gradient                            (CPP key only)
!!   namdyn_ldf    lateral diffusion scheme
!!======================================================================
!
!-----------------------------------------------------------------------
&namdyn_adv    !   formulation of the momentum advection
!-----------------------------------------------------------------------
   ln_dynadv_vec = .true.  !  vector form (T) or flux form (F)
   nn_dynkeg     = 0       ! scheme for grad(KE): =0   C2  ;  =1   Hollingsworth correction
   ln_dynadv_cen2= .false. !  flux form - 2nd order centered scheme
   ln_dynadv_ubs = .false. !  flux form - 3rd order UBS      scheme
   ln_dynzad_zts = .false. !  Use (T) sub timestepping for vertical momentum advection
/
!-----------------------------------------------------------------------
&nam_vvl    !   vertical coordinate options
!-----------------------------------------------------------------------
   ln_vvl_zstar  = .true.           !  zstar vertical coordinate
   ln_vvl_ztilde = .false.          !  ztilde vertical coordinate: only high frequency variations
   ln_vvl_layer  = .false.          !  full layer vertical coordinate
   ln_vvl_ztilde_as_zstar = .false. !  ztilde vertical coordinate emulating zstar
   ln_vvl_zstar_at_eqtor = .false.  !  ztilde near the equator
   rn_ahe3       = 0.0e0            !  thickness diffusion coefficient
   rn_rst_e3t    = 30.e0            !  ztilde to zstar restoration timescale [days]
   rn_lf_cutoff  = 5.0e0            !  cutoff frequency for low-pass filter  [days]
   rn_zdef_max   = 0.9e0            !  maximum fractional e3t deformation
   ln_vvl_dbg    = .true.           !  debug prints    (T/F)
/
!-----------------------------------------------------------------------
&namdyn_vor    !   option of physics/algorithm (not control by CPP keys)
!-----------------------------------------------------------------------
   ln_dynvor_ene = .false. !  enstrophy conserving scheme
   ln_dynvor_ens = .false. !  energy conserving scheme
   ln_dynvor_mix = .false. !  mixed scheme
   ln_dynvor_een = .true.  !  energy & enstrophy scheme
   ln_dynvor_een_old = .false.  !  energy & enstrophy scheme - original formulation
/
!-----------------------------------------------------------------------
&namdyn_hpg    !   Hydrostatic pressure gradient option
!-----------------------------------------------------------------------
   ln_hpg_zco  = .false.   !  z-coordinate - full steps
   ln_hpg_zps  = .true.    !  z-coordinate - partial steps (interpolation)
   ln_hpg_sco  = .false.   !  s-coordinate (standard jacobian formulation)
   ln_hpg_isf  = .false.   !  s-coordinate (sco ) adapted to isf
   ln_hpg_djc  = .false.   !  s-coordinate (Density Jacobian with Cubic polynomial)
   ln_hpg_prj  = .false.   !  s-coordinate (Pressure Jacobian scheme)
   ln_dynhpg_imp = .false. !  time stepping: semi-implicit time scheme  (T)
                                 !           centered      time scheme  (F)
/
!-----------------------------------------------------------------------
!namdyn_spg    !   surface pressure gradient   (CPP key only)
!-----------------------------------------------------------------------
!                          !  explicit free surface                     ("key_dynspg_exp")
!                          !  filtered free surface                     ("key_dynspg_flt")
!                          !  split-explicit free surface               ("key_dynspg_ts")

!-----------------------------------------------------------------------
&namdyn_ldf    !   lateral diffusion on momentum
!-----------------------------------------------------------------------
   !                       !  Type of the operator :
   ln_dynldf_lap    =  .true.   !  laplacian operator
   ln_dynldf_bilap  =  .false.  !  bilaplacian operator
   !                       !  Direction of action  :
   ln_dynldf_level  =  .false.  !  iso-level
   ln_dynldf_hor    =  .true.   !  horizontal (geopotential)            (require "key_ldfslp" in s-coord.)
   ln_dynldf_iso    =  .false.  !  iso-neutral                          (require "key_ldfslp")
   !                       !  Coefficient
   rn_ahm_0_lap     = 40000.    !  horizontal laplacian eddy viscosity   [m2/s]
   rn_ahmb_0        =     0.    !  background eddy viscosity for ldf_iso [m2/s]
   rn_ahm_0_blp     =     0.    !  horizontal bilaplacian eddy viscosity [m4/s]
   rn_cmsmag_1      =     3.    !  constant in laplacian Smagorinsky viscosity
   rn_cmsmag_2      =     3     !  constant in bilaplacian Smagorinsky viscosity
   rn_cmsh          =     1.    !  1 or 0 , if 0 -use only shear for Smagorinsky viscosity
   rn_ahm_m_blp     =    -1.e12 !  upper limit for bilap  abs(ahm) < min( dx^4/128rdt, rn_ahm_m_blp)
   rn_ahm_m_lap     = 40000.    !  upper limit for lap  ahm < min(dx^2/16rdt, rn_ahm_m_lap)
/

!!======================================================================
!!             Tracers & Dynamics vertical physics namelists
!!======================================================================
!!    namzdf            vertical physics
!!    namzdf_ric        richardson number dependent vertical mixing     ("key_zdfric")
!!    namzdf_tke        TKE dependent vertical mixing                   ("key_zdftke")
!!    namzdf_kpp        KPP dependent vertical mixing                   ("key_zdfkpp")
!!    namzdf_ddm        double diffusive mixing parameterization        ("key_zdfddm")
!!    namzdf_tmx        tidal mixing parameterization                   ("key_zdftmx")
!!    namzdf_tmx_new    new tidal mixing parameterization               ("key_zdftmx_new")
!!======================================================================
!
!-----------------------------------------------------------------------
&namzdf        !   vertical physics
!-----------------------------------------------------------------------
   rn_avm0     =   1.2e-4  !  vertical eddy viscosity   [m2/s]          (background Kz if not "key_zdfcst")
   rn_avt0     =   1.2e-5  !  vertical eddy diffusivity [m2/s]          (background Kz if not "key_zdfcst")
   nn_avb      =    0      !  profile for background avt & avm (=1) or not (=0)
   nn_havtb    =    0      !  horizontal shape for avtb (=1) or not (=0)
   ln_zdfevd   = .true.    !  enhanced vertical diffusion (evd) (T) or not (F)
   nn_evdm     =    0      !  evd apply on tracer (=0) or on tracer and momentum (=1)
   rn_avevd    =  100.     !  evd mixing coefficient [m2/s]
   ln_zdfnpc   = .false.   !  Non-Penetrative Convective algorithm (T) or not (F)
   nn_npc      =    1            !  frequency of application of npc
   nn_npcp     =  365            !  npc control print frequency
   ln_zdfexp   = .false.   !  time-stepping: split-explicit (T) or implicit (F) time stepping
   nn_zdfexp   =    3            !  number of sub-timestep for ln_zdfexp=T
/
!-----------------------------------------------------------------------
&namzdf_ric    !   richardson number dependent vertical diffusion       ("key_zdfric" )
!-----------------------------------------------------------------------
   rn_avmri    = 100.e-4   !  maximum value of the vertical viscosity
   rn_alp      =   5.      !  coefficient of the parameterization
   nn_ric      =   2       !  coefficient of the parameterization
   rn_ekmfc    =   0.7     !  Factor in the Ekman depth Equation
   rn_mldmin   =   1.0     !  minimum allowable mixed-layer depth estimate (m)
   rn_mldmax   =1000.0     !  maximum allowable mixed-layer depth estimate (m)
   rn_wtmix    =  10.0     !  vertical eddy viscosity coeff [m2/s] in the mixed-layer
   rn_wvmix    =  10.0     !  vertical eddy diffusion coeff [m2/s] in the mixed-layer
   ln_mldw     = .true.    !  Flag to use or not the mized layer depth param.
/
!-----------------------------------------------------------------------
&namzdf_tke    !   turbulent eddy kinetic dependent vertical diffusion  ("key_zdftke")
!-----------------------------------------------------------------------
   rn_ediff    =   0.1     !  coef. for vertical eddy coef. (avt=rn_ediff*mxl*sqrt(e) )
   rn_ediss    =   0.7     !  coef. of the Kolmogoroff dissipation
   rn_ebb      =  67.83    !  coef. of the surface input of tke (=67.83 suggested when ln_mxl0=T)
   rn_emin     =   1.e-6   !  minimum value of tke [m2/s2]
   rn_emin0    =   1.e-4   !  surface minimum value of tke [m2/s2]
   rn_bshear   =   1.e-20  ! background shear (>0) currently a numerical threshold (do not change it)
   nn_mxl      =   2       !  mixing length: = 0 bounded by the distance to surface and bottom
                           !                 = 1 bounded by the local vertical scale factor
                           !                 = 2 first vertical derivative of mixing length bounded by 1
                           !                 = 3 as =2 with distinct disspipative an mixing length scale
   nn_pdl      =   1       !  Prandtl number function of richarson number (=1, avt=pdl(Ri)*avm) or not (=0, avt=avm)
   ln_mxl0     = .true.    !  surface mixing length scale = F(wind stress) (T) or not (F)
   rn_mxl0     =   0.04    !  surface  buoyancy lenght scale minimum value
   ln_lc       = .true.    !  Langmuir cell parameterisation (Axell 2002)
   rn_lc       =   0.15    !  coef. associated to Langmuir cells
   nn_etau     =   1       !  penetration of tke below the mixed layer (ML) due to internal & intertial waves
                           !        = 0 no penetration
                           !        = 1 add a tke source below the ML
                           !        = 2 add a tke source just at the base of the ML
                           !        = 3 as = 1 applied on HF part of the stress    ("key_oasis3")
   rn_efr      =   0.05    !  fraction of surface tke value which penetrates below the ML (nn_etau=1 or 2)
   nn_htau     =   1       !  type of exponential decrease of tke penetration below the ML
                           !        = 0  constant 10 m length scale
                           !        = 1  0.5m at the equator to 30m poleward of 40 degrees
/
!------------------------------------------------------------------------
&namzdf_kpp    !   K-Profile Parameterization dependent vertical mixing  ("key_zdfkpp", and optionally:
!------------------------------------------------------------------------ "key_kppcustom" or "key_kpplktb")
   ln_kpprimix = .true.    !  shear instability mixing
   rn_difmiw   =  1.0e-04  !  constant internal wave viscosity [m2/s]
   rn_difsiw   =  0.1e-04  !  constant internal wave diffusivity [m2/s]
   rn_riinfty  =  0.8      !  local Richardson Number limit for shear instability
   rn_difri    =  0.0050   !  maximum shear mixing at Rig = 0    [m2/s]
   rn_bvsqcon  = -0.01e-07 !  Brunt-Vaisala squared for maximum convection [1/s2]
   rn_difcon   =  1.       !  maximum mixing in interior convection [m2/s]
   nn_avb      =  0        !  horizontal averaged (=1) or not (=0) on avt and amv
   nn_ave      =  1        !  constant (=0) or profile (=1) background on avt
/
!-----------------------------------------------------------------------
&namzdf_gls                !   GLS vertical diffusion                   ("key_zdfgls")
!-----------------------------------------------------------------------
   rn_emin       = 1.e-7   !  minimum value of e   [m2/s2]
   rn_epsmin     = 1.e-12  !  minimum value of eps [m2/s3]
   ln_length_lim = .true.  !  limit on the dissipation rate under stable stratification (Galperin et al., 1988)
   rn_clim_galp  = 0.267   !  galperin limit
   ln_sigpsi     = .true.  !  Activate or not Burchard 2001 mods on psi schmidt number in the wb case
   rn_crban      = 100.    !  Craig and Banner 1994 constant for wb tke flux
   rn_charn      = 70000.  !  Charnock constant for wb induced roughness length
   rn_hsro       =  0.02   !  Minimum surface roughness
   rn_frac_hs    =   1.3   !  Fraction of wave height as roughness (if nn_z0_met=2)
   nn_z0_met     =     2   !  Method for surface roughness computation (0/1/2)
   nn_bc_surf    =     1   !  surface condition (0/1=Dir/Neum)
   nn_bc_bot     =     1   !  bottom condition (0/1=Dir/Neum)
   nn_stab_func  =     2   !  stability function (0=Galp, 1= KC94, 2=CanutoA, 3=CanutoB)
   nn_clos       =     1   !  predefined closure type (0=MY82, 1=k-eps, 2=k-w, 3=Gen)
/
!-----------------------------------------------------------------------
&namzdf_ddm    !   double diffusive mixing parameterization             ("key_zdfddm")
!-----------------------------------------------------------------------
   rn_avts     = 1.e-4     !  maximum avs (vertical mixing on salinity)
   rn_hsbfr    = 1.6       !  heat/salt buoyancy flux ratio
/
!-----------------------------------------------------------------------
&namzdf_tmx    !   tidal mixing parameterization                        ("key_zdftmx")
!-----------------------------------------------------------------------
   rn_htmx     = 500.      !  vertical decay scale for turbulence (meters)
   rn_n2min    = 1.e-8     !  threshold of the Brunt-Vaisala frequency (s-1)
   rn_tfe      = 0.333     !  tidal dissipation efficiency
   rn_me       = 0.2       !  mixing efficiency
   ln_tmx_itf  = .true.    !  ITF specific parameterisation
   rn_tfe_itf  = 1.        !  ITF tidal dissipation efficiency
/
!-----------------------------------------------------------------------
&namzdf_tmx_new    !   new tidal mixing parameterization                ("key_zdftmx_new")
!-----------------------------------------------------------------------
   nn_zpyc     = 1         !  pycnocline-intensified dissipation scales as N (=1) or N^2 (=2)
   ln_mevar    = .true.    !  variable (T) or constant (F) mixing efficiency
   ln_tsdiff   = .true.    !  account for differential T/S mixing (T) or not (F)
/
!!======================================================================
!!                  ***  Miscellaneous namelists  ***
!!======================================================================
!!   namsol            elliptic solver / island / free surface
!!   nammpp            Massively Parallel Processing                    ("key_mpp_mpi)
!!   namctl            Control prints & Benchmark
!!   namc1d            1D configuration options                         ("key_c1d")
!!   namc1d_uvd        data: U & V currents                             ("key_c1d")
!!   namc1d_dyndmp     U & V newtonian damping                          ("key_c1d")
!!   namsto            Stochastic parametrization of EOS
!!======================================================================
!
!-----------------------------------------------------------------------
&namsol        !   elliptic solver / island / free surface
!-----------------------------------------------------------------------
   nn_solv     =      1    !  elliptic solver: =1 preconditioned conjugate gradient (pcg)
                           !                   =2 successive-over-relaxation (sor)
   nn_sol_arp  =      0    !  absolute/relative (0/1) precision convergence test
   rn_eps      =  1.e-6    !  absolute precision of the solver
   nn_nmin     =    300    !  minimum of iterations for the SOR solver
   nn_nmax     =    800    !  maximum of iterations for the SOR solver
   nn_nmod     =     10    !  frequency of test for the SOR solver
   rn_resmax   =  1.e-10   !  absolute precision for the SOR solver
   rn_sor      =  1.92     !  optimal coefficient for SOR solver (to be adjusted with the domain)
/
!-----------------------------------------------------------------------
&nammpp        !   Massively Parallel Processing                        ("key_mpp_mpi)
!-----------------------------------------------------------------------
   cn_mpi_send =  'I'      !  mpi send/recieve type   ='S', 'B', or 'I' for standard send,
                           !  buffer blocking send or immediate non-blocking sends, resp.
   nn_buffer   =   0       !  size in bytes of exported buffer ('B' case), 0 no exportation
   ln_nnogather=  .false.  !  activate code to avoid mpi_allgather use at the northfold
   jpni        =   0       !  jpni   number of processors following i (set automatically if < 1)
   jpnj        =   0       !  jpnj   number of processors following j (set automatically if < 1)
   jpnij       =   0       !  jpnij  number of local domains (set automatically if < 1)
/
!-----------------------------------------------------------------------
&namctl        !   Control prints & Benchmark
!-----------------------------------------------------------------------
   ln_ctl      = .false.   !  trends control print (expensive!)
   nn_print    =    0      !  level of print (0 no extra print)
   nn_ictls    =    0      !  start i indice of control sum (use to compare mono versus
   nn_ictle    =    0      !  end   i indice of control sum        multi processor runs
   nn_jctls    =    0      !  start j indice of control               over a subdomain)
   nn_jctle    =    0      !  end   j indice of control
   nn_isplt    =    1      !  number of processors in i-direction
   nn_jsplt    =    1      !  number of processors in j-direction
   nn_bench    =    0      !  Bench mode (1/0): CAUTION use zero except for bench
                           !     (no physical validity of the results)
   nn_timing   =    0      !  timing by routine activated (=1) creates timing.output file, or not (=0)
/
!-----------------------------------------------------------------------
&namc1d_uvd    !   data: U & V currents                                 ("key_c1d")
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_ucur     = 'ucurrent'  ,         -1        ,'u_current',   .false.    , .true. , 'monthly' ,  ''      ,  'Ume'   , ''
   sn_vcur     = 'vcurrent'  ,         -1        ,'v_current',   .false.    , .true. , 'monthly' ,  ''      ,  'Vme'   , ''
!
   cn_dir        = './'    !  root directory for the location of the files
   ln_uvd_init   = .false. !  Initialisation of ocean U & V with U & V input data (T) or not (F)
   ln_uvd_dyndmp = .false. !  damping of ocean U & V toward U & V input data (T) or not (F)
/
!-----------------------------------------------------------------------
&namc1d_dyndmp !   U & V newtonian damping                              ("key_c1d")
!-----------------------------------------------------------------------
   ln_dyndmp   =  .false.  !  add a damping term (T) or not (F)
/
!-----------------------------------------------------------------------
&namsto       ! Stochastic parametrization of EOS
!-----------------------------------------------------------------------
   ln_rststo = .false.           ! start from mean parameter (F) or from restart file (T)
   ln_rstseed = .true.           ! read seed of RNG from restart file
   cn_storst_in  = "restart_sto" !  suffix of stochastic parameter restart file (input)
   cn_storst_out = "restart_sto" !  suffix of stochastic parameter restart file (output)

   ln_sto_eos = .false.          ! stochastic equation of state
   nn_sto_eos = 1                ! number of independent random walks
   rn_eos_stdxy = 1.4            ! random walk horz. standard deviation (in grid points)
   rn_eos_stdz  = 0.7            ! random walk vert. standard deviation (in grid points)
   rn_eos_tcor  = 1440.0         ! random walk time correlation (in timesteps)
   nn_eos_ord  = 1               ! order of autoregressive processes
   nn_eos_flt  = 0               ! passes of Laplacian filter
   rn_eos_lim  = 2.0             ! limitation factor (default = 3.0)
/

!!======================================================================
!!                  ***  Diagnostics namelists  ***
!!======================================================================
!!   namnc4       netcdf4 chunking and compression settings             ("key_netcdf4")
!!   namtrd       dynamics and/or tracer trends
!!   namptr       Poleward Transport Diagnostics
!!   namflo       float parameters                                      ("key_float")
!!   namhsb       Heat and salt budgets
!!======================================================================
!
!-----------------------------------------------------------------------
&namnc4        !   netcdf4 chunking and compression settings            ("key_netcdf4")
!-----------------------------------------------------------------------
   nn_nchunks_i=   4       !  number of chunks in i-dimension
   nn_nchunks_j=   4       !  number of chunks in j-dimension
   nn_nchunks_k=   31      !  number of chunks in k-dimension
                           !  setting nn_nchunks_k = jpk will give a chunk size of 1 in the vertical which
                           !  is optimal for postprocessing which works exclusively with horizontal slabs
   ln_nc4zip   = .true.    !  (T) use netcdf4 chunking and compression
                           !  (F) ignore chunking information and produce netcdf3-compatible files
/
!-----------------------------------------------------------------------
&namtrd        !   diagnostics on dynamics and/or tracer trends
!              !       and/or mixed-layer trends and/or barotropic vorticity
!-----------------------------------------------------------------------
   ln_glo_trd  = .false.   ! (T) global domain averaged diag for T, T^2, KE, and PE
   ln_dyn_trd  = .false.   ! (T) 3D momentum trend output
   ln_dyn_mxl  = .FALSE.   ! (T) 2D momentum trends averaged over the mixed layer (not coded yet)
   ln_vor_trd  = .FALSE.   ! (T) 2D barotropic vorticity trends (not coded yet)
   ln_KE_trd   = .false.   ! (T) 3D Kinetic   Energy     trends
   ln_PE_trd   = .false.   ! (T) 3D Potential Energy     trends
   ln_tra_trd  = .FALSE.   ! (T) 3D tracer trend output
   ln_tra_mxl  = .false.   ! (T) 2D tracer trends averaged over the mixed layer (not coded yet)
   nn_trd      = 365       !  print frequency (ln_glo_trd=T) (unit=time step)
/
!!gm   nn_ctls     =   0       !  control surface type in mixed-layer trends (0,1 or n<jpk)
!!gm   rn_ucf      =   1.      !  unit conversion factor (=1 -> /seconds ; =86400. -> /day)
!!gm   cn_trdrst_in      = "restart_mld"   ! suffix of ocean restart name (input)
!!gm   cn_trdrst_out     = "restart_mld"   ! suffix of ocean restart name (output)
!!gm   ln_trdmld_restart = .false.         !  restart for ML diagnostics
!!gm   ln_trdmld_instant = .false.         !  flag to diagnose trends of instantantaneous or mean ML T/S
!!gm
!-----------------------------------------------------------------------
&namflo       !   float parameters                                      ("key_float")
!-----------------------------------------------------------------------
   jpnfl         = 1          !  total number of floats during the run
   jpnnewflo     = 0          !  number of floats for the restart
   ln_rstflo     = .false.    !  float restart (T) or not (F)
   nn_writefl    =      75    !  frequency of writing in float output file
   nn_stockfl    =    5475    !  frequency of creation of the float restart file
   ln_argo       = .false.    !  Argo type floats (stay at the surface each 10 days)
   ln_flork4     = .false.    !  trajectories computed with a 4th order Runge-Kutta (T)
                              !  or computed with Blanke' scheme (F)
   ln_ariane     = .true.     !  Input with Ariane tool convention(T)
   ln_flo_ascii  = .true.     !  Output with Ariane tool netcdf convention(F) or ascii file (T)
/
!-----------------------------------------------------------------------
&namptr       !   Poleward Transport Diagnostic
!-----------------------------------------------------------------------
   ln_diaptr  = .false.    !  Poleward heat and salt transport (T) or not (F)
   ln_subbas  = .false.     !  Atlantic/Pacific/Indian basins computation (T) or not
/
!-----------------------------------------------------------------------
&namhsb       !  Heat and salt budgets
!-----------------------------------------------------------------------
   ln_diahsb  = .false.    !  check the heat and salt budgets (T) or not (F)
/
!-----------------------------------------------------------------------
&nam_diaharm   !   Harmonic analysis of tidal constituents ('key_diaharm')
!-----------------------------------------------------------------------
    nit000_han = 1         ! First time step used for harmonic analysis
    nitend_han = 75        ! Last time step used for harmonic analysis
    nstep_han  = 15        ! Time step frequency for harmonic analysis
    tname(1)   = 'M2'      ! Name of tidal constituents
    tname(2)   = 'K1'
/
!-----------------------------------------------------------------------
&namdct        ! transports through sections
!-----------------------------------------------------------------------
    nn_dct      = 15       !  time step frequency for transports computing
    nn_dctwri   = 15       !  time step frequency for transports writing
    nn_secdebug = 112      !      0 : no section to debug
                           !     -1 : debug all section
                           !  0 < n : debug section number n
/

!!======================================================================
!!            ***  Observation & Assimilation namelists ***
!!======================================================================
!!   namobs       observation and model comparison                      ('key_diaobs')
!!   nam_asminc   assimilation increments                               ('key_asminc')
!!======================================================================
!
!-----------------------------------------------------------------------
&namobs       !  observation usage switch                               ('key_diaobs')
!-----------------------------------------------------------------------
   ln_t3d     = .false.    ! Logical switch for T profile observations
   ln_s3d     = .false.    ! Logical switch for S profile observations
   ln_ena     = .false.    ! Logical switch for ENACT insitu data set
   ln_cor     = .false.    ! Logical switch for Coriolis insitu data set
   ln_profb   = .false.    ! Logical switch for feedback insitu data set
   ln_sla     = .false.    ! Logical switch for SLA observations
   ln_sladt   = .false.    ! Logical switch for AVISO SLA data
   ln_slafb   = .false.    ! Logical switch for feedback SLA data
   ln_ssh     = .false.    ! Logical switch for SSH observations
   ln_sst     = .false.    ! Logical switch for SST observations
   ln_reysst  = .false.    ! Logical switch for Reynolds observations
   ln_ghrsst  = .false.    ! Logical switch for GHRSST observations
   ln_sstfb   = .false.    ! Logical switch for feedback SST data
   ln_sss     = .false.    ! Logical switch for SSS observations
   ln_seaice  = .false.    ! Logical switch for Sea Ice observations
   ln_vel3d   = .false.    ! Logical switch for velocity observations
   ln_velavcur= .false     ! Logical switch for velocity daily av. cur.
   ln_velhrcur= .false     ! Logical switch for velocity high freq. cur.
   ln_velavadcp = .false.  ! Logical switch for velocity daily av. ADCP
   ln_velhradcp = .false.  ! Logical switch for velocity high freq. ADCP
   ln_velfb   = .false.    ! Logical switch for feedback velocity data
   ln_grid_global = .false. ! Global distribtion of observations
   ln_grid_search_lookup = .false. !  Logical switch for obs grid search w/lookup table
   grid_search_file = 'grid_search'  !  Grid search lookup file header
! All of the *files* variables below are arrays. Use namelist_cfg to add more files
   enactfiles = 'enact.nc' !  ENACT input observation file names (specify full array in namelist_cfg)
   coriofiles = 'corio.nc' !  Coriolis input observation file name
   profbfiles = 'profiles_01.nc' ! Profile feedback input observation file name
   ln_profb_enatim = .false !        Enact feedback input time setting switch
   slafilesact = 'sla_act.nc' !  Active SLA input observation file names
   slafilespas = 'sla_pass.nc' ! Passive SLA input observation file names
   slafbfiles = 'sla_01.nc' ! slafbfiles: Feedback SLA input observation file names
   sstfiles = 'ghrsst.nc'   ! GHRSST input observation file names
   sstfbfiles = 'sst_01.nc' ! Feedback SST input observation file names
   seaicefiles = 'seaice_01.nc' ! Sea Ice input observation file names
   velavcurfiles = 'velavcurfile.nc'  ! Vel. cur. daily av. input file name
   velhrcurfiles = 'velhrcurfile.nc'  ! Vel. cur. high freq. input file name
   velavadcpfiles = 'velavadcpfile.nc' ! Vel. ADCP daily av. input file name
   velhradcpfiles = 'velhradcpfile.nc' ! Vel. ADCP high freq. input file name
   velfbfiles = 'velfbfile.nc' ! Vel. feedback input observation file name
   dobsini = 20000101.000000  !  Initial date in window YYYYMMDD.HHMMSS
   dobsend = 20010101.000000  !  Final date in window YYYYMMDD.HHMMSS
   n1dint = 0  !               Type of vertical interpolation method
   n2dint = 0  !               Type of horizontal interpolation method
   ln_nea = .false.   !        Rejection of observations near land switch
   nmsshc     = 0     !        MSSH correction scheme
   mdtcorr = 1.61     !        MDT  correction
   mdtcutoff = 65.0   !        MDT cutoff for computed correction
   ln_altbias = .false.    ! Logical switch for alt bias
   ln_ignmis  = .true.     ! Logical switch for ignoring missing files
   endailyavtypes = 820    ! ENACT daily average types - array (use namelist_cfg to set more values)
/
!-----------------------------------------------------------------------
&nam_asminc   !   assimilation increments                               ('key_asminc')
!-----------------------------------------------------------------------
    ln_bkgwri = .false.    !  Logical switch for writing out background state
    ln_trainc = .false.    !  Logical switch for applying tracer increments
    ln_dyninc = .false.    !  Logical switch for applying velocity increments
    ln_sshinc = .false.    !  Logical switch for applying SSH increments
    ln_asmdin = .false.    !  Logical switch for Direct Initialization (DI)
    ln_asmiau = .false.    !  Logical switch for Incremental Analysis Updating (IAU)
    nitbkg    = 0          !  Timestep of background in [0,nitend-nit000-1]
    nitdin    = 0          !  Timestep of background for DI in [0,nitend-nit000-1]
    nitiaustr = 1          !  Timestep of start of IAU interval in [0,nitend-nit000-1]
    nitiaufin = 15         !  Timestep of end of IAU interval in [0,nitend-nit000-1]
    niaufn    = 0          !  Type of IAU weighting function
    ln_salfix = .false.    !  Logical switch for ensuring that the sa > salfixmin
    salfixmin = -9999      !  Minimum salinity after applying the increments
    nn_divdmp = 0          !  Number of iterations of divergence damping operator
/
!-----------------------------------------------------------------------
&namsbc_wave   ! External fields from wave model
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable     ! time interp. !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name       !   (logical)  !  (T/F)  ! 'monthly' ! filename ! pairing  ! filename      !
   sn_cdg      =  'cdg_wave' ,        1          , 'drag_coeff' ,     .true.   , .false. , 'daily'   ,  ''      , ''       , ''
   sn_usd      =  'sdw_wave' ,        1          , 'u_sd2d'     ,     .true.   , .false. , 'daily'   ,  ''      , ''       , ''
   sn_vsd      =  'sdw_wave' ,        1          , 'v_sd2d'     ,     .true.   , .false. , 'daily'   ,  ''      , ''       , ''
   sn_wn       =  'sdw_wave' ,        1          , 'wave_num'   ,     .true.   , .false. , 'daily'   ,  ''      , ''       , ''
!
   cn_dir_cdg  = './'  !  root directory for the location of drag coefficient files
/
!-----------------------------------------------------------------------
&namdyn_nept  !   Neptune effect (simplified: lateral and vertical diffusions removed)
!-----------------------------------------------------------------------
   ! Suggested lengthscale values are those of Eby & Holloway (1994) for a coarse model
   ln_neptsimp       = .false.  ! yes/no use simplified neptune

   ln_smooth_neptvel = .false.  ! yes/no smooth zunep, zvnep
   rn_tslse          =  1.2e4   ! value of lengthscale L at the equator
   rn_tslsp          =  3.0e3   ! value of lengthscale L at the pole
   ! Specify whether to ramp down the Neptune velocity in shallow
   ! water, and if so the depth range controlling such ramping down
   ln_neptramp       = .true.   ! ramp down Neptune velocity in shallow water
   rn_htrmin         =  100.0   ! min. depth of transition range
   rn_htrmax         =  200.0   ! max. depth of transition range
/
