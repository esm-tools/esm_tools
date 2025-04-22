&diag_list
ldiag_solver     =.false.
lcurt_stress_surf=.false.
ldiag_curl_vel3  =.false.
ldiag_Ri         =.false.
ldiag_turbflux   =.false.
ldiag_salt3D     =.false.
ldiag_dMOC       =.false.
ldiag_DVD        =.false.
ldiag_forc       =.false.
ldiag_extflds    =.false.
/

&nml_general
io_listsize    =100 !number of streams to allocate. shallbe large or equal to the number of streams in &nml_list
vec_autorotate =.true.
/

! for sea ice related variables use_ice should be true, otherewise there will be no output
! for 'curl_surf' to work lcurt_stress_surf must be .true. otherwise no output
! for 'fer_C', 'bolus_u', 'bolus_v', 'bolus_w', 'fer_K' to work Fer_GM must be .true. otherwise no output
! 'otracers' - all other tracers if applicable
! for 'dMOC' to work ldiag_dMOC must be .true. otherwise no output
&nml_list
io_list =  'ssh       ',1, 'm', 4,
           'uice      ',1, 'm', 4,
           'vice      ',1, 'm', 4,
           'a_ice     ',1, 'm', 4,
           'm_ice     ',1, 'm', 4,
           'm_snow    ',1, 'm', 4,
           'MLD3      ',1, 'm', 4,
           'tx_sur    ',1, 'm', 4,
           'ty_sur    ',1, 'm', 4,
           'temp      ',1, 'm', 4,
           'salt      ',1, 'm', 4,
           'u         ',1, 'm', 4,
           'v         ',1, 'm', 4,
           'w         ',1, 'm', 4,
           'N2        ',1, 'm', 4,
           'fw        ',1, 'm', 4,
           'fh        ',1, 'm', 4,
           'atmice_x  ',1, 'm', 4,
           'atmice_y  ',1, 'm', 4,
           'atmoce_x  ',1, 'm', 4,
           'atmoce_y  ',1, 'm', 4,
           'iceoce_x  ',1, 'm', 4,
           'iceoce_y  ',1, 'm', 4,
           'strength_ice ',1, 'm', 4,
           'thdgr     ',1, 'm', 4,
           'uwind     ',1, 'm', 4,
           'vwind     ',1, 'm', 4,
/
