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
vec_autorotate =.false.
/

! for sea ice related variables use_ice should be true, otherewise there will be no output
! for 'curl_surf' to work lcurt_stress_surf must be .true. otherwise no output
! for 'fer_C', 'bolus_u', 'bolus_v', 'bolus_w', 'fer_K' to work Fer_GM must be .true. otherwise no output
! 'otracers' - all other tracers if applicable
! for 'dMOC' to work ldiag_dMOC must be .true. otherwise no output
&nml_list
io_list =  'sst       ',1, 'd', 4,
           'sss       ',1, 'd', 4,
    	     'ssh       ',1, 'd', 4,
           'uice      ',1, 'd', 4,
           'vice      ',1, 'd', 4,
           'a_ice     ',1, 'd', 4,
           'm_ice     ',1, 'd', 4,
           'alb       ',1, 'd', 4,
           'ist       ',1, 'd', 4,
           'qsi       ',1, 'd', 4,
           'm_snow    ',1, 'm', 4,
           'MLD1      ',1, 'm', 4,
           'MLD2      ',1, 'm', 4,
           'MLD3      ',1, 'm', 4,
           'tx_sur    ',1, 'm', 4,
           'ty_sur    ',1, 'm', 4,
           'temp      ',1, 'm', 4,
           'salt      ',1, 'm', 4,
           'N2        ',1, 'm', 4,
           'u         ',1, 'm', 4,
           'v         ',1, 'm', 4,
           'unod      ',1, 'm', 4,
           'vnod      ',1, 'm', 4,
           'w         ',1, 'm', 4,
/
