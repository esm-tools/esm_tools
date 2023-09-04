&diag_list
ldiag_solver     =.false.
lcurt_stress_surf=.false.
ldiag_curl_vel3  =.false.
ldiag_energy     =.true.
ldiag_salt3D     =.false.
ldiag_dMOC       =.true.
ldiag_DVD        =.false.
ldiag_forc       =.true.
/

&nml_output_settings
    keep_nth_level = 1
/

&nml_listsize
io_listsize=100 !number of streams to allocate. shallbe large or equal to the number of streams in &nml_list
/

! for sea ice related variables use_ice should be true, otherewise there will be no output
! for 'curl_surf' to work lcurt_stress_surf must be .true. otherwise no output
! for 'fer_C', 'bolus_u', 'bolus_v', 'bolus_w', 'fer_K' to work Fer_GM must be .true. otherwise no output
! 'otracers' - all other tracers if applicable
! for 'dMOC' to work ldiag_dMOC must be .true. otherwise no output
&nml_list
io_list =  'sst       ',90,'s', 4,
           'sss       ',1, 'd', 4,
           'ssh       ',1, 'd', 4,
           'uice      ',1, 'd', 4,
           'vice      ',1, 'd', 4,
           'a_ice     ',1, 'd', 4,
           'm_ice     ',1, 'd', 4,
           'm_snow    ',1, 'd', 4,
           'MLD1      ',1, 'm', 4,
           'MLD2      ',1, 'm', 4,
           'tx_sur    ',1, 'm', 4,
           'ty_sur    ',1, 'm', 4,
           'temp      ',1, 'm', 4,
           'salt      ',1, 'm', 4,
           'N2        ',1, 'm', 4,
           'Kv        ',1, 'm', 4,
           'u         ',1, 'm', 4,
           'v         ',1, 'm', 4,
           'w         ',1, 'm', 4,
           'sigma0    ',1, 'm', 4,
           'temp1-31  ',1, 'd', 4,
           'salt1-31  ',1, 'd', 4,
           'u1-31     ',1, 'd', 4,
           'v1-31     ',1, 'd', 4,
           'w1-31     ',1, 'd', 4,
           'snow      ',1, 'm', 4,
           'subli     ',1, 'm', 4,
           'thdgrsn   ',1, 'm', 4,
           'thdgr     ',1, 'm', 4,
           'flice     ',1, 'm', 4,
           'ist       ',1, 'm', 4,
           'alb       ',1, 'm', 4,
           'fh       ',1, 'm', 4,
           'fw       ',1, 'm', 4,
/
