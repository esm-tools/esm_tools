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
ldiag_destine    =.false. ! enables computation of heatcontent. (hc300m, hc700m, hc) in io_list
ldiag_trflx      =.false.
ldiag_uvw_sqr    =.false.
ldiag_trgrd_xyz  =.false.

/

&nml_general
io_listsize    =120 !number of streams to allocate. shallbe large or equal to the number of streams in &nml_list
vec_autorotate =.false.
compression_level = 1
/

! for sea ice related variables use_ice should be true, otherewise there will be no output
! for 'curl_surf' to work lcurt_stress_surf must be .true. otherwise no output
! for 'fer_C', 'bolus_u', 'bolus_v', 'bolus_w', 'fer_K' to work Fer_GM must be .true. otherwise no output
! 'otracers' - all other tracers if applicable
! for 'dMOC' to work ldiag_dMOC must be .true. otherwise no output
! for 'utemp', 'vtemp', 'usalt', 'vsalt' output, set ldiag_trflx=.true.
&nml_list
io_list =  'sst       ',3, 'h', 4,
           'sss       ',3, 'h', 4,
    	   'ssh       ',3, 'h', 4,
           'uice      ',3, 'h', 4,
           'vice      ',3, 'h', 4,
           'a_ice     ',3, 'h', 4,
           'm_ice     ',3, 'h', 4,
           'm_snow    ',3, 'h', 4,
           'MLD1      ',3, 'h', 4,
           'MLD2      ',3, 'h', 4,
           'MLD3      ',3, 'h', 4,
           'tx_sur    ',3, 'h', 4,
           'ty_sur    ',3, 'h', 4,
           'temp      ',3, 'h', 4,
           'salt      ',3, 'h', 8,
           'N2        ',3, 'h', 4,
           'Kv        ',3, 'h', 4,
           'u         ',3, 'h', 4,
           'v         ',3, 'h', 4,
           'unod      ',3, 'h', 4,
           'vnod      ',3, 'h', 4,
           'w         ',3, 'h', 4,
           'Av        ',3, 'h', 4,
           'bolus_u   ',3, 'h', 4,
           'bolus_v   ',3, 'h', 4,
           'bolus_w   ',3, 'h', 4,
           'fw        ',3, 'h', 4,
           'fh        ',3, 'h', 4,
           'otracers  ',3, 'h', 4,
/
