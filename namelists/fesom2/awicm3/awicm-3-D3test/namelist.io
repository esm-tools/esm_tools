&diag_list
ldiag_solver     =.false.
lcurt_stress_surf=.false.
ldiag_curl_vel3  =.false.
ldiag_energy     =.false.
ldiag_salt3D     =.false.
ldiag_dMOC       =.true.
ldiag_DVD        =.false.
ldiag_forc       =.true.
/

&nml_listsize
io_listsize=100 !number of streams to allocate. shallbe large or equal to the number of streams in &nml_list
/

# for sea ice related variables use_ice should be true, otherewise there will be no output
# for 'curl_surf' to work lcurt_stress_surf must be .true. otherwise no output
# for 'fer_C', 'bolus_u', 'bolus_v', 'bolus_w', 'fer_K' to work Fer_GM must be .true. otherwise no output
# 'otracers' - all other tracers if applicable
# for 'dMOC' to work ldiag_dMOC must be .true. otherwise no output
&nml_list
io_list =  'sst       ',1, 'd', 4,
           'sss       ',1, 'd', 4,
    	   'ssh       ',1, 'd', 4,
           'vve_5     ',1, 'm', 4,
           'uice      ',1, 'm', 4,
           'vice      ',1, 'm', 4,
           'a_ice     ',1, 'm', 4,
           'm_ice     ',1, 'm', 4,
           'thdgr     ',1, 'm', 4,
           'thdgrsn   ',1, 'm', 4,
           'm_snow    ',1, 'm', 4,
           'MLD1      ',1, 'm', 4,
           'MLD2      ',1, 'm', 4,
           'fh        ',1, 'm', 4,
           'fw        ',1, 'm', 4,
           'atmice_x  ',1, 'm', 4,
           'atmice_y  ',1, 'm', 4,
           'atmoce_x  ',1, 'm', 4,
           'atmoce_y  ',1, 'm', 4,
           'alpha     ',1, 'm', 4,
           'beta      ',1, 'm', 4,
           'runoff    ',1, 'm', 4,
           'evap      ',1, 'm', 4,
           'subl      ',1, 'm', 4,
           'prec      ',1, 'm', 4,
           'swr      ',1, 'm', 4,
           'lwr      ',1, 'm', 4,
           'qsi       ',1, 'm', 4,
           'qso       ',1, 'm', 4,
           'alb       ',1, 'm', 4,
           'ist       ',1, 'm', 4,
           'hsn       ',1, 'm', 4,
           'tx_sur    ',1, 'm', 4,
           'ty_sur    ',1, 'm', 4,
           'curl_surf ',1, 'm', 4,
           'fer_C     ',1, 'm', 4,
           'temp      ',1, 'm', 4,
           'salt      ',1, 'm', 4,
           'otracers  ',1, 'y', 4,
           'slope_x   ',1, 'y', 4,
           'slope_y   ',1, 'y', 4,
           'slope_z   ',1, 'y', 4,
           'N2        ',1, 'y', 4,
           'Kv        ',1, 'y', 4,
           'u         ',1, 'y', 4,
           'v         ',1, 'y', 4,
           'w         ',1, 'y', 4,
           'Av        ',1, 'y', 4,
           'bolus_u   ',1, 'y', 4,
           'bolus_v   ',1, 'y', 4,
           'bolus_w   ',1, 'y', 4,
	   'kpp_obldepth',1, 'm', 4,
           'kpp_sbuoyflx',1, 'm', 4,
           'dMOC      ',1, 'y', 4,
/
