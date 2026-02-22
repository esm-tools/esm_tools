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
ldiag_extflds    =.true.
/

&nml_general
io_listsize    =100 !number of streams to allocate. shallbe large or equal to the number of streams in &nml_list
vec_autorotate =.true.
lnextGEMS=.false.
nlev_upper=31
/


# multio support atm: [m_ice, a_ice, m_snow, uice, vice, sss, sst, ssh, temp, salt, u, v, w]
&nml_list
io_list =  'ssh       ',1, 'd', 8, 
	   'sst       ',1, 'd', 8, 
           'sss       ',1, 'd', 8,
           !'MLD1      ',1, 'd', 8,
           'MLD2      ',1, 'd', 8,
           !'fw        ',1, 'd', 8,
           !'fh        ',1, 'd', 8,
           !'atmice_x  ',1, 'd', 8,
           !'atmice_y  ',1, 'd', 8,
           !'atmoce_x  ',1, 'd', 8,
           !'atmoce_y  ',1, 'd', 8,
           'a_ice     ',1, 'd', 8,
           'm_ice     ',1, 'd', 8,
           'uice      ',1, 'd', 8,
           'vice      ',1, 'd', 8,
           'u         ',1, 'd', 8,
           'v         ',1, 'd', 8,
           'w         ',1, 'd', 8,
           'temp      ',1, 'd', 8,
           'salt      ',1, 'd', 8,
           !'N2        ',1, 'h', 8,
           !'evap      ',1, 'd', 8,
           !'prec      ',1, 'd', 8,
           'm_snow    ',1, 'd', 8,
           !'runoff    ',1, 'm', 8,
           !'Kv        ',1, 'y', 8,
           !'Av        ',1, 'y', 8,
/
