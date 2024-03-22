&diag_list
    ldiag_solver = .false.
    lcurt_stress_surf = .false.
    ldiag_curl_vel3 = .false.
    ldiag_ri = .false.
    ldiag_turbflux = .false.
    ldiag_salt3d = .false.
    ldiag_dmoc = .false.
    ldiag_dvd = .false.
    ldiag_forc = .false.
    ldiag_extflds = .false.
/

&nml_general
    io_listsize = 100
    vec_autorotate = .true.
/

&nml_list
    io_list = 
'sst       ', 1, 'm', 4, 
'sst       ', 1, 'd', 4, 
'sss       ', 1, 'd', 4, 
'ssh       ', 1, 'd', 4, 
'uice      ', 1, 'd', 4, 
'vice      ', 1, 'd', 4, 
'a_ice     ', 1, 'd', 4, 
'm_ice     ', 1, 'd', 4, 
'm_snow    ', 1, 'd', 4, 
'MLD1      ', 1, 'd', 4, 
'MLD2      ', 1, 'd', 4, 
'MLD3      ', 1, 'd', 4, 
'tx_sur    ', 1, 'd', 4, 
'ty_sur    ', 1, 'd', 4, 
'fw        ', 1, 'd', 4,
'momix_length',1,'d', 4,
'temp      ', 1, 'm', 4, 
'salt      ', 1, 'm', 8, 
'N2        ', 1, 'm', 4, 
'u         ', 1, 'm', 4, 
'v         ', 1, 'm', 4, 
'w         ', 1, 'm', 4, 
'rho       ', 1, 'm', 4
/
