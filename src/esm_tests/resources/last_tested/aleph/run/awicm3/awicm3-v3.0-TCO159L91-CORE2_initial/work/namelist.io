&diag_list
    ldiag_solver = .false.
    lcurt_stress_surf = .false.
    ldiag_curl_vel3 = .false.
    ldiag_energy = .false.
    ldiag_salt3d = .false.
    ldiag_dmoc = .false.
    ldiag_dvd = .false.
    ldiag_forc = .true.
/

&nml_listsize
    io_listsize = 100
/

&nml_list
    io_list = 'sst', 1, 'd', 4
/
