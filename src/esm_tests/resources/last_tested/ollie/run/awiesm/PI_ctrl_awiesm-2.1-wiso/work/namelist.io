&diag_list
    ldiag_solver = .false.
    lcurt_stress_surf = .false.
    ldiag_curl_vel3 = .false.
    ldiag_energy = .false.
    ldiag_salt3d = .false.
    ldiag_dmoc = .false.
    ldiag_dvd = .false.
    ldiag_forc = .false.
/

&nml_listsize
    io_listsize = 100
/

&nml_list
    io_list = 'sst       ', 1, 'm', 4, 'sss       ', 1, 'm', 4, 'ssh       ',
              1, 'm', 4, 'uice      ', 1, 'm', 4, 'vice      ', 1, 'm',
              4, 'a_ice     ', 1, 'm', 4, 'm_ice     ', 1, 'm', 4, 'm_snow    ',
              1, 'm', 4, 'MLD1      ', 1, 'm', 4, 'MLD2      ', 1, 'm',
              4, 'tx_sur    ', 1, 'm', 4, 'ty_sur    ', 1, 'm', 4, 'temp      ',
              1, 'y', 4, 'salt      ', 1, 'y', 4, 'N2        ', 1, 'y',
              4, 'Kv        ', 1, 'y', 4, 'u         ', 1, 'y', 4, 'v         ',
              1, 'y', 4, 'w         ', 1, 'y', 4, 'Av        ', 1, 'y',
              4, 'bolus_u   ', 1, 'y', 4, 'bolus_v   ', 1, 'y', 4, 'bolus_w   ',
              1, 'y', 4, 'h2o18     ', 1, 'm', 8, 'hDo16     ', 1, 'm',
              8, 'h2o16     ', 1, 'm', 8, 'h2o18_ice ', 1, 'm', 4, 'hDo16_ice ',
              1, 'm', 4, 'h2o16_ice ', 1, 'm', 4
/
