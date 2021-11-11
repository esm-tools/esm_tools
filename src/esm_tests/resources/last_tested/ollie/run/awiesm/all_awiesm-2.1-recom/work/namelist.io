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
    io_list = 'sst       ', 1, 'y', 4, 'sss       ', 1, 'y', 4, 'a_ice     ',
              1, 'y', 4, 'm_ice     ', 1, 'y', 4, 'temp      ', 1, 'y',
              4, 'salt      ', 1, 'y', 4, 'otracers  ', 1, 'y', 4, 'dpCO2s    ',
              1, 'y', 4, 'pCO2s     ', 1, 'y', 4, 'CO2f      ', 1, 'y',
              4, 'Hp        ', 1, 'y', 4, 'aFe       ', 1, 'y', 4, 'aN        ',
              1, 'y', 4, 'denb      ', 1, 'y', 4, 'benN      ', 1, 'y',
              4, 'benC      ', 1, 'y', 4, 'benC_13      ', 1, 'y', 4, 'benC_14      ',
              1, 'y', 4, 'benSi     ', 1, 'y', 4, 'benCalc   ', 1, 'y',
              4, 'benCalc_13   ', 1, 'y', 4, 'benCalc_14   ', 1, 'y', 4,
              'GNAd      ', 1, 'y', 4, 'GNAn      ', 1, 'y', 4, 'NNAd      ',
              1, 'y', 4, 'NNAn      ', 1, 'y', 4, 'GPPd      ', 1, 'y',
              4, 'GPPn      ', 1, 'y', 4, 'NPPd      ', 1, 'y', 4, 'NPPn      ',
              1, 'y', 4, 'sinkPON   ', 1, 'y', 4, 'sinkPOC   ', 1, 'y',
              4, 'sinkOpal  ', 1, 'y', 4, 'sinkCalc  ', 1, 'y', 4, 'sinkC13   ',
              1, 'y', 4, 'sinkC14   ', 1, 'y', 4, 'sinkCal13 ', 1, 'y',
              4, 'sinkCal14 ', 1, 'y', 4
/
