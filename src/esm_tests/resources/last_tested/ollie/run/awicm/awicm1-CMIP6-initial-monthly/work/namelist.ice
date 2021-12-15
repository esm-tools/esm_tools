&ice_stress
    pstar = 30000.0
    c_pressure = 20.0
/

&ice_fric
    cd_oce_ice = 0.0055
    kh_ice = 0.0
/

&ice_rheology
    evp_rheology = .true.
    evp_rheol_steps = 120
    evp_tdamp_ratio = 3
    vp_rheol_steps = 500
/

&ice_scheme
    ice_gamma_fct = 0.1
/

&ice_therm
    sice = 4.0
    h0 = 0.5
    emiss_ice = 0.97
    emiss_wat = 0.97
    albsn = 0.81
    albsnm = 0.77
    albi = 0.7
    albim = 0.68
    albw = 0.1
/
