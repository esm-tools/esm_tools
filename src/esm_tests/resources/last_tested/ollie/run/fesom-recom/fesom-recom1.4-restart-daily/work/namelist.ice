&ice_stress
    pstar = 27500
    c_pressure = 15
/

&ice_fric
    cd_oce_ice = 0.01
    kh_ice = 0.0
/

&ice_rheology
    evp_rheology = .true.
    evp_rheol_steps = 300
    evp_tdamp_ratio = 3
    vp_rheol_steps = 500
/

&ice_scheme
    ice_gamma_fct = 0.05
/

&ice_therm
    sice = 5
    h0 = 0.6
    emiss_ice = 0.97
    emiss_wat = 0.97
    albsn = 0.81
    albsnm = 0.77
    albi = 0.7
    albim = 0.68
    albw = 0.1
/
