! This is the namelist file for ice part

&ice_stress
Pstar = 30000.0			!20000. 27500. 30000.
c_pressure = 20.0		!20.0
/

&ice_fric
Cd_oce_ice = 5.5e-3    		!5.5e-3  3.0e-3
Kh_ice=0.0 
/

&ice_rheology
EVP_rheology=.true.
evp_rheol_steps=120	 
evp_Tdamp_ratio=3		!ratio dt/T_damp
vp_rheol_steps=500   
/

&ice_scheme
ice_gamma_fct=0.1
/

&ice_therm
Sice=4.0
h0=0.5
emiss_ice=0.97
emiss_wat=0.97
albsn=0.81  
albsnm=0.77   !0.77
albi=0.7
albim=0.68    !0.68
albw=0.1
/
