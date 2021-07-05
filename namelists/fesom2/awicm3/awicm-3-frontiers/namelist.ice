! Ice namelist
&ice_dyn
whichEVP=2             ! 0=standart; 1=mEVP; 2=aEVP
Pstar=30000.0          ! [N/m^2]
ellipse=2.0
c_pressure=20.0        ! ice concentration parameter used in ice strength computation
delta_min=1.0e-11      ! [s^(-1)]
evp_rheol_steps=120    ! number of EVP subcycles
alpha_evp=250          ! constant that control numerical stability of mEVP. Adjust with resolution. 
beta_evp=250           ! constant that control numerical stability of mEVP. Adjust with resolution.
c_aevp=0.15            ! a tuning constant in aEVP. Adjust with resolution.
Cd_oce_ice=0.0055      ! drag coef. oce - ice 
ice_gamma_fct=0.5      ! smoothing parameter
ice_diff=0.0           ! diffusion to stabilize
theta_io=0.0           ! rotation angle
ice_ave_steps=1        ! ice step=ice_ave_steps*oce_step
/

&ice_therm
Sice=4.0               ! Ice salinity 3.2--5.0 ppt.
h0=.5                  ! Lead closing parameter [m] 
emiss_ice=0.97         ! Emissivity of Snow/Ice,
emiss_wat=0.97         ! Emissivity of open water
! TCO159
albsn=0.77             ! Albedo: frozen snow  (boardband albedos must be lower for AWICM3 to account for lack for melt ponds etc.)
albsnm=0.65            !         melting snow
albi=0.65              !         frozen ice
albim=0.55             !         melting ice
albw=0.066             !         open water
! TL159
!albsn=0.72             ! Albedo: frozen snow  (boardband albedos must be lower for AWICM3 to account for lack for melt ponds etc.)
!albsnm=0.6             !         melting snow
!albi=0.55              !         frozen ice
!albim=0.45             !         melting ice
!albw=0.066             !         open water
con=2.1656             ! Thermal conductivities: ice; W/m/K
consn=0.31             !                         snow
/
