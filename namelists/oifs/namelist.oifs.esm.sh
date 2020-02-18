# namelist.ifs.sh

# Set coupling frequencies for ocean and chemistry coupling
(( ${cpl_freq_atm_oce_sec:-} )) && NFRCO=$(( cpl_freq_atm_oce_sec / TIME_STEP_oifs )) || NFRCO=0
(( ${cpl_freq_atm_ctm_hrs:-} )) && NFRCO_CHEM=$(( cpl_freq_atm_ctm_hrs * 3600 / TIME_STEP_oifs )) || NFRCO_CHEM=0

# Switch off warm ocean parametrisation for coupled runs
(( NFRCO > 0 )) && LEOCWA=FALSE || LEOCWA=TRUE

# Switch on/off TM5 feedback to IFS
(( ${tm5_fdbck_o3:-}   )) && LTM5O3=TRUE  || LTM5O3=FALSE
(( ${tm5_fdbck_ch4:-}  )) && LTM5CH4=TRUE || LTM5CH4=FALSE
(( ${tm5_fdbck_aero:-} )) && LTM5AER=TRUE || LTM5AER=FALSE

# Switch on/off SPPT and set the ensemble member number (defaults to zero)
#general_has_config sppt && LSPSDT=TRUE || LSPSDT=FALSE
NENSFNB=${ifs_ensemble_forecast_number:-0}

cat << EOF
! -----------------------------------------------------------------------------
! *** NAMECECFG: EC-Earth specific configuration parameters
! -----------------------------------------------------------------------------
&NAMECECFG
    ECE_CPL_NEMO_LIM = ${ECE_CPL_NEMO_LIM},    ! Whether IFS is coupled to NEMO/LIM
/
&NAMAWICFG
    AWI_ESM3_CPL_FESOM = ${AWI_ESM3_CPL_FESOM}, ! Whether IFS is coupled to FESOM2
/
&NAMDIM
    NPROMA   = -32,
    NUNDEFLD =  1,
/
&NAMGFL
    YQ_NL%LGP     = true,
    YQ_NL%LSP     = false,
    YO3_NL%LGP    = false,
    YQ_NL%LGPINGP = true,
    YR_NL%NREQIN  = -1,
    YS_NL%NREQIN  = -1,
/
&NAMPAR0
    LSTATS          = true,
    LDETAILED_STATS = false,
    LSYNCSTATS      = false,

    MP_TYPE         = 2,
    MBX_SIZE        = 32000000,
    NPROC           = ${nproca_oifs},

    NOUTPUT         = 1,
 /   
&NAMCT0
    NFPOS=2,
    LREFOUT=false,
    N3DINI=0,
    NSTOP=$(( SECONDS_SINCE_INITIAL_oifs / TIME_STEP_oifs )),
    NFRDHP=1,
    NFRDHFD=1,
    NFRHIS=8,
    NFRPOS=8,
    NPOSTS=0,
    NHISTS=0,
    NFRSDI=1, 
    LSLAG=true,
    LSLPHY=.false.,
    LSLPHY=.true.,
    NFRMASSCON     = $(( 12 * 3600 / TIME_STEP_oifs ))
/    
&NAMPAR1
  LSPLIT=true,
  NFLDIN=0,
  NSTRIN=1,
  NSTROUT=0,
  NOUTTYPE=1,
  LPPTSF=false,
  NPPBUFLEN=100000,
/   
&NAMDYN
  TSTEP=${TIME_STEP_oifs}.0,
  LMASCOR=true,
  LMASDRY=true,
/
&NAEPHY
  LEPHYS=true,
  LERADI=true,
  LELAIV=false,
  LBUD23=false,        	        ! enable computation of physics tendencies
  LWCOU=false,                  ! true if wave model is to be run
  LWCOU2W=false,                ! true if two-way interaction with wave model
  NSTPW=1,                      ! frequency of call to wave model (timesteps)
/   
&NAERAD
  NRPROMA=-8,
  CRTABLEDIR='${WORK_DIR}/rtables/',
/   
&NAMCMIP
  LGHGCMIP5=.true., 		! read CMIP5 gas
  LGHGPI=.true., 		!pi-control run (keep GHG at 1850 levels)
/
&NAMGEM
   NHTYP=2,
/   
&NAMDPHY
! NVXTR2=0,
! NVEXTR=0,
! NCEXTR=0,
/      
&NAMAFN
  TFP_FUA(1)%LLGP=.false.,
/   
&NAMRES
  NFRRES         = 1,
  NRESTS         = -1,-$(( SECONDS_SINCE_INITIAL_oifs / 3600 )),
/
&NAMFPC
    CFPFMT ='MODEL',
    NFP3DFP =        5,
    MFP3DFP = 129,130,131,132,133,
    RFP3P   = 100000,92500,85000,70000,50000,40000,30000,20000,10000,5000,1000,
    NFP2DF = 1,
    MFP2DF = 152,
    NFPPHY  =       51,
    MFPPHY  = 031,032,033,034,035,039,040,041,042,078,079,136,137,139,141,142,143,144,146,147,151,164,165,166,167,168,169,170,175,176,177,178,179,180,181,182,183,186,187,188,205,208,209,210,211,212,235,236,238,243,174098,
/
&NAMPPC
       LRSACC=true,             ! reset accumulated fields to zero at model output frequency
&NAMMCC
    LMCCIEC        = FALSE,
    LMCCEC         = TRUE,
    LMCC04 	   = FALSE,
    LMCCDYNSEAICE  = FALSE,
/
&NAMIOS
    CFRCF          = "./rcf",
    CIOSPRF        = "./srf",
/
&NAMGRIB
    NJTKDATE       = ${START_DATE_oifs}
    NJTKOFFSETSTEP = $((SECONDS_SINCE_INITIAL_START_oifs/TIME_STEP_oifs))
/
&NAMORB
    LCORBMD =  $orb_switch_oifs,
    ORBMODE = '$orb_mode_oifs',
    ORBIY   =  $orb_iyear_oifs,
/
&NAEAER
/
&NAEPHY
/
&NAERAD
/
&NALBAR
/
&NALORI
/
&NAM_DISTRIBUTED_VECTORS
/
&NAM926
/
&NAMAFN
/
&NAMANA
/
&NAMARPHY
/
&NAMCA
/
&NAMCAPE
/
&NAMCFU
/
&NAMCHK
/
&NAMCHET
/
&NAMCLDP
    RVICE      = 0.13,
    NAERCLD    = 0,
/
&NAMCLTC
/
&NAMCOM
/
&NAMCOS
/
&NAMCTAN
/
&NAMCUMF
/
&NAMCUMFS
/
&NAMCT1
/
&NAMCVA
/
&NAMDDH
/
&NAMDFHD
/
&NAMDFI
/
&NAMDIF
/
&NAMDIM
/
&NAMDIMO
/
&NAMDMSP
/
&NAMDPHY
/
&NAMDYN
/
&NAMDYNA
/
&NAMDYNCORE
/
&NAMEMIS_CONF
/
&NAMENKF
/
&NAMFA
/
&NAMFFT
/
&NAMFPC
/
&NAMFPD
/
&NAMFPDY2
/
&NAMFPDYH
/
&NAMFPDYP
/
&NAMFPDYS
/
&NAMFPDYT
/
&NAMFPDYV
/
&NAMFPEZO
/
&NAMFPF
/
&NAMFPG
/
&NAMFPIOS
/
&NAMFPPHY
/
&NAMFPSC2
/
&NAMFPSC2_DEP
/
&NAMFY2
/
&NAMGEM
/
&NAMGFL
/
&NAMGMS
/
&NAMGOES
/
&NAMGOM
/
&NAMGWD
/
&NAMGWWMS
/
&NAMHLOPT
/
&NAMINI
/
&NAMIOMI
/
&NAMJBCODES
/
&NAMJFH
/
&NAMJG
/
&NAMJO
/
&NAMKAP
/
&NAMLCZ
/
&NAMLEG
/
&NAMLFI
/
&NAMMCC
/
&NAMMCUF
/
&NAMMETEOSAT
/
&NAMMTS
/
&NAMMTSAT
/
&NAMMTT
/
&NAMMUL
/
&NAMNMI
/
&NAMNASA
/
&NAMNN
/
&NAMNPROF
/
&NAMNUD
/
&NAMOBS
/
&NAMONEDVAR
/
&NAMOPH
/
&NAMOPTCMEM
/
&NAMPAR0
/
&NAMPARAR
/
&NAMPAR1
/
&NAMPHY
/
&NAMPHY0
/
&NAMPHY1
/
&NAMPHY2
/
&NAMPHY3
/
&NAMPHYDS
/
&NAMPONG
/
&NAMRAD15
/
&NAMRADCMEM
/
&NAMRCOEF
/
&NAMRES
/
&NAMRINC
/
&NAMRIP
/
&NAMSATS
/
&NAMSCC
/
&NAMSCEN
/
&NAMSCM
/
&NAMSENS
/
&NAMSIMPHL
/
&NAMSKF
/
&NAMSPSDT
/
&NAMSSMI
/
&NAMSTA
/
&NAMSTOPH
/
&NAMTCWV
/
&NAMTESTVAR
/
&NAMTOPH
/
&NAMTOVS
/
&NAMTRAJP
/
&NAMTRANS
/
&NAMTRM
/
&NAMVAR
/
&NAMVARBC
/
&NAMVARBC_AIREP
/
&NAMVARBC_ALLSKY
/
&NAMVARBC_GBRAD
/
&NAMVARBC_RAD
/
&NAMVARBC_SFCOBS
/
&NAMVARBC_TCWV
/
&NAMVARBC_TO3
/
&NAMVAREPS
/
&NAMVDOZ
/
&NAMVFP
/
&NAMVRTL
/
&NAMVV0
/
&NAMVV1
/
&NAMVV2
/
&NAMVWRK
/
&NAMWAVELETJB
/
&NAMXFU
/
&NAMZDI
/
&NAPHLC
/
&NAV1IS
/
&NAEPHLI
/
&NAMCVER
/
&NAMPPVI
/
&NAMSPNG
/
&NAMRLX
/
EOF
