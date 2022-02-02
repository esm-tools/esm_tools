PROGRAM calnoro
 
  WRITE(*,*) 'CALNORO CALLED'
 
  WRITE(*,*) 'Truncation ?'
  READ (*,*) IRES
  CALL calres(ires, nlat)
 
  nlon = 2*nlat
  WRITE (*,*) 'RES: ', IRES,' NLAT: ',nlat,' NLON: ',nlon
 
  CALL sso_par_fil(nlat, nlon)
 
END PROGRAM calnoro

SUBROUTINE calres (ires, nlat)
  IF (ires < 21 .OR. ires > 1000) THEN
    WRITE(0,*) 'calres: resolution out of range (res=',ires,')'
    STOP
  END IF
 
  nlat = NINT((ires*3.+1.)/2.)
  IF (MOD(nlat,2) > 0) THEN
    nlat = nlat + 1
    nlat2 = NINT(((ires+1)*3.+1.)/2.)
    IF (nlat == nlat2) THEN
      WRITE(0,*) 'calres: can not calculate number of latitudes for res ', ires
      STOP
    END IF
  END IF
!  WRITE (*,*) 'res: ', ires, ' nlat: ', nlat

  RETURN
END SUBROUTINE calres

SUBROUTINE gauaw (pa, pw, nlat)

  ! Description:
  !
  ! Compute abscissas and weights for gaussian integration.
  !
  ! Method:
  !
  IMPLICIT NONE

  !  Scalar arguments
  INTEGER nlat

  !  Array arguments
  REAL   pa(nlat), pw(nlat)
  ! *pa*  - array, length at least *k,* to receive abscis abscissas.
  ! *pw*  - array, length at least *k,* to receive weights.


  !  Local scalars:
  REAL, PARAMETER :: eps = EPSILON(0.0)
  REAL :: api
  INTEGER, PARAMETER :: itemax = 20

  INTEGER  iter, ins2, isym, jn, jgl
  REAL     za, zw, z, zan
  REAL     zk, zkm1, zkm2, zx, zxn, zldn, zmod

  !  Intrinsic functions
  INTRINSIC ABS, COS, MOD, TAN, ASIN

  !  Executable statements

  api = 2.*ASIN(1.)

  ins2 = nlat/2+MOD(nlat,2)

  ! Find first approximation of the roots of the
  ! Legendre polynomial of degree nlat

  DO jgl = 1, ins2
    z = REAL(4*jgl-1)*api/REAL(4*nlat+2)
    pa(jgl) = COS(z+1./(TAN(z)*REAL(8*nlat**2)))
  END DO

  ! Computes roots and weights
  ! Perform the Newton loop
  ! Find 0 of Legendre polynomial with Newton loop

  DO jgl = 1, ins2

    za = pa(jgl)

    DO iter = 1, itemax+1
      zk = 0.0

      ! Newton iteration step

      zkm2 = 1.0
      zkm1 = za
      zx = za
      DO jn = 2, nlat
        zk = (REAL(2*jn-1)*zx*zkm1-REAL(jn-1)*zkm2)/REAL(jn)
        zkm2 = zkm1
        zkm1 = zk
      END DO
      zkm1 = zkm2
      zldn = (REAL(nlat)*(zkm1-zx*zk))/(1.-zx*zx)
      zmod = -zk/zldn
      zxn = zx+zmod
      zan = zxn

      ! computes weight

      zkm2 = 1.0
      zkm1 = zxn
      zx = zxn
      DO jn = 2,nlat
        zk = (REAL(2*jn-1)*zx*zkm1-REAL(jn-1)*zkm2)/REAL(jn)
        zkm2 = zkm1
        zkm1 = zk
      END DO
      zkm1 = zkm2
      zw = (1.0-zx*zx)/(REAL(nlat*nlat)*zkm1*zkm1)
      za = zan
      IF (ABS(zmod) <= eps) EXIT
    END DO

    pa(jgl) = zan
    pw(jgl) = zw * 2.

  ENDDO

!DIR$ IVDEP
!OCL NOVREC
  DO jgl = 1, nlat/2
    isym = nlat-jgl+1
    pa(isym) = -pa(jgl)
    pw(isym) =  pw(jgl)
  ENDDO

END SUBROUTINE gauaw

SUBROUTINE sso_par_fil(nlat, nlon)

!  USE mo_orogwd
  IMPLICIT NONE

  INTEGER nlat, nlon

  INTEGER, PARAMETER :: iusn=720, jusn=360
  !INTEGER, PARAMETER :: iusn=1440, jusn=720
  !INTEGER, PARAMETER :: iusn=2160, jusn=1080

  INTEGER i, j
  INTEGER ihead(8)

  REAL zgw(nlat)
  REAL xusn(iusn),yusn(jusn),zusn(iusn,jusn)
  REAL xlon(nlon),ylat(nlat)
  REAL zphi(nlon,nlat),zmea(nlon,nlat),                                 &
       zstd(nlon,nlat),zsig(nlon,nlat),                                 &
       zgam(nlon,nlat),zthe(nlon,nlat),                                 &
       zpic(nlon,nlat),zval(nlon,nlat)
  INTEGER mask(nlon,nlat)
  REAL xpi, zmin, zmax


  !     INITIALISATION

  CALL gauaw(ylat, zgw, nlat)
  DO j=1,nlat
    ylat(j) = ASIN(ylat(j))/(8.*ATAN(1.))*360
!    write (*,*) j, ylat(j)
  ENDDO

  xpi=ACOS(-1.)
  DO i=1,nlon
    xlon(i)=-xpi+2.*(i-1)*xpi/REAL(nlon)
  ENDDO
  DO j=1,nlat
    ylat(j)=ylat(j)*xpi/180.
!    write (*,*) j, ylat(j)
  ENDDO

  DO i=1,iusn
    xusn(i)=-xpi+2.*(REAL(i)-0.5)*xpi/REAL(iusn)
  ENDDO
  DO j=1,jusn
    yusn(j)=xpi/2.-(REAL(j)-0.5)*xpi/REAL(jusn)
  ENDDO

!  OPEN(15,file='USN_FMT',form='formatted')
  CALL readfield ("OROMEA", zusn, iusn, jusn)
  !CALL readfield ("hmean", zusn, iusn, jusn)

  zmin= 10000.
  zmax=-10000.
  DO j=1,jusn
    DO i=1,iusn
!      READ(15,*) zusn(i,j)
      zmin=MIN(zmin,zusn(i,j))
      zmax=MAX(zmax,zusn(i,j))
    ENDDO
  ENDDO

  CLOSE(15)

  PRINT *,' usn min. et max.:',zmin,zmax

  CALL grid_noro(iusn,jusn,xusn,yusn,zusn,                  &
                 nlon,nlat,xlon,ylat,                       &
                 zphi,zmea,zstd,zsig,zgam,zthe,             &
                 zpic,zval,mask)


  CALL shift180(zmea, nlat, nlon)
  CALL shift180(zstd, nlat, nlon)
  CALL shift180(zsig, nlat, nlon)
  CALL shift180(zgam, nlat, nlon)
  CALL shift180(zthe, nlat, nlon)
  CALL shift180(zpic, nlat, nlon)
  CALL shift180(zval, nlat, nlon)

  OPEN(22,file='sso_par_fil.srv',form='unformatted')


  ihead(2) = 0
  ihead(3) = 0
  ihead(4) = 0
  ihead(5) = nlon
  ihead(6) = nlat
  ihead(7) = 0
  ihead(8) = 0

  ihead(1) = 51
  WRITE(22) ihead
  WRITE(22) zmea
  ihead(1) = 52
  WRITE(22) ihead
  WRITE(22) zstd
  ihead(1) = 53
  WRITE(22) ihead
  WRITE(22) zsig
  ihead(1) = 54
  WRITE(22) ihead
  WRITE(22) zgam
  ihead(1) = 55
  WRITE(22) ihead
  WRITE(22) zthe
  ihead(1) = 56
  WRITE(22) ihead
  WRITE(22) zpic
  ihead(1) = 57
  WRITE(22) ihead
  WRITE(22) zval

  STOP
END SUBROUTINE sso_par_fil

SUBROUTINE shift180(field, nlat, nlon)

  IMPLICIT NONE

  INTEGER nlat, nlon

  REAL field(nlon, nlat)
  REAL tmpfield(nlon, nlat)

  tmpfield(:,:) = field(:,:)

  field(1:nlon/2,:) = tmpfield(nlon/2+1:nlon,:)
  field(nlon/2+1:nlon,:) = tmpfield(1:nlon/2,:)

END SUBROUTINE shift180

SUBROUTINE NF_ErrorHandling(func, farg, status)

  CHARACTER (*) func, farg
  INTEGER status

  INCLUDE 'netcdf.inc'

  IF (status /= NF_NOERR) THEN
    WRITE (0,*) func, '(',farg,') : ', NF_STRERROR(status)
    STOP
  END IF

END SUBROUTINE NF_ErrorHandling

SUBROUTINE readfield (varname, zfield, nlon, nlat)

  USE netcdf

  IMPLICIT NONE

  INTEGER nlat, nlon
  REAL    zfield(nlon, nlat)
  REAL    ztmp(nlon)
  CHARACTER(*) varname
  INTEGER fileid, varid, status
  INTEGER jlat, nx, ny
  INTEGER start(2), count(2)
  CHARACTER(LEN=200)                :: dimname

  WRITE (*,*) "READFIELD CALLED:"
  WRITE (*,*) " nlat:", nlat, "  nlon: ", nlon
  WRITE (*,*) " varname: ", varname

  status = nf90_open("topodata.nc", nf90_nowrite, fileid)
  CALL NF_ErrorHandling("nf_open", "topodata.nc", status)

  status = nf90_inquire_dimension(fileid, 1, dimname, nx)
  status = nf90_inquire_dimension(fileid, 2, dimname, ny)
  WRITE (*,*) " ny  :", ny, "  nx  : ", nx
  IF(nlon /= nx.OR.nlat /= ny)THEN
    WRITE (*,*) ' nlon or nlat bad dimensions'
    STOP
  ENDIF

  status = nf90_inq_varid(fileid, varname,  varid)
  CALL NF_ErrorHandling("nf_inq_varid", varname, status)

  DO jlat = 1, nlat

    start(1) = 1
    start(2) = jlat
    count(1) = nlon
    count(2) = 1

    status = nf90_get_var(fileid, varid, ztmp, start, count)
    CALL NF_ErrorHandling("nf_get_vara_real", varname, status)
!    status = nf_get_vara_double(fileid, varid, start, count, ztmp)
!    CALL NF_ErrorHandling("nf_get_vara_double", varname, status)

    zfield(1:nlon/2,jlat) = ztmp(nlon/2+1:nlon)
    zfield(nlon/2+1:nlon,jlat) = ztmp(1:nlon/2)

  END DO

  status = nf90_close(fileid)
  CALL NF_ErrorHandling("nf_close", "topodata.nc", status)

END SUBROUTINE readfield
