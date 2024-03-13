SUBROUTINE grid_noro(imdep, jmdep, xdata, ydata, zdata,           &
     imar, jmar, x, y,                                    &
     zphi,zmea,zstd,zsig,zgam,zthe,                       &
     zpic,zval,mask)
  !=======================================================================
  ! (F. Lott) (voir aussi z.x. Li, A. Harzallah et L. Fairhead)
  !
  !      Compute the Parameters of the SSO scheme as described in
  !      LOTT & MILLER (1997) and LOTT(1999).
  !      Target points are on a imarxjmar grid.
  !      At the poles (if any) the fields value is repeated
  !      jmar time.
  !      The parameters a,b,c,d represent the limite of the target
  !      gridpoint region. The means over this region are calculated
  !      from USN data, ponderated by a weight proportional to the
  !      surface occupated by the data inside the model gridpoint area.
  !      In most circumstances, this weight is the ratio between the
  !      surface of the USN gridpoint area and the surface of the
  !      model gridpoint area.
  !
  !           (c)
  !        ----d-----
  !        | . . . .|
  !        |        |
  !     (b)a . * . .b(a)
  !        |        |
  !        | . . . .|
  !        ----c-----
  !           (d)
  !=======================================================================
  ! INPUT:
  !        imdep, jmdep: dimensions X and Y input field
  !        xdata, ydata: coordinates X and Y input field
  !        zdata: Input field
  !        In this version it is assumed that the entry data come from
  !        the USNavy dataset: imdep=iusn=2160, jmdep=jusn=1080.
  ! OUTPUT:
  !        imar, jmar: dimensions X and Y Output field
  !        x, y: ccordinates  X and Y Output field.
  !             zmea:  Mean orographie
  !             zstd:  Standard deviation
  !             zsig:  Slope
  !             zgam:  Anisotropy
  !             zthe:  Orientation of the small axis
  !             zpic:  Maximum altitude
  !             zval:  Minimum altitude
  !=======================================================================


  IMPLICIT INTEGER (i,j)
  IMPLICIT REAL(x,z)                                                

  PARAMETER(iext=216)          

  !#include "dimensions.h"


  REAL xusn(imdep+2*iext),yusn(jmdep+2)	
  REAL zusn(imdep+2*iext,jmdep+2)

  INTEGER imdep, jmdep
  REAL xdata(imdep),ydata(jmdep)
  REAL zdata(imdep,jmdep)
  !
  INTEGER imar, jmar

  ! INTERMEDIATE FIELDS  (CORRELATIONS OF OROGRAPHY GRADIENT)

  REAL ztz(imar,jmar),zxtzx(imar,jmar)
  REAL zytzy(imar,jmar),zxtzy(imar,jmar)
  REAL weight(imar,jmar)
  REAL num_tot(imar,jmar),num_lan(imar,jmar)

  ! CORRELATIONS OF USN OROGRAPHY GRADIENTS

  REAL zxtzxusn(imdep+2*iext,jmdep+2),zytzyusn(imdep+2*iext,jmdep+2)
  REAL zxtzyusn(imdep+2*iext,jmdep+2)
  REAL x(imar),y(jmar),zphi(imar,jmar)
  ! INPUT FIELDS
  REAL zmea(imar,jmar),zstd(imar,jmar)
  REAL zsig(imar,jmar),zgam(imar,jmar),zthe(imar,jmar)
  REAL zpic(imar,jmar),zval(imar,jmar)
  INTEGER mask(imar,jmar)
  !
  REAL a(imar),b(imar),c(jmar),d(jmar)
  INTEGER ia(imar),ib(imar),ic(jmar),id(jmar)
  !
  PRINT *,' Subgrid Scale Orography Parameters'
  xpi=ACOS(-1.)
  rad    = 6371229.
  zdeltay=2.*xpi/REAL(jmdep)*rad

  !      print *,'xdata:',xdata
  !      print *,'ydata:',ydata
  !      print *,'x:',x
  !      print *,'y:',y
  !
  !  EXTENSION OF THE USN DATABASE TO POCEED COMPUTATIONS AT
  !  BOUNDARIES:
  !

  CALL data_treat(zdata,xdata,ydata,imdep,jmdep)


  DO j=1,jmdep
    yusn(j+1)=ydata(j)
    DO i=1,imdep
      zusn(i+iext,j+1)=zdata(i,j)
      xusn(i+iext)=xdata(i)
    ENDDO
    DO i=1,iext
      zusn(i,j+1)=zdata(imdep-iext+i,j)
      xusn(i)=xdata(imdep-iext+i)-2.*xpi
      zusn(imdep+iext+i,j+1)=zdata(i,j)
      xusn(imdep+iext+i)=xdata(i)+2.*xpi
    ENDDO
  ENDDO

  yusn(1)=ydata(1)+(ydata(1)-ydata(2))
  yusn(jmdep+2)=ydata(jmdep)+(ydata(jmdep)-ydata(jmdep-1))
  DO i=1,imdep/2+iext
    zusn(i,1)=zusn(i+imdep/2,2)
    zusn(i+imdep/2+iext,1)=zusn(i,2)
    zusn(i,jmdep+2)=zusn(i+imdep/2,jmdep+1)
    zusn(i+imdep/2+iext,jmdep+2)=zusn(i,jmdep+1)
  ENDDO
  !
  ! COMPUTE LIMITS OF MODEL GRIDPOINT AREA
  !     ( REGULAR GRID)
  !
  a(1) = x(1) - (x(2)-x(1))/2.0
  b(1) = (x(1)+x(2))/2.0
  DO i = 2, imar-1
    a(i) = b(i-1)
    b(i) = (x(i)+x(i+1))/2.0
  ENDDO
  a(imar) = b(imar-1)
  b(imar) = x(imar) + (x(imar)-x(imar-1))/2.0

  IF(y(2) <= y(1))THEN
    c(1) = y(1) - (y(2)-y(1))/2.0
    d(1) = (y(1)+y(2))/2.0
    DO j = 2, jmar-1
      c(j) = d(j-1)
      d(j) = (y(j)+y(j+1))/2.0
    ENDDO
    c(jmar) = d(jmar-1)
    d(jmar) = y(jmar) + (y(jmar)-y(jmar-1))/2.0
  ELSE
    c(1) = (y(1)+y(2))/2.0
    d(1) =  y(1) - (y(2)-y(1))/2.0
    DO j = 2, jmar-1
      d(j) = c(j-1)
      c(j) = (y(j)+y(j+1))/2.0
    ENDDO
    d(jmar)=c(jmar-1)
    c(jmar) = y(jmar) + (y(jmar)-y(jmar-1))/2.0
  ENDIF

  DO ii=1,imar
    DO i=2,imdep+2*iext-1
      IF(a(ii) >= xusn(i-1).AND.a(ii) < xusn(i))ia(ii)=i-1
      IF(b(ii) > xusn(i).AND.b(ii) <= xusn(i+1))ib(ii)=i+1
    ENDDO
  ENDDO
  DO jj=1,jmar
    DO j=2,jmdep+1
      IF(c(jj) >= yusn(j).AND.c(jj) < yusn(j-1))ic(jj)=j-1
      IF(d(jj) > yusn(j+1).AND.d(jj) <= yusn(j))id(jj)=j+1
    ENDDO
  ENDDO

  !
  !  initialisations:
  !
  DO i = 1, imar
    DO j = 1, jmar
      weight(i,j) = 0.0
      zxtzx(i,j)  = 0.0
      zytzy(i,j)  = 0.0
      zxtzy(i,j)  = 0.0
      ztz(i,j)    = 0.0
      zmea(i,j)   = 0.0
      zpic(i,j)  =-1.e+10
      zval(i,j)  = 1.e+10
    ENDDO
  ENDDO
  !
  !  COMPUTE SLOPES CORRELATIONS ON USN GRID
  !
  DO j = 1,jmdep+2
    DO i = 1, imdep+2*iext
      zytzyusn(i,j)=0.0
      zxtzxusn(i,j)=0.0
      zxtzyusn(i,j)=0.0
    ENDDO
  ENDDO


  DO j = 2,jmdep+1
    zdeltax=zdeltay*COS(yusn(j))
    DO i = 2, imdep+2*iext-1
      zytzyusn(i,j)=(zusn(i,j+1)-zusn(i,j-1))**2/zdeltay**2
      zxtzxusn(i,j)=(zusn(i+1,j)-zusn(i-1,j))**2/zdeltax**2
      zxtzyusn(i,j)=(zusn(i,j+1)-zusn(i,j-1))/zdeltay             &
                   *(zusn(i+1,j)-zusn(i-1,j))/zdeltax
    ENDDO
  ENDDO
  !
  !  SUMMATION OVER GRIDPOINT AREA
  !
  zleny=xpi/REAL(jmdep)*rad
  xincr=xpi/2./REAL(jmdep)
  DO ii = 1, imar
    DO jj = 1, jmar
      num_tot(ii,jj)=0.
      num_lan(ii,jj)=0.
      !        PRINT *,' iteration ii jj:',ii,jj
      DO j = ic(jj),id(jj)
        zlenx=zleny*COS(yusn(j))
        zdeltax=zdeltay*COS(yusn(j))
        zbordnor=(c(jj)-yusn(j)+xincr)*rad
        zbordsud=(yusn(j)-d(jj)+xincr)*rad
        weighy=MAX(0.,MIN(zbordnor,zbordsud,zleny))
        DO i = ia(ii),ib(ii)
          zbordest=(xusn(i)-a(ii)+xincr)*rad*COS(yusn(j))
          zbordoue=(b(ii)+xincr-xusn(i))*rad*COS(yusn(j))
          weighx=MAX(0.,MIN(zbordest,zbordoue,zlenx))
          num_tot(ii,jj)=num_tot(ii,jj)+1.0
          IF(zusn(i,j) >= 1.)num_lan(ii,jj)=num_lan(ii,jj)+1.0
          weight(ii,jj)=weight(ii,jj)+weighx*weighy
          zxtzx(ii,jj)=zxtzx(ii,jj)+zxtzxusn(i,j)*weighx*weighy
          zytzy(ii,jj)=zytzy(ii,jj)+zytzyusn(i,j)*weighx*weighy
          zxtzy(ii,jj)=zxtzy(ii,jj)+zxtzyusn(i,j)*weighx*weighy
          ztz(ii,jj)  =ztz(ii,jj)  +zusn(i,j)*zusn(i,j)*weighx*weighy
          ! mean
          zmea(ii,jj) =zmea(ii,jj)+zusn(i,j)*weighx*weighy
          ! peacks
          zpic(ii,jj)=MAX(zpic(ii,jj),zusn(i,j))
          ! valleys
          zval(ii,jj)=MIN(zval(ii,jj),zusn(i,j))
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  !
  !  COMPUTE PARAMETERS NEEDED BY THE LOTT & MILLER (1997) AND
  !  LOTT (1999) SSO SCHEME.
  !
  zllmmea=0.
  zllmstd=0.
  zllmsig=0.
  zllmgam=0.
  zllmpic=0.
  zllmval=0.
  zllmthe=0.
  zminthe=0.
  !     print 100,' '
  !100  format(1X,A1,'II JJ',4X,'H',8X,'SD',8X,'SI',3X,'GA',3X,'TH')
  DO ii = 1, imar
    DO jj = 1, jmar
      IF (weight(ii,jj)  /=  0.0) THEN
        !  Mask
        IF(num_lan(ii,jj)/num_tot(ii,jj) >= 0.5)THEN
          mask(ii,jj)=1
        ELSE
          mask(ii,jj)=0
        ENDIF
        !  Mean Orography:
        zmea (ii,jj)=zmea (ii,jj)/weight(ii,jj)
        zxtzx(ii,jj)=zxtzx(ii,jj)/weight(ii,jj)
        zytzy(ii,jj)=zytzy(ii,jj)/weight(ii,jj)
        zxtzy(ii,jj)=zxtzy(ii,jj)/weight(ii,jj)
        ztz(ii,jj)  =ztz(ii,jj)/weight(ii,jj)
        !  Standard deviation:
        zstd(ii,jj)=SQRT(MAX(0.,ztz(ii,jj)-zmea(ii,jj)**2))
      ELSE
        PRINT*, 'probleme,ii,jj=', ii,jj
      ENDIF
    ENDDO
  ENDDO

  ! CORRECT VALUES OF HORIZONTAL SLOPES NEAR THE POLES:

  IF(y(jmar) <= -89.95.OR.y(jmar) >= 89.95)THEN

    DO ii = 1, imar
      zxtzx(ii,1)=zxtzx(ii,2)
      zxtzx(ii,jmar)=zxtzx(ii,jmar-1)
      zxtzy(ii,1)=zxtzy(ii,2)
      zxtzy(ii,jmar)=zxtzy(ii,jmar-1)
      zytzy(ii,1)=zytzy(ii,2)
      zytzy(ii,jmar)=zytzy(ii,jmar-1)
    ENDDO

  ENDIF

  !  FILTERS TO SMOOTH OUT FIELDS FOR INPUT INTO SSO SCHEME.

  !  FIRST FILTER, MOVING AVERAGE OVER 9 POINTS.

  CALL mva9(zmea,imar,jmar)
  CALL mva9(zstd,imar,jmar)
  CALL mva9(zpic,imar,jmar)
  CALL mva9(zval,imar,jmar)
  CALL mva9(zxtzx,imar,jmar)
  CALL mva9(zxtzy,imar,jmar)
  CALL mva9(zytzy,imar,jmar)

  !  SECOND FILTER FOR SLOPES, MASK AND UNIFORM HORIS RESOLUTION

  DO ii = 1, imar
    DO jj = 1, jmar
      zxtzx(ii,jj)=zxtzx(ii,jj)*mask(ii,jj)
      zxtzy(ii,jj)=zxtzy(ii,jj)*mask(ii,jj)
      zytzy(ii,jj)=zytzy(ii,jj)*mask(ii,jj)
    ENDDO
  ENDDO

  CALL uni_res(zxtzx,y,imar,jmar)
  CALL uni_res(zxtzy,y,imar,jmar)
  CALL uni_res(zytzy,y,imar,jmar)


  DO ii = 1, imar
    DO jj = 1, jmar
      IF (weight(ii,jj)  /=  0.0) THEN
        !  Coefficients K, L et M:
        xk=(zxtzx(ii,jj)+zytzy(ii,jj))/2.
        xl=(zxtzx(ii,jj)-zytzy(ii,jj))/2.
        xm=zxtzy(ii,jj)
        xp=xk-SQRT(xl**2+xm**2)
        xq=xk+SQRT(xl**2+xm**2)
        xw=1.e-8
        IF(xp <= xw) xp=0.
        IF(xq <= xw) xq=xw
        IF(ABS(xm) <= xw) xm=xw*SIGN(1.,xm)
        ! slope:
        zsig(ii,jj)=SQRT(xq)*mask(ii,jj)
        ! isotropy:
        zgam(ii,jj)=xp/xq*mask(ii,jj)
        ! angle theta:
        zthe(ii,jj)=57.29577951*ATAN2(xm,xl)/2.*mask(ii,jj)
        zphi(ii,jj)=zmea(ii,jj)*mask(ii,jj)
        zmea(ii,jj)=zmea(ii,jj)*mask(ii,jj)
        zpic(ii,jj)=zpic(ii,jj)*mask(ii,jj)
        zval(ii,jj)=zval(ii,jj)*mask(ii,jj)
        zstd(ii,jj)=zstd(ii,jj)*mask(ii,jj)

        !          print 101,ii,jj,
        !    *           zmea(ii,jj),zstd(ii,jj),zsig(ii,jj),zgam(ii,jj),
        !    *           zthe(ii,jj)
        !101  format(1x,2(1x,i2),2(1x,f7.1),1x,f7.4,2x,f4.2,1x,f5.1)
      ELSE
        !           PRINT*, 'probleme,ii,jj=', ii,jj
      ENDIF
      zllmmea=MAX(zmea(ii,jj),zllmmea)
      zllmstd=MAX(zstd(ii,jj),zllmstd)
      zllmsig=MAX(zsig(ii,jj),zllmsig)
      zllmgam=MAX(zgam(ii,jj),zllmgam)
      zllmthe=MAX(zthe(ii,jj),zllmthe)
      zminthe=MIN(zthe(ii,jj),zminthe)
      zllmpic=MAX(zpic(ii,jj),zllmpic)
      zllmval=MAX(zval(ii,jj),zllmval)
    ENDDO
  ENDDO

  PRINT *,'  MEAN ORO:  ',zllmmea
  PRINT *,'  ST. DEV.:  ',zllmstd
  PRINT *,'  PENTE:     ',zllmsig
  PRINT *,'  ANISOTROP: ',zllmgam
  PRINT *,'  ANGLE:     ',zminthe,zllmthe	
  PRINT *,'  pic:       ',zllmpic
  PRINT *,'  val:       ',zllmval

  ! ENSURE PERIODICITY AT HORIZONTAL GRID END

  IF(xdata(imar) >= xdata(1)+2.*xpi-0.001)THEN
    DO jj=1,jmar
      zmea(imar,jj)=zmea(1,jj)
      zpic(imar,jj)=zpic(1,jj)
      zval(imar,jj)=zval(1,jj)
      zstd(imar,jj)=zstd(1,jj)
      zsig(imar,jj)=zsig(1,jj)
      zgam(imar,jj)=zgam(1,jj)
      zthe(imar,jj)=zthe(1,jj)
    ENDDO
  ENDIF

  !
  ! VALUES AT THE POLE (IF THERE ARE POLES
  ! ON THE GRID CONSIDERED)
  ! gamma and theta a 1. and 0. at poles
  !
  IF(ydata(1) <= -xpi/2+0.01.OR.ydata(1) >= xpi/2-0.01)THEN
    zmeanor=0.0
    zmeasud=0.0
    zstdnor=0.0
    zstdsud=0.0
    zsignor=0.0
    zsigsud=0.0
    zweinor=0.0
    zweisud=0.0
    zpicnor=0.0
    zpicsud=0.0
    zvalnor=0.0
    zvalsud=0.0

    DO ii=1,imar
      zweinor=zweinor+              weight(ii,   1)
      zweisud=zweisud+              weight(ii,jmar)
      zmeanor=zmeanor+zmea(ii,   1)*weight(ii,   1)
      zmeasud=zmeasud+zmea(ii,jmar)*weight(ii,jmar)
      zstdnor=zstdnor+zstd(ii,   1)*weight(ii,   1)
      zstdsud=zstdsud+zstd(ii,jmar)*weight(ii,jmar)
      zsignor=zsignor+zsig(ii,   1)*weight(ii,   1)
      zsigsud=zsigsud+zsig(ii,jmar)*weight(ii,jmar)
      zpicnor=zpicnor+zpic(ii,   1)*weight(ii,   1)
      zpicsud=zpicsud+zpic(ii,jmar)*weight(ii,jmar)
      zvalnor=zvalnor+zval(ii,   1)*weight(ii,   1)
      zvalsud=zvalsud+zval(ii,jmar)*weight(ii,jmar)
    ENDDO

    DO ii=1,imar
      zmea(ii,   1)=zmeanor/zweinor
      zmea(ii,jmar)=zmeasud/zweisud
      zphi(ii,   1)=zmeanor/zweinor
      zphi(ii,jmar)=zmeasud/zweisud
      zpic(ii,   1)=zpicnor/zweinor
      zpic(ii,jmar)=zpicsud/zweisud
      zval(ii,   1)=zvalnor/zweinor
      zval(ii,jmar)=zvalsud/zweisud
      zstd(ii,   1)=zstdnor/zweinor
      zstd(ii,jmar)=zstdsud/zweisud
      zsig(ii,   1)=zsignor/zweinor
      zsig(ii,jmar)=zsigsud/zweisud
      zgam(ii,   1)=1.
      zgam(ii,jmar)=1.
      zthe(ii,   1)=0.
      zthe(ii,jmar)=0.
    ENDDO

  ENDIF

  RETURN
END SUBROUTINE grid_noro

SUBROUTINE mva9(x,imar,jmar)

  ! MAKE A MOVING AVERAGE OVER 9 GRIDPOINTS OF THE X FIELDS

  REAL x(imar,jmar),xf(imar,jmar)
  REAL weight(-1:1,-1:1)


  sum=0.
  DO is=-1,1
    DO js=-1,1
      weight(is,js)=1./REAL((1+is**2)*(1+js**2))
      sum=sum+weight(is,js)
    ENDDO
  ENDDO

  DO is=-1,1
    DO js=-1,1
      weight(is,js)=weight(is,js)/sum
    ENDDO
  ENDDO

  DO j=2,jmar-1
    DO i=2,imar-1
      xf(i,j)=0.
      DO is=-1,1
        DO js=-1,1
          xf(i,j)=xf(i,j)+x(i+is,j+js)*weight(is,js)
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  DO j=2,jmar-1
    xf(1,j)=0.
    is=imar-1
    DO js=-1,1
      xf(1,j)=xf(1,j)+x(is,j+js)*weight(-1,js)
    ENDDO
    DO is=0,1
      DO js=-1,1
        xf(1,j)=xf(1,j)+x(1+is,j+js)*weight(is,js)
      ENDDO
    ENDDO
    xf(imar,j)=xf(1,j)
  ENDDO

  DO i=1,imar
    xf(i,1)=xf(i,2)
    xf(i,jmar)=xf(i,jmar-1)
  ENDDO

  DO i=1,imar
    DO j=1,jmar
      x(i,j)=xf(i,j)
    ENDDO
  ENDDO

  RETURN
END SUBROUTINE mva9

SUBROUTINE uni_res(xfield,ylat,imar,jmar)

  ! MAKE A MOVING AVERAGE OVER LONGITUDE POINTS
  ! TO UNIFORMIZE THE HORIZONTAL RESOLUTION

  REAL xfield(imar,jmar),xfilt(-imar:2*imar,jmar)
  REAL ylat(jmar)


  DO j=1,jmar
    DO i=1,imar
      xfilt(i+imar,j) = xfield(i,j)
      xfilt(i-imar,j) = xfield(i,j)
      xfilt(i     ,j) = xfield(i,j)
    ENDDO
  ENDDO

  DO j=2,jmar-1
    ism=1./COS(ylat(j))
    !f77      ism=MIN(ism,imar-1)
    ism=MIN(ism,imar-1)
    DO i=1,imar
      xfield(i,j)=0.
      DO is=-ism,ism
        xfield(i,j)=xfield(i,j)+xfilt(i+is,j)/REAL(2*ism+1)
      ENDDO
    ENDDO
  ENDDO

  !  POLES TREATMENT FOR SLOPES

  IF(COS(ylat(1)) == 0)THEN
    DO i=1,imar
      xfield(i,1)   =0.
      DO ii=1,imar
        xfield(i,1)   =xfield(i,1)+xfilt(ii,1)/REAL(imar)
      ENDDO
    ENDDO
  ELSE
    ism=1./COS(ylat(1))
    !f77        ism=MIN(ism,imar-1)
    ism=MIN(ism,imar-1)
    DO i=1,imar
      xfield(i,1)   =0.
      DO is=-ism,ism
        xfield(i,1)=xfield(i,1)+xfilt(i+is,1)/REAL(2*ism+1)
      ENDDO
    ENDDO
  ENDIF

  IF(COS(ylat(jmar)) == 0)THEN
    DO i=1,imar
      xfield(i,jmar)=0.
      DO ii=1,imar
        xfield(i,jmar)=xfield(i,jmar)+xfilt(ii,jmar)/REAL(imar)
      ENDDO
    ENDDO
  ELSE
    ism=1./COS(ylat(jmar))
    !f77        ism=MIN(ism,imar-1)
    ism=MIN(ism,imar-1)
    DO i=1,imar
      xfield(i,jmar)   =0.
      DO is=-ism,ism
        xfield(i,jmar)=xfield(i,jmar)+xfilt(i+is,jmar)/REAL(2*ism+1)
      ENDDO
    ENDDO
  ENDIF

  RETURN
END SUBROUTINE uni_res

SUBROUTINE data_treat(zdata,xdata,ydata,imdep,jmdep)
  PARAMETER(iwork=3000,jwork=1500)
  REAL zdata(imdep,jmdep),xdata(imdep),ydata(jmdep)
  REAL zwork(-iwork:2*iwork,jwork),xwork(-iwork:2*iwork)
  REAL xscale(jwork)
  INTEGER iscale(jwork)

  IF(iwork < imdep.OR.jwork < jmdep) THEN
    WRITE (*,*) 'data_treat: iwork = ', iwork, '  imdep= ', imdep
    WRITE (*,*) '            jwork = ', jwork, '  jmdep= ', jmdep
    STOP 'iwork < imdep.OR.jwork < jmdep'
  END IF

  pi=ACOS(-1.)
  DO i=1,imdep
    xwork(i)=xdata(i)
    xwork(i-imdep)=xdata(i)-2.*pi
    xwork(i+imdep)=xdata(i)+2.*pi
    DO j=1,jmdep
      zwork(i      ,j)=zdata(i,j)
      zwork(i-imdep,j)=zdata(i,j)
      zwork(i+imdep,j)=zdata(i,j)
    ENDDO
  ENDDO

  xincr=(xdata(2)-xdata(1))/2.
  DO j=1,jmdep
    xscale(j)=1./ABS(COS(ydata(j)))*xincr
    !f77         iscale(j)=MIN(1./ABS(COS(ydata(j))),imdep)
    iscale(j)=1./ABS(COS(ydata(j)))
    iscale(j)=MIN(iscale(j),imdep)
    !        print *,j,iscale(j)
    DO i=1,imdep
      zdata(i,j)=0.
      weight=0.
      zlan=0.
      ztot=0.
      DO is=-iscale(j),iscale(j)
        weig = MIN(2.*xincr,                                      &
               MAX(xwork(i+is)+xincr-xwork(i)+xscale(j),0.),      &
               MAX(xwork(i)+xscale(j)-xwork(i+is)+xincr,0.))
        IF(weig > 0.)THEN
          ztot=ztot+1.
          IF(zwork(i+is,j) >= 1.)zlan=zlan+1.
          weight=weight+weig
          zdata(i,j)=zdata(i,j)+zwork(i+is,j)*weig
        ENDIF
      ENDDO
      IF(zlan/ztot >= 0.5)THEN
        zdata(i,j)=zdata(i,j)/weight
      ELSE
        zdata(i,j)=0.
      ENDIF
    ENDDO
  ENDDO


  RETURN
END SUBROUTINE data_treat
