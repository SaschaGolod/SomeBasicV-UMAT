      subroutine vumat(
C Read only (unmodifiable)variables -
     1  nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal,
     2  stepTime, totalTime, dt, cmname, coordMp, charLength,
     3  props, density, strainInc, relSpinInc,
     4  tempOld, stretchOld, defgradOld, fieldOld,
     5  stressOld, stateOld, enerInternOld, enerInelasOld,
     6  tempNew, stretchNew, defgradNew, fieldNew,
C Write only (modifiable) variables -
     7  stressNew, stateNew, enerInternNew, enerInelasNew )
C
      include 'vaba_param.inc'
C
      dimension props(nprops), density(nblock), coordMp(nblock,*),
     1  charLength(nblock), strainInc(nblock,ndir+nshr),
     2  relSpinInc(nblock,nshr), tempOld(nblock),
     3  stretchOld(nblock,ndir+nshr),
     4  defgradOld(nblock,ndir+nshr+nshr),
     5  fieldOld(nblock,nfieldv), stressOld(nblock,ndir+nshr),
     6  stateOld(nblock,nstatev), enerInternOld(nblock),
     7  enerInelasOld(nblock), tempNew(nblock),
     8  stretchNew(nblock,ndir+nshr),
     8  defgradNew(nblock,ndir+nshr+nshr),
     9  fieldNew(nblock,nfieldv),
     1  stressNew(nblock,ndir+nshr), stateNew(nblock,nstatev),
     2  enerInternNew(nblock), enerInelasNew(nblock)
C
      character*80 cmname
      PARAMETER( ZERO = 0.D0, ONE = 1.D0, TWO = 2.D0, THREE = 3.D0,
     1THIRD = 1.D0/3.D0, HALF = .5D0, TWO_THIRDS = 2.D0/3.D0,
     2THREE_HALFS = 1.5D0 )
      
      REAL*8 E,XNU,YIELD,TWOMU,THREMU,SIXMU,ALAMDA,DMG,
     1TRACESTRESS,S2,S(6),
     2SIGMAMIS,PROPS,STATENEW,STATEOLD,TRACE,STRAININC,STRESSNEW,
     3 STRESSOLD
      integer I,J,km

C
C ----------------------------------------------------------------
C     UMAT FOR ISOTROPIC ELASTICITY
C     CANNOT BE USED FOR PLANE STRESS
C ----------------------------------------------------------------
C     PROPS(1) - E
C     PROPS(2) - NU
C ----------------------------------------------------------------
C

C     ELASTIC PROPERTIES 
      E      = PROPS(1)
      XNU    = PROPS(2)
      YIELD  = PROPS(3)
C
C     ELASTIC CONSTANTS
C
      TWOMU  = E / ( ONE + XNU )
      THREMU = THREE_HALFS * TWOMU
      SIXMU  = THREE * TWOMU
      ALAMDA = TWOMU * ( E - TWOMU ) / ( SIXMU - TWO * E )

      do 100 km = 1,nblock
      DMG=STATEOLD(km,1)

      IF( DMG .EQ. ONE ) THEN            

C
C     TRIAL STRESS
C
         TRACE = STRAININC (km,1)+ STRAININC (km,2) + STRAININC (km,3)
         STRESSNEW(km, 1)=STRESSOLD(km, 1) + ALAMDA*TRACE
     1   +               TWOMU*STRAININC(km,1)
         STRESSNEW(km, 2)=STRESSOLD(km, 2) + ALAMDA*TRACE
     1   +               TWOMU*STRAININC(km, 2)
         STRESSNEW(km, 3)=STRESSOLD(km, 3) + ALAMDA*TRACE
     1   +               TWOMU*STRAININC(km,3)
         STRESSNEW(km, 4)=STRESSOLD(km, 4)
     1   +               TWOMU*STRAININC(km, 4)
         STRESSNEW(km, 5)=STRESSOLD(km, 5)
     1   +               TWOMU*STRAININC(km, 5)
         STRESSNEW(km, 6)=STRESSOLD(km, 6)
     1   +               TWOMU*STRAININC(km, 6)
      TRACESTRESS=ZERO
      TRACESTRESS=STRESSNEW(km,1)+STRESSNEW(km,2)+STRESSNEW(km,3)    
C
C     DEFINING S MATRIX IN VOIGHT NOTATION     
      S2=ZERO
      S(1)=STRESSNEW(km, 1)-TRACESTRESS
      S(2)=STRESSNEW(km, 2)-TRACESTRESS
      S(3)=STRESSNEW(km, 3)-TRACESTRESS
      DO J=1,3
        S2=S2+(S(J)**TWO)
      END DO
      S(4)=STRESSNEW(km, 4)
      S(5)=STRESSNEW(km, 5)
      S(6)=STRESSNEW(km, 6)
      DO J=4,6
        S2=S2+(TWO*(S(J)**TWO))
      END DO  
      SIGMAMIS=SQRT((THREE/TWO)*S2)
      IF(SIGMAMIS>YIELD)THEN
      STATENEW(km,1)=0.D0
      ELSE
      STATENEW(km,1)=1.D0   
      END IF      
      ELSE 
      DO I=1,6
      STRESSNEW(km, I)=ZERO
      END DO    
      STATENEW(km,1)=0.D0
      END IF
  100 continue

      return
      end