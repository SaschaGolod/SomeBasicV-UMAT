      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4 JSTEP(4)
	
      REAL*8 EMOD,ENU,EBULK3,EG2,EG,EG3,ELAM,ONE,TWO,THREE
	  ONE=1.0
	  TWO=2.0
	  THREE=3.0
C ----------------------------------------------------------------
C      UMAT FOR ISOTROPIC ELASTICITY
C      CANNOT BE USED FOR PLANE STRESS
C ----------------------------------------------------------------
C      PROPS(1) - E
C      PROPS(2) - NU
C ----------------------------------------------------------------
C
      IF (NDI.NE.3) THEN
		print*, 'TEST: UMAT wird aufgerufen!'
	  WRITE (7, *) '***UMAT CALLED ERROR: THIS UMAT MAY ONLY BE USED'//
     & ' FOR ELEMENTS WITH THREE DIRECT STRESS COMPONENTS'
	  WRITE (1, *) 'test channel 1'
	  WRITE (2, *) 'test channel 2'
	  WRITE (3, *) 'test channel 3'
	  WRITE (4, *) 'test channel 4'
c	  WRITE (5, *) 'test channel 5' der channel ist nur zum einlesen!
	  WRITE (6, *) 'test channel 6'
	  WRITE (7, *) 'test channel 7'
	  WRITE (8, *) 'test channel 8'
	  WRITE (9, *) 'test channel 9'
	  WRITE (10, *) 'test channel 10'
	  WRITE (11, *) 'test channel 11'
	  WRITE (12, *) 'test channel 12'
	  WRITE (13, *) 'test channel 13'
	  WRITE (14, *) 'test channel 14'
	  WRITE (15, *) 'test channel 15'

      CALL PrintError(NDI)

c       CALL XIT
       ENDIF
C 
C      ELASTIC PROPERTIES
	  EMOD=PROPS(1)
	  ENU=PROPS(2)
	  EBULK3=EMOD/(ONE-TWO*ENU)
	  EG2=EMOD/(ONE+ENU)
	  EG=EG2/TWO
	  EG3=THREE*EG
	  ELAM=(EBULK3-EG2)/THREE
	  print*,'ELAM',ELAM
C     
C      ELASTIC STIFFNESS
C
	  DO K1=1,NDI
	  DO K2=1,NDI
	  DDSDDE(K2,K1)=ELAM
	  END DO
	  DDSDDE(K1,K1)=EG2+ELAM
	  END DO
	  DO K1=NDI+1, NTENS
	  DDSDDE(K1,K1)=EG
	  END DO
C
C      CALCULATE STRESS
C
	  DO K1=1, NTENS
	  DO K2=1, NTENS
	  STRESS(K2)=STRESS(K2)+DDSDDE(K2,K1)*DSTRAN(K1)
	  END DO
	  END DO
C
      RETURN
      END


	  SUBROUTINE PrintError(NDI)
		IMPLICIT NONE
	  
		INTEGER NDI              ! <--- bruauch ich nicht.
		INTEGER LOP, INTV(1)
		REAL*8 REALV(1)
		CHARACTER*8 CHARV(1)
		CHARACTER*500 STRING
	  
		INTV(1) = NDI
		REALV(1) = 0.0D0
		CHARV(1) = 'NDI'
	  
		LOP = -3
		STRING = 'Analysis is being terminated '//
     6	'from a user subroutine'// CHAR(10) //
     & ' ***UMAT CALLED ERROR: THIS UMAT MAY ONLY BE USED.'//
     & ' FOR ELEMENTS WITH THREE DIRECT STRESS COMPONENTS'
	  
		CALL STDB_ABQERR(LOP, STRING, INTV, REALV, CHARV)
	  
	  END SUBROUTINE PrintError
	  
	  