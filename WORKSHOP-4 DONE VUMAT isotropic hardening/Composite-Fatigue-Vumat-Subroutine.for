        
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
       REAL*8 C(4,4),
     1     EPSOLD(NBLOCK,4), EPSNEW(NBLOCK,4),E01,E02,G012,EF1,EF2,EF12,
     2     E1, E2, E3, G12, NU12, NU21,NU13,ALFA1,ALFA2,ALFA3,BETA1,
     3     BETA3,GAMA1,GAMA2,GAMA3,LANDA1,LANDA2,LANDA3,NU31,NU23,
     4     NU32,S,DSTRESS(NBLOCK,3),X,Y,SU,K,ALFA,R1(NBLOCK),R2(NBLOCK),
     5     R3(NBLOCK),DW,DW1,DW2,DW3,N,NF,NFF,X0,Y0,SU0,FACTOR,
     6     DE(NBLOCK,3),R,XT,XC,YT,YC,NFM,NFFT,NFFC,NFMT,NFMC,E11,E12,
     7     SU1,SU2,E21,E22,G121,G122
   
      Parameter(ZERO=0.D0 , ONE=1.D0 , TWO=2.D0)
     
     
      character*80 cmname

      INTEGER I,J
C      PRINT*,'STEPTIME',STEPTIME

c       IF (TOTALTIME==0.1)THEN
c        PRINT*,'E1**',E1
c      END IF             
C=================================

C............START PROGRAM........................        
      do 100 km = 1,nblock
      E1   =   PROPS(1)
      E2   =   PROPS(2)
      E3   =   PROPS(3)
      NU12 =   PROPS(4)
      NU23 =   PROPS(5)
      NU13 =   PROPS(6)
      G12 =    PROPS(7)
c       IF (TOTALTIME==0.1)THEN
c         PRINT*,'E1*',E1
c      END IF      
C================================
C================================
C================================        
      EF1  =   PROPS(8)
      EF2  =   PROPS(9)
      EF12 =   PROPS(10)
      ALFA1=   PROPS(11)
      ALFA2=   PROPS(12)
      ALFA3=   PROPS(13)
      GAMA1=   PROPS(14)
      GAMA2=   PROPS(15)
      GAMA3=   PROPS(16)
      BETA1=   PROPS(17)
      BETA2=   PROPS(18)
      BETA3=   PROPS(19)
      LANDA1=  PROPS(20)
      LANDA2=  PROPS(21)
      LANDA3=  PROPS(22)      
      XT    =  PROPS(23)
      XC    =  PROPS(24)
      YT    =  PROPS(25)
      YC    =  PROPS(26)
      SU   =   PROPS(27)
      K    =   PROPS(28)
      ALFA =   PROPS(29)
      FACTOR=  PROPS(30)

C=================================
      E01=E1
      E02=E2
      G012=G12 
      XT0=XT
      YT0=YT
      XC0=XC
      YC0=YC     
      SU0=SU        
C=================================    
C=================================            
      IF (TOTALTIME .GT. 0.1)THEN
      E1=STATEOLD(KM,5)
      E2=STATEOLD(KM,6)
      G12=STATEOLD(KM,7)
      XT=STATEOLD(KM,8)
      YT=STATEOLD(KM,9)
      SU=STATEOLD(KM,10)
      XC=STATEOLD(KM,27)
      YC=STATEOLD(KM,28)
      END IF
c      IF (TOTALTIME==0.1)THEN
c         PRINT*,'E1***',E1
c       END IF
C=================================
C=================================
C=================================    
C=================================       
       
C ...........Elastic properties...................

C............compute Poisson ratio................
      NU21 = NU12*E2/E1
      NU32 = NU23*E3/E2
      NU31 = NU13*E3/E1 
C............user-defined state variables.........
C     STATEV1 = EPS1
C     STATEV2 = EPS2
C     STATEV3 = EPS3
C     STATEV4 = EPS12
C     STATEV5 = E1
C     STATEV6 = E2
C     STATEV7 = G12
C     STATEV8 = XT
C     STATEV9 = YT
C     STATEV10 = SU
C     STATEV11 = MAXS1
C     STATEV12 = MAXS2
C     STATEV13 = MAXS3
C     STATEV14 = MINS1
C     STATEV15 = MINS2
C     STATEV16 = MINS3
C     STATEV17 = NF
C     STATEV18 = N
C     STATEV19 = REMOVE ELEMENT IF EQUAL 0.0
C     STATEV20 = NFF        
C     STATEV21 = MAXE1
C     STATEV22 = MAXE2
C     STATEV23 = MAXE3
C     STATEV24 = MINE1
C     STATEV25 = MINE2
C     STATEV26 = MINE3
C     STATEV27 = XC
C     STATEV28 = YC 
C     STATEV29 = NFM
C............COMPUTE STIFNESS MATRIX..............
      DO I=1,4
        DO J=1,4
          C(I,J) = ZERO
        END DO
      END DO 
C............UNDAMAGE STIFNESS MATRIX.............            
       S = ONE-(NU12*NU21)-(NU23*NU32)-(NU31*NU13)-(TWO*NU21*NU32*NU13)
       
       C(1,1) = E1*(ONE-(NU23*NU32))/S
       C(1,2) = E1*(NU21+NU31*NU23)/S
       C(1,3) = E1*(NU31+NU21*NU32)/S
       C(1,4) = ZERO
       
       C(2,1) = C(1,2)
       C(2,2) = E2*(ONE-(NU13*NU31))/S
       C(2,3) = E2*(NU32+NU12*NU31)/S
       C(2,4) = ZERO
       
       C(3,1) = C(1,3)
       C(3,2) = C(2,3)
       C(3,3) = E3*(ONE-(NU12*NU21))/S
       C(3,4) = ZERO
       
       C(4,1) = ZERO
       C(4,2) = ZERO
       C(4,3) = ZERO 
       C(4,4) = G12      
c      IF (TOTALTIME==0.1)THEN
c        PRINT*,'E1****',E1
c      END IF
       
C............COMPUTE STRAIN.......................         
       EPSOLD(KM,1) = STATEOLD(KM,1)
       EPSOLD(KM,2) = STATEOLD(KM,2)
       EPSOLD(KM,3) = STATEOLD(KM,3)
       EPSOLD(KM,4) = STATEOLD(KM,4)
        
         
       EPSNEW(KM,1) = STRAININC(KM,1)+EPSOLD(KM,1)
       EPSNEW(KM,2) = STRAININC(KM,2)+EPSOLD(KM,2)
       EPSNEW(KM,3) = -(C(3,1)*EPSNEW(KM,1)+C(3,2)*EPSNEW(KM,2))/C(3,3)
       EPSNEW(KM,4) = STRAININC(KM,4)+EPSOLD(KM,4)
         
         
       STRAININC(KM,3)=EPSNEW(KM,3)-EPSOLD(KM,3)
c       IF (TOTALTIME==0.1)THEN
c         PRINT*,'C(3,1)',C(3,1)
c         PRINT*,'C(1,3)',C(1,3)
c         PRINT*,'E1',E1
c         PRINT*,'NU31',NU31
c         PRINT*,'NU32',NU32
c         PRINT*,'NU23',NU23
c         PRINT*,'S',S
c         PRINT*,'EPSNEW(KM,1)',EPSNEW(KM,1)
c         PRINT*,'C(3,2)',C(3,2)
c         PRINT*,'EPSNEW(KM,2)',EPSNEW(KM,2)
c         PRINT*,'C(3,3)',C(3,3)
c         PRINT*,'EPSNEW(KM,3)',EPSNEW(KM,3)

c      END IF 
C========================================================================= 
C=============================PART2=======================================
C=========================================================================      
C............CALCULATE STRESS.....................         
       STRESSNEW(KM,1)= C(1,1)*EPSNEW(KM,1)+C(1,2)*EPSNEW(KM,2)+
     1                    C(1,3)*EPSNEW(KM,3)
     
       STRESSNEW(KM,2)= C(2,1)*EPSNEW(KM,1)+C(2,2)*EPSNEW(KM,2)+
     1                    C(2,3)*EPSNEW(KM,3)
            
       STRESSNEW(KM,3)= ZERO
            
       STRESSNEW(KM,4)=2.0*C(4,4)*EPSNEW(KM,4)     
C===========================================================
C=============================================================
      IF (STRESSNEW(KM,1).GE.ZERO)THEN
      NFFT=(STRESSNEW(KM,1)/XT)**2+(STRESSNEW(KM,4)/SU)**2
      NFF=NFFT
      ELSE
      NFFC=(STRESSNEW(KM,1)/XC)**2
      NFF=NFFC
      END IF
      IF (STRESSNEW(KM,2).GE.ZERO)THEN
      NFMT=(STRESSNEW(KM,2)/YT)**2+(STRESSNEW(KM,4)/SU)**2
      NFM=NFMT
      ELSE
      NFMC=(STRESSNEW(KM,2)/(TWO*SU))**2+((YC/(TWO*SU))**2-ONE)*
     1(STRESSNEW(KM,2)/-YC)+(STRESSNEW(KM,4)/SU)**2
      NFM=NFMC
      END IF 
      STATENEW(KM,20)=NFF
      STATENEW(KM,29)=NFM 
C========================================================== 
      STATENEW(KM,19)=STATEOLD(KM,19)     
      IF (NFF>=ONE)THEN
      STATENEW(KM,19)=0.0
      END IF  
      IF (NFM>=ONE)THEN
      STATENEW(KM,19)=0.0  
      END IF      
C===========================================================      
            
      DO I=1,2        
      IF (STRESSNEW(KM,I) .GT. STATEOLD(KM,10+I))THEN
         STATENEW(KM,10+I)=STRESSNEW(KM,I)
      ELSE
         STATENEW(KM,10+I)=STATEOLD(KM,10+I)
      END IF
C============================================================
      IF (EPSNEW(KM,I) .GT. STATEOLD(KM,20+I))THEN
         STATENEW(KM,20+I)=EPSNEW(KM,I)
      ELSE
         STATENEW(KM,20+I)=STATEOLD(KM,20+I)
      END IF      
     
C============================================================      
      IF (STRESSNEW(KM,I) .LT. STATEOLD(KM,13+I))THEN
         STATENEW(KM,13+I)=STRESSNEW(KM,I)
      ELSE
         STATENEW(KM,13+I)=STATEOLD(KM,13+I)
      END IF  
C============================================================
      IF (EPSNEW(KM,I) .LT. STATEOLD(KM,23+I))THEN
         STATENEW(KM,23+I)=EPSNEW(KM,I)
      ELSE
         STATENEW(KM,23+I)=STATEOLD(KM,23+I)
      END IF      
      END DO
C=============================================================      
      IF (STRESSNEW(KM,4) .GT. STATEOLD(KM,13))THEN
         STATENEW(KM,13)=STRESSNEW(KM,4)
      ELSE
         STATENEW(KM,13)=STATEOLD(KM,13)
      END IF  
      IF (STRESSNEW(KM,4) .LT. STATEOLD(KM,16))THEN
         STATENEW(KM,16)=STRESSNEW(KM,4)
      ELSE
         STATENEW(KM,16)=STATEOLD(KM,16)
      END IF  
C=============================================================
      IF (EPSNEW(KM,4) .GT. STATEOLD(KM,23))THEN
         STATENEW(KM,23)=EPSNEW(KM,4)
      ELSE
         STATENEW(KM,23)=STATEOLD(KM,23)
      END IF  
      IF (EPSNEW(KM,4) .LT. STATEOLD(KM,26))THEN
         STATENEW(KM,26)=EPSNEW(KM,4)
      ELSE
         STATENEW(KM,26)=STATEOLD(KM,26)
      END IF           
C        PRINT*,'EPSNEW(KM,1)**',EPSNEW(KM,1)
C        PRINT*,'EPSNEW(KM,2)**',EPSNEW(KM,2)
C        PRINT*,'EPSNEW(KM,3)**',EPSNEW(KM,3)
C        PRINT*,'EPSNEW(KM,4)**',EPSNEW(KM,4)
C=============================================================  
C................UPDATE STATEV STRAIN.........................
                           
      STATENEW(KM,1) = EPSNEW(KM,1)
      STATENEW(KM,2) = EPSNEW(KM,2)
      STATENEW(KM,3) = EPSNEW(KM,3)
      STATENEW(KM,4) = EPSNEW(KM,4)
      IF(TOTALTIME .LE. 0.1)THEN         
      STATENEW(KM,5)=E01
      STATENEW(KM,6)=E02
      STATENEW(KM,7)=G012
      STATENEW(KM,8)=XT0
      STATENEW(KM,9)=YT0
      STATENEW(KM,10)=SU0
      STATENEW(KM,27)=XC0
      STATENEW(KM,28)=YC0
      ELSE 
      STATENEW(KM,5)=STATEOLD(KM,5)
      STATENEW(KM,6)=STATEOLD(KM,6)
      STATENEW(KM,7)=STATEOLD(KM,7)
      STATENEW(KM,8)=STATEOLD(KM,8)
      STATENEW(KM,9)=STATEOLD(KM,9)
      STATENEW(KM,10)=STATEOLD(KM,10)
      STATENEW(KM,27)=STATEOLD(KM,27)
      STATENEW(KM,28)=STATEOLD(KM,28)
      END IF
      STATENEW(KM,17)=STATEOLD(KM,17)
      NF=STATENEW(KM,17)
      STATENEW(KM,18)=STATEOLD(KM,18)      

C      STATENEW(KM,20)=STATEOLD(KM,20)
C      STATENEW(KM,29)=STATEOLD(KM,29)
    
C      print*,'totaltime',totaltime
C============================================================           
      IF (STEPTIME==0.1)THEN

C============================================================      
C==============================================================
        IF (TOTALTIME==0.1)THEN 
C        print*,'totaltime',totaltime  
        DSTRESS(KM,1)=STATENEW(KM,11)-STATENEW(KM,14)
        DSTRESS(KM,2)=STATENEW(KM,12)-STATENEW(KM,15)
        DSTRESS(KM,3)=STATENEW(KM,13)-STATENEW(KM,16)
C==============================================================
        DE(KM,1)=STATENEW(KM,21)-STATENEW(KM,24)
        DE(KM,2)=STATENEW(KM,22)-STATENEW(KM,25)
        DE(KM,3)=STATENEW(KM,23)-STATENEW(KM,26)        
C==============================================================
        R=(STATENEW(KM,14)/STATENEW(KM,11))
C==============================================================                
        IF (STATENEW(KM,11)==0)THEN
        R=0.0
        DW=0.0
        END IF
C============================================================== 
        IF ((STATENEW(KM,11)>=ZERO) .AND. (STATENEW(KM,14)>=ZERO) ) THEN
        DW1= (STATENEW(KM,11)* STATENEW(KM,21)- STATENEW(KM,14)*
     1STATENEW(KM,24))*E1/(XT**2)   
        ELSEIF ((STATENEW(KM,11)<ZERO) .AND. (STATENEW(KM,14)<ZERO))THEN
        DW1= (STATENEW(KM,11)* STATENEW(KM,21)- STATENEW(KM,14)*
     1STATENEW(KM,24))*E1/(XC**2)
        ELSE
        DW1=(STATENEW(KM,11)* STATENEW(KM,21)+ STATENEW(KM,14)*
     1STATENEW(KM,24))*E1/(XC**2+XT**2)
        END IF
C        PRINT*,'STATENEW(KM,11)',STATENEW(KM,11)
C        PRINT*,'STATENEW(KM,14)',STATENEW(KM,14)
C        PRINT*,'STATENEW(KM,21)',STATENEW(KM,21)
C        PRINT*,'STATENEW(KM,24)',STATENEW(KM,24)
C==============================================================         
        IF ((STATENEW(KM,12)>=ZERO) .AND. (STATENEW(KM,15)>=ZERO)) THEN
        DW2= (STATENEW(KM,12)* STATENEW(KM,22)- STATENEW(KM,15)*
     1STATENEW(KM,25))*E2/(YT**2)   
        ELSEIF ((STATENEW(KM,12)<ZERO) .AND. (STATENEW(KM,15)<ZERO))THEN
        DW2= (STATENEW(KM,12)* STATENEW(KM,22)- STATENEW(KM,15)*
     1STATENEW(KM,25))*E2/(YC**2)
        ELSE
        DW2=(STATENEW(KM,12)* STATENEW(KM,22)+ STATENEW(KM,15)*
     1STATENEW(KM,25))*E2/(YC**2+YT**2)
        END IF
C        PRINT*,'STATENEW(KM,12)',STATENEW(KM,12)
C        PRINT*,'STATENEW(KM,15)',STATENEW(KM,15)
C        PRINT*,'STATENEW(KM,22)',STATENEW(KM,22)
C        PRINT*,'STATENEW(KM,25)',STATENEW(KM,25)
C============================================================
        IF ((STATENEW(KM,13)>=ZERO) .AND. (STATENEW(KM,16)>=ZERO)) THEN
        DW3= (STATENEW(KM,13)* STATENEW(KM,23)- STATENEW(KM,16)*
     1STATENEW(KM,26))*G12/(SU**2)   
        ELSEIF ((STATENEW(KM,13)<ZERO) .AND. (STATENEW(KM,16)<ZERO))THEN
        DW3= (STATENEW(KM,13)* STATENEW(KM,23)- STATENEW(KM,16)*
     1STATENEW(KM,26))*G12/(SU**2)
        ELSE
        DW3=(STATENEW(KM,13)* STATENEW(KM,23)+ STATENEW(KM,16)*
     1STATENEW(KM,26))*G12/(2*SU**2)
        END IF       
C============================================================        
        DW=(DW1+DW2+DW3)
        NF=LOG((DW/K)**(1/ALFA))                            
        STATENEW(KM,17)=NF
        IF(NF.LE.0)THEN
        NF=0.0
        END IF 
        END IF      
C============================================================
         IF (TOTALTIME==0.1)THEN
             N=LOG(FACTOR)+NF
C            PRINT*,'N',N
C             PRINT*,'NF',NF
C             PRINT*,'LOG(FACTOR)',LOG(FACTOR)
            STATENEW(KM,18)=N
         ELSE
             N=LOG(FACTOR+1.0)+(STATEOLD(KM,18))
             STATENEW(KM,18)=N
            IF (N .GE. NF)THEN
            N=LOG(0.99)+NF
            STATENEW(KM,18)=N
            END IF
         END IF 
C========================================================
      IF (STATENEW(KM,11)>=ZERO .AND. STATENEW(KM,14)>=ZERO)THEN
      XT=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA1)**
     1(ONE/ALFA1))*(XT0-STATENEW(KM,11))+STATENEW(KM,11)
      XC=(XT/STATENEW(KM,8))*XC
      ELSEIF (STATENEW(KM,11)<ZERO .AND. STATENEW(KM,14)<ZERO)THEN
      XT=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA1)**
     1(ONE/ALFA1))*(XT0-ABS(STATENEW(KM,14)))+ABS(STATENEW(KM,14))
      XC=(XT/STATENEW(KM,8))*XC 
      ELSEIF (STATENEW(KM,11)>=ZERO .AND. STATENEW(KM,14)<=ZERO)THEN
       XT=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA1)**
     1(ONE/ALFA1))*(XT0-STATENEW(KM,11))+(STATENEW(KM,11))
       XC=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA1)**
     1(ONE/ALFA1))*(XC0-ABS(STATENEW(KM,14)))+ABS(STATENEW(KM,14))     
      END IF 
C========================================================
      IF (STATENEW(KM,12)>=ZERO .AND. STATENEW(KM,15)>=ZERO)THEN
      YT=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA2)**
     1(ONE/ALFA2))*(YT0-STATENEW(KM,12))+STATENEW(KM,12)
      YC=(YT/STATENEW(KM,9))*YC
      ELSEIF (STATENEW(KM,12)<ZERO .AND. STATENEW(KM,15)<ZERO)THEN
      YT=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA2)**
     1(ONE/ALFA2))*(YT0-ABS(STATENEW(KM,15)))+ABS(STATENEW(KM,15))
      YC=(YT/STATENEW(KM,9))*YC 
      ELSEIF (STATENEW(KM,12)>=ZERO .AND. STATENEW(KM,15)<=ZERO)THEN
      YT=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA2)**
     1(ONE/ALFA2))*(YT0-STATENEW(KM,12))+(STATENEW(KM,12))
      YC=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA2)**
     1(ONE/ALFA2))*(YC0-ABS(STATENEW(KM,15)))+ABS(STATENEW(KM,15))     
      END IF
C========================================================       
      IF (STATENEW(KM,13)>=ZERO .AND. STATENEW(KM,16)>=ZERO)THEN
      SU=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA3)**
     1(ONE/ALFA3))*(SU0-STATENEW(KM,13))+STATENEW(KM,13)
      ELSEIF (STATENEW(KM,13)<ZERO .AND. STATENEW(KM,16)<ZERO)THEN
      SU=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA3)**
     1(ONE/ALFA3))*(SU0-ABS(STATENEW(KM,16)))+ABS(STATENEW(KM,16))
      ELSEIF (STATENEW(KM,13)>=ZERO .AND. STATENEW(KM,16)<=ZERO)THEN
      SU1=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA3)**
     1(ONE/ALFA3))*(SU0-STATENEW(KM,13))+(STATENEW(KM,13))   
      SU2=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**BETA3)**
     1(ONE/ALFA3))*(SU0-ABS(STATENEW(KM,16)))+ABS(STATENEW(KM,16))   
      SU=MIN(SU1,SU2)
      END IF
C============================================================  
C============================================================        
      STATENEW(KM,8)=XT
      STATENEW(KM,27)=XC
      STATENEW(KM,9)=YT
      STATENEW(KM,28)=YC
      STATENEW(KM,10)=SU  
C============================================================= 
      IF (STATENEW(KM,11)>=ZERO .AND. STATENEW(KM,14)>=ZERO)THEN
      E1=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA1)**(ONE/GAMA1))*
     1(E01-(STATENEW(KM,11))/(XT0/E01))+(STATENEW(KM,11))/(XT0/E01)
      ELSEIF (STATENEW(KM,11)<ZERO .AND. STATENEW(KM,14)<ZERO)THEN
      E1=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA1)**(ONE/GAMA1))*
     1(E01-ABS(STATENEW(KM,14))/(XC0/E01))+ABS(STATENEW(KM,14))/
     2(XC0/E01)
      ELSEIF (STATENEW(KM,11)>=ZERO .AND. STATENEW(KM,14)<=ZERO)THEN   
      E11=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA1)**(ONE/GAMA1))*
     1(E01-(STATENEW(KM,11))/(XT0/E01))+(STATENEW(KM,11))/(XT0/E01)
      E12=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA1)**(ONE/GAMA1))*
     1(E01-ABS(STATENEW(KM,14))/(XC0/E01))+ABS(STATENEW(KM,14))/
     2(XC0/E01)
      E1=MIN(E11,E12)
      END IF
c      IF (TOTALTIME==0.5)THEN
c         PRINT*,'E1-4',E1
c         PRINT*,'N-4',N
c        PRINT*,'NF-4',NF
c         PRINT*,'LANDA1-4',LANDA1
c         PRINT*,'GAMA1-4',GAMA1
c         PRINT*,'STATENEW(KM,11)-4',STATENEW(KM,11)
c         PRINT*,'E01-4',E01
c         PRINT*,'XT0-4',XT0

      
C==============================================================      
      IF (STATENEW(KM,12)>=ZERO .AND. STATENEW(KM,15)>=ZERO)THEN
      E2=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA2)**(ONE/GAMA2))*
     1(E02-(STATENEW(KM,12))/(YT0/E02))+(STATENEW(KM,12))/(YT0/E02)
      ELSEIF (STATENEW(KM,12)<ZERO .AND. STATENEW(KM,15)<ZERO)THEN
      E2=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA2)**(ONE/GAMA2))*
     1(E02-ABS(STATENEW(KM,15))/(YC0/E02))+ABS(STATENEW(KM,15))/
     2(YC0/E02)
      ELSEIF (STATENEW(KM,12)>=ZERO .AND. STATENEW(KM,15)<=ZERO)THEN   
      E21=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA2)**(ONE/GAMA2))*
     1(E02-(STATENEW(KM,12))/(YT0/E02))+(STATENEW(KM,12))/(YT0/E02)
      E22=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA2)**(ONE/GAMA2))*
     1(E02-ABS(STATENEW(KM,15))/(YC0/E02))+ABS(STATENEW(KM,15))/
     2(YC0/E02)
      E2=MIN(E21,E22)
      END IF
C==============================================================      
      IF (STATENEW(KM,13)>=ZERO .AND. STATENEW(KM,16)>=ZERO)THEN
      G12=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA3)**(ONE/GAMA3))*
     1(G012-(STATENEW(KM,13))/(SU0/G012))+(STATENEW(KM,13))/(SU0/G012)
      ELSEIF (STATENEW(KM,13)<ZERO .AND. STATENEW(KM,16)<ZERO)THEN
      G12=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA3)**(ONE/GAMA3))*
     1(G012-ABS(STATENEW(KM,16))/(SU0/G012))+ABS(STATENEW(KM,16))/
     2(SU0/G012)
      ELSEIF (STATENEW(KM,13)>=ZERO .AND. STATENEW(KM,16)<=ZERO)THEN   
      G121=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA3)**(ONE/GAMA3))*
     1(G012-(STATENEW(KM,13))/(SU0/G012))+(STATENEW(KM,13))/(SU0/G012)
      G122=((ONE-((N-LOG(0.25))/(NF-LOG(0.25)))**LANDA3)**(ONE/GAMA3))*
     1(G012-ABS(STATENEW(KM,16))/(SU0/G012))+ABS(STATENEW(KM,16))/
     2(SU0/G012)
      G12=MIN(G121,G122)
      END IF      
C===========================================================      
      IF (G12<ZERO)THEN
       G12=0.001*G012
      END IF
C============================================================= 
      STATENEW(KM,5)=E1
      STATENEW(KM,6)=E2
      STATENEW(KM,7)=G12
C==============================================================
      STATENEW(KM,11) = ZERO
      STATENEW(KM,12) = ZERO
      STATENEW(KM,13) = ZERO
      STATENEW(KM,14) = ZERO
      STATENEW(KM,15) = ZERO
      STATENEW(KM,16) = ZERO                       
C       PRINT*,'E1',E1
C       PRINT*,'E2',E2
C       PRINT*,'G12',G12

C        END IF
        END IF                       
  100 continue
      return
      end