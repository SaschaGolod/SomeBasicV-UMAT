!DIR$ FREEFORM
subroutine vumat( &  ! Read only(unmodifiable) variables -
    nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
    stepTime, totalTime, dt, cmname, coordMp, charLength, &
    props, density, strainInc, relSpinInc, &
    tempOld, stretchOld, defgradOld, fieldOld, &
    stressOld, stateOld, enerInternOld, enerInelasOld, &
    tempNew, stretchNew, defgradNew, fieldNew, & ! Write only(modifiable) variables -
    stressNew, stateNew, enerInternNew, enerInelasNew)
!
include 'vaba_param.inc'
!
dimension props(nprops), density(nblock), coordMp(nblock, *), &
    charLength(nblock), strainInc(nblock, ndir + nshr), &
    relSpinInc(nblock, nshr), tempOld(nblock), &
    stretchOld(nblock, ndir + nshr), &
    defgradOld(nblock, ndir + nshr + nshr), &
    fieldOld(nblock, nfieldv), stressOld(nblock, ndir + nshr), &
    stateOld(nblock, nstatev), enerInternOld(nblock), &
    enerInelasOld(nblock), tempNew(nblock), &
    stretchNew(nblock, ndir + nshr), &
    defgradNew(nblock, ndir + nshr + nshr), &
    fieldNew(nblock, nfieldv), &
    stressNew(nblock, ndir + nshr), stateNew(nblock, nstatev), &
    enerInternNew(nblock), enerInelasNew(nblock)
!
REAL*8 C(4, 4), &
    EPSOLD(NBLOCK, 4), EPSNEW(NBLOCK, 4), E01, E02, G012, EF1, EF2, EF12, &
    E1, E2, E3, G12, NU12, NU21, NU13, ALFA1, ALFA2, ALFA3, BETA1, &
    BETA3, GAMA1, GAMA2, GAMA3, LANDA1, LANDA2, LANDA3, NU31, NU23, &
    NU32, S, DSTRESS(NBLOCK, 3), X, Y, SU, K, ALFA, R1(NBLOCK), R2(NBLOCK), &
    R3(NBLOCK), DW, DW1, DW2, DW3, N, NF, NFF, X0, Y0, SU0, FACTOR, &
    DE(NBLOCK, 3), R, XT, XC, YT, YC, NFM, NFFT, NFFC, NFMT, NFMC, E11, E12, &
    SU1, SU2, E21, E22, G121, G122

Parameter(ZERO=0.D0, ONE=1.D0, TWO=2.D0)

character*80 cmname

INTEGER I, J
! PRINT *, 'STEPTIME', STEPTIME

! IF(TOTALTIME == 0.1) THEN
! PRINT *, 'E1**', E1
! END IF
! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =

!............START PROGRAM........................
do 100 km = 1, nblock
    E1 = PROPS(1)
    E2 = PROPS(2)
    E3 = PROPS(3)
    NU12 = PROPS(4)
    NU23 = PROPS(5)
    NU13 = PROPS(6)
    G12 = PROPS(7)
    ! IF(TOTALTIME == 0.1) THEN
    ! PRINT *, 'E1*', E1
    ! END IF
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    EF1 = PROPS(8)
    EF2 = PROPS(9)
    EF12 = PROPS(10)
    ALFA1 = PROPS(11)
    ALFA2 = PROPS(12)
    ALFA3 = PROPS(13)
    GAMA1 = PROPS(14)
    GAMA2 = PROPS(15)
    GAMA3 = PROPS(16)
    BETA1 = PROPS(17)
    BETA2 = PROPS(18)
    BETA3 = PROPS(19)
    LANDA1 = PROPS(20)
    LANDA2 = PROPS(21)
    LANDA3 = PROPS(22)
    XT = PROPS(23)
    XC = PROPS(24)
    YT = PROPS(25)
    YC = PROPS(26)
    SU = PROPS(27)
    K = PROPS(28)
    ALFA = PROPS(29)
    FACTOR = PROPS(30)

    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    E01 = E1
    E02 = E2
    G012 = G12
    XT0 = XT
    YT0 = YT
    XC0 = XC
    YC0 = YC
    SU0 = SU
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    IF (TOTALTIME .GT. 0.1) THEN
        E1 = STATEOLD(KM, 5)
        E2 = STATEOLD(KM, 6)
        G12 = STATEOLD(KM, 7)
        XT = STATEOLD(KM, 8)
        YT = STATEOLD(KM, 9)
        SU = STATEOLD(KM, 10)
        XC = STATEOLD(KM, 27)
        YC = STATEOLD(KM, 28)
    END IF
    ! IF(TOTALTIME == 0.1) THEN
    ! PRINT *, 'E1***', E1
    ! END IF
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =

    !...........Elastic properties...................

    !............compute Poisson ratio................
    NU21 = NU12*E2/E1
    NU32 = NU23*E3/E2
    NU31 = NU13*E3/E1
    !............user - defined state variables.........
    ! STATEV1 = EPS1
    ! STATEV2 = EPS2
    ! STATEV3 = EPS3
    ! STATEV4 = EPS12
    ! STATEV5 = E1         Elastic modulus
    ! STATEV6 = E2
    ! STATEV7 = G12        Shear modulus
    ! STATEV8 = XT         Tensile strength
    ! STATEV9 = YT
    ! STATEV10 = SU        Shear strength
    ! STATEV11 = MAXS1     Maximum stress
    ! STATEV12 = MAXS2
    ! STATEV13 = MAXS3
    ! STATEV14 = MINS1
    ! STATEV15 = MINS2
    ! STATEV16 = MINS3
    ! STATEV17 = NF                                              ich vermute Number to Failure
    ! STATEV18 = N          Number of Cycles
    ! STATEV19 = REMOVE ELEMENT IF EQUAL 0.0
    ! STATEV20 = NFF
    ! STATEV21 = MAXE1
    ! STATEV22 = MAXE2
    ! STATEV23 = MAXE3
    ! STATEV24 = MINE1
    ! STATEV25 = MINE2
    ! STATEV26 = MINE3
    ! STATEV27 = XC
    ! STATEV28 = YC
    ! STATEV29 = NFM
    !............COMPUTE STIFNESS MATRIX..............
    DO I = 1, 4
        DO J = 1, 4
            C(I, J) = ZERO
        END DO
    END DO
    !............UNDAMAGE STIFNESS MATRIX.............
    S = ONE - (NU12*NU21) - (NU23*NU32) - (NU31*NU13) - (TWO*NU21*NU32*NU13)

    C(1, 1) = E1*(ONE - (NU23*NU32))/S
    C(1, 2) = E1*(NU21 + NU31*NU23)/S
    C(1, 3) = E1*(NU31 + NU21*NU32)/S
    C(1, 4) = ZERO

    C(2, 1) = C(1, 2)
    C(2, 2) = E2*(ONE - (NU13*NU31))/S
    C(2, 3) = E2*(NU32 + NU12*NU31)/S
    C(2, 4) = ZERO

    C(3, 1) = C(1, 3)
    C(3, 2) = C(2, 3)
    C(3, 3) = E3*(ONE - (NU12*NU21))/S
    C(3, 4) = ZERO

    C(4, 1) = ZERO
    C(4, 2) = ZERO
    C(4, 3) = ZERO
    C(4, 4) = G12
    ! IF(TOTALTIME == 0.1) THEN
    ! PRINT *, 'E1****', E1
    ! END IF

    !............COMPUTE STRAIN.......................
    EPSOLD(KM, 1) = STATEOLD(KM, 1)
    EPSOLD(KM, 2) = STATEOLD(KM, 2)
    EPSOLD(KM, 3) = STATEOLD(KM, 3)
    EPSOLD(KM, 4) = STATEOLD(KM, 4)

    EPSNEW(KM, 1) = STRAININC(KM, 1) + EPSOLD(KM, 1)
    EPSNEW(KM, 2) = STRAININC(KM, 2) + EPSOLD(KM, 2)
    EPSNEW(KM, 3) = -(C(3, 1)*EPSNEW(KM, 1) + C(3, 2)*EPSNEW(KM, 2))/C(3, 3)
    EPSNEW(KM, 4) = STRAININC(KM, 4) + EPSOLD(KM, 4)

    STRAININC(KM, 3) = EPSNEW(KM, 3) - EPSOLD(KM, 3)
    ! IF(TOTALTIME == 0.1) THEN
    ! PRINT *, 'C(3,1)', C(3, 1)
    ! PRINT *, 'C(1,3)', C(1, 3)
    ! PRINT *, 'E1', E1
    ! PRINT *, 'NU31', NU31
    ! PRINT *, 'NU32', NU32
    ! PRINT *, 'NU23', NU23
    ! PRINT *, 'S', S
    ! PRINT *, 'EPSNEW(KM,1)', EPSNEW(KM, 1)
    ! PRINT *, 'C(3,2)', C(3, 2)
    ! PRINT *, 'EPSNEW(KM,2)', EPSNEW(KM, 2)
    ! PRINT *, 'C(3,3)', C(3, 3)
    ! PRINT *, 'EPSNEW(KM,3)', EPSNEW(KM, 3)

    ! END IF
    !=========================================================================
    !=============================PART2=======================================
    !=========================================================================
    !............CALCULATE STRESS.....................
    STRESSNEW(KM, 1) = C(1, 1)*EPSNEW(KM, 1) + C(1, 2)*EPSNEW(KM, 2) + &
                       C(1, 3)*EPSNEW(KM, 3)

    STRESSNEW(KM, 2) = C(2, 1)*EPSNEW(KM, 1) + C(2, 2)*EPSNEW(KM, 2) + &
                       C(2, 3)*EPSNEW(KM, 3)

    STRESSNEW(KM, 3) = ZERO

    STRESSNEW(KM, 4) = 2.0*C(4, 4)*EPSNEW(KM, 4)
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    IF (STRESSNEW(KM, 1) .GE. ZERO) THEN
        NFFT = (STRESSNEW(KM, 1)/XT)**2 + (STRESSNEW(KM, 4)/SU)**2
        NFF = NFFT
    ELSE
        NFFC = (STRESSNEW(KM, 1)/XC)**2
        NFF = NFFC
    END IF
    IF (STRESSNEW(KM, 2) .GE. ZERO) THEN
        NFMT = (STRESSNEW(KM, 2)/YT)**2 + (STRESSNEW(KM, 4)/SU)**2
        NFM = NFMT
    ELSE
        NFMC = (STRESSNEW(KM, 2)/(TWO*SU))**2 + ((YC/(TWO*SU))**2 - ONE)* &
               (STRESSNEW(KM, 2)/-YC) + (STRESSNEW(KM, 4)/SU)**2
        NFM = NFMC
    END IF
    STATENEW(KM, 20) = NFF
    STATENEW(KM, 29) = NFM
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    STATENEW(KM, 19) = STATEOLD(KM, 19)
    IF (NFF >= ONE) THEN
        STATENEW(KM, 19) = 0.0
    END IF
    IF (NFM >= ONE) THEN
        STATENEW(KM, 19) = 0.0
    END IF
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =

    DO I = 1, 2
    IF (STRESSNEW(KM, I) .GT. STATEOLD(KM, 10 + I)) THEN
        STATENEW(KM, 10 + I) = STRESSNEW(KM, I)
    ELSE
        STATENEW(KM, 10 + I) = STATEOLD(KM, 10 + I)
    END IF
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    IF (EPSNEW(KM, I) .GT. STATEOLD(KM, 20 + I)) THEN
        STATENEW(KM, 20 + I) = EPSNEW(KM, I)
    ELSE
        STATENEW(KM, 20 + I) = STATEOLD(KM, 20 + I)
    END IF

    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    IF (STRESSNEW(KM, I) .LT. STATEOLD(KM, 13 + I)) THEN
        STATENEW(KM, 13 + I) = STRESSNEW(KM, I)
    ELSE
        STATENEW(KM, 13 + I) = STATEOLD(KM, 13 + I)
    END IF
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    IF (EPSNEW(KM, I) .LT. STATEOLD(KM, 23 + I)) THEN
        STATENEW(KM, 23 + I) = EPSNEW(KM, I)
    ELSE
        STATENEW(KM, 23 + I) = STATEOLD(KM, 23 + I)
    END IF
    END DO
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    IF (STRESSNEW(KM, 4) .GT. STATEOLD(KM, 13)) THEN
        STATENEW(KM, 13) = STRESSNEW(KM, 4)
    ELSE
        STATENEW(KM, 13) = STATEOLD(KM, 13)
    END IF
    IF (STRESSNEW(KM, 4) .LT. STATEOLD(KM, 16)) THEN
        STATENEW(KM, 16) = STRESSNEW(KM, 4)
    ELSE
        STATENEW(KM, 16) = STATEOLD(KM, 16)
    END IF
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    IF (EPSNEW(KM, 4) .GT. STATEOLD(KM, 23)) THEN
        STATENEW(KM, 23) = EPSNEW(KM, 4)
    ELSE
        STATENEW(KM, 23) = STATEOLD(KM, 23)
    END IF
    IF (EPSNEW(KM, 4) .LT. STATEOLD(KM, 26)) THEN
        STATENEW(KM, 26) = EPSNEW(KM, 4)
    ELSE
        STATENEW(KM, 26) = STATEOLD(KM, 26)
    END IF
    ! PRINT *, 'EPSNEW(KM,1)**', EPSNEW(KM, 1)
    ! PRINT *, 'EPSNEW(KM,2)**', EPSNEW(KM, 2)
    ! PRINT *, 'EPSNEW(KM,3)**', EPSNEW(KM, 3)
    ! PRINT *, 'EPSNEW(KM,4)**', EPSNEW(KM, 4)
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
    !................UPDATE STATEV STRAIN.........................

    STATENEW(KM, 1) = EPSNEW(KM, 1)
    STATENEW(KM, 2) = EPSNEW(KM, 2)
    STATENEW(KM, 3) = EPSNEW(KM, 3)
    STATENEW(KM, 4) = EPSNEW(KM, 4)
    IF (TOTALTIME .LE. 0.1) THEN
        STATENEW(KM, 5) = E01
        STATENEW(KM, 6) = E02
        STATENEW(KM, 7) = G012
        STATENEW(KM, 8) = XT0
        STATENEW(KM, 9) = YT0
        STATENEW(KM, 10) = SU0
        STATENEW(KM, 27) = XC0
        STATENEW(KM, 28) = YC0
    ELSE
        STATENEW(KM, 5) = STATEOLD(KM, 5)
        STATENEW(KM, 6) = STATEOLD(KM, 6)
        STATENEW(KM, 7) = STATEOLD(KM, 7)
        STATENEW(KM, 8) = STATEOLD(KM, 8)
        STATENEW(KM, 9) = STATEOLD(KM, 9)
        STATENEW(KM, 10) = STATEOLD(KM, 10)
        STATENEW(KM, 27) = STATEOLD(KM, 27)
        STATENEW(KM, 28) = STATEOLD(KM, 28)
    END IF
    STATENEW(KM, 17) = STATEOLD(KM, 17)
    NF = STATENEW(KM, 17)
    STATENEW(KM, 18) = STATEOLD(KM, 18)

    ! STATENEW(KM, 20) = STATEOLD(KM, 20)
    ! STATENEW(KM, 29) = STATEOLD(KM, 29)

    ! print *, 'totaltime', totaltime
    ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
    IF (STEPTIME == 0.1) THEN

        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (TOTALTIME == 0.1) THEN
            ! print *, 'totaltime', totaltime
            DSTRESS(KM, 1) = STATENEW(KM, 11) - STATENEW(KM, 14)
            DSTRESS(KM, 2) = STATENEW(KM, 12) - STATENEW(KM, 15)
            DSTRESS(KM, 3) = STATENEW(KM, 13) - STATENEW(KM, 16)
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            DE(KM, 1) = STATENEW(KM, 21) - STATENEW(KM, 24)
            DE(KM, 2) = STATENEW(KM, 22) - STATENEW(KM, 25)
            DE(KM, 3) = STATENEW(KM, 23) - STATENEW(KM, 26)
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            R = (STATENEW(KM, 14)/STATENEW(KM, 11))
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            IF (STATENEW(KM, 11) == 0) THEN
                R = 0.0
                DW = 0.0
            END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            IF ((STATENEW(KM, 11) >= ZERO) .AND. (STATENEW(KM, 14) >= ZERO)) THEN
                DW1 = (STATENEW(KM, 11)*STATENEW(KM, 21) - STATENEW(KM, 14)*&
               STATENEW(KM, 24))*E1/(XT**2)
            ELSEIF ((STATENEW(KM, 11) < ZERO) .AND. (STATENEW(KM, 14) < ZERO)) THEN
                DW1 = (STATENEW(KM, 11)*STATENEW(KM, 21) - STATENEW(KM, 14)*&
               STATENEW(KM, 24))*E1/(XC**2)
            ELSE
                DW1 = (STATENEW(KM, 11)*STATENEW(KM, 21) + STATENEW(KM, 14)*&
               STATENEW(KM, 24))*E1/(XC**2 + XT**2)
            END IF
            ! PRINT *, 'STATENEW(KM,11)', STATENEW(KM, 11)
            ! PRINT *, 'STATENEW(KM,14)', STATENEW(KM, 14)
            ! PRINT *, 'STATENEW(KM,21)', STATENEW(KM, 21)
            ! PRINT *, 'STATENEW(KM,24)', STATENEW(KM, 24)
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            IF ((STATENEW(KM, 12) >= ZERO) .AND. (STATENEW(KM, 15) >= ZERO)) THEN
                DW2 = (STATENEW(KM, 12)*STATENEW(KM, 22) - STATENEW(KM, 15)*&
               STATENEW(KM, 25))*E2/(YT**2)
            ELSEIF ((STATENEW(KM, 12) < ZERO) .AND. (STATENEW(KM, 15) < ZERO)) THEN
                DW2 = (STATENEW(KM, 12)*STATENEW(KM, 22) - STATENEW(KM, 15)*&
               STATENEW(KM, 25))*E2/(YC**2)
            ELSE
                DW2 = (STATENEW(KM, 12)*STATENEW(KM, 22) + STATENEW(KM, 15)*&
               STATENEW(KM, 25))*E2/(YC**2 + YT**2)
            END IF
            ! PRINT *, 'STATENEW(KM,12)', STATENEW(KM, 12)
            ! PRINT *, 'STATENEW(KM,15)', STATENEW(KM, 15)
            ! PRINT *, 'STATENEW(KM,22)', STATENEW(KM, 22)
            ! PRINT *, 'STATENEW(KM,25)', STATENEW(KM, 25)
            ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            IF ((STATENEW(KM, 13) >= ZERO) .AND. (STATENEW(KM, 16) >= ZERO)) THEN
                DW3 = (STATENEW(KM, 13)*STATENEW(KM, 23) - STATENEW(KM, 16)*&
               STATENEW(KM, 26))*G12/(SU**2)
            ELSEIF ((STATENEW(KM, 13) < ZERO) .AND. (STATENEW(KM, 16) < ZERO)) THEN
                DW3 = (STATENEW(KM, 13)*STATENEW(KM, 23) - STATENEW(KM, 16)*&
               STATENEW(KM, 26))*G12/(SU**2)
            ELSE
                DW3 = (STATENEW(KM, 13)*STATENEW(KM, 23) + STATENEW(KM, 16)*&
               STATENEW(KM, 26))*G12/(2*SU**2)
            END IF
            ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
            DW = (DW1 + DW2 + DW3)
            NF = LOG((DW/K)**(1/ALFA))
            STATENEW(KM, 17) = NF
            IF (NF .LE. 0) THEN
                NF = 0.0
            END IF
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (TOTALTIME == 0.1) THEN
            N = LOG(FACTOR) + NF
            ! PRINT *, 'N', N
            ! PRINT *, 'NF', NF
            ! PRINT *, 'LOG(FACTOR)', LOG(FACTOR)
            STATENEW(KM, 18) = N
        ELSE
            N = LOG(FACTOR + 1.0) + (STATEOLD(KM, 18))
            STATENEW(KM, 18) = N
            IF (N .GE. NF) THEN
                N = LOG(0.99) + NF
                STATENEW(KM, 18) = N
            END IF
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (STATENEW(KM, 11) >= ZERO .AND. STATENEW(KM, 14) >= ZERO) THEN
            XT = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA1)**&
           (ONE/ALFA1))*(XT0 - STATENEW(KM, 11)) + STATENEW(KM, 11)
            XC = (XT/STATENEW(KM, 8))*XC
        ELSEIF (STATENEW(KM, 11) < ZERO .AND. STATENEW(KM, 14) < ZERO) THEN
            XT = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA1)**&
           (ONE/ALFA1))*(XT0 - ABS(STATENEW(KM, 14))) + ABS(STATENEW(KM, 14))
            XC = (XT/STATENEW(KM, 8))*XC
        ELSEIF (STATENEW(KM, 11) >= ZERO .AND. STATENEW(KM, 14) <= ZERO) THEN
            XT = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA1)**&
           (ONE/ALFA1))*(XT0 - STATENEW(KM, 11)) + (STATENEW(KM, 11))
            XC = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA1)**&
           (ONE/ALFA1))*(XC0 - ABS(STATENEW(KM, 14))) + ABS(STATENEW(KM, 14))
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (STATENEW(KM, 12) >= ZERO .AND. STATENEW(KM, 15) >= ZERO) THEN
            YT = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA2)**&
           (ONE/ALFA2))*(YT0 - STATENEW(KM, 12)) + STATENEW(KM, 12)
            YC = (YT/STATENEW(KM, 9))*YC
        ELSEIF (STATENEW(KM, 12) < ZERO .AND. STATENEW(KM, 15) < ZERO) THEN
            YT = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA2)**&
           (ONE/ALFA2))*(YT0 - ABS(STATENEW(KM, 15))) + ABS(STATENEW(KM, 15))
            YC = (YT/STATENEW(KM, 9))*YC
        ELSEIF (STATENEW(KM, 12) >= ZERO .AND. STATENEW(KM, 15) <= ZERO) THEN
            YT = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA2)**&
           (ONE/ALFA2))*(YT0 - STATENEW(KM, 12)) + (STATENEW(KM, 12))
            YC = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA2)**&
           (ONE/ALFA2))*(YC0 - ABS(STATENEW(KM, 15))) + ABS(STATENEW(KM, 15))
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (STATENEW(KM, 13) >= ZERO .AND. STATENEW(KM, 16) >= ZERO) THEN
            SU = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA3)**&
           (ONE/ALFA3))*(SU0 - STATENEW(KM, 13)) + STATENEW(KM, 13)
        ELSEIF (STATENEW(KM, 13) < ZERO .AND. STATENEW(KM, 16) < ZERO) THEN
            SU = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA3)**&
           (ONE/ALFA3))*(SU0 - ABS(STATENEW(KM, 16))) + ABS(STATENEW(KM, 16))
        ELSEIF (STATENEW(KM, 13) >= ZERO .AND. STATENEW(KM, 16) <= ZERO) THEN
            SU1 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA3)**&
           (ONE/ALFA3))*(SU0 - STATENEW(KM, 13)) + (STATENEW(KM, 13))
            SU2 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**BETA3)**&
           (ONE/ALFA3))*(SU0 - ABS(STATENEW(KM, 16))) + ABS(STATENEW(KM, 16))
            SU = MIN(SU1, SU2)
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        STATENEW(KM, 8) = XT
        STATENEW(KM, 27) = XC
        STATENEW(KM, 9) = YT
        STATENEW(KM, 28) = YC
        STATENEW(KM, 10) = SU
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
        IF (STATENEW(KM, 11) >= ZERO .AND. STATENEW(KM, 14) >= ZERO) THEN
            E1 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA1)**(ONE/GAMA1))*&
           (E01 - (STATENEW(KM, 11))/(XT0/E01)) + (STATENEW(KM, 11))/(XT0/E01)
        ELSEIF (STATENEW(KM, 11) < ZERO .AND. STATENEW(KM, 14) < ZERO) THEN
            E1 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA1)**(ONE/GAMA1))*&
           (E01 - ABS(STATENEW(KM, 14))/(XC0/E01)) + ABS(STATENEW(KM, 14))/&
           (XC0/E01)
        ELSEIF (STATENEW(KM, 11) >= ZERO .AND. STATENEW(KM, 14) <= ZERO) THEN
            E11 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA1)**(ONE/GAMA1))*&
           (E01 - (STATENEW(KM, 11))/(XT0/E01)) + (STATENEW(KM, 11))/(XT0/E01)
            E12 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA1)**(ONE/GAMA1))*&
           (E01 - ABS(STATENEW(KM, 14))/(XC0/E01)) + ABS(STATENEW(KM, 14))/&
           (XC0/E01)
            E1 = MIN(E11, E12)
        END IF
        ! IF(TOTALTIME == 0.5) THEN
        ! PRINT *, 'E1-4', E1
        ! PRINT *, 'N-4', N
        ! PRINT *, 'NF-4', NF
        ! PRINT *, 'LANDA1-4', LANDA1
        ! PRINT *, 'GAMA1-4', GAMA1
        ! PRINT *, 'STATENEW(KM,11)-4', STATENEW(KM, 11)
        ! PRINT *, 'E01-4', E01
        ! PRINT *, 'XT0-4', XT0

        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (STATENEW(KM, 12) >= ZERO .AND. STATENEW(KM, 15) >= ZERO) THEN
            E2 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA2)**(ONE/GAMA2))*&
           (E02 - (STATENEW(KM, 12))/(YT0/E02)) + (STATENEW(KM, 12))/(YT0/E02)
        ELSEIF (STATENEW(KM, 12) < ZERO .AND. STATENEW(KM, 15) < ZERO) THEN
            E2 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA2)**(ONE/GAMA2))*&
           (E02 - ABS(STATENEW(KM, 15))/(YC0/E02)) + ABS(STATENEW(KM, 15))/&
           (YC0/E02)
        ELSEIF (STATENEW(KM, 12) >= ZERO .AND. STATENEW(KM, 15) <= ZERO) THEN
            E21 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA2)**(ONE/GAMA2))*&
           (E02 - (STATENEW(KM, 12))/(YT0/E02)) + (STATENEW(KM, 12))/(YT0/E02)
            E22 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA2)**(ONE/GAMA2))*&
           (E02 - ABS(STATENEW(KM, 15))/(YC0/E02)) + ABS(STATENEW(KM, 15))/&
           (YC0/E02)
            E2 = MIN(E21, E22)
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        IF (STATENEW(KM, 13) >= ZERO .AND. STATENEW(KM, 16) >= ZERO) THEN
            G12 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA3)**(ONE/GAMA3))*&
           (G012 - (STATENEW(KM, 13))/(SU0/G012)) + (STATENEW(KM, 13))/(SU0/G012)
        ELSEIF (STATENEW(KM, 13) < ZERO .AND. STATENEW(KM, 16) < ZERO) THEN
            G12 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA3)**(ONE/GAMA3))*&
           (G012 - ABS(STATENEW(KM, 16))/(SU0/G012)) + ABS(STATENEW(KM, 16))/&
           (SU0/G012)
        ELSEIF (STATENEW(KM, 13) >= ZERO .AND. STATENEW(KM, 16) <= ZERO) THEN
            G121 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA3)**(ONE/GAMA3))*&
           (G012 - (STATENEW(KM, 13))/(SU0/G012)) + (STATENEW(KM, 13))/(SU0/G012)
            G122 = ((ONE - ((N - LOG(0.25))/(NF - LOG(0.25)))**LANDA3)**(ONE/GAMA3))*&
           (G012 - ABS(STATENEW(KM, 16))/(SU0/G012)) + ABS(STATENEW(KM, 16))/&
           (SU0/G012)
            G12 = MIN(G121, G122)
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
        IF (G12 < ZERO) THEN
            G12 = 0.001*G012
        END IF
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == =
        STATENEW(KM, 5) = E1
        STATENEW(KM, 6) = E2
        STATENEW(KM, 7) = G12
        ! ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==
        STATENEW(KM, 11) = ZERO
        STATENEW(KM, 12) = ZERO
        STATENEW(KM, 13) = ZERO
        STATENEW(KM, 14) = ZERO
        STATENEW(KM, 15) = ZERO
        STATENEW(KM, 16) = ZERO
        ! PRINT *, 'E1', E1
        ! PRINT *, 'E2', E2
        ! PRINT *, 'G12', G12

        ! END IF
    END IF
100 continue
    return
end
