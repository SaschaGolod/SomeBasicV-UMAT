        !COMPILER-GENERATED INTERFACE MODULE: Tue Oct  7 10:33:44 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE VUHARD__genmod
          INTERFACE 
            SUBROUTINE VUHARD(SYIELD,HARD,EQPLAS,TABLE,NVALUE)
              INTEGER(KIND=4) :: NVALUE
              REAL(KIND=8) :: SYIELD
              REAL(KIND=8) :: HARD
              REAL(KIND=8) :: EQPLAS
              REAL(KIND=8) :: TABLE(2,NVALUE)
            END SUBROUTINE VUHARD
          END INTERFACE 
        END MODULE VUHARD__genmod
