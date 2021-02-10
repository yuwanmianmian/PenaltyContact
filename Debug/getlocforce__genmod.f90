        !COMPILER-GENERATED INTERFACE MODULE: Wed Feb 03 14:46:25 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GETLOCFORCE__genmod
          INTERFACE 
            SUBROUTINE GETLOCFORCE(PS,P1,P2,MATPROP,PS0,P10,P20,FORCE,  &
     &ISINTERPENTRATION)
              REAL(KIND=8), INTENT(IN) :: PS(2)
              REAL(KIND=8), INTENT(IN) :: P1(2)
              REAL(KIND=8), INTENT(IN) :: P2(2)
              REAL(KIND=8), INTENT(IN) :: MATPROP(3)
              REAL(KIND=8), INTENT(IN) :: PS0(2)
              REAL(KIND=8), INTENT(IN) :: P10(2)
              REAL(KIND=8), INTENT(IN) :: P20(2)
              REAL(KIND=8), INTENT(OUT) :: FORCE(6)
              LOGICAL(KIND=4), INTENT(OUT) :: ISINTERPENTRATION
            END SUBROUTINE GETLOCFORCE
          END INTERFACE 
        END MODULE GETLOCFORCE__genmod
