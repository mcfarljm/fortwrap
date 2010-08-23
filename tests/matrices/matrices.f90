MODULE matrices

  IMPLICIT NONE

CONTAINS

  FUNCTION one_norm(m,n,A)
    INTEGER, INTENT(in) :: m,n,A(m,n)
    INTEGER :: one_norm
    INTEGER :: icol, colsum
    one_norm = -HUGE(one_norm)
    DO icol = 1,n
      colsum = SUM(A(:,icol))
      IF (colsum > one_norm) one_norm = colsum
    END DO
  END FUNCTION one_norm

  ! Test assumed size 
  FUNCTION one_norm_2(m,A) RESULT(one_norm)
    INTEGER, INTENT(in) :: m, A(m,*)
    INTEGER :: one_norm
    INTEGER :: icol, colsum, n
    n = 2
    one_norm = -HUGE(one_norm)
    DO icol = 1,n
      colsum = SUM(A(:,icol))
      IF (colsum > one_norm) one_norm = colsum
    END DO
  END FUNCTION one_norm_2

  SUBROUTINE multiply(m,n,A,b,Ab)
    INTEGER, INTENT(in) :: m,n, b(n)
    INTEGER, INTENT(in), DIMENSION(m,n) :: A
    INTEGER, INTENT(out) :: Ab(m)
    Ab = MATMUL(A,b)
  END SUBROUTINE multiply

END MODULE matrices
