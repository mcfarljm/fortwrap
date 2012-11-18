MODULE multidim_arrays

  IMPLICIT NONE

CONTAINS

  FUNCTION inner_prod(n,a,b) RESULT(y)
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in), dimension(n) :: a, b
    INTEGER :: y
    y = DOT_PRODUCT(a,b)
  END FUNCTION inner_prod

  ! Test assumed size
  FUNCTION inner_prod_2(n,a,b) RESULT(y)
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in), DIMENSION(*) :: a
    INTEGER, INTENT(in) :: b(*)
    INTEGER :: y
    y = DOT_PRODUCT(a(1:n),b(1:n))
  END FUNCTION inner_prod_2

  SUBROUTINE mat_vec_mult(m,n,A,b,Ab)
    INTEGER, INTENT(in) :: m,n, b(n)
    INTEGER, INTENT(in), DIMENSION(m,n) :: A
    INTEGER, INTENT(out) :: Ab(m)
    Ab = MATMUL(A,b)
  END SUBROUTINE mat_vec_mult

  SUBROUTINE three_d_array_test(A)
    INTEGER, INTENT(out) :: A(4,3,2)

    INTEGER :: i,j,k
    DO k=1,2
      DO j=1,3
        DO i=1,4
          A(i,j,k) = (i+j)**k
        END DO
      END DO
    END DO
  END SUBROUTINE three_d_array_test

END MODULE multidim_arrays
