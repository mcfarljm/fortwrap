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

END MODULE multidim_arrays
