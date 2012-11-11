MODULE arrays

  IMPLICIT NONE

CONTAINS

  FUNCTION array_in(n,x) RESULT(y)
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in) :: x(n)
    INTEGER :: y
    y = SUM(x)
  END FUNCTION array_in

  SUBROUTINE array_out(nx,ny,x,y)
    INTEGER, INTENT(in) :: nx,ny
    INTEGER, INTENT(out) :: x(nx)
    INTEGER, DIMENSION(ny), INTENT(out) :: y
    x = 20
    y = 30
  END SUBROUTINE  array_out

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

END MODULE arrays
