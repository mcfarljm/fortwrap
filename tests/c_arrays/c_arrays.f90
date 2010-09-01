MODULE c_arrays

  IMPLICIT NONE

CONTAINS

  FUNCTION array_in(n,x) RESULT(y)
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in) :: x(n)
    INTEGER :: y
    y = SUM(x)
  END FUNCTION array_in

  ! Test assumed size array
  FUNCTION array_in_2(x) RESULT(y)
    INTEGER, INTENT(in) :: x(*)
    INTEGER :: y
    y = SUM(x(1:4))
  END FUNCTION array_in_2

  SUBROUTINE array_out(nx,ny,x,y)
    INTEGER, INTENT(in) :: nx,ny
    INTEGER, INTENT(out) :: x(nx)
    INTEGER, DIMENSION(ny), INTENT(out) :: y
    x = 20
    y = 30
  END SUBROUTINE  array_out

  FUNCTION inner_prod(n,a,b) RESULT(y)
    INTEGER, INTENT(in) :: n, a(n), b(n)
    INTEGER :: y
    y = DOT_PRODUCT(a,b)
  END FUNCTION inner_prod

END MODULE c_arrays
