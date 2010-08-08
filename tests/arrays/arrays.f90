MODULE arrays

  IMPLICIT NONE

CONTAINS

  FUNCTION array_in(n,x) RESULT(y)
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in) :: x(n)
    INTEGER :: y
    y = SUM(x)
  END FUNCTION array_in

  SUBROUTINE array_out(n,x)
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(out) :: x(n)
    x = 20
  END SUBROUTINE  array_out

END MODULE arrays
