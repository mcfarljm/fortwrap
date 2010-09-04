MODULE reals

  IMPLICIT NONE

CONTAINS

  ! REAL(KIND=4) and REAL get mapped to C floats
  FUNCTION add_floats(a,b) RESULT(y)
    REAL(KIND=4), INTENT(in) :: a,b
    REAL :: y
    y = a+b
  END FUNCTION add_floats

  ! REAL(KIND=8), REAL*8, and DOUBLE PRECISION get mapped to C doubles
  FUNCTION add_doubles(a,b) RESULT(y)
    REAL(KIND=8), INTENT(in) :: a
    DOUBLE PRECISION, INTENT(in) :: b
    REAL*8 :: y
    y = a+b
  END FUNCTION add_doubles

END MODULE reals
