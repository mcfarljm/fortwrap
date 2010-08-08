MODULE reals

  IMPLICIT NONE

CONTAINS

  FUNCTION add_floats(a,b) RESULT(y)
    REAL, INTENT(in) :: a,b
    REAL :: y
    y = a+b
  END FUNCTION add_floats

  FUNCTION add_doubles(a,b) RESULT(y)
    REAL*8, INTENT(in) :: a,b
    REAL*8 :: y
    y = a+b
  END FUNCTION add_doubles
  
END MODULE reals
