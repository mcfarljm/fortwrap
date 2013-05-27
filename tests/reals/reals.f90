MODULE reals

  IMPLICIT NONE

  INTEGER, PARAMETER :: wp = 4  ! Test named precision

CONTAINS

  ! REAL(KIND=4) and REAL get mapped to C floats
  FUNCTION add_floats(a,b,c) RESULT(y)
    REAL(KIND=4), INTENT(in) :: a
    REAL(4), INTENT(in) :: b
    REAL(wp), INTENT(in) :: c
    REAL :: y
    y = a+b+c
  END FUNCTION add_floats

  ! REAL(KIND=8), REAL*8, and DOUBLE PRECISION get mapped to C doubles
  FUNCTION add_doubles(a,b) RESULT(y)
    REAL(KIND=8), INTENT(in) :: a
    DOUBLE PRECISION, INTENT(in) :: b
    REAL*8 :: y
    y = a+b
  END FUNCTION add_doubles

  ! Test lower case parsing:

  function add_floats_lower(a,b) result(y)
    real(kind=4), intent(in) :: a,b
    real :: y
    y = a+b
  end function add_floats_lower

  function add_doubles_lower(a,b) result(y)
    real(kind=8), intent(in) :: a
    double precision, intent(in) :: b
    real*8 :: y
    y = a+b
  end function add_doubles_lower

END MODULE reals
