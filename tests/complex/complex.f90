MODULE complex

  IMPLICIT NONE

CONTAINS

  FUNCTION add_complex(x1, x2) RESULT(y)
    COMPLEX, INTENT(in) :: x1, x2
    COMPLEX :: y
    y = x1 + x2
  END FUNCTION add_complex


END MODULE complex
