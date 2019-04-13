MODULE complex

  USE ISO_C_BINDING
  IMPLICIT NONE

CONTAINS

  FUNCTION add_complex(x1, x2) RESULT(y)
    COMPLEX, INTENT(in) :: x1, x2
    COMPLEX :: y
    y = x1 + x2
  END FUNCTION add_complex

  FUNCTION add_complex4(x1, x2) RESULT(y)
    COMPLEX(4), INTENT(in) :: x1, x2
    COMPLEX(4) :: y
    y = x1 + x2
  END FUNCTION add_complex4

  FUNCTION add_complex8(x1, x2) RESULT(y)
    COMPLEX(8), INTENT(in) :: x1, x2
    COMPLEX(8) :: y
    y = x1 + x2
  END FUNCTION add_complex8

  FUNCTION add_complex_cfloat(x1, x2) RESULT(y)
    COMPLEX(C_FLOAT_COMPLEX), INTENT(in) :: x1, x2
    COMPLEX(C_FLOAT_COMPLEX) :: y
    y = x1 + x2
  END FUNCTION add_complex_cfloat

  FUNCTION add_complex_cdouble(x1, x2) RESULT(y)
    COMPLEX(C_DOUBLE_COMPLEX), INTENT(in) :: x1, x2
    COMPLEX(C_DOUBLE_COMPLEX) :: y
    y = x1 + x2
  END FUNCTION add_complex_cdouble

  FUNCTION add_complex_byval(x1, x2) RESULT(y)
    COMPLEX(C_FLOAT_COMPLEX), INTENT(in), VALUE :: x1, x2
    COMPLEX(C_FLOAT_COMPLEX) :: y
    y = x1 + x2
  END FUNCTION add_complex_byval

  FUNCTION complex_sum(x) RESULT(y)
    COMPLEX(C_FLOAT_COMPLEX), INTENT(in) :: x(:)
    COMPLEX(C_FLOAT_COMPLEX) :: y
    y = SUM(x)
  END FUNCTION complex_sum


END MODULE complex
