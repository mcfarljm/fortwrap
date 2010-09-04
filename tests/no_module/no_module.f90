SUBROUTINE int_sub(x,y)
  INTEGER, INTENT(in) :: x
  INTEGER, INTENT(out) :: y
  y = x + 1
END SUBROUTINE int_sub

FUNCTION int_func(x)
  INTEGER, INTENT(in) :: x
  INTEGER :: int_func
  int_func = x + 1
END FUNCTION int_func


