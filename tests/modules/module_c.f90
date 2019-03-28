MODULE module_c

  IMPLICIT NONE

  TYPE ObjectC
    INTEGER :: x
  END TYPE ObjectC

CONTAINS

  SUBROUTINE ObjectC_ctor(o, x)
    TYPE (ObjectC) :: o
    INTEGER, INTENT(in) :: x
    o%x = x
  END SUBROUTINE ObjectC_ctor

  FUNCTION getval(o)
    TYPE (ObjectC), INTENT(in) :: o
    INTEGER :: getval
    getval = o%x
  END FUNCTION getval

END MODULE module_c
