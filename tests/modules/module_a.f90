! This test case illustrates a name clash when all three modules are USE'd
! in the same place.  If the bind(c) wrapper code were to do that, it would
! not compile.  The approach of using a separate wrapper around each module
! resolves this, but requires care to ensure that each wrapper module has
! the necessary USE statements.

MODULE module_a

  IMPLICIT NONE

  TYPE ObjectA
    INTEGER :: x
  END TYPE ObjectA

CONTAINS

  SUBROUTINE ObjectA_ctor(o, x)
    TYPE (ObjectA) :: o
    INTEGER, INTENT(in) :: x
    o%x = x
  END SUBROUTINE ObjectA_ctor

  FUNCTION getval(o)
    TYPE (ObjectA), INTENT(in) :: o
    INTEGER :: getval
    getval = o%x
  END FUNCTION getval

END MODULE module_a
