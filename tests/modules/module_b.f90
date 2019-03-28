MODULE module_b

  USE module_a
  IMPLICIT NONE

  PRIVATE
  ! Deliberately don't make ObjectA public, even though it is in the API
  ! for ObjectB_ctor.  This is to make sure that FortWrap handles this
  ! correctly, since the wrapper around module_b must USE module_a
  PUBLIC :: ObjectB, ObjectB_ctor, getval_b

  TYPE ObjectB
    INTEGER :: x
  END TYPE ObjectB

CONTAINS

  SUBROUTINE ObjectB_ctor(o, a)
    TYPE (ObjectB) :: o
    TYPE (ObjectA), INTENT(in) :: a
    o%x = getval(a)
  END SUBROUTINE ObjectB_ctor

  FUNCTION getval_b(o)
    TYPE (ObjectB), INTENT(in) :: o
    INTEGER :: getval_b
    getval_b = o%x
  END FUNCTION getval_b

END MODULE module_b
