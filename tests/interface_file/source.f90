MODULE source

  IMPLICIT NONE

  ! For the Fortran module, these can be made private since they are
  ! accessed via the generic interface object_ctor
  PRIVATE :: object_ctor_i, object_ctor_f

  TYPE Object
    INTEGER :: i
    REAL :: f
  END TYPE Object

  INTERFACE object_ctor
    MODULE PROCEDURE object_ctor_i, object_ctor_f
  END INTERFACE

CONTAINS

  SUBROUTINE object_ctor_i(o,x)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: x
    o%i = x
    o%f = 0.0
  END SUBROUTINE object_ctor_i

  SUBROUTINE object_ctor_f(o,f)
    TYPE (Object) :: o
    REAL, INTENT(in) :: f
    o%f = f
    o%i = 0
  END SUBROUTINE object_ctor_f

  SUBROUTINE work_sub(x)
    INTEGER :: x
  END SUBROUTINE work_sub

  ! Fortran names may be overly verbose because they don't have their own
  ! namespace within Fortran.  It would make sense to rename these to
  ! shorter names within the generated C++ interface
  FUNCTION object_add(o,i) RESULT(y)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: i
    INTEGER :: y
    y = o%i + INT(o%f) + i
  END FUNCTION object_add

END MODULE source
