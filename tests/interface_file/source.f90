MODULE source

  IMPLICIT NONE

  ! For the Fortran module, these can be made private since they are
  ! accessed via the generic interface object_ctor
  ! Todo: Is there a way to support this with the ISO_C_BINDING wrapping?
  ! May not be important if long-term plan is to move towards type bound
  ! procedures
!!$  PRIVATE :: object_constructor_i, object_constructor_f

  TYPE Object
    INTEGER :: i
    REAL :: f
  END TYPE Object

  INTERFACE object_constructor
    MODULE PROCEDURE object_constructor_i, object_constructor_f
  END INTERFACE

  ! An object that will get renamed
  TYPE Object_to_rename
    INTEGER :: i
  END TYPE Object_to_rename

  TYPE Object_to_ignore
  END TYPE Object_to_ignore

CONTAINS

  ! Demonstrate use of a different regular expression for matching
  !  constructors (default is to match "_ctor")
  SUBROUTINE object_constructor_i(o,x)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: x
    o%i = x
    o%f = 0.0
  END SUBROUTINE object_constructor_i

  SUBROUTINE object_constructor_f(o,f)
    TYPE (Object) :: o
    REAL, INTENT(in) :: f
    o%f = f
    o%i = 0
  END SUBROUTINE object_constructor_f

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

  ! Patterns can be used to facilitate renaming multiple methods.  Pattern
  ! matching is case insensitive, so "^object" will also match here:
  FUNCTION Object_multiply(o,i) RESULT(y)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: i
    INTEGER :: y
    y = o%i * i
  END FUNCTION Object_multiply

  SUBROUTINE Object2_constructor(o, i)
    TYPE (Object_to_rename) :: o
    INTEGER, INTENT(in) :: i
    o%i = i
  END SUBROUTINE Object2_constructor

  FUNCTION object2_val(o) RESULT(i)
    TYPE (Object_to_rename) :: o
    INTEGER :: i
    i = o%i
  END FUNCTION object2_val

  SUBROUTINE object_ignore_ctor(o)
    TYPE (Object_to_ignore) :: o
  END SUBROUTINE object_ignore_ctor

END MODULE source
