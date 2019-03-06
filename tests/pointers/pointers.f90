MODULE pointers

  IMPLICIT NONE

  TYPE Object
    INTEGER :: x
  END TYPE Object

CONTAINS

  SUBROUTINE object_ctor(o,x)
    TYPE(Object) :: o
    INTEGER, INTENT(in) :: x
    o%x = x
  END SUBROUTINE object_ctor

  SUBROUTINE set_value(o,x)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: x
    o%x = x
  END SUBROUTINE set_value

  FUNCTION get_value(o) RESULT(x)
    TYPE (Object), INTENT(in) :: o
    INTEGER :: x
    x = o%x
  END FUNCTION get_value

  ! Todo: idea here would be to wrap this as a factory function that
  ! returns a new Object, but internally sets up the memory automatically.
  !
  ! Tested and appears that with TYPE POINTER result, gfortran returns the
  ! pointer by value...
  FUNCTION get_alias(o) RESULT(alias)
    TYPE (Object), INTENT(in), TARGET :: o
    TYPE (Object), POINTER :: alias

    alias => o
  END FUNCTION get_alias

END MODULE pointers
