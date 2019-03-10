MODULE pointers

  IMPLICIT NONE

  TYPE Object
    INTEGER :: x
  END TYPE Object

  TYPE Container
    TYPE (Object) :: o
  END TYPE Container

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

  ! Make sure additional wrapper code needed for post-processing string
  ! outputs works in conjunction with function returning pointer
  FUNCTION get_alias_with_char(o, s) RESULT(alias)
    TYPE (Object), INTENT(in), TARGET :: o
    CHARACTER(len=*), INTENT(out) :: s
    TYPE (Object), POINTER :: alias

    alias => o
    s = 'string'
  END FUNCTION get_alias_with_char

  FUNCTION create_new_object(x) RESULT(o)
    INTEGER, INTENT(in) :: x
    TYPE (Object), POINTER :: o

    ALLOCATE( o )
    o%x = x
  END FUNCTION create_new_object

  SUBROUTINE container_ctor(c, x)
    TYPE (Container) :: c
    INTEGER, INTENT(in) :: x

    c%o%x = x
  END SUBROUTINE container_ctor

  FUNCTION get_object_pointer(c) RESULT(o)
    TYPE (Container), TARGET :: c
    TYPE (Object), POINTER :: o

    o => c%o
  END FUNCTION get_object_pointer

END MODULE pointers
