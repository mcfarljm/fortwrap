MODULE func_pointers

  IMPLICIT NONE

  ABSTRACT INTERFACE
  FUNCTION int_template(a,b) RESULT(y)
    INTEGER, INTENT(in) :: a,b
    INTEGER :: y
  END FUNCTION int_template
  END INTERFACE
  
  TYPE Container
    PROCEDURE(int_template), POINTER, NOPASS :: f
    INTEGER :: a,b
  END TYPE Container

  CONTAINS

    SUBROUTINE container_ctor(c,f,a,b)
      TYPE (Container) :: c
      PROCEDURE(int_template), POINTER :: f
      INTEGER, INTENT(in) :: a,b
      c%f => f
      c%a = a
      c%b = b
    END SUBROUTINE container_ctor

    FUNCTION container_callf(c) RESULT(y)
      TYPE (Container) :: c
      INTEGER :: y
      procedure(int_template), pointer :: f
      f => c%f
      y = f(c%a,c%b)
    END FUNCTION container_callf

    FUNCTION callf(f,a,b) RESULT(y)
      PROCEDURE(int_template), POINTER :: f
      INTEGER, INTENT(in) :: a,b
      INTEGER :: y
      y = f(a,b)
    END FUNCTION callf
  
END MODULE func_pointers
