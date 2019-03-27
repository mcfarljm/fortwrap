! Note the generated C bindings pass the function pointer by value.  This
! would not be compatible with wrapping procedures that use the procedure
! pointer argument as an output.  FortWrap prevents wrapping intent(out),
! but it doesn't require intent(in) because that seems to be incompatible
! with the use cases below where the pointer is stored (gfortran 5.4 won't
! compile the below code if the procedure pointer arguments are declared as
! intent(in)).

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

    ! With current wrapping approach, the optional procedure pointer
    ! argument will always be present, so the function should be written
    ! such that it treats a null pointer as if the argument is not present
    FUNCTION callf_opt(a,b,f) RESULT(y)
      INTEGER, INTENT(in) :: a,b
      PROCEDURE(int_template), POINTER, OPTIONAL :: f
      INTEGER :: y

      IF (PRESENT(f)) THEN
        ! Check that the optional pointer is associated
        IF (ASSOCIATED(f)) THEN
          y = f(a,b)
          RETURN
        END IF
      END IF
      
      y = -1
    END FUNCTION callf_opt
  
END MODULE func_pointers
