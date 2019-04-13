! Note the generated C bindings pass the function pointer by value.  This
! would not be compatible with wrapping procedures that use the procedure
! pointer argument as an output.

MODULE func_pointers

  USE ISO_C_BINDING
  IMPLICIT NONE

  ABSTRACT INTERFACE
    FUNCTION int_template(a,b) RESULT(y)
      INTEGER, INTENT(in) :: a,b
      INTEGER :: y
    END FUNCTION int_template
    
    FUNCTION int_template_value(a,b) RESULT(y)
      INTEGER, INTENT(in), VALUE :: a,b
      INTEGER :: y
    END FUNCTION int_template_value

    FUNCTION int_template_bindc(a,b) RESULT(y) BIND(c)
      IMPORT C_INT
      INTEGER(C_INT), INTENT(in), VALUE :: a,b
      INTEGER(C_INT) :: y
    END FUNCTION int_template_bindc

  END INTERFACE
  
  TYPE Container
    PROCEDURE(int_template), POINTER, NOPASS :: f
    INTEGER :: a,b
  END TYPE Container

  CONTAINS

    SUBROUTINE container_ctor(c,f,a,b)
      TYPE (Container) :: c
      PROCEDURE(int_template), POINTER, INTENT(in) :: f
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

    SUBROUTINE getf_sub(c, f)
      TYPE (Container) :: c
      PROCEDURE(int_template), POINTER, INTENT(out) :: f
      f => c%f
    END SUBROUTINE getf_sub

    SUBROUTINE getf_opt(c, f)
      TYPE (Container) :: c
      PROCEDURE(int_template), POINTER, INTENT(out), OPTIONAL :: f
      ! Called from C++, f will always be present.  When a NULL (not
      ! present) argument is used from C++, the Fortran wrapper still
      ! declares and passes in a local variable, so accessing it in the case
      ! of intent(out) is safe
      IF (PRESENT(f)) f => c%f
    END SUBROUTINE getf_opt

    FUNCTION callf(f,a,b) RESULT(y)
      PROCEDURE(int_template), POINTER, INTENT(in) :: f
      INTEGER, INTENT(in) :: a,b
      INTEGER :: y
      y = f(a,b)
    END FUNCTION callf

    FUNCTION callf_value(f,a,b) RESULT(y)
      PROCEDURE(int_template_value), POINTER, INTENT(in) :: f
      INTEGER, INTENT(in), VALUE :: a,b
      INTEGER :: y
      y = f(a,b)
    END FUNCTION callf_value

    FUNCTION callf_bindc(f,a,b) RESULT(y)
      PROCEDURE(int_template_bindc), POINTER, INTENT(in) :: f
      INTEGER(C_INT), INTENT(in) :: a,b
      INTEGER(C_INT) :: y
      y = f(a,b)
    END FUNCTION callf_bindc

    ! With current wrapping approach, the optional procedure pointer
    ! argument will always be present, so the function should be written
    ! such that it treats a null pointer as if the argument is not present
    FUNCTION callf_opt(a,b,f) RESULT(y)
      INTEGER, INTENT(in) :: a,b
      PROCEDURE(int_template), POINTER, OPTIONAL, INTENT(in) :: f
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
