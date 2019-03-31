MODULE c_ptrs

  USE ISO_C_BINDING, ONLY : C_PTR
  IMPLICIT NONE

  TYPE Container
    TYPE (C_PTR) :: p
  END TYPE Container

CONTAINS

  SUBROUTINE set_pointer(c, p)
    TYPE (Container) :: c
    TYPE (C_PTR), INTENT(in) :: p

    c%p = p
  END SUBROUTINE set_pointer

  SUBROUTINE set_byval(c, p)
    TYPE (Container) :: c
    TYPE (C_PTR), VALUE :: p

    c%p = p
  END SUBROUTINE set_byval

  SUBROUTINE get_pointer(c, p)
    TYPE (Container), INTENT(in) :: c
    TYPE (C_PTR), INTENT(out) :: p

    p = c%p
  END SUBROUTINE get_pointer

END MODULE c_ptrs
