MODULE class_pointers

  IMPLICIT NONE

  TYPE, ABSTRACT :: Base
    INTEGER :: x
  CONTAINS
    PROCEDURE(getval_template), DEFERRED :: getval
!!$    PROCEDURE :: get_base_alias
  END TYPE Base

  TYPE, EXTENDS(Base) :: ChildAType
  CONTAINS
    PROCEDURE :: getval => childa_getval
    PROCEDURE :: alias_t => childa_alias_t
  END TYPE ChildAType

  ABSTRACT INTERFACE
    FUNCTION getval_template(self) RESULT(val)
      IMPORT Base
      CLASS (Base) :: self
      INTEGER :: val
    END FUNCTION getval_template
  END INTERFACE

CONTAINS

  SUBROUTINE childa_ctor(c, x)
    CLASS (ChildAType) :: c
    INTEGER, INTENT(in) :: x
    c%x = x
  END SUBROUTINE childa_ctor

  FUNCTION childa_getval(self) RESULT(val)
    CLASS (ChildAType) :: self
    INTEGER :: val
    val = self%x
  END FUNCTION childa_getval

  FUNCTION childa_alias_t(self) RESULT(p)
    CLASS (ChildAType), TARGET :: self
    TYPE (ChildAType), POINTER :: p
    p => self
  END FUNCTION childa_alias_t

  
!!$  FUNCTION get_base_alias(b) RESULT(p)
!!$    CLASS (Base), INTENT(in), TARGET :: b
!!$    CLASS (Base), POINTER :: p
!!$
!!$    p => b
!!$  END FUNCTION get_base_alias

END MODULE class_pointers
