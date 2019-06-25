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
    PROCEDURE :: alias_c => childa_alias_c
  END TYPE ChildAType

  TYPE, EXTENDS(Base) :: ChildBType
  CONTAINS
    PROCEDURE :: getval => childb_getval
  END TYPE ChildBType

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

  FUNCTION childa_alias_c(self) RESULT(p)
    CLASS (ChildAType), TARGET :: self
    CLASS (ChildAType), POINTER :: p
    p => self
  END FUNCTION childa_alias_c

  SUBROUTINE childb_ctor(c, x)
    CLASS (ChildBType) :: c
    INTEGER, INTENT(in) :: x
    c%x = x
  END SUBROUTINE childb_ctor

  !> Return 2x to differentiate
  FUNCTION childb_getval(self) RESULT(val)
    CLASS (ChildBType) :: self
    INTEGER :: val
    val = self%x * 2
  END FUNCTION childb_getval

!!$  FUNCTION get_base_alias(b) RESULT(p)
!!$    CLASS (Base), INTENT(in), TARGET :: b
!!$    CLASS (Base), POINTER :: p
!!$
!!$    p => b
!!$  END FUNCTION get_base_alias

END MODULE class_pointers
