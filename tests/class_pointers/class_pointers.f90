MODULE class_pointers

  IMPLICIT NONE

  TYPE, ABSTRACT :: Base
    INTEGER :: x
  CONTAINS
    PROCEDURE :: getx
    PROCEDURE(gety_template), DEFERRED :: gety
  END TYPE Base

  TYPE, EXTENDS(Base) :: Derived
    INTEGER :: y
  CONTAINS
    PROCEDURE :: gety
    PROCEDURE :: get_t_alias => get_derived_t_alias_class
    PROCEDURE :: get_c_alias => get_derived_c_alias
  END TYPE Derived

  ABSTRACT INTERFACE
    FUNCTION gety_template(b) RESULT(y)
      IMPORT Base
      CLASS (Base), INTENT(in) :: b
      INTEGER :: y
    END FUNCTION gety_template
  END INTERFACE

CONTAINS

  SUBROUTINE derived_ctor(d, x, y)
    CLASS (Derived) :: d
    INTEGER, INTENT(in) :: x, y
    d%x = x
    d%y = y
  END SUBROUTINE derived_ctor

  FUNCTION getx(b)
    CLASS (Base), INTENT(in) :: b
    INTEGER :: getx
    getx = b%x
  END FUNCTION getx

  FUNCTION gety(b) RESULT(y)
    CLASS (Derived), INTENT(in) :: b
    INTEGER :: y
    y = b%y
  END FUNCTION gety

  FUNCTION get_derived_t_alias(d) RESULT(p)
    TYPE (Derived), INTENT(in), TARGET :: d
    TYPE (Derived), POINTER :: p

    p => d
  END FUNCTION get_derived_t_alias

  FUNCTION get_derived_t_alias_class(d) RESULT(p)
    CLASS (Derived), INTENT(in), TARGET :: d
    TYPE (Derived), POINTER :: p

    p => d
  END FUNCTION get_derived_t_alias_class

  FUNCTION get_derived_c_alias(d) RESULT(p)
    CLASS (Derived), INTENT(in), TARGET :: d
    CLASS (Derived), POINTER :: p

    p => d
  END FUNCTION get_derived_c_alias

END MODULE class_pointers
