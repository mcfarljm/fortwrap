MODULE DerivedTypes

  TYPE ObjectA
    INTEGER :: x
  END TYPE ObjectA

  TYPE ObjectB
    TYPE (ObjectA) :: a
  END TYPE ObjectB

CONTAINS

  SUBROUTINE a_ctor(a,x)
    TYPE(ObjectA) :: a
    INTEGER, INTENT(in) :: x
    a%x = x
  END SUBROUTINE a_ctor

  FUNCTION getx(a)
    TYPE (ObjectA) :: a
    INTEGER :: getx
    getx = a%x
  END FUNCTION getx
  
END MODULE DerivedTypes
