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

  ! Demonstrate how derived types can be passed as arguments
  SUBROUTINE b_ctor(b,a)
    TYPE (ObjectB) :: b
    TYPE (ObjectA), INTENT(in) :: a
    b%a = a
  END SUBROUTINE b_ctor

  FUNCTION getax(b)
    TYPE (ObjectB) :: b
    INTEGER :: getax
    getax = b%a%x
  END FUNCTION getax
  
END MODULE DerivedTypes
