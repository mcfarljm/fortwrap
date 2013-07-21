MODULE DerivedTypes

  TYPE ObjectA
    INTEGER :: x
  END TYPE ObjectA

  type ObjectB
    type (ObjectA) :: a
  end type ObjectB

CONTAINS

  SUBROUTINE a_ctor(a,x)
    TYPE(ObjectA) :: a
    INTEGER, INTENT(in) :: x
    a%x = x
  END SUBROUTINE a_ctor

  FUNCTION getx(a)
    type (ObjectA) :: a
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
    TYPE (objectB) :: b
    INTEGER :: getax
    getax = b%a%x
  END FUNCTION getax
  
END MODULE DerivedTypes
