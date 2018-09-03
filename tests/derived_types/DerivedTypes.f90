MODULE DerivedTypes

  PUBLIC
  PRIVATE :: PrivateObject, private_dtor, private_ctor

  TYPE ObjectA
    INTEGER :: x
  END TYPE ObjectA

  type ObjectB
    type (ObjectA) :: a
  end type ObjectB

  TYPE PrivateObject
  END TYPE PrivateObject

  
CONTAINS

  SUBROUTINE a_ctor(a,x)
    TYPE(ObjectA) :: a
    INTEGER, INTENT(in) :: x
    a%x = x
  END SUBROUTINE a_ctor

  !> Cleanup code automatically added to C++ destructor
  SUBROUTINE a_dtor(a)
    TYPE (ObjectA) :: a
  END SUBROUTINE a_dtor
  
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

  SUBROUTINE private_ctor(o)
    TYPE (PrivateObject) :: o
  END SUBROUTINE private_ctor

  SUBROUTINE private_dtor(o)
    TYPE (PrivateObject) :: o
  END SUBROUTINE private_dtor

  
END MODULE DerivedTypes
