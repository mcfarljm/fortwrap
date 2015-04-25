MODULE classes

  IMPLICIT NONE

 !> Base shape type
  TYPE, ABSTRACT :: Shape
    INTEGER :: num = 101
  CONTAINS
    PROCEDURE (get_area_template), DEFERRED :: get_area
  END TYPE Shape
  
  ABSTRACT INTERFACE
    !> Compute the area for a shape
    FUNCTION get_area_template(s) RESULT(a)
      IMPORT :: Shape
      CLASS(Shape), INTENT(in) :: s
      INTEGER :: a
    END FUNCTION get_area_template
  END INTERFACE

 !> A round object
  TYPE, EXTENDS(Shape) :: Circle
    INTEGER :: radius
  CONTAINS
    PROCEDURE :: get_area => Circle_area
    PROCEDURE :: get_diameter => Circle_diameter
  END TYPE Circle

  INTERFACE Circle
    PROCEDURE Circle_ctor
  END INTERFACE Circle

CONTAINS

  FUNCTION Circle_ctor(radius) RESULT(s)
    INTEGER, INTENT(in) :: radius
    TYPE (Circle) :: s

    s%radius = radius
  END FUNCTION Circle_ctor

  !> This is a user-written wrapper around the constructor above that
  !! returns a derivd type.  Currently, FortWrap does not wrap procedures
  !! that return a derived type, so if such a procedure is in the interface,
  !! a subroutine-style version should be added before running FortWrap
  SUBROUTINE Circle_ctor_sub(s, radius)
    TYPE (Circle) :: s
    INTEGER, INTENT(in) :: radius
    s = circle_ctor(radius)
  END SUBROUTINE Circle_ctor_sub

  !> Compute area of a circle
  FUNCTION Circle_area(s) RESULT(a)
    CLASS(Circle), INTENT(in) :: s
    INTEGER :: a
    a = 3 * s%radius**2 ! Keep things simple and round pi to 3
  END FUNCTION Circle_area

  !> Compute area of a circle
  FUNCTION Circle_diameter(s) RESULT(diameter)
    CLASS(Circle), INTENT(in) :: s
    INTEGER :: diameter
    diameter = 2 * s%radius
  END FUNCTION Circle_diameter

END MODULE classes
