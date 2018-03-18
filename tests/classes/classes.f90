MODULE classes

  IMPLICIT NONE
  
  PRIVATE
  ! TODO: make robust to case where, e.g., Polygon is not public
  PUBLIC :: Shape, Circle, Polygon, Square, Circle_ctor_sub,&
    Square_ctor_sub

 !> Base shape type
  TYPE, ABSTRACT :: Shape
    INTEGER :: num = 101
  CONTAINS
    PROCEDURE (get_area_template), DEFERRED :: get_area
    ! Fortran will allow this, but C++ cannot use static virtual methods,
    ! so a virtual method for Shape will not get created
    PROCEDURE(is_round_template), DEFERRED, NOPASS :: is_round
  END TYPE Shape
  
  ABSTRACT INTERFACE
    !> Compute the area for a shape
    FUNCTION get_area_template(s) RESULT(a)
      IMPORT :: Shape
      CLASS(Shape), INTENT(in) :: s
      INTEGER :: a
    END FUNCTION get_area_template

    !> Whether shape is round
    FUNCTION is_round_template()
      IMPORT :: Shape
      LOGICAL :: is_round_template
    END FUNCTION is_round_template
  END INTERFACE

 !> A round object
  TYPE, EXTENDS(Shape) :: Circle
    INTEGER :: radius
  CONTAINS
    ! Verify that two procedures on same line get parsed correctly.  Also
    ! verify use of different case (circle_diameter vs Circle_diameter)
    PROCEDURE :: get_area => Circle_area, get_diameter => circle_diameter
    PROCEDURE, NOPASS :: is_round => Circle_is_round
  END TYPE Circle

  INTERFACE Circle
    PROCEDURE Circle_ctor
  END INTERFACE Circle

  TYPE, ABSTRACT, EXTENDS(Shape) :: Polygon
    INTEGER :: nsides
  CONTAINS
    PROCEDURE :: num_sides => polygon_num_sides
  END TYPE Polygon

  TYPE, EXTENDS(Polygon) :: Square
    INTEGER :: side_length
  CONTAINS
    PROCEDURE :: get_area => Square_area
    PROCEDURE, NOPASS :: is_round => Square_is_round
    PROCEDURE, NOPASS :: get_area_static => Square_area_static
  END TYPE Square

  INTERFACE Square
    PROCEDURE Square_ctor
  END INTERFACE Square

CONTAINS

  !> Constructors that return a derived type like this aren't wrapped by
  !! FortWrap.  The user should create a subroutine version, as below,
  !! which FortWrap can wrap. 
  !!
  !! One reason using this form in the Fortran is nice is because you can
  !! write code that dynamically allocates objects, such as:
  !! CLASS(Shape), allocatable :: s
  !! ALLOCATE(s, source=Circle(4))
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
    s = Circle(radius)
  END SUBROUTINE Circle_ctor_sub

  !> Compute area of a circle
  FUNCTION Circle_area(s) RESULT(a)
    CLASS(Circle), INTENT(in) :: s
    INTEGER :: a
    a = 3 * s%radius**2 ! Keep things simple and round pi to 3
  END FUNCTION Circle_area

  !> Whether circle is round
  FUNCTION Circle_is_round()
    LOGICAL :: Circle_is_round
    Circle_is_round = .TRUE.
  END FUNCTION Circle_is_round

  !> Compute area of a circle
  FUNCTION Circle_diameter(s) RESULT(diameter)
    CLASS(Circle), INTENT(in) :: s
    INTEGER :: diameter
    diameter = 2 * s%radius
  END FUNCTION Circle_diameter

  FUNCTION Square_ctor(side_length) RESULT(s)
    INTEGER, INTENT(in) :: side_length
    TYPE (Square) :: s

    s%side_length = side_length
    s%nsides = 4
  END FUNCTION Square_ctor

  SUBROUTINE Square_ctor_sub(s, side_length)
    TYPE (Square) :: s
    INTEGER, INTENT(in) :: side_length
    s = Square(side_length)
  END SUBROUTINE Square_ctor_sub

  FUNCTION polygon_num_sides(s) RESULT(n)
    CLASS(Polygon) :: s
    INTEGER :: n
    n = s%nsides
  END FUNCTION polygon_num_sides

  FUNCTION Square_area(s) RESULT(a)
    CLASS(Square), INTENT(in) :: s
    INTEGER :: a
    a = s%side_length**2
  END FUNCTION Square_area

  FUNCTION Square_is_round()
    LOGICAL :: Square_is_round
    Square_is_round = .FALSE.
  END FUNCTION Square_is_round

  ! Test NOPASS TBP with argument.  Also tests use of different case in
  ! procedure and TBP definitions
  FUNCTION square_area_static(side) RESULT(area)
    INTEGER, INTENT(in) :: side
    INTEGER :: area
    area = side**2
  END FUNCTION square_area_static

END MODULE classes
