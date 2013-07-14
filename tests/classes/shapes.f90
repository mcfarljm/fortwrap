MODULE shapes

  IMPLICIT NONE

  !> Base shape type
  TYPE, ABSTRACT :: Shape
    INTEGER :: num = 101
  CONTAINS
    PROCEDURE (get_area_template), DEFERRED :: get_area
    PROCEDURE :: get_num, what_am_i
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
    INTEGER :: radius = 5
  CONTAINS
    PROCEDURE :: get_area => Circle_area
    PROCEDURE :: get_diameter => Circle_diameter
  END TYPE Circle

  TYPE, ABSTRACT, EXTENDS(Shape) :: Polygon
    INTEGER :: nsides
  CONTAINS
    PROCEDURE :: num_sides
  END TYPE Polygon

  !> An object with sharp corners
  TYPE, EXTENDS(Polygon) :: Square
    INTEGER :: side
  CONTAINS
    PROCEDURE :: get_area => Square_area
  END TYPE Square

CONTAINS

  SUBROUTINE Circle_ctor(s, radius)
    USE ISO_C_BINDING
    CLASS(Circle), TARGET :: s
    INTEGER, INTENT(in) :: radius
    s%radius = radius
!!$    PRINT*, 'cloc in Circle_ctor:', c_loc(s)
    !PRINT*, 'Shape num in ctor:', s%num
  END SUBROUTINE Circle_ctor

  SUBROUTINE Square_ctor(s, side)
    TYPE(Square) :: s
    INTEGER, INTENT(in) :: side
    s%side = side
    s%nsides = 4
!!$    PRINT*, 'in Square ctor'
    !PRINT*, 'Shape num in ctor:', s%num
  END SUBROUTINE Square_ctor

  !> Compute area of a circle
  FUNCTION Circle_area(s) RESULT(a)
    USE ISO_C_BINDING
    CLASS(Circle), TARGET :: s
    INTEGER :: a
!!$    PRINT*, 'c loc in Circle area:', c_loc(s)
!!$    PRINT*, 'Shape number', s%num
!!$    PRINT*, 'Circle radius:', s%radius
    a = 3 * s%radius**2
  END FUNCTION Circle_area

  FUNCTION Circle_diameter(s) RESULT(d)
    CLASS(Circle) :: s
    INTEGER :: d
    d = 2*s%radius*3
  END FUNCTION Circle_diameter

  FUNCTION Square_area(s) RESULT(a)
    CLASS(Square) :: s
    INTEGER :: a
    ! Do something with s
    a = s%side**2
  END FUNCTION Square_area

  FUNCTION square_area_dt(s) RESULT(a)
    TYPE (Square) :: s
    INTEGER :: a
    a = s%side**2
  END FUNCTION square_area_dt

  FUNCTION num_sides(s) RESULT(n)
    CLASS(Polygon) :: s
    INTEGER :: n
    n = s%nsides
  END FUNCTION num_sides

  FUNCTION get_num(s) RESULT(n)
    CLASS(Shape) :: s
    INTEGER :: n
    n = s%num
  END FUNCTION get_num

  SUBROUTINE what_am_i(s)
    CLASS(Shape) :: s
    SELECT TYPE(s)
    TYPE is (Circle)
      PRINT*, 'Circle'
    TYPE is (Square)
      PRINT*, 'Square'
    END SELECT
  END SUBROUTINE what_am_i

  FUNCTION add_area(s1, s2) RESULT(a)
    CLASS(Shape), INTENT(in) :: s1, s2
    INTEGER :: a
    a = s1%get_area() + s2%get_area()
  END FUNCTION add_area

END MODULE shapes
