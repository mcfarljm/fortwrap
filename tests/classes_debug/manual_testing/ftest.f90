PROGRAM prog

  USE shapes
  USE ISO_C_BINDING

  TYPE (Circle) :: c, c3
  TYPE (Square) :: sq

  CLASS(Shape), ALLOCATABLE :: s
  CLASS(Polygon), ALLOCATABLE :: p

  CALL c%ctor(4)
  PRINT*, 'circle area:', c%get_area()

  CALL sq%ctor(2)
  PRINT*, 'square area:', sq%get_area()
  PRINT*, 'square nsides:', sq%num_sides()

  PRINT*, 'circle num:', c%get_num()

  PRINT*, 'total area:', add_area(c, sq)

  ALLOCATE(s, source=Circle(4))
  PRINT*, 's alloc area:', s%get_area()
  !ALLOCATE(Circle::src=circle_ctor2(5))
  !CALL circle_ctor(s, 5)
  !CALL s%ctor(5)
  DEALLOCATE(s)

!!$  CALL circle_ctor2(s, 1)
!!$  PRINT*, 'circle 2 area:', s%get_area()

  c3 = Circle(5)
  PRINT*, 'c3 area:', c3%get_area()

END PROGRAM prog
