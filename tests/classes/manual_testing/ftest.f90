PROGRAM prog

  USE shapes

  TYPE (Circle) :: c
  TYPE (Square) :: sq

  CALL Circle_ctor(c, 4)
  PRINT*, 'circle area:', c%get_area()

  CALL square_ctor(sq, 2)
  PRINT*, 'square area:', sq%get_area()
  PRINT*, 'square nsides:', sq%num_sides()

  PRINT*, 'circle num:', c%get_num()

END PROGRAM prog
