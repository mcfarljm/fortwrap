MODULE CppWrappers

  USE ISO_C_BINDING
  USE Shapes

  TYPE Shape_allocatable_container_
    CLASS(shape), ALLOCATABLE :: s
  END TYPE Shape_allocatable_container_
  
CONTAINS

  SUBROUTINE allocate_shape_allocatable(shape_container_cptr)
    TYPE (C_PTR) :: shape_container_cptr

    TYPE (Shape_allocatable_container_), POINTER :: shape_container_fptr

    ALLOCATE( shape_container_fptr )
    shape_container_cptr = C_LOC(shape_container_fptr)
  END SUBROUTINE allocate_shape_allocatable

 SUBROUTINE allocate_Square(Square_cptr)
    TYPE (C_PTR) :: Square_cptr

    TYPE(Square), POINTER, SAVE :: Square_fptr

    ALLOCATE( Square_fptr )
    Square_cptr = C_LOC(Square_fptr)
  END SUBROUTINE allocate_Square

 SUBROUTINE deallocate_Square(Square_cptr)
    TYPE (C_PTR), VALUE :: Square_cptr

    TYPE(Square), POINTER :: Square_fptr

    CALL C_F_POINTER(Square_cptr, Square_fptr)
    DEALLOCATE( Square_fptr )
  END SUBROUTINE deallocate_Square

 SUBROUTINE allocate_Circle(Circle_cptr)
    TYPE (C_PTR) :: Circle_cptr

    TYPE(Circle), POINTER, SAVE :: Circle_fptr

    ALLOCATE( Circle_fptr )
    Circle_cptr = C_LOC(Circle_fptr)
  END SUBROUTINE allocate_Circle

 SUBROUTINE deallocate_Circle(Circle_cptr)
    TYPE (C_PTR), VALUE :: Circle_cptr

    TYPE(Circle), POINTER :: Circle_fptr

    CALL C_F_POINTER(Circle_cptr, Circle_fptr)
    DEALLOCATE( Circle_fptr )
  END SUBROUTINE deallocate_Circle


END MODULE CppWrappers
