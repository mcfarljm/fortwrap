MODULE CppWrappers

  USE ISO_C_BINDING
  USE Shapes
  
CONTAINS

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
