! Demonstrate how Doxygen comments are transferred to C++ header files
!
! After wrapping with FortWrap, open the header file Object.h to see the how
! the doxygen comments are transferred.  FortWrap also automatically
! decorates arguments with intent in/out specifiers and ARRAY flags

MODULE comments

  !> Brief description of Object
  !!
  !! Detailed description of Object
  TYPE Object

  END TYPE Object

  IMPLICIT NONE

CONTAINS

  !> Basic object constructor
  SUBROUTINE object_ctor_basic(o)
    TYPE (Object) :: o
  END SUBROUTINE object_ctor_basic

  !> Detailed constructor for object
  SUBROUTINE object_ctor_detailed(o,i,x)
    TYPE (Object) :: o
    !> This is an integer argument
    !! This comment can continue on multiple lines
    INTEGER, INTENT(in) :: i
    REAL, INTENT(in) :: x !< This is a floating point argument
  END SUBROUTINE object_ctor_detailed

  !> This is a subroutine that operates on an object
  !!
  !! Detailed procedure description goes here
  SUBROUTINE foo(o,i_in,i_out,x_array_in,x_array_out)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: n, i_in
    INTEGER, INTENT(out) :: i_out
    REAL, INTENT(in) :: x_array_in(10)
    REAL, INTENT(out) :: x_array_out(10)
  END SUBROUTINE foo


END MODULE comments
