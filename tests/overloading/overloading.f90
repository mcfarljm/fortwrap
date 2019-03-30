MODULE overloading

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: foo

  INTERFACE foo
    MODULE PROCEDURE foo_private0, &
      foo_private1
    MODULE PROCEDURE :: foo_private2
  END INTERFACE foo
  
CONTAINS

  FUNCTION foo_private0()
    INTEGER :: foo_private0
    foo_private0 = 0
  END FUNCTION foo_private0

  FUNCTION foo_private1(x1)
    INTEGER, INTENT(in) :: x1
    INTEGER :: foo_private1
    foo_private1 = 1
  END FUNCTION foo_private1

  FUNCTION foo_private2(x1, x2)
    INTEGER, INTENT(in) :: x1, x2
    INTEGER :: foo_private2
    foo_private2 = 2
  END FUNCTION foo_private2


END MODULE overloading
