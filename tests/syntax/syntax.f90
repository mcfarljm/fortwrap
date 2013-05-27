! For verifying parsing of various syntax
MODULE syntax

  IMPLICIT NONE

CONTAINS

  FUNCTION add_ints1(a,b) RESULT(y)
    INTEGER ( KIND = 1 ) , INTENT(in) :: a  ! Comment with & character
    INTEGER(1), INTENT(in) :: b
    INTEGER*1 :: y
    y = a+b
  END FUNCTION add_ints1
  
  FUNCTION add_ints2(a,b) RESULT(y)
    INTEGER*2, INTENT(in) :: a,& ! Line continuation with comment
      b
    INTEGER(KIND=2  ) :: y
    y = a+b
  END FUNCTION add_ints2

  FUNCTION add_ints4(a,b) RESULT(y)
    INTEGER (KIND  =4), INTENT(in) :: a  ! Comment with colons
    INTEGER, INTENT(in) :: b
    INTEGER :: y
    y = a+b
  END FUNCTION add_ints4

END MODULE syntax
