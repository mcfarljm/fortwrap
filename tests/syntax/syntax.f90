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

  ! Verifies argument clashes in contained procedures, and also that we
  ! don't wrap contained procedures
  FUNCTION contains_arg_clash(a,b) RESULT(y)
    INTEGER, INTENT(in) :: a, b
    INTEGER :: y
    
    y = a + b
    
  CONTAINS

    SUBROUTINE foo(a)
      REAL*8, INTENT(in) :: a
    END SUBROUTINE foo

    SUBROUTINE bar(b)
      REAL*8, INTENT(in) :: b
    END SUBROUTINE bar
  END FUNCTION contains_arg_clash

  FUNCTION argument_case_sensitivity(x) RESULT(Y)
    INTEGER, INTENT(in) :: X
    INTEGER :: y
    y = x
  END FUNCTION argument_case_sensitivity

END MODULE syntax
