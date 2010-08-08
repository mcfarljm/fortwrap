MODULE strings

  IMPLICIT NONE

  INTEGER, PARAMETER :: STRING_LEN=20

CONTAINS

  SUBROUTINE string_literal_len(s)
    CHARACTER(len=20), INTENT(out) :: s
    s = 'String A'
  END SUBROUTINE string_literal_len
  
  SUBROUTINE string_param_len(s)
    CHARACTER(len=STRING_LEN), INTENT(out) :: s
    s = 'String B'
  END SUBROUTINE string_param_len

END MODULE strings
