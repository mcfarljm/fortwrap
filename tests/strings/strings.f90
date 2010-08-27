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

  FUNCTION string_in_test(s) RESULT(t)
    CHARACTER(len=20), INTENT(in) :: s
    LOGICAL :: t
    IF (TRIM(s) == 'Test String') THEN
      t = .TRUE.
    ELSE
      t = .FALSE.
    END IF
  END FUNCTION string_in_test

  ! Test combination of intent(in) and intent(out), with additional
  ! interspersed arguments
  SUBROUTINE multiple_args(dumint,s1,dumfloat,s2)
    INTEGER, INTENT(in) :: dumint
    CHARACTER(len=STRING_LEN), INTENT(in) :: s1
    REAL, INTENT(in) :: dumfloat
    CHARACTER(len=STRING_LEN), INTENT(out) :: s2
    IF (LEN_TRIM(s1) < 13) THEN
      s2 = TRIM(s1) // ' suffix'
    END IF
  END SUBROUTINE multiple_args

END MODULE strings
