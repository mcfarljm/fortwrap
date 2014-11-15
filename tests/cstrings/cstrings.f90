MODULE strings

  IMPLICIT NONE

  INTEGER, PARAMETER :: STRING_LEN=20, STRING_LEN2 = 20, &
    STRING_LEN3 = 20
  integer, parameter :: STRING_LEN4 = 20 ! Test comment

CONTAINS

  SUBROUTINE string_out_literal_len(s)
    CHARACTER(len=20 ), INTENT(out) :: s
    s = 'String A'
  END SUBROUTINE string_out_literal_len

  SUBROUTINE string_out_literal_len2(s)
    CHARACTER*8, INTENT(out) :: s
    s = 'String A'
  END SUBROUTINE string_out_literal_len2
  
  SUBROUTINE string_out_param_len(s1,s2,s3, s4)
    CHARACTER( len=STRING_LEN), INTENT(out) :: s1
    character(len =STRING_LEN2), INTENT(out) :: s2
    CHARACTER(LEN= STRING_LEN3), INTENT(out) :: s3
    CHARACTER(len=STRING_LEN4), INTENT(out) :: s4
    s1 = 'String A'
    s2 = 'String B'
    s3 = 'String C'
    s4 = 'String D'
  END SUBROUTINE string_out_param_len

  SUBROUTINE string_out_param_len2(s1,s2,s3, s4)
    CHARACTER(len=STRING_LEN), INTENT(out) :: s1, s2, s3, s4
    s1 = 'String A'
    s2 = 'String B'
    s3 = 'String C'
    s4 = 'String D'
  END SUBROUTINE string_out_param_len2

  SUBROUTINE string_out_assumed_len(s1,x,s2)
    CHARACTER(len=* ), INTENT(out) :: s1,s2
    INTEGER, INTENT(in) :: x ! Dummy
    s1 = 'String A'
    s2 = 'String B'
  END SUBROUTINE string_out_assumed_len

  FUNCTION string_in_test(s) RESULT(t)
    CHARACTER(len=20), INTENT(in) :: s
    LOGICAL :: t
    IF (s == 'Test String') THEN
      t = .TRUE.
    ELSE
      t = .FALSE.
    END IF
  END FUNCTION string_in_test

  !> Make sure conversion gets handled properly when dummy length is less
  !! than actual length
  FUNCTION string_in_cutoff(s) RESULT(t)
    CHARACTER(len=6), INTENT(in) :: s
    LOGICAL :: t
    IF (s == 'Test S') THEN
      t = .TRUE.
    ELSE
      t = .FALSE.
    END IF
  END FUNCTION string_in_cutoff

  FUNCTION string_in_assumed_len(s1,x,s2) RESULT(t)
    CHARACTER(len=*), INTENT(in) :: s1, s2
    INTEGER, INTENT(in) :: x ! Dummy
    LOGICAL :: t
    IF (s1=='Test String 1' .AND. s2=='String 2') THEN
      t = .TRUE.
    ELSE
      t = .FALSE.
    END IF
  END FUNCTION string_in_assumed_len

  ! Test combination of intent(in) and intent(out), with additional
  ! interspersed arguments
  SUBROUTINE multiple_args(dumint,s1,dumfloat,s2)
    INTEGER, INTENT(in) :: dumint
    CHARACTER(20), INTENT(in) :: s1
    REAL, INTENT(in) :: dumfloat
    CHARACTER(len=STRING_LEN), INTENT(out) :: s2
    IF (LEN_TRIM(s1) < 13) THEN
      s2 = TRIM(s1) // ' suffix'
    END IF
  END SUBROUTINE multiple_args

  ! Test optional arguments
  FUNCTION optional_in(s) RESULT(res)
    CHARACTER(len=STRING_LEN), OPTIONAL, INTENT(in) :: s
    INTEGER :: res ! 0=fail, 1=success(present), 2=success(not present)
    
    res = 0
    IF (PRESENT(s)) THEN
      IF (s == 'Test String') res = 1
    ELSE ! Not present
      res = 2
    END IF
  END FUNCTION optional_in

 FUNCTION optional_in_assumed(s) RESULT(res)
    CHARACTER(len=*), OPTIONAL, INTENT(in) :: s
    INTEGER :: res ! 0=fail, 1=success(present), 2=success(not present)
    
    res = 0
    IF (PRESENT(s)) THEN
      IF (s == 'Test String') res = 1
    ELSE ! Not present
      res = 2
    END IF
  END FUNCTION optional_in_assumed

  FUNCTION optional_out(s) RESULT(res)
    CHARACTER(len=STRING_LEN), OPTIONAL, INTENT(out) :: s
    INTEGER :: res ! 1=present, 0=not present
    
    res = 0
    IF (PRESENT(s)) THEN
      res = 1
      s = 'Test String'
    END IF
  END FUNCTION optional_out

  FUNCTION optional_out_assumed(s) RESULT(res)
    CHARACTER(len=*), OPTIONAL, INTENT(out) :: s
    INTEGER :: res ! 1=present, 0=not present
    
    res = 0
    IF (PRESENT(s)) THEN
      res = 1
      s = 'Test String'
    END IF
  END FUNCTION optional_out_assumed


END MODULE strings
