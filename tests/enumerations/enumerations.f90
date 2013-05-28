MODULE enumerations

  IMPLICIT NONE

  ENUM, BIND(C)
    ENUMERATOR :: RED, GREEN
    ENUMERATOR :: BLUE
  END ENUM

  ENUM, BIND(C)
    ENUMERATOR :: RED_A, GREEN_A
    ENUMERATOR :: BLUE_A
  END ENUM

CONTAINS

  FUNCTION is_red(x) RESULT(is)
    INTEGER, INTENT(in) :: x
    LOGICAL :: is
    IF (x == RED) THEN
      is = .TRUE.
    ELSE
      is = .FALSE.
    END IF
  END FUNCTION is_red

  FUNCTION is_red_a(x) RESULT(is)
    INTEGER, INTENT(in) :: x
    LOGICAL :: is
    IF (x == RED_A) THEN
      is = .TRUE.
    ELSE
      is = .FALSE.
    END IF
  END FUNCTION is_red_a

  FUNCTION is_green(x) RESULT(is)
    INTEGER, INTENT(in) :: x
    LOGICAL :: is
    IF (x == GREEN) THEN
      is = .TRUE.
    ELSE
      is = .FALSE.
    END IF
  END FUNCTION is_green

  FUNCTION is_green_a(x) RESULT(is)
    INTEGER, INTENT(in) :: x
    LOGICAL :: is
    IF (x == GREEN_A) THEN
      is = .TRUE.
    ELSE
      is = .FALSE.
    END IF
  END FUNCTION is_green_a

  FUNCTION is_blue(x) RESULT(is)
    INTEGER, INTENT(in) :: x
    LOGICAL :: is
    IF (x == BLUE) THEN
      is = .TRUE.
    ELSE
      is = .FALSE.
    END IF
  END FUNCTION is_blue

  FUNCTION is_blue_a(x) RESULT(is)
    INTEGER, INTENT(in) :: x
    LOGICAL :: is
    IF (x == BLUE_A) THEN
      is = .TRUE.
    ELSE
      is = .FALSE.
    END IF
  END FUNCTION is_blue_a
  
END MODULE enumerations
