MODULE logicals

  USE ISO_C_BINDING
  IMPLICIT NONE

CONTAINS

  FUNCTION get_true()
    LOGICAL :: get_true
    get_true = .TRUE.
  END FUNCTION get_true

  FUNCTION get_false()
    LOGICAL :: get_false
    get_false = .FALSE.
  END FUNCTION get_false

  FUNCTION get_iso_true()
    LOGICAL(C_BOOL) :: get_iso_true
    get_iso_true = .TRUE.
  END FUNCTION get_iso_true

  FUNCTION get_iso_false()
    LOGICAL (C_BOOL) :: get_iso_false
    get_iso_false = .FALSE.
  END FUNCTION get_iso_false

  FUNCTION logical_in(l)
    LOGICAL, INTENT(in) :: l
    INTEGER :: logical_in
    IF (l) THEN
      logical_in = 1
    ELSE
      logical_in = 0
    END IF
  END FUNCTION logical_in

  SUBROUTINE true_out(l)
    LOGICAL, INTENT(out) :: l
    l = .TRUE.
  END SUBROUTINE true_out

  SUBROUTINE false_out(l)
    LOGICAL, INTENT(out) :: l
    l = .FALSE.
  END SUBROUTINE false_out

END MODULE logicals
