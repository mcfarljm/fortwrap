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

END MODULE logicals
