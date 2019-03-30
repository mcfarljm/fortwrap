MODULE module_d

  USE module_a
  IMPLICIT NONE

  PRIVATE
  ! Deliberately don't expose ObjectA to verify that FortWrap adds the
  ! necessary USE statement in the wrapper code
  PUBLIC :: getvala_fromd


CONTAINS

  ! Test a "method" defined in a different module than the type
  FUNCTION getvala_fromd(o) RESULT(val)
    TYPE (ObjectA), INTENT(in) :: o
    INTEGER :: val
    val = o%x
  END FUNCTION getvala_fromd

END MODULE module_d
