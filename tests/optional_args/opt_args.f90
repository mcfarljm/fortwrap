MODULE opt_args

  IMPLICIT NONE

  CONTAINS

    FUNCTION add_allopt(a,b,c) RESULT(y)
      INTEGER, INTENT(in), OPTIONAL :: a,b,c
      INTEGER :: y
      y = 0
      IF (PRESENT(a)) y = y + a
      IF (PRESENT(b)) y = y + b
      IF (PRESENT(c)) y = y + c
    END FUNCTION add_allopt

    FUNCTION add_mixed(a,b,c,d) RESULT(y)
      INTEGER, INTENT(in) :: a,b ! test optional ::
      INTEGER, INTENT(in), OPTIONAL :: c,d
      INTEGER :: y
      y = a+b
      IF (PRESENT(c)) y = y + c
      IF (PRESENT(d)) y = y + d
    END FUNCTION add_mixed

    FUNCTION add_with_hidden(a,b,c,d) RESULT(y)
      INTEGER, INTENT(in) :: a
      INTEGER, INTENT(in), OPTIONAL :: b(:) ! Hidden
      INTEGER, INTENT(in), OPTIONAL :: c
      INTEGER, INTENT(in), OPTIONAL :: d(:) ! Hidden
      INTEGER :: y
      y = a
      IF (PRESENT(b)) y = y + 1
      IF (PRESENT(c)) y = y + c
      IF (PRESENT(d)) y = y + 1
    END FUNCTION add_with_hidden
  
END MODULE opt_args
