MODULE orphans

CONTAINS

  FUNCTION add(a,b) RESULT(c)
    INTEGER, INTENT(in) :: a,b
    INTEGER :: c
    c = a + b
  END FUNCTION add
  
END MODULE orphans
