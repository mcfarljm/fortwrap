#include "FortWrap.h"

// Procedure pointers are called directly without wrappers, so can't
// convert pass-by-value to pass-by-reference.  Check the generated
// header file for the correct function signature.
int add(const int *a, const int *b) 
{
  return *a + *b;
}


int main(void) 
{

  int a=1, b=2;

  // Demonstrate that function pointers can be stored in a derived type
  Container c(add, a,b);
  if (c.container_callf() != a + b)
    return 1;

  // Demonstrate direct use of a function pointer callback
  if (FortFuncs::callf(add, a, b) != a + b)
    return 2;

  return 0;
}
