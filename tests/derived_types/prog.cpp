#include "FortWrap.h"

int main(void) 
{
  ObjectA a(5);
  if (a.getx() != 5)
    return 1;

  // Demonstrate that ObjectA can be passed to other wrapped procedures:
  ObjectB b(&a);
  if (b.getax() != 5)
    return 2;
  
  return 0;
}
