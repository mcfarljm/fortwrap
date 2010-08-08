#include "CENTAUR.h"

int main(void) 
{
  ObjectA a(5);
  if (a.getx() != 5)
    return 1;
  
  return 0;
}
