#include "FortWrap.h"

int main(void) 
{
  int x;
  x = FortFuncs::add(1,2);
  if (x != 3) return 1;
  return 0;
}
