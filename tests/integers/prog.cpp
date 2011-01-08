#include "FortWrap.h"

int main(void)
{

  signed char a1=1, b1=2;
  if (FortFuncs::add_ints1(a1,b1) != 3)
    return 1;

  short a2=1000, b2=2000;
  if (FortFuncs::add_ints2(a2,b2) != 3000)
    return 2;

  int a4=100000, b4=200000;
  if (FortFuncs::add_ints4(a4,b4) != 300000)
    return 3;

  return 0;
}

