#include "FortWrap.h"

int main(void)
{

  signed char a1=1, b1=2;
  if (FortFuncs::add_ints1(a1,b1) != 3)
    return 1;
  if (FortFuncs::add_ints1_lower(a1,b1) != 3)
    return 2;

  short a2=1000, b2=2000;
  if (FortFuncs::add_ints2(a2,b2) != 3000)
    return 3;
  if (FortFuncs::add_ints2_lower(a2,b2) != 3000)
    return 4;

  int a4=100000, b4=200000;
  if (FortFuncs::add_ints4(a4,b4) != 300000)
    return 5;
  if (FortFuncs::add_ints4_lower(a4,b4) != 300000)
    return 6;

  long long a8=5000000000, b8=6000000000;
  if (FortFuncs::add_ints8(a8,b8) != (a8+b8))
    return 7;

  return 0;
}

