#include "FortWrap.h"

int main(void)
{

  signed char a1=1, b1=2;
  if (FortFuncs::add_ints1(a1,b1) != 3)
    return 1;

  short a2=1000, b2=2000;
  if (FortFuncs::add_ints2(a2,b2) != 3000)
    return 3;

  int a4=100000, b4=200000;
  if (FortFuncs::add_ints4(a4,b4) != 300000)
    return 5;

  int a5=1, b5=2;
  if (FortFuncs::contains_arg_clash(a5,b5) != (a5+b5))
    return 7;

  int x=10;
  if (FortFuncs::argument_case_sensitivity(x) != x)
    return 9;

  return 0;
}

