#include "FortWrap.h"

int main(void)
{

  if (FortFuncs::foo_private0() != 0)
    return 1;

  if (FortFuncs::foo_private1(1) != 1)
    return 2;

  if (FortFuncs::foo_private2(1,2) != 2)
    return 3;
  

  return 0;
}

