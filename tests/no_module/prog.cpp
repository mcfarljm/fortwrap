#include "FortWrap.h"

int main(void)
{

  int y;
  FortFuncs::int_sub(2,&y);
  if (y!=3)
    return 1;

  y = FortFuncs::int_func(10);
  if (y!=11)
    return 2;

  return 0;
}

