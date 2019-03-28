#include "FortWrap.h"

int main(void)
{

  ObjectA a(1);
  ObjectB b(&a);
  ObjectC c(3);

  if (a.getval() != 1)
    return 1;
  if (b.getval_b() != 1)
    return 2;
  if (c.getval() != 3)
    return 3;

  return 0;
}

