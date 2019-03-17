#include "FortWrap.h"
#include <cstdio>

int main(void)
{

  ChildAType* childa = new ChildAType(1);

  if (childa->getval() != 1)
    return 1;

  ChildAType* pa1 = childa->alias_t();
  if (pa1->getval() != 1)
    return 2;

  ChildAType* pa2 = childa->alias_c();
  if (pa2->getval() != 1)
    return 3;

  delete childa;
  delete pa1;
  delete pa2;

  return 0;
}

