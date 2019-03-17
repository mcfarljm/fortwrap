#include "FortWrap.h"
#include <cstdio>

int main(void)
{

  ChildAType* childa = new ChildAType(1);

  if (childa->getval() != 1)
    return 1;

  ChildAType* pa = childa->alias_t();
  if (pa->getval() != 1)
    return 1;
  

  return 0;
}

