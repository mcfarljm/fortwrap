#include "FortWrap.h"
#include <stdio.h>

int main(void)
{

  Object o1(3);

  // Once the alias is wrapped, test it by getting an alias, modifying the original, and then verifying that the modification affected the alias.

  Object *o2 = o1.get_alias();

  printf("o1 value: %d\n", o1.get_value());
  printf("o2 value: %d\n", o2->get_value());

  return 0;
}

