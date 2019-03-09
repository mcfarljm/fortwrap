#include "FortWrap.h"
#include <stdio.h>

int main(void)
{

  Object* o1 = new Object(3);

  Object *o2 = o1->get_alias();

  if (o1->get_value() != 3)
    return 1;
  if (o2->get_value() != 3)
    return 2;

  // Verify that modification affects the alias:
  o1->set_value(5);
  if (o2->get_value() != 5)
    return 3;


  delete o1;
  delete o2;

  return 0;
}

