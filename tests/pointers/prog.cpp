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

  Object *o3 = FortFuncs::create_new_object(10);
  o3->_acquire(); // Set ownership flag since this is a new reference
  if (o3->get_value() != 10)
    return 4;

  Container c(1);
  Object *o4 = c.get_object_pointer();
  if (o4->get_value() != 1)
    return 5;


  delete o1;
  delete o2;
  delete o3;
  delete o4;

  return 0;
}

