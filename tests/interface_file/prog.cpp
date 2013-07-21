#include "FortWrap.h"

int main(void)
{

  // Use integer constructor
  Object o1(5);
  if (o1.add(2) != 7)
    return 1;

  // Use float constructor
  Object o2((float) 10.0);
  if (o2.add(2) != 12)
    return 2;

  // Test renamed object
  Steve s(5);
  if (s.val() != 5)
    return 3;

  return 0;
}

