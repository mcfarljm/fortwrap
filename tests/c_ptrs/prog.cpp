#include "FortWrap.h"
#include <cstdio>

int main(void)
{

  int x, x2;
  int* xp;

  Container c;

  c.set_pointer(&x);
  c.get_pointer((void**) &xp);
  if (&x != xp)
    return 1;

  c.set_byval(&x2);
  c.get_pointer((void**) &xp);
  if (&x2 != xp)
    return 2;

  return 0;
}

