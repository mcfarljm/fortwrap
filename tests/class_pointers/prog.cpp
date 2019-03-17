#include "FortWrap.h"
#include <cstdio>

int main(void)
{

  Base *b = new Derived(1,2);

  printf("getx: %d\n", b->getx());
  printf("gety: %d\n", b->gety());

  Derived *d = new Derived(3,4);

  Derived *dp = d->get_derived_t_alias();

  printf("getx: %d\n", dp->getx());
  printf("gety: %d\n", dp->gety());

  delete b;
  delete d;
  delete dp;

  return 0;
}

