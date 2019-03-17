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

  Derived *dp2 = d->get_t_alias();

  printf("getx: %d\n", dp2->getx());
  printf("gety: %d\n", dp2->gety());

  Derived *dp3 = d->get_c_alias();

  printf("getx: %d\n", dp3->getx());
  printf("gety: %d\n", dp3->gety());  

  delete b;
  delete d;
  delete dp;
  delete dp2;
  delete dp3;

  return 0;
}

