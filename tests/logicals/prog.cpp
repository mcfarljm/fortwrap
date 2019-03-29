#include "FortWrap.h"

int main(void)
{

  if (! FortFuncs::get_true())
    return 1;
  if (FortFuncs::get_false())
    return 2;

  if (! FortFuncs::get_iso_true())
    return 11;
  if (FortFuncs::get_iso_false())
    return 12;

  if (FortFuncs::logical_in(1) != 1)
    return 20;
  bool l;
  FortFuncs::true_out(&l);
  if (!l)
    return 30;
  FortFuncs::false_out(&l);
  if (l)
    return 31;

  return 0;
}

