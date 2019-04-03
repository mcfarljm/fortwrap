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

  return 0;
}

