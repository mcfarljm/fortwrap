#include "FortWrap.h"

int main(void)
{

  if (! FortFuncs::is_red(FortConstants::RED))
    return 1;

  if (! FortFuncs::is_red_a(FortConstants::RED_A))
    return 2;

  if (! FortFuncs::is_green(FortConstants::GREEN))
    return 3;

  if (! FortFuncs::is_green_a(FortConstants::GREEN_A))
    return 4;

  if (! FortFuncs::is_blue(FortConstants::BLUE))
    return 5;

  if (! FortFuncs::is_blue_a(FortConstants::BLUE_A))
    return 6;  
  
  
  return 0;
}

