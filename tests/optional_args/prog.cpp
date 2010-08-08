#include "FortWrap.h"

int main(void) 
{

  // Optional arguments have default value of not present (NULL).  To
  // provide the optional arugment, must pass by reference.
  
  int a=1, b=2, c=4;

  if (FortFuncs::add_allopt() != 0)
    return 1;

  if (FortFuncs::add_allopt(&a) != a)
    return 2;

  if (FortFuncs::add_allopt(&a,NULL,&c) != a+c)
    return 3;

  if (FortFuncs::add_allopt(&a,&b,&c) != a+b+c)
    return 4;

  
  int d=8;
  
  if (FortFuncs::add_mixed(a,b) != a+b)
    return 5;

  if (FortFuncs::add_mixed(a,b,&c) != a+b+c)
    return 6;

  if (FortFuncs::add_mixed(a,b,&c,&d) != a+b+c+d)
    return 7;
  
  return 0;
}
