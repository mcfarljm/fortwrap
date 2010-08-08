#include "FortWrap.h"
#include <iostream>
using namespace std;


int main(void) 
{
  float fa=1.0, fb=2.0;
  double da=1.0, db=2.0;

  if (FortFuncs::add_floats(fa,fb) != fa+fb)
    return 1;

  if (FortFuncs::add_doubles(da,db) != da+db) 
    return 2;

  return 0;
}
