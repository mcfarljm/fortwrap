#include "FortWrap.h"
#include <iostream>
using namespace std;


int main(void) 
{
  float fa=1.0, fb=2.0, fc=3.0;
  if (FortFuncs::add_floats(fa,fb,fc) != fa+fb+fc)
    return 1;
  if (FortFuncs::add_floats_lower(fa,fb) != fa+fb)
    return 2;

  double da=1.0e50, db=2.0e50;
  if (FortFuncs::add_doubles(da,db) != da+db)
    return 3;
  if (FortFuncs::add_doubles_lower(da,db) != da+db)
    return 4;

  return 0;
}
