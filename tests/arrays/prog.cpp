#include "FortWrap.h"

int main(void)
{

  // Set up v = [1,2,3,4]
  std::vector<int> v(4);
  for (int i=0; i<v.size(); i++)
    v[i] = i+1;

  if (FortFuncs::array_in(&v)  != 10)
    return 1;

  FortFuncs::array_out(&v);
  if ((int) count(v.begin(), v.end(), 20) != v.size())
    return 2;
  
  return 0;
}

