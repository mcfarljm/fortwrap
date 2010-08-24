#include "FortWrap.h"
#include <algorithm> 		// For count

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

  std::vector<int> a(3), b(3);
  for (int i=0; i<a.size(); i++) 
    {
      a[i] = i;
      b[i] = i+1;
    }
  if (FortFuncs::inner_prod(&a,&b) != 8)
    return 3;

  // Test assumed size
  if (FortFuncs::inner_prod(&a,&b) != 8)
    return 4;
  
  return 0;
}

