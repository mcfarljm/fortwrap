#include "FortWrap.h"
#include <algorithm> 		// For count
#include <iostream>

int main(void)
{

  // Set up v = [1,2,3,4]
  std::vector<int> v(4);
  for (std::vector<int>::size_type i=0; i<v.size(); i++)
    v[i] = i+1;

  if (FortFuncs::array_in(&v)  != 10)
    return 1;

  std::vector<int> v2(10);
  FortFuncs::array_out(&v,&v2);
  if (static_cast<size_t>(count(v.begin(), v.end(), 20)) != v.size())
    return 2;
  else if (static_cast<size_t>(count(v2.begin(), v2.end(), 30)) != v2.size())
    return 3;
  

  std::vector<int> a(3), b(3);
  for (std::vector<int>::size_type i=0; i<a.size(); i++) 
    {
      a[i] = i;
      b[i] = i+1;
    }
  if (FortFuncs::inner_prod(&a,&b) != 8)
    return 4;

  // Test assumed size
  if (FortFuncs::inner_prod_2(3,&a,&b) != 8)
    return 5;

  // Set up v3 = [1,2,3,4]
  std::vector<int> v3(4);
  for (std::vector<int>::size_type i=0; i<v3.size(); i++)
    v3[i] = i+1;

  SizeContainer c(v3.size());
  if (c.sum_with_dt_size(&v3) != 10)
    return 10;
  
  return 0;
}

