#include "FortWrap.h"

int main(void)
{

  // Set up v = [1,2,3,4]
  int v[4] = { 1, 2, 3, 4 };

  if (FortFuncs::array_in(4,v)  != 10)
    return 1;

  int v2[2];
  FortFuncs::array_out(4,2,v,v2);
  if (! (v[0]==20 && v[1]==20 && v[2]==20 && v[3]==20) )
    return 3;
  else if (! (v2[0]==30 && v2[1]==30) )
    return 4;

  int a[3] = { 0, 1, 2 };
  int b[3] = { 1, 2, 3 };
  if (FortFuncs::inner_prod(3,a,b) != 8)
    return 5;

  // Test assumed size
  if (FortFuncs::inner_prod_2(3,a,b) != 8)
    return 6;  

  int v5[4] = {1,2,3,4};
  if (FortFuncs::assumed_shape_in(4, v5) != 10)
    return 10;

  int v3[4] = {1, 2, 3, 4};
  for (int i=0; i<4; i++)
    v3[i] = i+1;

  SizeContainer c(4);
  if (c.sum_with_dt_size(v3) != 10)
    return 20;
  
  return 0;
}

