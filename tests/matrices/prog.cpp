#include "FortWrap.h"

int main(void)
{

  // Create X = [ 0, 1
  //              1, 2
  //              2, 3 ]
  FortranMatrix<int> X(3,2);
  for (int i=0; i<3; i++) 
    {
      X(i,0) = i;
      X(i,1) = i+1;
    }
  if (FortFuncs::one_norm(&X) != 6)
    return 1;

  // Test assumed size
  if (FortFuncs::one_norm_2(&X) != 6)
    return 1;

  std::vector<int> a(2), Xa(3), Xa_test(3);
  a[0] = 1;
  a[1] = 2;
  // Xa_test holds the correct solution
  Xa_test[0] = 2;
  Xa_test[1] = 5;
  Xa_test[2] = 8;

  FortFuncs::multiply(&X,&a,&Xa);
  for (int i=0; i<3; i++) 
    {
      if (Xa[i] != Xa_test[i])
	return 3;
    }

  return 0;
}

