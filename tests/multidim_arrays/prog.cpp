#include "FortWrap.h"
#include <algorithm> 		// For count
#include <iostream>
#include "math.h" // For pow

int main(void)
{
  // 1-D array tests:
  {
    int a[3], b[3];
    for (int i=0; i<3; i++) 
      {
	a[i] = i;
	b[i] = i+1;
      }
    if (FortFuncs::inner_prod(3,a,b) != 8)
      return 0;

    // Test assumed size
    if (FortFuncs::inner_prod_2(3,a,b) != 8)
      return 1;
  }

  // 2-D array test:
  {  
    // Create X = [ 0, 1
    //              1, 2
    //              2, 3 ]
    int X[2][3];
    for (int i=0; i<3; i++) 
      {
	X[0][i] = i;
	X[1][i] = i+1;
      }  
  
    int a[2], Xa[3], Xa_test[3];
    a[0] = 1;
    a[1] = 2;
    // Xa_test holds the correct solution
    Xa_test[0] = 2;
    Xa_test[1] = 5;
    Xa_test[2] = 8;

    FortFuncs::mat_vec_mult(3,2,(int*)X,a,Xa);
    for (int i=0; i<3; i++) 
      {
	if (Xa[i] != Xa_test[i])
	  return 2;
      }
  }

  // 3-D array test
  {
    int X[2][3][4];
    FortFuncs::three_d_array_test((int*)X);
    // Check solution
    for (int i=0; i<2; i++) {
      for (int j=0; j<3; j++) {
	for (int k=0; k<4; k++) {
	  if (X[i][j][k] != pow(j+k+2,i+1))
	    return 3;
	}
      }
    }
  }

  return 0;
}

