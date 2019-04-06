#include "FortWrap.h"

int main(void)
{

  std::complex<float> xf1(1.0, 2.0), xf2(3.0, 4.0);

  if (FortFuncs::add_complex(xf1, xf2) != xf1 + xf2)
    return 1;
  if (FortFuncs::add_complex4(xf1, xf2) != xf1 + xf2)
    return 2;
  if (FortFuncs::add_complex_cfloat(xf1, xf2) != xf1 + xf2)
    return 3;
  if (FortFuncs::add_complex_byval(xf1, xf2) != xf1 + xf2)
    return 4;

  std::complex<double> xd1(1.0, 2.0), xd2(3.0, 4.0);

  if (FortFuncs::add_complex8(xd1, xd2) != xd1 + xd2)
    return 10;
  if (FortFuncs::add_complex_cdouble(xd1, xd2) != xd1 + xd2)
    return 11;

  std::vector<std::complex<float>> xvec;
  xvec.push_back(xf1);
  xvec.push_back(xf2);
  if (FortFuncs::complex_sum(&xvec) != xf1 + xf2)
    return 20;
  
  return 0;
}

