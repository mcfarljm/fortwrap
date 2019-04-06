#include "FortWrap.h"

int main(void)
{

  std::complex<float> x1(1.0, 2.0), x2(3.0, 4.0);

  if (FortFuncs::add_complex(x1, x2) != x1 + x2)
    return 1;

  return 0;
}

