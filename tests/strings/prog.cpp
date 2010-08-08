#include "FortWrap.h"
#include <string>
#include <iostream>

int main(void)
{

  std::string s;

  FortFuncs::string_literal_len(&s);
  if (s.compare("String A") != 0)
    return 1;

  FortFuncs::string_param_len(&s);
  if (s.compare("String B") != 0)
    return 2;

  return 0;
}

