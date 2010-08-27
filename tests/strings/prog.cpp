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

  // Test intent(in)
  if (! FortFuncs::string_in_test("Test String"))
    return 3;

  // Test combination
  FortFuncs::multiple_args(0,"Test String",0.0,&s);
  if (s.compare("Test String suffix") != 0)
    return 4;

  return 0;
}

