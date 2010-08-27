#include "FortWrap.h"
#include <string>
#include <iostream>

int main(void)
{

  std::string s1;

  FortFuncs::string_literal_len(&s1);
  if (s1.compare("String A") != 0)
    return 1;

  FortFuncs::string_param_len(&s1);
  if (s1.compare("String B") != 0)
    return 2;

  // Test intent(in)
  s1 = "Test String";
  if (! FortFuncs::string_in_test(&s1))
    return 3;

  // Test combination
  std::string s2;
  FortFuncs::multiple_args(0,&s1,0.0,&s2);
  if (s2.compare("Test String suffix") != 0)
    return 4;

  return 0;
}

