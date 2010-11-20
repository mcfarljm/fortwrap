#include "FortWrap.h"
#include <string>
#include <iostream>

int main(void)
{

  std::string s, s2, s3, s4;

  FortFuncs::string_literal_len(&s);
  if (s.compare("String A") != 0)
    return 1;

  FortFuncs::string_param_len(&s, &s2, &s3, &s4);
  if (s.compare("String A") != 0 || s2.compare("String B") != 0 ||
      s3.compare("String C") != 0 || s4.compare("String D") != 0)
    return 2;

  // Test intent(in)
  if (! FortFuncs::string_in_test("Test String"))
    return 3;

  // Test combination
  FortFuncs::multiple_args(0,"Test String",0.0,&s);
  if (s.compare("Test String suffix") != 0)
    return 4;

  // Test optionals

  // Intent(in) optionals
  if (FortFuncs::optional_in("Test String") != 1) // Present
    return 5;
  if (FortFuncs::optional_in() != 2) // Not present
    return 6;

  // Intent(out) optionals
  if (FortFuncs::optional_out(&s) != 1) // Present
    return 7;
  if (s.compare("Test String") != 0)
    return 8;

  return 0;
}

