#include "FortWrap.h"
#include <string>
#include <iostream>

int main(void)
{

  std::string s, s2, s3, s4;

  FortFuncs::string_out_literal_len(&s);
  if (s.compare("String A") != 0)
    return 1;

  FortFuncs::string_out_literal_len2(&s);
  if (s.compare("String A") != 0)
    return 2;

  FortFuncs::string_out_param_len(&s, &s2, &s3, &s4);
  if (s.compare("String A") != 0 || s2.compare("String B") != 0 ||
      s3.compare("String C") != 0 || s4.compare("String D") != 0)
    return 3;

  FortFuncs::string_out_param_len2(&s, &s2, &s3, &s4);
  if (s.compare("String A") != 0 || s2.compare("String B") != 0 ||
      s3.compare("String C") != 0 || s4.compare("String D") != 0)
    return 3;

  FortFuncs::string_out_assumed_len(&s, 0, &s2);
  if (s.compare("String A") != 0 || s2.compare("String B") != 0)
    return 4;  

  // Test intent(in)
  if (! FortFuncs::string_in_test("Test String"))
    return 5;

  if (! FortFuncs::string_in_cutoff("Test String"))
    return 6;

  if (! FortFuncs::string_in_assumed_len("Test String 1", 0, "String 2"))
    return 7;

  // Test combination
  FortFuncs::multiple_args(0,"Test String",0.0,&s);
  if (s.compare("Test String suffix") != 0)
    return 8;

  // Test optionals

  // Intent(in) optionals
  if (FortFuncs::optional_in("Test String") != 1) // Present
    return 9;
  if (FortFuncs::optional_in() != 2) // Not present
    return 10;

  if (FortFuncs::optional_in_assumed("Test String") != 1) // Present
    return 11;
  if (FortFuncs::optional_in_assumed() != 2) // Not present
    return 12;  

  // Intent(out) optionals
  if (FortFuncs::optional_out(&s) != 1) // Present
    return 13;
  if (s.compare("Test String") != 0)
    return 14;

  return 0;
}

