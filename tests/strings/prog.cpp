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

  std::string sa1, sa2;
  // Note: assumed-length output arguments will determine the size of
  // the temporary array (which actually gets passed to the Fortran
  // code) based on the size of the input string.  Thus, the input
  // string should be initialized or resized before calling
  sa1.resize(10); sa2.resize(10);

  FortFuncs::string_out_assumed_len(&sa1, 0, &sa2);
  if (sa1.compare("String A") != 0 || sa2.compare("String B") != 0)
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
  if (FortFuncs::optional_out() != 0) // Not present
    return 15;

  sa1.resize(11);
  if (FortFuncs::optional_out_assumed(&sa1) != 1) // Present
    return 16;
  if (sa1.compare("Test String") != 0)
    return 17;
  if (FortFuncs::optional_out_assumed() != 0) // Not present
    return 18;

  return 0;
}

