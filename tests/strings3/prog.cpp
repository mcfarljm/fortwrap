#include "FortWrap.h"
#include <string>
#include <iostream>
#include <cstring>

int main(void)
{

  const int CHARLEN=32;

  char s[CHARLEN], s2[CHARLEN], s3[CHARLEN], s4[CHARLEN];

  FortFuncs::string_out_literal_len(s);
  if (strcmp(s, "String A") != 0)
    return 1;

  FortFuncs::string_out_literal_len2(s);
  if (strcmp(s, "String A") != 0)
    return 2;

  FortFuncs::string_out_param_len(s, s2, s3, s4);
  if (strcmp(s, "String A") != 0 || strcmp(s2, "String B") != 0 ||
      strcmp(s3, "String C") != 0 || strcmp(s4, "String D") != 0)
    return 3;

  FortFuncs::string_out_param_len2(s, s2, s3, s4);
  if (strcmp(s, "String A") != 0 || strcmp(s2, "String B") != 0 ||
      strcmp(s3, "String C") != 0 || strcmp(s4, "String D") != 0)
    return 3;

  const int CHARLEN2=10;
  char sa1[CHARLEN2], sa2[CHARLEN2];

  FortFuncs::string_out_assumed_len(CHARLEN2, sa1, 0, CHARLEN2, sa2);
  if (strcmp(sa1, "String A") != 0 || strcmp(sa2, "String B") != 0)
    return 4;  

  // Test intent(in)
  if (! FortFuncs::string_in_test("Test String"))
    return 5;

  if (! FortFuncs::string_in_cutoff("Test String"))
    return 6;

  if (! FortFuncs::string_in_assumed_len("Test String 1", 0, "String 2"))
    return 7;

  // Test combination
  FortFuncs::multiple_args(0,"Test String",0.0,s);
  if (strcmp(s, "Test String suffix") != 0)
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
  if (FortFuncs::optional_out(s) != 1) // Present
    return 13;
  if (strcmp(s, "Test String") != 0)
    return 14;
  if (FortFuncs::optional_out() != 0) // Not present
    return 15;

  if (FortFuncs::optional_out_assumed(CHARLEN, s) != 1) // Present
    return 16;
  if (strcmp(s, "Test String") != 0)
    return 17;
  if (FortFuncs::optional_out_assumed(CHARLEN) != 0) // Not present
    return 18;

  return 0;
}

