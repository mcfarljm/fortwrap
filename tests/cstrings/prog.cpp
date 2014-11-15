#include "FortWrap.h"
#include <iostream>
#include <cstring>

int main(void)
{
  const int size = 20;
  char s[size+1], s2[size+1], s3[size+1], s4[size+1];

  FortFuncs::string_out_literal_len(s, size);
  if (strcmp(s,"String A") != 0)
    return 1;

  FortFuncs::string_out_literal_len2(s, size);
  if (strcmp(s,"String A") != 0)
    return 2;

  FortFuncs::string_out_param_len(s, size, s2, size, s3, size, s4, size);
  if (strcmp(s,"String A") != 0 || strcmp(s2,"String B") != 0 ||
      strcmp(s3,"String C") != 0 || strcmp(s4,"String D") != 0)
    return 3;
  
  FortFuncs::string_out_param_len2(s, size, s2, size, s3, size, s4, size);
  if (strcmp(s,"String A") != 0 || strcmp(s2,"String B") != 0 ||
      strcmp(s3,"String C") != 0 || strcmp(s4,"String D") != 0)
    return 4;

  const int size2=10;
  char sa1[size2+1], sa2[size2+1];

  FortFuncs::string_out_assumed_len(sa1, size2, 0, sa2, size2);
  if (strcmp(sa1,"String A") != 0 || strcmp(sa2,"String B") != 0)
    return 5;  

  // Test intent(in)
  if (! FortFuncs::string_in_test("Test String"))
    return 6;

  if (! FortFuncs::string_in_cutoff("Test String"))
    return 7;

  if (! FortFuncs::string_in_assumed_len("Test String 1", 0, "String 2"))
    return 8;

  // Test combination
  FortFuncs::multiple_args(0,"Test String",0.0,s,size);
  if (strcmp(s,"Test String suffix") != 0)
    return 9;

  // Test optionals

  // Intent(in) optionals
  if (FortFuncs::optional_in("Test String") != 1) // Present
    return 10;
  if (FortFuncs::optional_in() != 2) // Not present
    return 11;

  if (FortFuncs::optional_in_assumed("Test String") != 1) // Present
    return 12;
  if (FortFuncs::optional_in_assumed() != 2) // Not present
    return 13;  

  // Intent(out) optionals
  if (FortFuncs::optional_out(s,size) != 1) // Present
    return 14;
  if (strcmp(s,"Test String") != 0)
    return 15;
  if (FortFuncs::optional_out() != 0) // Not present
    return 16;

  char sb1[12];
  if (FortFuncs::optional_out_assumed(sb1, 11) != 1) // Present
    return 17;
  if (strcmp(sb1,"Test String") != 0)
    return 18;
  if (FortFuncs::optional_out_assumed() != 0) // Not present
    return 19;

  return 0;
}

