# Changes

## 2.1.3
- Fix use of ordered dictionaries causing problems on some newer
  versions of Python

## 2.1.2
- Added `%pattern` keyword to interface file to facilitate bulk renaming

## 2.1.1
- Minor fixes to command option names

## 2.1.0
- Upgraded code to be compatible with Python 2 and 3.  Should work on
  Python 2 even if the "future" package is not available.
- Updated argument parsing to use argparse instead of getopt

## 2.0.2
- Minor fix for procedure pointer storage size on 64-bit platforms

## 2.0.1
- Minor fixes for wrapping CLASS data

## 2.0
- Experimental support for Fortran object oriented features:
  - Wrapping of type bound procedures
  - Fortran type extension translated into C++ classes with
    inheritance structure
  - Support for abstract types and deferred procedures (translated
    into pure virtual methods in C++)
- Minor fixes:
  - Fix to prevent argument clash when wrapping procedures that
    contain other procedure definitions with the same argument names
  - Fix to not use case-sensitivity when matching names in the
    argument list to their type declarations

## 1.0.5
- Added option `--no-std-string`, which uses a wrapper class to manage
  character output argument conversions, instead of `std::string`.  This
  can eliminate C++ library conflicts in some cases.
- Fix for renaming of derived types with the interface file
- Fix to make derived type name matching case insensitive
- Proper exclusion of ALLOCATABLE and POINTER arguments from wrapper
  code

## 1.0.4
- Fixing bug for optional assumed length strings

## 1.0.3
- Added support for assumed length strings (intent(in) and
  intent(out))

## 1.0.2
- Added support for `INTEGER(8)`
- Added option to specify name of constants class

## 1.0.1
- Added support for enumerations

## 1.0
- Added option to disable wrapping non-method procedures
- Added option to suppress warnings for procedures not wrapped
- Added option to change name of main header file
- Added support for KIND spec's without KIND keyword (e.g. REAL(8)).
- Added support for `C_INT`, `C_FLOAT`, and `C_DOUBLE` KIND spec's
- Added support for use of named PARAMETER in KIND spec
- Added capability to customize regular expressions used for
  identifying constructors and destructors
- Better handling of ABSTRACT INTERFACE
- Better handling of comments

## 0.9.17
- Minor fix for newline before `#include <stdlib.h>`
- Changing `<stdlib.h>` to `<cstdlib>`

## 0.9.16
- New option `--array-as-ptr` for wrapping 1-D arrays with '*' instead
  of '[]'
- Minor tweak to support re-wrapping with swig -includeall
- Fix for return codes

## 0.9.15
- Added support for higher dimensional (>2) arrays
- Added option to turn off use of FortranMatrix wrapper class
- Changed `--c-arrays` option to `--no-vector`

## 0.9.14
- Fixes for parsing lower case type definitions, such as 'real'

## 0.9.13
- Changed default compiler from g95 to gfortran

## 0.9.12
- Added support for integer kinds 1,2,4
- Added support for character length specification via '*':
  `CHARACTER*20`
- Improved robustness for processing Fortran integer parameter
  declarations, which might be used in string length definitions

## 0.9.11
- Added capability to wrap non-module (top-level) procedures
- Added exception handler to gracefully handle FortWrap internal
  errors
- Improved error and warning messages
- Fixes to handle data statements with * and KIND; however, wrapper
  generation is currently only supported for `REAL*8`, `REAL(KIND=4)`, and
  `REAL(KIND=8)`
- Fixed bug parsing assumed shape arrays
- Added support for `DOUBLE PRECISION` statement
- Added error handling for COMPLEX types (which are not supported)

## 0.9.10
- Fixed bug parsing functions with array return values
- Fixed bug handling DIMENSION when it is followed by INTENT

## 0.9.9
- Added support for string arguments that are optional
- Changed interface for intent(in) strings to use character arrays,
  which makes it easier to pass string literals
- Fixed bug using intent(out) strings with functions
- Made run_tests.py script more portable: should now work on Windows
  under msys and cygwin (and possibly cmd.exe)

## 0.9.8
- Added a brief walkthrough to the manual
- Added support for intent(in) strings

## 0.9.7
- Fixed issues parsing high-dimensional arrays and certain
  declarations with DIMENSION
- Fix for makefile (had CPPFLAGS and CXXFLAGS mixed up, and it was
  working with gcc 4.1 but not with 4.6)
- Tweaks to function_pointer and array tests for improved
  compatibility
- Correction to manual: FortWrap actually does support assumed size
  arrays

## 0.9.6
- Added support for a configuration file (-i command option), which
  allows more control over the wrapping process (renaming or ignoring
  objects/procedures, etc.)
- More additions to documentation

## 0.9.5
- Fixed bug for processing compiler specification with -c

## 0.9.4
- Added capability to list files to wrap on the command line
- Added option `--global` to wrap non-method procedures as global
  functions instead of static members of dummy class
- Added option `--dummy-class` for specifying the name of the class used
  to wrap non-method procedures
- Improved error checking for invalid source files
- Exit without generating any files if there is nothing to wrap

## 0.9.3
- Added option `--c-arrays` to generate C-style array wrappers instead
  of C++ vector containers

## 0.9.2 
- Added proper handling for top-level procedures (print warning) and
  multi-module files
- Expanded documentation

## 0.9.1 
- Added support for lowercase Fortran keywords

## 0.9 
- Initial release
