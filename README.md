# FortWrap

FortWrap is a python script that parses Fortran 90/95/200X source files and
generates wrapper code for interfacing with the original Fortran code
from C++. FortWrap is intended to be used with Fortran code that takes
an object oriented approach and makes use of Fortran derived
types. The resulting wrapper code provides a C++ interface that wraps
the Fortran derived types with C++ "proxy classes".

Currently, FortWrap is targetted at the gfortran compiler,
but the generated C++ code should work with any C++ compiler,
including g++.

## Features

* Fortran derived types wrapped in C++ proxy classes
* Experimental support for polymorphism (CLASS variables)
* Arrays wrapped with C++ vectors
* Support for optional arguments
* Support for wrapping procedure pointers
* Support for string arguments
* Fortran doxygen comments transferred to C++ header files
* Name mangling support for gfortran compiler
* Wrappers respect Fortran public/private statements
* Generated code can be re-wrapped with swig -c++

## Running FortWrap

`fortwrap.py` is a standalone executable python script that may be run using
either `python fortwrap.py [args]` or `fortwrap.py [args]`.  Use `fortwrap.py
-h` to print usage information.

## Documentation

Refer to the html documentation at `docs/manual.html`.  The `tests` directory
provides working examples of most of the main FortWrap features.

## Running the tests

The easiest way to get started is to look at the simple test programs
in the tests directory.

To run the tests, edit the makefile `tests/Tests.mk` to specify your
Fortran compiler (currently only tested with gfortran).  The root directory contains a python script
`run_tests.py` to execute all tests.  For each test, the script will
change to the individual test directory, execute fortwrap.py to
generate wrapper code, execute make to compile and link a simple test
program, and finally run the test program.

To manually run a test, first make sure the compiler specified in
tests/Tests.mk is valid.  Then change to a test directory, for
example, tests/arrays.  Execute `../../fortwrap.py -g -d wrap`
to generate the C++ wrapper code (some tests, for example c_arrays,
require different FortWrap options, which are defined in
run_tests.py).  Then execute `make` to build the simple test program
in that directory, prog.cpp.

## Notes

The internals of FortWrap are in a stable state and have been used
successfully to wrap very large Fortran projects (~40,000 lines of
code).  However, FortWrap is not intended to wrap all Fortran
constructs.  In particular, FortWrap is geared towards wrapping
derived types and procedures that operate on them.  FortWrap is not
intended to wrap legacy code and should not be used with Fortran 77
code.  For more details regarding the Fortran constructs that FortWrap
is set up to wrap, refer to the html documentation and the tests
directory.
