# Current workflow for use with fortwrap_thermopack_demo :

First: `pip install -e .`

Navigate to the `fortwrap_thermopack_demo` directory (from now reffered to as `demo`). 

The script `build.sh` does the following:

 * Compile the Fortran source
 * Build the archive `libdemo_fortran.a`
 * Move the archive and `.mod` files to the `demo/wrappers` directory
 * Run `python -m fortwrap -g -i ../FortWrapOptions.txt` from the `demo/src` directory
 * Compile the resulting `FortranISOWrappers.f90` and add it to the `libdemo_fortran.a` archive
 * Compile the `.cpp` files in the `demo/wrappers` directory
 * Compile `demo/main.cpp`

To build the python wrappings, you must additionally run `cmake ..` from the `demo/wrappers/build` directory.

The `demo/wrappers/CMakeLists.txt` does the following:

 * Define the `pybind_module` target
 * Add the `demo/wrappers/.cpp` files to the target
    * It is the binding module `pybind11_bindings.cpp` that is used, not `bindings.cpp`
 * Link to the archive `libdemo_fortran.a`
 * Link to `libgfortran.dylib`

# FortWrap

FortWrap is a python script that parses Fortran 90/95/2003 source files and
generates wrapper code for interfacing with the original Fortran code
from C++. FortWrap is intended to be used with Fortran code that takes
an object oriented approach and makes use of Fortran derived
types. The resulting wrapper code provides a C++ interface that wraps
the Fortran derived types with C++ "proxy classes".

Currently, FortWrap is targetted at the gfortran compiler,
but the generated C++ code should work with any C++ compiler,
including g++.

## News

The wrapping approach is currently being reworked to use
`ISO_C_BINDING`, making it portable for use with different compilers
and eliminating current reliance on gfortran-specific ABI conventions.
This work can be followed on the `iso_c_binding` branch, which
includes a test suite that covers essentially the same feature
set as the current release version.

Here is a preview of the `ISO_C_BINDING` wrapping.  Start with a
source function:

``` Fortran
  SUBROUTINE set_string(o, s)
    TYPE (Object) :: o
    CHARACTER(len=*), INTENT(in) :: s
  END SUBROUTINE set_string
```

FortWrap generates the following Fortran C binding wrapper:

``` Fortran
  SUBROUTINE example__set_string_wrap(o, s, s_len__) BIND(C)
    TYPE(C_PTR), VALUE :: o
    TYPE(C_PTR), VALUE :: s
    INTEGER(C_SIZE_T), VALUE :: s_len__
    TYPE(Object), POINTER :: o__p
    CHARACTER(s_len__), POINTER :: s__p
    CALL C_F_POINTER(o, o__p)
    CALL C_F_POINTER(s, s__p)
    CALL set_string(o__p, s__p)
  END SUBROUTINE example__set_string_wrap
```

As well as the associated C prototype:

``` C
void example__set_string_wrap(void* o, const char* s, size_t s_len__);
```

And the following C++ method of the C++ proxy class (notice that the C++ code passes the actual string length "behind the scenes"):

``` C++
void Object::set_string(const char* s) {
  int s_len__ = 0;
  if (s) s_len__ = strlen(s); // Protect Optional args
  example__set_string_wrap(data_ptr, s, s_len__);
}
```

Refer to the documentation and tests on the `iso_c_binding` branch for
more information on this wrapping approach.

## Checklist

Things to consider before using FortWrap:

* No legacy code:
  * All code to be wrapped must be contained in a module
  * Procedure dummy arguments must be defined using `::` syntax
* String wrapping:
  * Strings must be either `INTENT(IN)` or `INTENT(OUT)`
  * Arrays of strings are not supported (consider using a container type or a "get/set" API with scalar arguments)
* Arguments can not have `POINTER` or `ALLOCATABLE` attribute
  * Exception: Derived type (including `CLASS`) function return values may have the `POINTER` attribute
* Arrays of derived types are not supported
* Procedure pointer arguments must use an abstract interface

## Features

* Fortran derived types wrapped in C++ proxy classes using opaque pointer handling
* Experimental support for polymorphism (CLASS variables)
* Arrays wrapped with C++ vectors by default
* Wrapping of assumed size and assumed shape arrays
* Support for optional arguments
* Support for wrapping procedure pointers
* Support for string arguments
* Support for `COMPLEX` arguments
* Fortran doxygen comments transferred to C++ header files
* Wrappers respect Fortran public/private statements
* Generated code can be re-wrapped with swig -c++

## Installation

Download `fortwrap.py`.  Optionally, make it executable and place it in your PATH.

## Running FortWrap

`fortwrap.py` is a standalone executable python script that may be run using
either `python fortwrap.py [args]` or `fortwrap.py [args]`.  Use `fortwrap.py
-h` to print usage information.

## Documentation

Refer to the documentation at `docs/manual.md`.  The `tests` directory
provides working examples of most of the main FortWrap features.

## Running the tests

The easiest way to get started is to look at the simple test programs
in the tests directory.

The root directory contains a python script `run_tests.py` and
Makefiles that are currently set up to run the tests using the
`gfortran` compiler.  However, for the most part, the tests should be
compatible with other compilers as well.  For each test, the script
will change to the individual test directory, execute fortwrap.py to
generate wrapper code, execute `make` to compile and link a simple
test program, and finally run the test program.

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
is set up to wrap, refer to the documentation and the tests
directory.

## Examples

For simplicity, some of the examples below are not
shown with derived types.  When the first argument is not a
derived type, FortWrap by default wraps the routine as a static
method of the special "utility class" `FortFuncs` (this can
be overriden with the `--global` option).


### Derived Types

"`ctor`" procedures are wrapped as C++ constructors.
Multiple constructors are supported.  "`dtor`" procedures
are automatically called by the C++ destructor.  For
example:

``` Fortran
MODULE m

  TYPE Object
    REAL, ALLOCATABLE :: x(:)
  END TYPE Object

CONTAINS

  SUBROUTINE default_ctor(o,n)
    TYPE(Object) :: o
    INTEGER, INTENT(in) :: n
    ALLOCATE(o%x(n))
  END SUBROUTINE default_ctor

  SUBROUTINE value_ctor(o,n,val)
    TYPE(Object) :: o
    INTEGER, INTENT(in) :: n
    REAL, INTENT(in) :: val
    ALLOCATE(o%x(n))
    o%x = val
  END SUBROUTINE value_ctor

  SUBROUTINE object_dtor(o)
    TYPE(Object) :: o
    IF(ALLOCATED(o%x)) DEALLOCATE(o%x)
  END SUBROUTINE object_dtor

END MODULE m
```

will generate multiple constructors for the C++
class `Object`:

``` C++
Object(int n);
Object(int n, float val);
```

The Fortran destructor `object_dtor` will automatically
be called by the C++ destructor.

### Arrays

``` Fortran
FUNCTION inner_prod(n,a,b) RESULT(y)
  INTEGER, INTENT(in) :: n, a(n), b(n)
  INTEGER :: y
  y = DOT_PRODUCT(a,b)
END FUNCTION inner_prod
```

generates a method of the "utility class" `FortFuncs`
(the utility class is used to wrap functions that do not operate
on a derived type):

``` C++
static int inner_prod(const std::vector<int>* a, const std::vector<int>* b);
```

### Optional Arguments

``` Fortran
FUNCTION add_mixed(a,b,c,d) RESULT(y)
  INTEGER, INTENT(in) :: a,b
  INTEGER, INTENT(in), OPTIONAL :: c,d
  INTEGER :: y
  y = a+b
  IF (PRESENT(c)) y = y + c
  IF (PRESENT(d)) y = y + d
END FUNCTION add_mixed
```

generates the following method:

``` C++
static int add_mixed(int a, int b, const int* c=NULL, const int* d=NULL);
```

Note that `a` and `b` use pass-by-value since
they are not optional.  The optional arguments `c`
and `d` use pass-by-reference.  Passing `NULL`
(which is the default) indicates that the argument is not
provided.
    
These wrappers are particularly powerful when using swig
with `-c++ -keyword`, since the optional parameters can
then be passed by keyword in the target language

## Known Issues

* Scalar character arguments not wrapped correctly (generated code won't compile)
* Old-style dummy argument definitions that do not include `::` are not recognized
* Enumerators are not wrapped correctly if some names within a set are made private
