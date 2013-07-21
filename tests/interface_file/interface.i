Any line not starting with "%" is treated as a comment and ignored

Add some routines that are defined as PRIVATE in the Fortran code:
%include object_constructor_i
%include object_constructor_f

Use different regular expressions for matching constructor:
%ctor .*_constructor

Create a more appropriate method name for the C+ interface:
%rename object_add add

Exclude a procedure:
%ignore work_sub

Test for renaming a derived type
%rename Object_to_rename Steve
%rename object2_val val
