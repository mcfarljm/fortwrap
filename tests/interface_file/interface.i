Any line not starting with "%" is treated as a comment and ignored

Add some routines that are defined as PRIVATE in the Fortran code:
%include object_constructor_i
%include object_constructor_f

Use different regular expressions for matching constructor:
%ctor .*_constructor

Create a more appropriate method names for the C+ interface.
Leave the second argument blank to use empty replacement text:
%pattern ^object_

Exclude a procedure:
%ignore work_sub

Test for renaming a derived type
%rename Object_to_rename Steve
%rename object2_val val

Test ignoring a derived TYPE
%ignore Object_to_ignore
