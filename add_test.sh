if [ $# -ne 2 ]; then
    echo Usage: $0 testdir fort_source_file
    exit
fi

cd tests
svn mkdir $1
cd $1
# Set svn props
echo -e '*.mod\nprog' > tmp
svn ps svn:ignore --file tmp .
'rm' tmp
# Create wrap dir
svn mkdir wrap
svn ps svn:ignore '*' wrap
# Add Makefile
cp ../orphans/Makefile .
svn add Makefile
# Set up Fortran source file
echo -e "MODULE $2\n\n  IMPLICIT NONE\n\nCONTAINS\n\n\nEND MODULE $2" > $2.f90
svn add $2.f90
# Set up C++ test program
echo -e "#include \"FortWrap.h\"\n\nint main(void)\n{\n\n  return 0;\n}\n" > prog.cpp
svn add prog.cpp
