if [ $# -eq 0 ]; then
    echo Usage: $0 testdir [fort_source_file]
    exit
fi

testdir=$1

if [ $# -eq 1 ]; then
    fortfile=$1
else
    fortfile=$2
fi

cd tests
svn mkdir $testdir
cd $testdir
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
echo -e "MODULE $fortfile\n\n  IMPLICIT NONE\n\nCONTAINS\n\n\nEND MODULE $fortfile" > $fortfile.f90
svn add $fortfile.f90
# Set up C++ test program
echo -e "#include \"FortWrap.h\"\n\nint main(void)\n{\n\n  return 0;\n}\n" > prog.cpp
svn add prog.cpp
