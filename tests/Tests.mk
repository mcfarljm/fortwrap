FC = gfortran
CXXFLAGS = -Wall -g -I $(WRAP) -std=c++11 -pedantic-errors
# Replace -std=f2008ts with =std=f2018 on newer versions of gfortran
FFLAGS = -g -I $(WRAP) -std=f2008ts

# Directory for wrapper code
WRAP = wrap

FOBJ = $(patsubst %.f90,%.o,$(wildcard *.f90) $(wildcard $(WRAP)/*.f90))
CPPOBJ = $(patsubst %.cpp,%.o,$(wildcard $(WRAP)/*.cpp))

OBJ = $(FOBJ) $(CPPOBJ)

prog: prog.o $(OBJ)
	$(FC) -o $@ $^ -lstdc++

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $^

clean:
	rm -f *.o *.mod $(WRAP)/*.o

realclean:
	make clean
	rm -f prog wrap/*
