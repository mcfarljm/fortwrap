FC = gfortran
CXXFLAGS = -Wall -g -I $(WRAP) -std=c++11 -pedantic-errors
FFLAGS = -g -I $(WRAP)

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
