FC = gfortran
CXX = g++
CXXFLAGS = -g
FFLAGS = -g
CFLAGS = -g

OBJ = shapes.o

FPROGS = ftest
CPROGS = ctest
CXXPROGS = cpptest

PROGS = $(FPROGS) $(CXXPROGS) $(CPROGS)

COBJ = $(OBJ) CppWrappers.o
CXXOBJ = $(OBJ) CppWrappers.o Shape.o

all: $(OBJ) $(PROGS)

$(FPROGS): %: %.o $(OBJ)
	$(FC) -o $@ $^

$(CXXPROGS): %: %.o $(CXXOBJ)
	$(FC) -o $@ $^ -lstdc++

$(CPROGS): %: %.o $(COBJ)
	$(FC) -o $@ $^


%.o: %.f90
	$(FC) $(FFLAGS) -c $^

clean:
	rm -f *.mod *.o $(PROGS)
