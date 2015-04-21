FC = gfortran
CXX = g++
CXXFLAGS = -g
FFLAGS = -g

OBJ = shapes.o

FPROGS = ftest
CPROGS = ctest

PROGS = $(FPROGS) $(CPROGS)

COBJ = $(OBJ) CppWrappers.o Shape.o

all: $(OBJ) $(PROGS)

$(FPROGS): %: %.o $(OBJ)
	$(FC) -o $@ $^

$(CPROGS): %: %.o $(COBJ)
	$(FC) -o $@ $^ -lstdc++

%.o: %.f90
	$(FC) $(FFLAGS) -c $^

clean:
	rm -f *.mod *.o $(PROGS)
