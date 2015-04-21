FC = gfortran
CXX = g++
CXXFLAGS = -g
FFLAGS = -g

OBJ = shapes.o

FPROGS = ftest
CXXPROGS = cpptest

PROGS = $(FPROGS) $(CXXPROGS)

COBJ = $(OBJ) CppWrappers.o Shape.o

all: $(OBJ) $(PROGS)

$(FPROGS): %: %.o $(OBJ)
	$(FC) -o $@ $^

$(CXXPROGS): %: %.o $(COBJ)
	$(FC) -o $@ $^ -lstdc++

%.o: %.f90
	$(FC) $(FFLAGS) -c $^

clean:
	rm -f *.mod *.o $(PROGS)
