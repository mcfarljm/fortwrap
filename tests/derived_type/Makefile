FC = g95
CPPFLAGS = -I $(WRAP)
FFLAGS = -I $(WRAP)

WRAP = wrap

FOBJ = DerivedTypes.o $(WRAP)/CppWrappers.o
CPPOBJ = $(WRAP)/ObjectA.o $(WRAP)/ObjectB.o
OBJ = $(FOBJ) $(CPPOBJ)

prog: prog.o $(OBJ)
	$(FC) -o $@ $^ -lstdc++

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $^

clean:
	rm -f *.o *.mod $(WRAP)/*.o