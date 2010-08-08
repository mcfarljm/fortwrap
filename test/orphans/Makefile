FC = g95
CPPFLAGS = -I $(WRAP)
FFLAGS = -I $(WRAP)

WRAP = wrap

FOBJ = orphans.o
CPPOBJ = $(WRAP)/FortFuncs.o
OBJ = $(FOBJ) $(CPPOBJ)

prog: prog.o $(OBJ)
	$(CXX) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $^

clean:
	rm -f *.o *.mod $(WRAP)/*.o