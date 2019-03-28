include ../Tests.mk

# Enforce build order so that .mod file is available
module_b.o: module_b.f90 module_a.o
	$(FC) $(FFLAGS) -o $@ -c $<

