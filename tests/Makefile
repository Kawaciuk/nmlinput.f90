src = main.f90
lib = nmlinput.f90
libdir = ../src

FC := gfortran

all: test

test: main.out
	./$^

main.out: $(src) nmlinput.o
	$(FC) $(FCFLAGS) $^ -o $@

nmlinput.o: $(libdir)/$(lib)
	$(FC) $(FCFLAGS) -c $< -o $@

.PHONY: all clean test
