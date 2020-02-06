FC=ifort
FFLAGS=-O2 -qopenmp
# FC=gfortran
# FFLAGS=-O2 -fopenmp
#
SRCS=$(wildcard *.f90)
EXES=$(patsubst %.f90,%.out,$(SRCS))

all: $(EXES)

clean:
	rm $(EXES)

%.out: %.f90
	$(FC) $(FFLAGS) -o $@ $<




