FC=mpiifort
OPTS=-convert big_endian -assume byterecl -align array64byte,all -fpic -qopenmp -qopenmp-threadprivate compat -fp-model strict -ftz
PACKDIR=/dodrio/scratch/projects/starting_2022_075/accord/rootpack/43t2_bf.11.INTEL2021_6_0.x/
INCS=$(shell cat $(PACKDIR)/src/.incpath.local $(PACKDIR)/src/.modpath.local)
LIBS=-L$(PACKDIR)/lib -lifsaux.local -L/dodrio/scratch/projects/starting_2022_075/accord/software/iimpi2022a/lib64/ -leccodes_f90 -leccodes \
	-L/dodrio/scratch/projects/starting_2022_075/accord/software/iimpi2022a/auxlibs/lib/ -lgribex

all: replace_orography

replace_orography: replace_orography.F90
	$(FC) $(INCS) $(OPTS) -o replace_orography replace_orography.F90 $(LIBS)

clean:
	rm replace_orography
